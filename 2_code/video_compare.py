#!/usr/bin/env python3
"""
Multimodal Video Similarity Analysis
======================================
Three complementary similarity measures in one report:

1. CLIP (visual)     — do the videos *look* alike?
2. CLAP (acoustic)   — do the videos *sound* alike?
3. Whisper + SentEmb — do the narrations *say* the same things?

Setup (M2 Mac):
    conda activate clip
    pip install torch torchvision open-clip-torch
    pip install openai-whisper sentence-transformers
    pip install librosa soundfile transformers
    pip install opencv-python matplotlib numpy

Usage:
    python video_compare.py --video1 "video_a.mp4" --video2 "video_b.mov" \
                            --label1 "Treatment A" --label2 "Treatment B" \
                            --max_frames 200 --output comparison_report

    # Skip a modality if needed:
    python video_compare.py ... --skip_clap
    python video_compare.py ... --skip_whisper
    python video_compare.py ... --skip_clip
"""

import argparse
import os
import sys
import subprocess
import tempfile
from datetime import datetime

import cv2
import numpy as np
import torch
import torch.nn.functional as F


# ═══════════════════════════════════════════════════════════════════════════
# VIDEO / AUDIO LOADING
# ═══════════════════════════════════════════════════════════════════════════

def extract_frames(video_path, max_frames=200, resize=224):
    cap = cv2.VideoCapture(video_path)
    if not cap.isOpened():
        sys.exit(f"ERROR: cannot open {video_path}")

    total = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    fps = cap.get(cv2.CAP_PROP_FPS)
    duration = total / fps if fps > 0 else 0

    if total <= max_frames:
        indices = list(range(total))
    else:
        indices = np.linspace(0, total - 1, max_frames, dtype=int).tolist()

    frames = []
    for idx in indices:
        cap.set(cv2.CAP_PROP_POS_FRAMES, idx)
        ret, frame = cap.read()
        if not ret:
            continue
        frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
        frame = cv2.resize(frame, (resize, resize))
        frames.append(frame)
    cap.release()

    return np.array(frames), duration, fps


def extract_audio(video_path, output_wav, sr=48000):
    cmd = [
        'ffmpeg', '-y', '-i', video_path,
        '-vn', '-acodec', 'pcm_s16le',
        '-ar', str(sr), '-ac', '1',
        output_wav
    ]
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"    ffmpeg error: {result.stderr[-300:]}")
        sys.exit(f"ERROR: ffmpeg failed on {video_path}")


def load_audio(wav_path, sr=48000):
    import soundfile as sf
    audio, file_sr = sf.read(wav_path, dtype='float32')
    # Already mono from ffmpeg, but just in case
    if audio.ndim > 1:
        audio = audio.mean(axis=1)
    return audio, len(audio) / file_sr


# ═══════════════════════════════════════════════════════════════════════════
# 1. CLIP — VISUAL SIMILARITY
# ═══════════════════════════════════════════════════════════════════════════

def run_clip(frames1, frames2, device):
    import open_clip
    from PIL import Image

    print("  Loading CLIP model...")
    model, _, preprocess = open_clip.create_model_and_transforms(
        'ViT-B-32', pretrained='laion2b_s34b_b79k', device=device)
    model.eval()

    def encode(frames, batch_size=32):
        embeddings = []
        for i in range(0, len(frames), batch_size):
            batch = frames[i:i + batch_size]
            tensors = torch.stack([
                preprocess(Image.fromarray(f)) for f in batch
            ]).to(device)
            with torch.no_grad():
                emb = model.encode_image(tensors)
            emb = F.normalize(emb, p=2, dim=-1)
            embeddings.append(emb.cpu())
        return torch.cat(embeddings, dim=0)

    print("  Encoding frames...")
    emb1 = encode(frames1)
    emb2 = encode(frames2)

    sim_matrix = (emb1 @ emb2.T).numpy()

    mean1 = F.normalize(emb1.mean(dim=0, keepdim=True), p=2, dim=-1)
    mean2 = F.normalize(emb2.mean(dim=0, keepdim=True), p=2, dim=-1)
    video_sim = (mean1 @ mean2.T).item()

    # Temporally-aligned diagonal
    n = min(sim_matrix.shape)
    idx1 = np.linspace(0, sim_matrix.shape[0] - 1, n, dtype=int)
    idx2 = np.linspace(0, sim_matrix.shape[1] - 1, n, dtype=int)
    matched = np.array([sim_matrix[i, j] for i, j in zip(idx1, idx2)])

    return {
        'video_sim': video_sim,
        'matrix': sim_matrix,
        'matched': matched,
        'mean_pairwise': float(sim_matrix.mean()),
        'max_pairwise': float(sim_matrix.max()),
        'min_pairwise': float(sim_matrix.min()),
        'std_pairwise': float(sim_matrix.std()),
        'matched_mean': float(matched.mean()),
        'matched_std': float(matched.std()),
    }


# ═══════════════════════════════════════════════════════════════════════════
# 2. CLAP — ACOUSTIC SIMILARITY
# ═══════════════════════════════════════════════════════════════════════════

def run_clap(audio1, audio2, sr=48000):
    from transformers import ClapModel, ClapProcessor

    print("  Loading CLAP model...")
    processor = ClapProcessor.from_pretrained(
        "laion/larger_clap_music_and_speech")
    model = ClapModel.from_pretrained(
        "laion/larger_clap_music_and_speech")
    model.eval()

    def embed_chunks(audio, chunk_sec=10, hop_sec=5):
        chunk_len = chunk_sec * sr
        hop_len = hop_sec * sr
        chunks = []
        for start in range(0, max(1, len(audio) - chunk_len + 1), hop_len):
            chunk = audio[start:start + chunk_len]
            if len(chunk) < chunk_len:
                chunk = np.pad(chunk, (0, chunk_len - len(chunk)))
            chunks.append(chunk)
        if not chunks:
            chunks.append(np.pad(audio, (0, max(0, chunk_len - len(audio)))))

        embeddings = []
        for i in range(0, len(chunks), 4):
            batch = chunks[i:i + 4]
            inputs = processor(
                audios=batch, sampling_rate=sr,
                return_tensors="pt", padding=True)
            with torch.no_grad():
                emb = model.get_audio_features(**inputs)
            emb = F.normalize(emb, p=2, dim=-1)
            embeddings.append(emb)
        return torch.cat(embeddings, dim=0)

    print("  Embedding audio chunks...")
    emb1 = embed_chunks(audio1)
    emb2 = embed_chunks(audio2)

    chunk_sim = (emb1 @ emb2.T).numpy()
    mean1 = F.normalize(emb1.mean(dim=0, keepdim=True), p=2, dim=-1)
    mean2 = F.normalize(emb2.mean(dim=0, keepdim=True), p=2, dim=-1)
    overall_sim = (mean1 @ mean2.T).item()

    return {
        'overall_sim': overall_sim,
        'matrix': chunk_sim,
        'mean_pairwise': float(chunk_sim.mean()),
        'max_pairwise': float(chunk_sim.max()),
        'min_pairwise': float(chunk_sim.min()),
        'std_pairwise': float(chunk_sim.std()),
    }


# ═══════════════════════════════════════════════════════════════════════════
# 3. WHISPER + SENTENCE EMBEDDINGS — SEMANTIC SIMILARITY
# ═══════════════════════════════════════════════════════════════════════════

def run_whisper(wav1, wav2, label1, label2):
    import whisper

    print("  Loading Whisper model...")
    model = whisper.load_model("base")

    print(f"  Transcribing {label1}...")
    r1 = model.transcribe(wav1)
    text1, seg1 = r1["text"].strip(), r1.get("segments", [])
    print(f"    {len(text1.split())} words")

    print(f"  Transcribing {label2}...")
    r2 = model.transcribe(wav2)
    text2, seg2 = r2["text"].strip(), r2.get("segments", [])
    print(f"    {len(text2.split())} words")

    return text1, text2, seg1, seg2


def run_text_similarity(text1, text2, seg1=None, seg2=None):
    from sentence_transformers import SentenceTransformer

    print("  Loading sentence embedding model...")
    model = SentenceTransformer('all-MiniLM-L6-v2')

    emb = model.encode([text1, text2], normalize_embeddings=True)
    doc_sim = float(np.dot(emb[0], emb[1]))

    seg_sim = None
    if seg1 and seg2:
        texts1 = [s["text"].strip() for s in seg1 if s["text"].strip()]
        texts2 = [s["text"].strip() for s in seg2 if s["text"].strip()]
        if texts1 and texts2:
            e1 = model.encode(texts1, normalize_embeddings=True)
            e2 = model.encode(texts2, normalize_embeddings=True)
            seg_sim = e1 @ e2.T

    return {
        'doc_sim': doc_sim,
        'seg_matrix': seg_sim,
        'seg_mean': float(seg_sim.mean()) if seg_sim is not None else None,
        'seg_max': float(seg_sim.max()) if seg_sim is not None else None,
        'seg_std': float(seg_sim.std()) if seg_sim is not None else None,
    }


# ═══════════════════════════════════════════════════════════════════════════
# VISUALIZATION
# ═══════════════════════════════════════════════════════════════════════════

def plot_heatmap(matrix, path, label1, label2, title, xlabel, ylabel):
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(figsize=(8, 6))
    im = ax.imshow(matrix, cmap='RdYlGn', vmin=-0.1, vmax=1.0,
                   aspect='auto', origin='lower')
    ax.set_xlabel(f'{label2} {xlabel}')
    ax.set_ylabel(f'{label1} {ylabel}')
    ax.set_title(title)
    plt.colorbar(im, ax=ax, label='Cosine similarity')
    plt.tight_layout()
    plt.savefig(path, dpi=150)
    plt.close()


def plot_timeseries(matched, path, label1, label2):
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(figsize=(8, 3.5))
    x = np.linspace(0, 100, len(matched))
    ax.plot(x, matched, color='#2c7bb6', linewidth=1.5)
    ax.fill_between(x, matched, alpha=0.15, color='#2c7bb6')
    ax.axhline(y=matched.mean(), color='#d7191c', linestyle='--',
               linewidth=1, label=f'Mean = {matched.mean():.3f}')
    ax.set_xlabel('Video progress (%)')
    ax.set_ylabel('Cosine similarity')
    ax.set_title(f'CLIP Temporally-Aligned Similarity: {label1} vs {label2}')
    ax.set_ylim(-0.1, 1.0)
    ax.legend()
    plt.tight_layout()
    plt.savefig(path, dpi=150)
    plt.close()


def plot_summary_bar(results_dict, path, label1, label2):
    """Bar chart comparing the three headline similarity scores."""
    import matplotlib.pyplot as plt

    names = list(results_dict.keys())
    values = list(results_dict.values())
    colors = ['#2c7bb6', '#d7191c', '#fdae61']

    fig, ax = plt.subplots(figsize=(6, 4))
    bars = ax.bar(names, values, color=colors[:len(names)], width=0.5)
    for bar, val in zip(bars, values):
        ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height() + 0.01,
                f'{val:.3f}', ha='center', va='bottom', fontsize=12)
    ax.set_ylim(0, 1.0)
    ax.set_ylabel('Cosine similarity')
    ax.set_title(f'Multimodal Similarity: {label1} vs {label2}')
    ax.axhline(y=0.5, color='gray', linestyle=':', linewidth=0.8, alpha=0.5)
    plt.tight_layout()
    plt.savefig(path, dpi=150)
    plt.close()


# ═══════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(
        description='Multimodal video similarity analysis')
    parser.add_argument('--video1', required=True)
    parser.add_argument('--video2', required=True)
    parser.add_argument('--label1', default=None)
    parser.add_argument('--label2', default=None)
    parser.add_argument('--output', default='comparison')
    parser.add_argument('--max_frames', type=int, default=200)
    parser.add_argument('--skip_clip', action='store_true')
    parser.add_argument('--skip_clap', action='store_true')
    parser.add_argument('--skip_whisper', action='store_true')
    args = parser.parse_args()

    label1 = args.label1 or os.path.splitext(os.path.basename(args.video1))[0]
    label2 = args.label2 or os.path.splitext(os.path.basename(args.video2))[0]

    # Device
    if torch.backends.mps.is_available():
        device = torch.device('mps')
        device_name = "Apple Silicon (MPS)"
    elif torch.cuda.is_available():
        device = torch.device('cuda')
        device_name = "CUDA"
    else:
        device = torch.device('cpu')
        device_name = "CPU"
    print(f"Device: {device_name}")

    # ── Load video frames ──
    clip_results = None
    if not args.skip_clip:
        print("\n══ CLIP: Visual Similarity ══")
        print("  Extracting frames...")
        frames1, dur1, fps1 = extract_frames(args.video1, args.max_frames)
        frames2, dur2, fps2 = extract_frames(args.video2, args.max_frames)
        print(f"  {label1}: {len(frames1)} frames ({dur1:.1f}s)")
        print(f"  {label2}: {len(frames2)} frames ({dur2:.1f}s)")
        clip_results = run_clip(frames1, frames2, device)
        print(f"  Video-level similarity: {clip_results['video_sim']:.4f}")

    # ── Load audio ──
    clap_results = None
    whisper_results = None
    text1, text2 = None, None

    need_audio = not args.skip_clap or not args.skip_whisper
    if need_audio:
        if subprocess.run(['which', 'ffmpeg'], capture_output=True).returncode != 0:
            sys.exit("ERROR: ffmpeg not found. Install: brew install ffmpeg")

        print("\n  Extracting audio...")
        tmpdir = tempfile.mkdtemp()
        wav1 = os.path.join(tmpdir, "audio1.wav")
        wav2 = os.path.join(tmpdir, "audio2.wav")
        extract_audio(args.video1, wav1)
        extract_audio(args.video2, wav2)
        audio1, adur1 = load_audio(wav1)
        audio2, adur2 = load_audio(wav2)
        print(f"  {label1}: {adur1:.1f}s audio")
        print(f"  {label2}: {adur2:.1f}s audio")

    if not args.skip_clap:
        print("\n══ CLAP: Acoustic Similarity ══")
        clap_results = run_clap(audio1, audio2)
        print(f"  Overall similarity: {clap_results['overall_sim']:.4f}")

    if not args.skip_whisper:
        print("\n══ Whisper + Text Embeddings: Semantic Similarity ══")
        text1, text2, seg1, seg2 = run_whisper(wav1, wav2, label1, label2)
        whisper_results = run_text_similarity(text1, text2, seg1, seg2)
        print(f"  Document-level similarity: {whisper_results['doc_sim']:.4f}")

    # Cleanup temp audio
    if need_audio:
        os.remove(wav1)
        os.remove(wav2)
        os.rmdir(tmpdir)

    # ═══════════════════════════════════════════════════════════════════════
    # BUILD REPORT
    # ═══════════════════════════════════════════════════════════════════════

    r = []
    r.append("=" * 64)
    r.append("MULTIMODAL VIDEO SIMILARITY REPORT")
    r.append("=" * 64)
    r.append(f"  Date:     {datetime.now().strftime('%Y-%m-%d %H:%M')}")
    r.append(f"  Video 1:  {label1}")
    r.append(f"            {os.path.basename(args.video1)}")
    r.append(f"  Video 2:  {label2}")
    r.append(f"            {os.path.basename(args.video2)}")
    r.append(f"  Device:   {device_name}")
    r.append("")

    # ── CLIP section ──
    if clip_results:
        r.append("─" * 64)
        r.append("1. VISUAL SIMILARITY (CLIP ViT-B/32)")
        r.append("   What it measures: whether the videos look alike")
        r.append("─" * 64)
        r.append(f"  Frames sampled:  {args.max_frames} per video")
        r.append(f"  Model:           ViT-B/32 (OpenCLIP, laion2b_s34b_b79k)")
        r.append("")
        r.append(f"  VIDEO-LEVEL COSINE SIMILARITY:  {clip_results['video_sim']:.4f}")
        r.append("")
        r.append(f"  Frame-pair statistics:")
        r.append(f"    Mean:  {clip_results['mean_pairwise']:.4f}")
        r.append(f"    Max:   {clip_results['max_pairwise']:.4f}")
        r.append(f"    Min:   {clip_results['min_pairwise']:.4f}")
        r.append(f"    Std:   {clip_results['std_pairwise']:.4f}")
        r.append(f"  Temporally-aligned:")
        r.append(f"    Mean:  {clip_results['matched_mean']:.4f}")
        r.append(f"    Std:   {clip_results['matched_std']:.4f}")
        r.append("")

    # ── CLAP section ──
    if clap_results:
        r.append("─" * 64)
        r.append("2. ACOUSTIC SIMILARITY (CLAP)")
        r.append("   What it measures: whether the videos sound alike")
        r.append("─" * 64)
        r.append(f"  Model:  laion/larger_clap_music_and_speech")
        r.append(f"  Chunks: 10s windows, 5s hop")
        r.append("")
        r.append(f"  OVERALL COSINE SIMILARITY:      {clap_results['overall_sim']:.4f}")
        r.append("")
        r.append(f"  Chunk-pair statistics:")
        r.append(f"    Mean:  {clap_results['mean_pairwise']:.4f}")
        r.append(f"    Max:   {clap_results['max_pairwise']:.4f}")
        r.append(f"    Min:   {clap_results['min_pairwise']:.4f}")
        r.append(f"    Std:   {clap_results['std_pairwise']:.4f}")
        r.append("")

    # ── Whisper / transcript section ──
    if whisper_results:
        r.append("─" * 64)
        r.append("3. SEMANTIC SIMILARITY (Whisper + Sentence Embeddings)")
        r.append("   What it measures: whether the narrations say the same things")
        r.append("─" * 64)
        r.append(f"  Transcription:  Whisper (base)")
        r.append(f"  Embeddings:     all-MiniLM-L6-v2 (sentence-transformers)")
        r.append("")
        r.append(f"  DOCUMENT-LEVEL COSINE SIMILARITY: {whisper_results['doc_sim']:.4f}")
        r.append("")
        if whisper_results['seg_mean'] is not None:
            r.append(f"  Segment-pair statistics:")
            r.append(f"    Mean:  {whisper_results['seg_mean']:.4f}")
            r.append(f"    Max:   {whisper_results['seg_max']:.4f}")
            r.append(f"    Std:   {whisper_results['seg_std']:.4f}")
            r.append("")
        r.append(f"  {label1} transcript ({len(text1.split())} words):")
        for i in range(0, len(text1), 72):
            r.append(f"    {text1[i:i+72]}")
        r.append("")
        r.append(f"  {label2} transcript ({len(text2.split())} words):")
        for i in range(0, len(text2), 72):
            r.append(f"    {text2[i:i+72]}")
        r.append("")

    # ── Summary ──
    r.append("=" * 64)
    r.append("SUMMARY")
    r.append("=" * 64)
    summary = {}
    if clip_results:
        r.append(f"  Visual  (CLIP):       {clip_results['video_sim']:.4f}")
        summary['Visual\n(CLIP)'] = clip_results['video_sim']
    if clap_results:
        r.append(f"  Acoustic (CLAP):      {clap_results['overall_sim']:.4f}")
        summary['Acoustic\n(CLAP)'] = clap_results['overall_sim']
    if whisper_results:
        r.append(f"  Semantic (transcript): {whisper_results['doc_sim']:.4f}")
        summary['Semantic\n(transcript)'] = whisper_results['doc_sim']
    r.append("")
    r.append("  Interpretation (cosine similarity):")
    r.append("    0.85 - 1.00  Very high — nearly identical content")
    r.append("    0.70 - 0.85  High — same type of content")
    r.append("    0.50 - 0.70  Moderate — related content")
    r.append("    0.30 - 0.50  Low — weakly related")
    r.append("    < 0.30       Minimal relationship")
    r.append("=" * 64)

    report_text = "\n".join(r)

    # ── Print ──
    print("\n" + report_text)

    # ── Save all outputs ──
    print("\n── Saving outputs ──")
    prefix = args.output

    # Text report
    with open(f"{prefix}.txt", 'w') as f:
        f.write(report_text + "\n")
    print(f"  {prefix}.txt")

    # Plots
    if clip_results:
        plot_heatmap(clip_results['matrix'],
                     f"{prefix}_clip_matrix.png",
                     label1, label2,
                     'CLIP Frame-to-Frame Cosine Similarity',
                     'frame index', 'frame index')
        print(f"  {prefix}_clip_matrix.png")

        plot_timeseries(clip_results['matched'],
                        f"{prefix}_clip_timeseries.png",
                        label1, label2)
        print(f"  {prefix}_clip_timeseries.png")

        np.savetxt(f"{prefix}_clip_matrix.csv",
                   clip_results['matrix'], delimiter=',', fmt='%.6f')
        print(f"  {prefix}_clip_matrix.csv")

    if clap_results:
        plot_heatmap(clap_results['matrix'],
                     f"{prefix}_clap_matrix.png",
                     label1, label2,
                     'CLAP Acoustic Similarity (chunk-level)',
                     'chunk index', 'chunk index')
        print(f"  {prefix}_clap_matrix.png")

    if whisper_results and whisper_results['seg_matrix'] is not None:
        plot_heatmap(whisper_results['seg_matrix'],
                     f"{prefix}_transcript_matrix.png",
                     label1, label2,
                     'Transcript Semantic Similarity (segment-level)',
                     'segment index', 'segment index')
        print(f"  {prefix}_transcript_matrix.png")

    # Transcripts
    if text1 is not None:
        with open(f"{prefix}_transcript_1.txt", 'w') as f:
            f.write(f"# Transcript: {label1}\n")
            f.write(f"# Source: {os.path.basename(args.video1)}\n")
            f.write(f"# Model: Whisper (base)\n\n")
            f.write(text1 + "\n")
        print(f"  {prefix}_transcript_1.txt")
    if text2 is not None:
        with open(f"{prefix}_transcript_2.txt", 'w') as f:
            f.write(f"# Transcript: {label2}\n")
            f.write(f"# Source: {os.path.basename(args.video2)}\n")
            f.write(f"# Model: Whisper (base)\n\n")
            f.write(text2 + "\n")
        print(f"  {prefix}_transcript_2.txt")

    # Summary bar chart
    if len(summary) > 1:
        plot_summary_bar(summary, f"{prefix}_summary.png", label1, label2)
        print(f"  {prefix}_summary.png")

    print("\nDone.")


if __name__ == '__main__':
    main()
