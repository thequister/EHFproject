#!/usr/bin/env python3
"""
Video Similarity via CLIP Embeddings
=====================================
Computes cosine similarity between two videos using OpenAI's CLIP model.
Unlike ViSiL (which detects near-duplicate re-uploads), CLIP captures
*semantic* similarity — whether frames depict similar content, objects,
text, and meaning. This makes it far more interpretable for comparing
experimental treatment videos.

Setup (M2 Mac):
    pip install torch torchvision open-clip-torch opencv-python matplotlib numpy

Usage:
    python video_similarity_clip.py --video1 path/to/thd_treatment.mp4 \
                                     --video2 path/to/wmt_treatment.mp4 \
                                     --output similarity_report

Output:
    - Console summary (overall similarity, per-frame stats)
    - similarity_report_matrix.png  (frame × frame heatmap)
    - similarity_report_timeseries.png (matched-frame similarity over time)
    - similarity_report.csv (raw frame-pair similarities)
"""

import argparse
import os
import sys

import cv2
import numpy as np
import torch
import torch.nn.functional as F

# ── video loading ────────────────────────────────────────────────────────────

def extract_frames(video_path, max_frames=60, resize=224):
    """
    Sample frames uniformly from a video.
    Returns numpy array of shape (N, H, W, 3) in RGB uint8.
    """
    cap = cv2.VideoCapture(video_path)
    if not cap.isOpened():
        sys.exit(f"ERROR: cannot open {video_path}")

    total = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    fps = cap.get(cv2.CAP_PROP_FPS)
    duration = total / fps if fps > 0 else 0
    print(f"  {os.path.basename(video_path)}: {total} frames, "
          f"{fps:.1f} fps, {duration:.1f}s")

    # Sample at most max_frames, uniformly spaced
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

    print(f"  Extracted {len(frames)} frames")
    return np.array(frames), duration, fps


# ── CLIP encoding ────────────────────────────────────────────────────────────

def load_clip_model(device):
    """Load OpenCLIP model. Uses ViT-B/32 by default — good balance of
    speed and quality for this use case."""
    import open_clip
    model, _, preprocess = open_clip.create_model_and_transforms(
        'ViT-B-32', pretrained='laion2b_s34b_b79k', device=device
    )
    model.eval()
    return model, preprocess


def encode_frames(model, preprocess, frames, device, batch_size=32):
    """
    Encode video frames into CLIP embedding space.
    Returns L2-normalized embeddings of shape (N, D).
    """
    from PIL import Image

    embeddings = []
    for i in range(0, len(frames), batch_size):
        batch = frames[i:i + batch_size]
        # preprocess expects PIL images
        tensors = torch.stack([
            preprocess(Image.fromarray(f)) for f in batch
        ]).to(device)

        with torch.no_grad():
            emb = model.encode_image(tensors)

        # L2-normalize so dot product = cosine similarity
        emb = F.normalize(emb, p=2, dim=-1)
        embeddings.append(emb.cpu())

    return torch.cat(embeddings, dim=0)


# ── similarity computation ───────────────────────────────────────────────────

def compute_similarities(emb1, emb2):
    """
    Returns:
        sim_matrix : (N1, N2) cosine similarity matrix
        video_sim  : scalar, cosine similarity of mean embeddings
        matched    : (min(N1,N2),) best-match similarities along diagonal
    """
    # Frame-to-frame similarity matrix
    sim_matrix = (emb1 @ emb2.T).numpy()

    # Video-level: cosine similarity of the mean (centroid) embedding
    mean1 = F.normalize(emb1.mean(dim=0, keepdim=True), p=2, dim=-1)
    mean2 = F.normalize(emb2.mean(dim=0, keepdim=True), p=2, dim=-1)
    video_sim = (mean1 @ mean2.T).item()

    # Temporally-aligned similarity (diagonal of the matrix,
    # interpolated to the shorter video's length)
    n = min(sim_matrix.shape)
    idx1 = np.linspace(0, sim_matrix.shape[0] - 1, n, dtype=int)
    idx2 = np.linspace(0, sim_matrix.shape[1] - 1, n, dtype=int)
    matched = np.array([sim_matrix[i, j] for i, j in zip(idx1, idx2)])

    return sim_matrix, video_sim, matched


# ── visualization ────────────────────────────────────────────────────────────

def plot_heatmap(sim_matrix, output_path, label1, label2):
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(8, 6))
    im = ax.imshow(sim_matrix, cmap='RdYlGn', vmin=-0.1, vmax=1.0,
                   aspect='auto', origin='lower')
    ax.set_xlabel(f'{label2} frame index')
    ax.set_ylabel(f'{label1} frame index')
    ax.set_title('Frame-to-Frame Cosine Similarity (CLIP ViT-B/32)')
    plt.colorbar(im, ax=ax, label='Cosine similarity')
    plt.tight_layout()
    plt.savefig(output_path, dpi=150)
    plt.close()
    print(f"  Saved heatmap: {output_path}")


def plot_timeseries(matched, output_path, label1, label2):
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(8, 3.5))
    x = np.linspace(0, 100, len(matched))
    ax.plot(x, matched, color='#2c7bb6', linewidth=1.5)
    ax.fill_between(x, matched, alpha=0.15, color='#2c7bb6')
    ax.axhline(y=matched.mean(), color='#d7191c', linestyle='--',
               linewidth=1, label=f'Mean = {matched.mean():.3f}')
    ax.set_xlabel('Video progress (%)')
    ax.set_ylabel('Cosine similarity')
    ax.set_title(f'Temporally-Aligned Similarity: {label1} vs {label2}')
    ax.set_ylim(-0.1, 1.0)
    ax.legend()
    plt.tight_layout()
    plt.savefig(output_path, dpi=150)
    plt.close()
    print(f"  Saved timeseries: {output_path}")


def save_csv(sim_matrix, output_path):
    """Save the full similarity matrix as CSV for further analysis in R."""
    np.savetxt(output_path, sim_matrix, delimiter=',', fmt='%.6f')
    print(f"  Saved CSV: {output_path}")


# ── main ─────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description='Compare two videos using CLIP cosine similarity')
    parser.add_argument('--video1', required=True,
                        help='Path to first video (e.g., THD treatment)')
    parser.add_argument('--video2', required=True,
                        help='Path to second video (e.g., WMT treatment)')
    parser.add_argument('--output', default='similarity_report',
                        help='Output file prefix (default: similarity_report)')
    parser.add_argument('--max_frames', type=int, default=60,
                        help='Max frames to sample per video (default: 60)')
    parser.add_argument('--label1', default=None,
                        help='Label for video 1 (default: filename)')
    parser.add_argument('--label2', default=None,
                        help='Label for video 2 (default: filename)')
    args = parser.parse_args()

    label1 = args.label1 or os.path.splitext(os.path.basename(args.video1))[0]
    label2 = args.label2 or os.path.splitext(os.path.basename(args.video2))[0]

    # Device selection: MPS (Apple Silicon) > CUDA > CPU
    if torch.backends.mps.is_available():
        device = torch.device('mps')
        print(f"Using Apple Silicon GPU (MPS)")
    elif torch.cuda.is_available():
        device = torch.device('cuda')
        print(f"Using CUDA GPU")
    else:
        device = torch.device('cpu')
        print(f"Using CPU")

    # 1. Extract frames
    print("\n── Extracting frames ──")
    frames1, dur1, fps1 = extract_frames(args.video1, args.max_frames)
    frames2, dur2, fps2 = extract_frames(args.video2, args.max_frames)

    # 2. Load CLIP and encode
    print("\n── Loading CLIP model ──")
    model, preprocess = load_clip_model(device)

    print("\n── Encoding frames ──")
    print(f"  Encoding {label1}...")
    emb1 = encode_frames(model, preprocess, frames1, device)
    print(f"  Encoding {label2}...")
    emb2 = encode_frames(model, preprocess, frames2, device)
    print(f"  Embedding shape: {emb1.shape[1]}d")

    # 3. Compute similarities
    print("\n── Computing similarities ──")
    sim_matrix, video_sim, matched = compute_similarities(emb1, emb2)

    # 4. Build report
    report = []
    report.append("=" * 60)
    report.append("RESULTS")
    report.append("=" * 60)
    report.append(f"  Video 1:  {label1} ({dur1:.1f}s, {len(frames1)} frames)")
    report.append(f"  Video 2:  {label2} ({dur2:.1f}s, {len(frames2)} frames)")
    report.append(f"  Model:    CLIP ViT-B/32 (OpenCLIP, laion2b_s34b_b79k)")
    report.append(f"  Max frames sampled: {args.max_frames}")
    report.append("")
    report.append(f"  Video-level cosine similarity:      {video_sim:.4f}")
    report.append(f"    (cosine sim of mean frame embeddings)")
    report.append("")
    report.append(f"  Frame-pair statistics:")
    report.append(f"    Mean pairwise similarity:          {sim_matrix.mean():.4f}")
    report.append(f"    Max pairwise similarity:           {sim_matrix.max():.4f}")
    report.append(f"    Min pairwise similarity:           {sim_matrix.min():.4f}")
    report.append(f"    Std pairwise similarity:           {sim_matrix.std():.4f}")
    report.append("")
    report.append(f"  Temporally-aligned similarity:")
    report.append(f"    Mean matched-frame similarity:     {matched.mean():.4f}")
    report.append(f"    Std matched-frame similarity:      {matched.std():.4f}")
    report.append("=" * 60)
    report.append("")
    report.append("Interpretation notes:")
    report.append("  CLIP cosine similarity ranges from -1 to 1 in theory, but")
    report.append("  in practice image pairs typically fall between 0.1 and 0.9.")
    report.append("")
    report.append("  Rough benchmarks (CLIP ViT-B/32):")
    report.append("    ~0.85-1.0   Near-identical frames (same scene, minor crop)")
    report.append("    ~0.70-0.85  Same type of scene (e.g., two office interiors)")
    report.append("    ~0.50-0.70  Related content (e.g., two corporate videos)")
    report.append("    ~0.30-0.50  Weak relationship (e.g., indoor vs outdoor)")
    report.append("    ~0.10-0.30  Unrelated content")
    report.append("")
    report.append("  For your paper, the video-level cosine similarity of the mean")
    report.append("  embeddings is the cleanest single number to report. The heatmap")
    report.append("  and timeseries show where the videos converge and diverge")
    report.append("  visually over their runtime.")

    report_text = "\n".join(report)

    # Print to console
    print("\n" + report_text)

    # 5. Save outputs
    print("\n── Saving outputs ──")
    plot_heatmap(sim_matrix, f"{args.output}_matrix.png", label1, label2)
    plot_timeseries(matched, f"{args.output}_timeseries.png", label1, label2)
    save_csv(sim_matrix, f"{args.output}.csv")

    # Save text report
    report_path = f"{args.output}.txt"
    with open(report_path, 'w') as f:
        f.write(report_text + "\n")
    print(f"  Saved report: {report_path}")


if __name__ == '__main__':
    main()
