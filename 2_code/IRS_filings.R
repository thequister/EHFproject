#install.packages("devtools")
#devtools::install_github("UrbanInstitute/nccsdata")
#devtools::install_github( 'ultinomics/xmltools' )
#devtools::install_github( 'nonprofit-open-data-collective/irs990efile' )
library(nccsdata)
library(irs990efile)
library(curl)
library(here)

#NTEE codes
#M20 Disaster Preparedness and Relief Service
#P60
#Y40 "Fraternal Beneficiary Societies"

#NAICS
# 624230: Emergency and Relief Services

#EIN
# Home Depot/Homer Fund: 58-2491657
# EAF: 45-1813056
# Walmart/ACNT: 71-0858484
# SEIU home health: 13-4129368  Y43

#BMF download
#update URL and filename if downloaded again
bmfurl <- "https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/BMF_UNIFIED_V1.1.csv"
destination <- here::here("0_raw_data", "IRS", "bmf_unified_v1_1_062624.csv")

# Download the data file
curl_download(bmfurl, 
  destfile = destination)

#Download data dictionary
bmfurl<- "https://nccsdata.s3.amazonaws.com/harmonized/harmonized_data_dictionary.xlsx"
destination <- here::here("0_raw_data", "IRS", "bmf_datadictionary_062624.xlsx")
curl_download(bmfurl, 
  destfile = destination)


# need to redo by pulling multiple years from NCCS
# subsetting by common TEXPER year
# finding unique EIN and then comparing that
# list to the EIN list from irs990efile pz

test <- nccsdata::get_data(
  dsname = "core",
  scope.orgtype = "CHARITIES",
  scope.formtype = "PZ",
  time = "2019",
  ntee.code = c("M20", "P60")
)

compare_pc <- nccsdata::get_data(
  dsname = "core",
  scope.orgtype = "CHARITIES",
  scope.formtype = "PC",
  time = "2019"#,
  #ntee.code = c("M20", "P60")
)

n_compare_pc<-nrow(compare_pc)
compare_pc_uein<-unique(compare_pc$EIN)
norg_compare_pc<-length(compare_pc_uein)

compare_pz <- nccsdata::get_data(
  dsname = "core",
  scope.orgtype = "CHARITIES",
  scope.formtype = "PZ",
  time = "2019"#,
  #ntee.code = c("M20", "P60")
)

n_compare_pz <- nrow(compare_pz)
compare_pz_uein<-unique(compare_pz$EIN)
norg_compare_pz<-length(compare_pz_uein)


index <- irs990efile::build_index( tax.years=2019 )

n_index <- nrow(index)
n_index_pz <- nrow(index[index$FormType %in% c("990", "990EZ"),])
index_uein <- unique(index$EIN)
index_pz_uein <- unique(index$EIN[index$FormType %in% c("990", "990EZ")])
norg_index<- length(index_uein)
norg_index_pz<-length(index_pz_uein)


sum(compare_pz_uein %in% compare_pc_uein)/norg_compare_pz #46%
sum( compare_pc_uein %in% compare_pz_uein )/norg_compare_pc #100%
sum(compare_pz_uein %in% index_uein)/norg_compare_pz #74.1%
sum(compare_pz_uein %in% index_pz_uein)/norg_compare_pz #73.98%
sum(index_uein %in% compare_pz_uein)/norg_index #63%
sum(index_pz_uein %in% compare_pz_uein)/norg_index_pz #75%

length(intersect(index_uein,compare_pz_uein)) #324600
length(intersect(index_pz_uein,compare_pz_uein)) #323846