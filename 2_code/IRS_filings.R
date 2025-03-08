#install.packages("devtools")
#devtools::install_github("UrbanInstitute/nccsdata")
#devtools::install_github( 'ultinomics/xmltools' )
#devtools::install_github( 'nonprofit-open-data-collective/irs990efile' )
#library(nccsdata)
#library(irs990efile)
library(curl)
library(tidyverse)
library(ggplot2)
library(here)

set.seed(1976)

#helper fcns
first_4_digits <- function(x) {
  x_str <- as.character(x)
  # Remove any non-digit characters (like decimal points)
  x_str <- gsub("[^0-9]", "", x_str)
  # Select the first 4 digits
  first_4 <- as.numeric(substr(x_str, 1, 4))
  return(first_4)
}

## Walmart and THD 990 data for plots

irs990_data <-  readr::read_csv(
  here::here("1_secondary_data", "EHFCompanyInfo", "990_EHF.csv")
)

net_assets_plot <-ggplot(irs990_data, 
                     aes(x = year, y = assets_net/1000000, color = ehf,
                         linetype = ehf)) +
  geom_line(linewidth=1.2) +                      # Add lines to the plot
  labs(title = "Home Depot and Walmart Foundation Net Assets",
       x = "Year",
       y = "Net Assets ($MM)") +               # Add labels and legend title
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank()
  )

ggsave(
  filename = "thd_wmt_assets.png",
  plot = net_assets_plot,
  path = here("4_output", "plots"),
  width =6,
  height =4,
  units ="in"
  )

grants_plot <-ggplot(irs990_data, 
                     aes(x = year, y = indiv_grants/1000000, color = ehf,
                         linetype = ehf)) +
  geom_line(linewidth=1.2) +                      # Add lines to the plot
  labs(title = "Home Depot and Walmart EHF grants to individuals",
       x = "Year",
       y = "Total grants ($MM)") +               # Add labels and legend title
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank()
  )

ggsave(
  filename = "thd_wmt_grants.png",
  plot = grants_plot,
  path = here("4_output", "plots"),
  width =6,
  height =4,
  units ="in"
)


### EHF outsourcers 

outsourcers <-  readr::read_csv(
  here::here("1_secondary_data", "EHFCompanyInfo", "EHF_outsourcers.csv")
)

outsourcers <- outsourcers |> mutate(
  year = as_date(
    paste(year,"-01-01", sep="")
  ),
  year = year(year)
) |> 
  filter(in_ehf==1)


outsource_revenue_plot <-ggplot(outsourcers, 
                     aes(x = as.integer(year), y = revenue/1000000, color = name,
                         linetype = name)) +
  geom_line(linewidth=1.2) +                      # Add lines to the plot
  labs(title = "Revenues of major 3rd-party EHF managers",
       x = "Year",
       y = "Total revenue ($MM log scale)") +     # Add labels and legend title
  scale_y_log10() +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank()
  )


outsource_dgrant_plot <-ggplot(outsourcers, 
                                aes(x = round(year,0), y = grants_paid_domestic/1000000, color = name,
                                    linetype = name)) +
  geom_line(linewidth=1.2) +                      # Add lines to the plot
  labs(title = "Grants paid by major 3rd party EHF managers",
       x = "Year",
       y = "Grants paid to domestic individuals ($MM, log scale)") +               # Add labels and legend title
  scale_y_log10() +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank()
  )

outsource_fgrant_plot <-ggplot(outsourcers, 
                               aes(x = round(year,0), y = grants_paid_foreign/1000000, color = name,
                                   linetype = name)) +
  geom_line(linewidth=1.2) +                      # Add lines to the plot
  labs(title = "Grants paid by major 3rd party EHF managers",
       x = "Year",
       y = "Grants paid to foreign individuals ($MM log scale)") +               # Add labels and legend title
  scale_y_log10() +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank()
  )







###

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
# America's Charities: 54-1517707

# BMF download
# most updated from NCCS
## downloaded 6/26/24
##update URL and filename if downloaded again
#bmfurl <- "https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/BMF_UNIFIED_V1.1.csv"
#destination <- here::here("0_raw_data", "IRS", "bmf_unified_v1_1_062624.csv")

# Download the data file
#curl_download(bmfurl, 
#  destfile = destination)

#Download data dictionary
## downloaded 6/26/24
#bmfurl<- "https://nccsdata.s3.amazonaws.com/harmonized/harmonized_data_dictionary.xlsx"
#destination <- here::here("0_raw_data", "IRS", "bmf_datadictionary_062624.xlsx")
#curl_download(bmfurl, 
#  destfile = destination)

# BMF

# 1st 10k records in BMF data

bmf_10k<-data.table::fread(
  file =here::here("0_raw_data", "IRS", "bmf_unified_v1_1_062624.csv"),
  nrows = 10000
)

PZ_h_2017_10k<-data.table::fread(
  file ="/Users/thequist/Library/CloudStorage/Dropbox/EHF_cloud/EHFproject/0_raw_data/IRS/CORE-2017-501C3-CHARITIES-PZ-HRMN.csv",
  #file =here::here("0_raw_data", "IRS", "CORE-2017-501C3-CHARITIES-PZ-HRMN.csv"),
  nrows = 10000
)

bmf_all<-data.table::fread(
  file =here::here("0_raw_data", "IRS", "bmf_unified_v1_1_062624.csv"),
  nThread = 4
)



PC_h_2019_200k <- data.table::fread(
  file = "https://nccsdata.s3.amazonaws.com/harmonized/core/501c3-pc/CORE-2019-501C3-CHARITIES-PC-HRMN.csv",
  nrows=200000,
  nThread = 4
)
  

############## using older API ##################

# need to redo by pulling multiple years from NCCS
# subsetting by common TEXPER year
# finding unique EIN and then comparing that
# list to the EIN list from irs990efile pz

#comparing coverage over datasets for 2019
# test <- nccsdata::get_data(
#   dsname = "core",
#   scope.orgtype = "CHARITIES",
#   scope.formtype = "PZ",
#   time = "2019",
#   ntee.code = c("M20", "P60")
# )
# 
# compare_pc <- nccsdata::get_data(
#   dsname = "core",
#   scope.orgtype = "CHARITIES",
#   scope.formtype = "PC",
#   time = "2019"#,
#   #ntee.code = c("M20", "P60")
# )

n_compare_pc<-nrow(compare_pc)
compare_pc_uein<-unique(compare_pc$EIN)
norg_compare_pc<-length(compare_pc_uein)

# compare_pz <- nccsdata::get_data(
#   dsname = "core",
#   scope.orgtype = "CHARITIES",
#   scope.formtype = "PZ",
#   time = "2019"#,
#   #ntee.code = c("M20", "P60")
# )

n_compare_pz <- nrow(compare_pz)
compare_pz_uein<-unique(compare_pz$EIN)
norg_compare_pz<-length(compare_pz_uein)


# index <- irs990efile::build_index( tax.years=2019 )

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

# NCCS data 2017
## doing this sequentially, as downloading one big file
## caused problems

# compare_pz_15 <- nccsdata::get_data(
#   dsname = "core",
#   scope.orgtype = "CHARITIES",
#   scope.formtype = "PZ",
#   time = c("2015")
# )
# 
# compare_pz_16 <- nccsdata::get_data(
#   dsname = "core",
#   scope.orgtype = "CHARITIES",
#   scope.formtype = "PZ",
#   time = c("2016")
# )
# 
# 
# compare_pz_17 <- nccsdata::get_data(
#   dsname = "core",
#   scope.orgtype = "CHARITIES",
#   scope.formtype = "PZ",
#   time = c("2017")
# )
# 
# compare_pz_18 <- nccsdata::get_data(
#   dsname = "core",
#   scope.orgtype = "CHARITIES",
#   scope.formtype = "PZ",
#   time = c("2018")
# )


compare_pz_15 <- compare_pz_15 |> 
  filter(
    first_4_digits(TAXPER) == 2017) #0 2017 returns

compare_pz_16 <- compare_pz_16 |> 
  filter(
    first_4_digits(TAXPER) == 2017)

compare_pz_17 <- compare_pz_17 |> 
  filter(
    first_4_digits(TAXPER) == 2017)


compare_pz_18 <- compare_pz_18 |> 
  filter(
    first_4_digits(TAXPER) == 2017)

compare_pz_19 <- compare_pz |> 
  filter(
    first_4_digits(TAXPER) == 2017)

compare_pz_ein_2017 <- unique(
  c( compare_pz_16$EIN,
     compare_pz_17$EIN,
     compare_pz_18$EIN,
     compare_pz_19$EIN)
  )

n_compare_pz_2017 <- length(compare_pz_ein_2017)

# index_2017 <- irs990efile::build_index( tax.years=2017 )

n_index_2017 <- nrow(index_2017)

index_2017_pz <- index_2017 |> 
  filter(FormType %in% c("990", "990EZ") & TaxStatus == "501c3")
n_index_2017_pz <-nrow(index_2017_pz)

#n_index_2017_pz <- nrow(index_2017[index_2017$FormType %in% c("990", "990EZ"),])

index_2017_uein <- unique(index_2017$EIN)
#index_2017_pz_uein <- unique(index_2017$EIN[index_2017$FormType %in% c("990", "990EZ")])
index_2017_pz_uein<- unique(index_2017_pz$EIN)
norg_index_2017<- length(index_2017_uein)
norg_index_2017_pz<-length(index_2017_pz_uein)

sum(compare_pz_ein_2017 %in% index_2017_pz_uein)/n_compare_pz_2017 #74.4%
sum(index_2017_pz_uein %in% compare_pz_ein_2017)/norg_index_2017_pz #70%

length(intersect(index_2017_pz_uein,compare_pz_ein_2017)) #272482

