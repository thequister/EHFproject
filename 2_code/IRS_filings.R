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

outsourcers <- outsourcers |> 
  mutate(
    year = as_date(
      paste(year,"-01-01", sep="")
    )
  ) |> 
  filter(in_ehf==1)


outsource_revenue_plot <-ggplot(outsourcers, 
                     aes(x = year, y = revenue/1000000, color = name,
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
                                aes(x = year, y = grants_paid_domestic/1000000, color = name,
                                    linetype = name)) +
  geom_line(linewidth=1.2) +                      # Add lines to the plot
  labs(title = "Grants paid by major 3rd party EHF managers",
       x = "Year",
       y = "Grants paid to domestic individuals ($MM, log scale)") +               # Add labels and legend title
  scale_y_log10(labels = scales::label_number()) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none" # Removes the legend
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


