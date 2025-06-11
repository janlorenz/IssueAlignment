library(tidyverse)
library(arrow)

# Data download and basic prepa ration: 
# This URL leads to the selection of variables at the ESS download wizard
# https://ess.sikt.no/en/data-builder/?rounds=0+1+2+3+4+5+6+7+8+9+10+11&seriesVersion=884&tab=variables&variables=1.0.21-23_30_70
# Downloaded as CSV

# Read from zip and save as parquet
read_csv(unz("ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.zip", "ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv")) |>
 	filter(freehms <= 5, gincdif <= 5, impcntr <= 4, lrscale <= 10, euftf <= 10) |>
 	mutate(
 		# Scaling data to values within [-1, 1].
 		# v' = -1 + 2 * (v-m)/(M-m), where m is the lowest, M the highest possible answer
 		across(c(freehms, gincdif), ~ -1 + 2 * (.x - 1)/(5-1)),
 		across(c(lrscale, euftf), ~ -1 + 2 * (.x - 0)/(10-0)),
 		impcntr = -1 + 2* (impcntr - 1)/(4-1),
 		# Flipping some scales: some questions are asked in a "negative" sense:
 		# e.g. -1 --> "more immigrants" and 1 --> "less immigrants"
 		across(c(freehms, gincdif, impcntr), ~ .x * -1),
 		year = (essround*2 + 2000), 
 		year = if_else(year == 2022, 2023, false = year)
 	) |> write_parquet("ESS1-11.parquet")