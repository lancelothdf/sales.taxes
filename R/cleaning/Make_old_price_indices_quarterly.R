### In this R-file we create a quarterly and a yearly file with all Nielsen data and tax rate information

library(data.table)
library(lfe)
library(futile.logger)
library(AER)
library(readstata13)


setwd("/project2/igaarder")

## useful filepaths ------------------------------------------------------------
pi_data_path <- "Data/Nielsen/Master_file.dta"


#### First Step: Make quarterly data (all_nielsen_data_2006_2016_quarterly.csv)
all_pi <- read.dta13(pi_data_path)
all_pi <- as.data.table(all_pi)


all_pi <- all_pi[, c("fips_state", "fips_county", "store_code_uc", "product_module_code", "year", "quarter", "price_p")]
all_pi$ln_pricei2 <- exp(all_pi$price_p)  ##Previously (in the old files) we had taken the log twice.  So reversing this mistake. This is FE_store + constant (see UPC_level_price_regressions.do and 11172018_First_stage.do)

all_pi <- all_pi[, c("fips_state", "fips_county", "store_code_uc", "product_module_code", "year", "quarter", "ln_pricei2")]
fwrite(all_pi, "Data/Nielsen/Quarterly_old_pi.csv")
