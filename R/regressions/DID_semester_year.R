#' Run diff-n-diff on updated semesterly data. Try different FE definitions
#' Simple DID dissign to interact with initial level of tax rate in the future
#' 

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/DID_semesterly_and_yearly.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

outcomes <- c("D.ln_cpricei2", "D.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

### Run DID semester data --------------------------------
LRdiff_res <- data.table(NULL)
for (Y in c(outcomes)) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~ D.ln_sales_tax | ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
    
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, window := "semester"]
    res1.dt[, spec := "changes"]
    # Add summary values
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N_obs := nrow(all_pi)]
    res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
    res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
    res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
    res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
    res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                         "product_module_code"))]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
  }
}

### Set up Yearly Data ---------------------------------
all_pi <- fread(data.year)

outcomes.changes <- c("D.ln_cpricei2", "D.ln_quantity3")
outcomes.levels <- c("ln_cpricei2", "ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


# take first differences of outcomes and treatment
all_pi <- all_pi[order(store_code_uc, product_module_code, year),] ##Sort on store by year (in ascending order)


all_pi[, D.ln_cpricei2 := ln_cpricei2 - shift(ln_cpricei2, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_quantity3 := ln_quantity3 - shift(ln_quantity3, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

all_pi[, D.ln_sales_tax := ln_sales_tax - shift(ln_sales_tax, n=1, type="lag"),
       by = .(store_code_uc, product_module_code)]

# Make group FE
all_pi[, store_by_module := .GRP, by = .(store_code_uc, product_module_code)]
all_pi[, region_by_module_by_time := .GRP, by = .(region, product_module_code, year)]
all_pi[, division_by_module_by_time := .GRP, by = .(division, product_module_code, year)]


### Run DID yearly data in changes and levels --------------------------------
LRdiff_res <- data.table(NULL)

## changes
for (Y in c(outcomes.changes)) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~ D.ln_sales_tax | ", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
    
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, window := "semester"]
    res1.dt[, spec := "changes"]
    # Add summary values
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N_obs := nrow(all_pi)]
    res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
    res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
    res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
    res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
    res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                         "product_module_code"))]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
  }
}
## Run in levels
for (Y in c(outcomes.levels)) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~ ln_sales_tax | store_by_module +", FE, " | 0 | module_by_state"
    ))
    flog.info("Estimating with %s as outcome with %s FE.", Y, FE)
    res1 <- felm(formula = formula1, data = all_pi,
                 weights = all_pi$base.sales)
    flog.info("Finished estimating with %s as outcome with %s FE.", Y, FE)
    
    
    ## attach results
    flog.info("Writing results...")
    res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
    res1.dt[, outcome := Y]
    res1.dt[, controls := FE]
    res1.dt[, window := "semester"]
    res1.dt[, spec := "levels"]
    # Add summary values
    res1.dt[, Rsq := summary(res1)$r.squared]
    res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
    res1.dt[, N_obs := nrow(all_pi)]
    res1.dt[, N_modules := length(unique(all_pi$product_module_code))]
    res1.dt[, N_stores :=  length(unique(all_pi$store_code_uc))]
    res1.dt[, N_counties := uniqueN(all_pi, by = c("fips_state", "fips_county"))]
    res1.dt[, N_years := uniqueN(all_pi, by = c("year"))]
    res1.dt[, N_county_modules := uniqueN(all_pi, by = c("fips_state", "fips_county",
                                                         "product_module_code"))]
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
    
  }
}


