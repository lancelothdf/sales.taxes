#' Sales Taxes Project
#' Run standard Two-way FE model
#' Check estimates of long run design
#' 




library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)

setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data_county.csv"

## output filepaths ----------------------------------------------
output.results.file <- "Data/LR_TWFE_design_county.csv"

## Open data  -----
all_pi <- fread(data.semester)


# Create a categorical variable for county_by_module
all_pi[, county_by_module := .GRP, by = .(fips_state, fips_county, product_module_code)]

# Create demeaned variables
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(county_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(county_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(county_by_module)]
all_pi[, w.ln_pricei2 := ln_pricei2 - mean(ln_pricei2), by = .(county_by_module)]
all_pi[, w.ln_sales := ln_sales - mean(ln_sales), by = .(county_by_module)]

## Create lead demeaned tax rate for pre-trends
all_pi <- all_pi[order(fips_state, fips_county, product_module_code, year, semester),] ##Sort on countyXmodule by year-semester (in ascending order)
for (val in 1:4) {

  lead.X <- paste0("F", val, ".w.ln_sales_tax")
  all_pi[, (lead.X) := shift(w.ln_sales_tax, n=val, type="lead"),
         by = .(fips_state, fips_county, product_module_code)]
}

# Need to demean
all_pi[, module_by_time := .GRP, by = .(product_module_code, semester, year)]
all_pi[, L.ln_cpricei2 := ln_cpricei2 - D.ln_cpricei2]
all_pi[, dm.L.ln_cpricei2 := L.ln_cpricei2 - mean(L.ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, na.rm = T), by = module_by_time]
all_pi[, dm.ln_quantity3 := ln_quantity3 - mean(ln_quantity3, na.rm = T), by = module_by_time]


# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Price 
pct1.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=control$base.sales)
pct1.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.01, na.rm = T, weight=treated$base.sales)

pct99.control <- quantile(control$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=control$base.sales)
pct99.treated <- quantile(treated$dm.L.ln_cpricei2, probs = 0.99, na.rm = T, weight=treated$base.sales)

all_pi[, cs_price := ifelse(dm.L.ln_cpricei2 > max(pct1.treated, pct1.control) & 
                              dm.L.ln_cpricei2 < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_price := ifelse(is.na(dm.L.ln_cpricei2), 0, cs_price)]

## Keep within the common support
#all_pi <- all_pi[cs_price == 1,]

# ## cut the tails (keep between 1st and 99th percentile)
# pct1 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.01, na.rm = T, weight=base.sales)
# pct99 <- quantile(all_pi$dm.ln_cpricei2, probs = 0.99, na.rm = T, weight=base.sales)
# all_pi <- all_pi[(dm.ln_cpricei2 > pct1 & dm.ln_cpricei2 < pct99),]

#### Prepare Estimations

outcomes <- c("w.ln_cpricei2", "w.ln_quantity3", "w.ln_pricei2", "w.ln_sales")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

LRdiff_res <- data.table(NULL)
## Run in levels
for (Y in c(outcomes)) {
  for (FE in FE_opts) {
    
    formula1 <- as.formula(paste0(
      Y, "~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
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
    
    ## Run pre-trends
    RHS <- "ln_sales_tax"
    for (k in 2:4) {
      
      formula1 <- as.formula(paste0(
        Y, "~ F", k,".w.ln_sales_tax | ", FE, " | 0 | module_by_state"
      ))
      flog.info("Estimating pretrend %s with %s as outcome with %s FE.", k, Y, FE)
      res1 <- felm(formula = formula1, data = all_pi,
                   weights = all_pi$base.sales)
      flog.info("Finished estimating %s with %s as outcome with %s FE.", k, Y, FE)
      
      
      ## attach results
      flog.info("Writing results...")
      res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
      res1.dt[, outcome := Y]
      res1.dt[, controls := FE]
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
}
