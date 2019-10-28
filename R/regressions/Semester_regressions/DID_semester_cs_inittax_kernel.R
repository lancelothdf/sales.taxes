#' Sales Taxes Project
#' This code run a Basic DiD model by initial tax rate or price level
#' The idea is to try to see whether we observe non linearities in this sense
#' We estimate both the "short run" and the "long run" models splitting the sample
#' by quantiles increasing the number of groups.
#' Here initial level means previous period and we use kernel local estimators around interest points


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
output.results.file <- "Data/DID_kernel_initial_tax_semester.csv"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)
all_pi[, w.ln_sales_tax := ln_sales_tax - mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, w.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2), by = .(store_by_module)]
all_pi[, w.ln_quantity3 := ln_quantity3 - mean(ln_quantity3), by = .(store_by_module)]

# Create lagged value (initial)
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
# Need to keep data that has initial tax rate level
all_pi <- all_pi[!is.na(L.ln_sales_tax)]

# Defining common support
control <- all_pi[D.ln_sales_tax == 0,]
treated <- all_pi[D.ln_sales_tax != 0,]

# Tax rate
pct1.control <- quantile(control$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)
pct1.treated <- quantile(treated$L.ln_sales_tax, probs = 0.01, na.rm = T, weight=all_pi$base.sales)

pct99.control <- quantile(control$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)
pct99treated <- quantile(treated$L.ln_sales_tax, probs = 0.99, na.rm = T, weight=all_pi$base.sales)

all_pi[, cs_tax := ifelse(L.ln_sales_tax > max(pct1.treated, pct1.control) & 
                            L.ln_sales_tax < min(pct99treated, pct99.control), 1, 0)]
# Make sure missings are 0s
all_pi[, cs_tax := ifelse(is.na(L.ln_sales_tax), 0, cs_tax)]

## Keep within the common support
all_pi <- all_pi[cs_tax == 1,]

# Discretize taxrate: common support
tax_values <-seq(min(all_pi$L.ln_sales_tax, na.rm = T), max(all_pi$L.ln_sales_tax, na.rm = T), length.out = 15)
# The value of 0 is problematic: replace it for a very small value
if (tax_values[1] == 0) tax_values[1] <- 0.001

# compute bandwidths to estimate
bw <- tax_values[15]-tax_values[1]
pot.bws <- c(1/3, 1/5, 1/7, 1/10)


outcomes.changes <- c("D.ln_cpricei2", "D.ln_quantity3")
outcomes.within <- c("w.ln_cpricei2", "w.ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


LRdiff_res <- data.table(NULL)
## Run in Diffs
for (Y in c(outcomes.changes)) {
  for (h in pot.bws) {
    for (FE in FE_opts) {
      for (point in tax_values) {
        
        # Define Sample
        sample <- all_pi[abs(L.ln_sales_tax - (point)) <= (h)*(bw), ]
        # Create weight: Epanechnikov
        sample[, w := base.sales*(3/4)*(1-((L.ln_sales_tax - (point))/(h)*(bw))^2)]
        
        formula1 <- as.formula(paste0(
          Y, "~ D.ln_sales_tax | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating with %s as outcome with %s FE in %s", Y, FE, point)
        res1 <- felm(formula = formula1, data = sample,
                     weights = sample$w)
        flog.info("Finished estimating with %s as outcome with %s FE in %s", Y, FE, point)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, window := "semester"]
        res1.dt[, spec := "changes"]
        res1.dt[, at := point]
        res1.dt[, bdw := h]
        # Add summary values
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        res1.dt[, N_obs := nrow(sample)]
        res1.dt[, N_modules := length(unique(sample$product_module_code))]
        res1.dt[, N_stores :=  length(unique(sample$store_code_uc))]
        res1.dt[, N_counties := uniqueN(sample, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(sample, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(sample, by = c("fips_state", "fips_county",
                                                             "product_module_code"))]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, output.results.file)
        
        
      }
      
      
    }
  }
}


## Run within

## Run in Diffs
for (Y in c(outcomes.within)) {
  for (h in pot.bws) {
    for (FE in FE_opts) {
      for (point in price_values) {
        
        # Define Sample
        sample <- all_pi[abs(dm.L.ln_cpricei2 - (point)) <= (h)*(bw)]
        # Create weight: Epanechnikov
        sample[, w := base.sales*(3/4)*(1-((dm.L.ln_cpricei2 - (point))/(h)*(bw))^2)]
        
        formula1 <- as.formula(paste0(
          Y, "~ w.ln_sales_tax | ", FE, " | 0 | module_by_state"
        ))
        flog.info("Estimating with %s as outcome with %s FE in %s", Y, FE, point)
        res1 <- felm(formula = formula1, data = sample,
                     weights = sample$w)
        flog.info("Finished estimating with %s as outcome with %s FE in %s", Y, FE, point)
        
        
        ## attach results
        flog.info("Writing results...")
        res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
        res1.dt[, outcome := Y]
        res1.dt[, controls := FE]
        res1.dt[, window := "semester"]
        res1.dt[, spec := "within"]
        res1.dt[, at := point]
        res1.dt[, bdw := h]
        # Add summary values
        res1.dt[, Rsq := summary(res1)$r.squared]
        res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
        res1.dt[, N_obs := nrow(sample)]
        res1.dt[, N_modules := length(unique(sample$product_module_code))]
        res1.dt[, N_stores :=  length(unique(sample$store_code_uc))]
        res1.dt[, N_counties := uniqueN(sample, by = c("fips_state", "fips_county"))]
        res1.dt[, N_years := uniqueN(sample, by = c("year"))]
        res1.dt[, N_county_modules := uniqueN(sample, by = c("fips_state", "fips_county",
                                                             "product_module_code"))]
        LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
        fwrite(LRdiff_res, output.results.file)
        
      }
    }
  }
}

