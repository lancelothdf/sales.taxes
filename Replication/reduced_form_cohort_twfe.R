#' Author: Lancelot Henry de Frahan and Santiago Lacouture
#'
# Run similar regressions as "reduced_form_evidence.R", but with county X module data 
# + Importantly run the regression cohort-by-cohort then average the effect across cohorts
# Here we define a cohort based on time of ``treatment"

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)


########## !!!!!!! ###########
setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data set contains quarterly Laspeyres indices and sales from 2006 to
#' 2016. It also contains sales tax rates from the same time period.
all_goods_pi_path <- "Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv"
#' This data set contains an old price index that Lance constructed, from
old_pi_path <- "Data/Nielsen/Quarterly_old_pi.csv"
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.

######## !!!!!! ############
data.full.path <- "Data/Nielsen/semester_nielsen_data.csv"
#data.full.path <- "LRdiff_semesterly_COUNTY_random.csv"


## output filepaths ----------------------------------------------
# output.results.file.fs <- "Data/Cohort_TWFE_fs.csv" ### Main results
# output.results.file.fsc <- "Data/Cohort_TWFE_fsc.csv" ### Main results county 
output.results.file <- "Data/Replication/Cohort_TWFE_div.csv" ### Main results county separate
boot.results.file <- "Data/Replication/Boot_cohort_TWFE_div.csv" ### Bootstrap county separate


##### Slight modifications to data -----
all_pi <- fread("Data/Replication/all_pi.csv")

# Restrict to non-imputed tax
all_pi <- all_pi[non_imp_tax == 1,]

# Make FEs a factor variable to precisely interact
all_pi[, region_by_module_by_time := factor(region_by_module_by_time)]
all_pi[, division_by_module_by_time := factor(division_by_module_by_time)]

##### Collapsing to county level -----
# We have attempted to run estimation at the store/product/semester level
# We get a memory error: "Error: cannot allocate vector of size 3578.9 Gb"
# Since tax variation comes at county level, we will collapse it at this level (county/product/semester level)

## Collapse at county-level to save some memory
all_pi <- all_pi[, list(w.ln_cpricei2 = weighted.mean(w.ln_cpricei2, w = base.sales),
                        w.ln_pricei2 = weighted.mean(w.ln_pricei2, w = base.sales),
                        w.ln_quantity3 = weighted.mean(w.ln_quantity3, w = base.sales),
                        w.ln_sales_tax = weighted.mean(w.ln_sales_tax, w = base.sales),
                        base.sales = sum(base.sales),
                        sales = sum(sales)),
                 by = .(fips_state, fips_county, product_module_code, year, semester,
                        region_by_module_by_time, division_by_module_by_time, module_by_state,
                        cal_time)]

## By-cohort TWFE
outcomes <- c("w.ln_cpricei2", "w.ln_quantity3", "w.ln_pricei2")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")


####  Fully saturated version  --------------
# We can't run this. We get a memry error
# Error: cannot allocate vector of size 427.1 Gb
# Intead we run by-cohort and bootstrap to obtain VarCov matrx

# 
# LRdiff_res <- data.table(NULL)
# for (FE in FE_opts) {
#   
#   ## Capture cohort ids
#   ids <- unique(all_pi[[FE]])
#   ids <- sort(ids)
#   
#   ## Capture cohort weights
#   all_pi_co <- all_pi[, .(base.sales = sum(base.sales)), by = c(FE)]
#   all_pi_co[, base.sales := base.sales/sum(base.sales)]
#   all_pi_co <- all_pi_co[order(get(FE))]
#   cohort.weights <- all_pi_co$base.sales
#   
#   ## Capture formula for linear test: weighted sum
#   lh.form <-  paste0(cohort.weights, paste0("*w.ln_sales_tax:", FE), ids
#                     , collapse = " + ")
#   lc.formula0 <- paste0(lh.form, " = 0")
#   
#   for (Y in c(outcomes)) {
#       
#     formula1 <- as.formula(paste0(
#       Y, "~ w.ln_sales_tax:", FE, " | ", FE, " | 0 | module_by_state"
#     ))
#     flog.info("Estimating with %s as outcome with %s FE", Y, FE)
#     res1 <- felm(formula = formula1, data = all_pi,
#                  weights = all_pi$base.sales)
#     flog.info("Finished estimating with %s as outcome with %s FE", Y, FE)
#     
#     
#     flog.info("Writing results...")
#     res1.dt <- data.table(coef(summary(res1)), keep.rownames=T)
#     res1.dt[, outcome := Y]
#     res1.dt[, FEd := FE]
#     res1.dt[, Rsq := summary(res1)$r.squared]
#     res1.dt[, adj.Rsq := summary(res1)$adj.r.squared]
#     LRdiff_res <- rbind(LRdiff_res,res1.dt) ### Create table LRdiff_res in which we store all results (we start with the results we had just stored in res1.dt)
#     fwrite(LRdiff_res, output.results.file.fs)  ## Write results to a csv file 
#     
#     
#     lc.test0 <- glht(res1, linfct = c(lc.formula0))
# 
#     # Calculate the p-value
#     pval <- 2*(1 - pnorm(abs(coef(summary(lc.test0))[[1]]/sqrt(vcov(summary(lc.test0)))[[1]])))
#     
#     
#     lp.dt <- data.table(
#       rn = "agg.ln_sales_tax",
#       Estimate = coef(summary(lc.test0))[[1]],
#       `Cluster s.e.` = sqrt(vcov(summary(lc.test0)))[[1]],
#       `Pr(>|t|)` = pval,
#       outcome = Y,
#       FEd = FE,
#       Rsq = summary(res1)$r.squared,
#       adj.Rsq = summary(res1)$adj.r.squared)
#     LRdiff_res <- rbind(LRdiff_res, lp.dt, fill = T) ## Merge results to LRdiff_res
#     fwrite(LRdiff_res, output.results.file.fs) ## Write resulting file to a csv file
#     
#     
#   }
# }

####### Alternative: separate by-cohort county-level ------

reg.output.co <- function(X, dep.var, indep.var, data, FE, w) {
  
  # Capture formula
  formula1 <- as.formula(paste0(dep.var, " ~ ", indep.var))
  # Capture subset of data relevant
  co.data <- data[get(FE) == X,]

  # Check number of observations. Don't even estimate if N < 3
  if (nrow(co.data[!is.na(get(dep.var)) & !is.na(get(w))]) < 3) {
    
    res1.dt <- data.table(
      Estimate = NA,
      `Std. Error` = NA,
      `Pr(>|t|)` = NA,
      outcome = dep.var,
      cohort = X,
      `FE` = FE)
    
  }
  else {
    # Run regression
    res1 <- lm(formula = formula1, 
               data = co.data,
               weights = co.data[[w]])
    
    ## Store results
    if(!is.na(coef(res1)[2])) {
      
      res1.dt <- data.table(
        Estimate = coef(summary(res1))[ indep.var, "Estimate"],
        `Std. Error` = coef(summary(res1))[ indep.var, "Std. Error"],
        `Pr(>|t|)` = coef(summary(res1))[ indep.var, "Pr(>|t|)"],
        outcome = dep.var,
        cohort = X,
        `FE` = FE)
      
    } else { # just in case...
      
      res1.dt <- data.table(
        Estimate = NA,
        `Std. Error` = NA,
        `Pr(>|t|)` = NA,
        outcome = dep.var,
        cohort = X,
        `FE` = FE)
      
    }
    
    
    res1.dt[, paste0(w) := sum(data[get(FE) == X,][[w]])]
    if ("base.sales" %in% colnames(data) & "sales" %in% colnames(data)) {
      res1.dt[, sales := sum(data[get(FE) == X,]$sales)]
    }
    res1.dt[, "wVAR" := weighted.mean((data[get(FE) == X,][[indep.var]] - 
                                            weighted.mean(data[get(FE) == X,][[indep.var]], 
                                                          w = data[get(FE) == X,][[w]], na.rm = T))^2,
                                         w = data[get(FE) == X,][[w]], na.rm = T)]
    
  }
  
  
  return(res1.dt)
}

LRdiff_res <- data.table(NULL)
for (fe in FE_opts) {

  c_ids <- unique(sort(all_pi[[fe]])) ## Define cohorts based on YearXsemesterXmoduleXCensus Region/division
  for (y in c(outcomes)) {
    flog.info("Iteration 0. Estimating on %s using %s as FE", y, fe)
    res.l <- sapply(c_ids, FUN = reg.output.co, 
                    dep.var = y, indep.var = "w.ln_sales_tax", 
                    data = all_pi, FE = fe, w = "base.sales",
                    simplify = F)
    flog.info("Writing results...")
    res1.dt = as.data.table(data.table::rbindlist(res.l, fill = T))
    LRdiff_res <- rbind(LRdiff_res, res1.dt, fill = T)
    fwrite(LRdiff_res, output.results.file)
  }
}



#### Now bootstrap the results
### Start manual bootstrap
set.seed(1941)
ids <- unique(all_pi$module_by_state)
LRdiff_boot <- data.table(NULL)
for (rep in 1:200) {

  flog.info("Iteration %s", rep)

  # Sample by block
  sampled.ids <- data.table(module_by_state = sample(ids, replace = T))
  # Merge data to actual data
  sampled.data <- merge(sampled.ids, all_pi, by = c("module_by_state") , allow.cartesian = T, all.x = T)
  
  for (fe in FE_opts) {
    ## Remake list of unique division/region by module by time
    c_ids <- unique(sort(sampled.data[[FE]])) ## Define cohorts based on YearXsemesterXmoduleXCensus division/Region
    for (y in c(outcomes)) {
  
      flog.info("Estimating on %s using %s as FE", y, fe)
      res.l <- sapply(c_ids, FUN =  reg.output.co, 
                      dep.var = y, indep.var = "w.ln_sales_tax", 
                      data = sampled.data, FE = fe, w = "base.sales",
                      simplify = F)
      flog.info("Writing results...")
      res1.dt = as.data.table(data.table::rbindlist(res.l))
      res1.dt[, iter := rep]
      LRdiff_boot <- rbind(LRdiff_boot, res1.dt, fill = T)
      fwrite(LRdiff_boot, boot.results.file)
    }
  }
}

