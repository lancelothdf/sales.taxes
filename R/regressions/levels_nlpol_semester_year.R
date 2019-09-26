#' Run Non linear estimation using polynomials (parametric) in levels
#' Year and quarter

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
output.results.file <- "Data/pol_semesterly_and_yearly_levels.csv"
output.path <- "../../home/slacouture/NLP"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

outcomes <- c("ln_cpricei2", "ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")

RHS <- "ln_sales_tax"

## For predicted values
# Discretize taxrate
tax_values <-seq(min(all_pi$ln_sales_tax), max(all_pi$ln_sales_tax), length.out = 15)
# The value of 0 is problematic: replace it for a very small value
if (tax_values[1] == 0) tax_values[1] <- 0.001

### Run level twoway FE semester data --------------------------------
LRdiff_res <- data.table(NULL)
for (n in 2:6) {
  # First create power
  all_pi[, paste0("ln_sales_tax_",n) := ln_sales_tax^(n)]
  # Add to formula
  RHS <- paste(RHS, paste0("ln_sales_tax_",n), sep = " + ")

  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~", RHS ," | ", FE, " + store_by_module | 0 | module_by_state"
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
      res1.dt[, degree.or := n]
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
      

      
      # Compute predicted values of the derivative
      pred_b <- rep(0,15)
      pred_se <- rep(0,15)
      for (i in 1:15) {
        plc.formula1 <- paste0("ln_sales_tax +", paste0(paste0(paste0(2:n,"*",paste0((tax_values[i]),"^",1:(n-1))), "*ln_sales_tax_",2:6), collapse = " + "), " = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b[i] <- coef(summary(pplc.test1))[[1]]
        pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]
      }
      # Create data
      coef.dt <- data.table(tax_values, pred_b, pred_se)
      
      # Output file
      graphout <- paste0(output.path,"/", Y, "_", n,"_", FE, "semester.png")
      # Plot
      ggplot(data = coef.dt, mapping = aes(x = tax_values, y = pred_b)) +
        geom_point(size = 2, alpha = .5) +
        geom_line(linetype = "dashed") +
        geom_errorbar(aes(ymin = pred_b - 1.96 * pred_se,
                          ymax = pred_b + 1.96 * pred_se), width = .005) +
        theme_bw() +
        labs(x = "Sales Tax", y = paste0("Predicted response on ",Y), color = NULL) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha = .8)
      ggsave(Plot)
      
    }
  }
}

### Set up Yearly Data ---------------------------------
all_pi <- fread(data.year)

outcomes <- c("ln_cpricei2", "ln_quantity3")
FE_opts <- c("region_by_module_by_time", "division_by_module_by_time")
RHS <- "ln_sales_tax"

## merge region a division
geo_dt <- structure(list(
  fips_state = c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L,
                 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
                 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L,
                 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L),
  region = c(3L, 4L, 4L, 3L, 4L, 4L, 1L, 3L, 3L, 3L, 4L, 4L, 2L, 2L, 2L, 2L, 3L,
             3L, 1L, 3L, 1L, 2L, 2L, 3L, 2L, 4L, 2L, 4L, 1L, 1L, 4L, 1L, 3L, 2L,
             2L, 3L, 4L, 1L, 1L, 3L, 2L, 3L, 3L, 4L, 1L, 3L, 4L, 3L, 2L, 4L),
  division = c(6L, 9L, 8L,  7L, 9L, 8L, 1L, 5L, 5L, 5L, 9L, 8L, 3L, 3L, 4L, 4L,
               6L, 7L, 1L, 5L, 1L, 3L, 4L, 6L, 4L, 8L, 4L, 8L, 1L, 2L, 8L, 2L,
               5L, 4L, 3L,  7L, 9L, 2L, 1L, 5L, 4L, 6L, 7L, 8L, 1L, 5L, 9L, 5L, 3L, 8L)),
  class = "data.frame", row.names = c(NA, -50L))
setDT(geo_dt)
all_pi <- merge(all_pi, geo_dt, by = "fips_state")

all_pi <- data.table(all_pi)
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
all_pi[, module_by_state := .GRP, by = .(product_module_code, fips_state)]


# Create initial level of tax rate
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
# Create interaction and interaction squared
all_pi[, D.ln_sales_tax_L.ln_sales_tax := L.ln_sales_tax*D.ln_sales_tax]
all_pi[, D.ln_sales_tax_L.ln_sales_tax_2 := ((L.ln_sales_tax)^2)*D.ln_sales_tax]


# Keep relevant years
all_pi <- all_pi[between(year, 2008, 2014)]


## For predicted values
# Discretize taxrate
tax_values <-seq(min(all_pi$ln_sales_tax), max(all_pi$ln_sales_tax), length.out = 15)
# The value of 0 is problematic: replace it for a very small value
if (tax_values[1] == 0) tax_values[1] <- 0.001

### Run Level yearly data --------------------------------
for (n in 2:6) {
  # First create power
  all_pi[, paste0("ln_sales_tax_",n) := ln_sales_tax^(n)]
  # Add to formula
  RHS <- paste(RHS, paste0("ln_sales_tax_",n), sep = " + ")
  for (Y in c(outcomes)) {
    for (FE in FE_opts) {
      
      formula1 <- as.formula(paste0(
        Y, "~", RHS ," | ", FE, " + store_by_module | 0 | module_by_state"
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
      res1.dt[, window := "year"]
      res1.dt[, degree.or := n]
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
      
      
      # Compute predicted values of the derivative
      pred_b <- rep(0,15)
      pred_se <- rep(0,15)
      for (i in 1:15) {
        plc.formula1 <- paste0("ln_sales_tax +", paste0(paste0(paste0(2:n,"*",paste0((tax_values[i]),"^",1:(n-1))), "*ln_sales_tax_",2:6), collapse = " + "), " = 0")
        # Predictred
        pplc.test1 <- glht(res1, linfct = c(plc.formula1))
        pred_b[i] <- coef(summary(pplc.test1))[[1]]
        pred_se[i] <- sqrt(vcov(summary(pplc.test1)))[[1]]
      }
      # Create data
      coef.dt <- data.table(tax_values, pred_b, pred_se)
      
      # Output file
      graphout <- paste0(output.path,"/", Y, "_", n,"_", FE, "year.png")
      # Plot
      ggplot(data = coef.dt, mapping = aes(x = tax_values, y = pred_b)) +
        geom_point(size = 2, alpha = .5) +
        geom_line(linetype = "dashed") +
        geom_errorbar(aes(ymin = pred_b - 1.96 * pred_se,
                          ymax = pred_b + 1.96 * pred_se), width = .005) +
        theme_bw() +
        labs(x = "Sales Tax", y = paste0("Predicted response on ",Y), color = NULL) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha = .8)
      ggsave(Plot)
    }
  }
}

