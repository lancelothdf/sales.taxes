#' Explore quantities and prices 
#' Plot scatter plots to disentangle what goes on at larger values of tax rates
#' We plot the residualized outcomes in the semester data
#' We use division x module x time for now


library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)


setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"

## output filepaths ----------------------------------------------
output.path <- "../../home/slacouture/NLP"


### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

outcomes <- c("ln_cpricei2", "ln_quantity3")
outcomesFD <- c("D.ln_cpricei2", "D.ln_quantity3")

# #### Run loop res vs. lev ----------------------
# for (Y in outcomes) {
#   
#   # Run the FE regression with weights
#   formula1 <- as.formula(paste0(
#     Y, " ~ 1 | division_by_module_by_time + store_by_module | 0 | module_by_state"
#   ))
#   res1 <- felm(formula = formula1, data = all_pi,
#                weights = all_pi$base.sales)
#   
#   # Residualize
#   all_pi$res <- res1$residuals
#   
#   # Plot the residuals
#   graphout <- paste0(output.path, "/res_",Y, ".png")
#   ggplot(all_pi, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
#     stat_binhex(aes(fill=log(..count..)), bins = 50)
#   ggsave(graphout)
#   
#   # Plot the residuals comming from non-0 changes
#   graph.data <- all_pi[D.ln_sales_tax !=0,]
#   graphout <- paste0(output.path, "/res_",Y, "no0change.png")
#   ggplot(graph.data, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
#     stat_binhex(aes(fill=log(..count..)), bins = 50)
#   ggsave(graphout)
#   
#   
# }
# 
# for (Y in outcomesFD) {
#   
#   # Run the FE regression with weights
#   formula1 <- as.formula(paste0(
#     Y, " ~ 1 | division_by_module_by_time | 0 | module_by_state"
#   ))
#   res1 <- felm(formula = formula1, data = all_pi,
#                weights = all_pi$base.sales)
#   
#   # Residualize
#   all_pi$res <- res1$residuals
#   
#   # Plot the residuals
#   graphout <- paste0(output.path, "/res_",Y, ".png")
#   ggplot(all_pi, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
#     stat_binhex(aes(fill=log(..count..)), bins = 50)
#   ggsave(graphout)
#   
# 
#   # Plot the residuals comming from non-0 changes
#   graph.data <- all_pi[D.ln_sales_tax !=0,]
#   graphout <- paste0(output.path, "/res_",Y, "no0change.png")
#   ggplot(graph.data, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
#     stat_binhex(aes(fill=log(..count..)), bins = 50)
#   ggsave(graphout)
#   
# 
# }

### Res vs. res set up --------------

# Create "initial" tax rate bin
all_pi[, L.ln_sales_tax := ln_sales_tax - D.ln_sales_tax]
all_pi[, init_tax_bin := floor(L.ln_sales_tax/0.0125) + 1]

# Residualize tax rate and D. tax rate
taxlev <- felm(formula = as.formula("ln_sales_tax ~ 1 | division_by_module_by_time + store_by_module | 0 | module_by_state"),
               data = all_pi,
               weights = all_pi$base.sales)
all_pi$ln_sales_tax.res <- taxlev$residuals

taxfd <- felm(formula = as.formula("D.ln_sales_tax ~ 1 | division_by_module_by_time | 0 | module_by_state"),
               data = all_pi,
               weights = all_pi$base.sales)
all_pi$D.ln_sales_tax.res <- taxfd$residuals


#### Run loop res vs. res set up ----------------------
# for (Y in outcomes) {
#   
#   # Run the FE regression with weights
#   formula1 <- as.formula(paste0(
#     Y, " ~ 1 | division_by_module_by_time + store_by_module | 0 | module_by_state"
#   ))
#   res1 <- felm(formula = formula1, data = all_pi,
#                weights = all_pi$base.sales)
#   
#   # Residualize
#   all_pi$res <- res1$residuals
#   
#   # Plot the residuals by bin
#   for (bin in unique(all_pi$init_tax_bin)) {
#     
#     ## Full residuals
#     graph.data <- all_pi[init_tax_bin == bin,]
#     graphout <- paste0(output.path, "/res by bin/res_",Y, "_bin", bin,".png")
#     ggplot(graph.data, aes(x = ln_sales_tax.res, y = res, weights = base.sales)) + 
#       stat_binhex(aes(fill=log(..count..)), bins = 50, colour="white") + scale_fill_gradientn(colours=c("yellow","black")) +
#       labs(x = "(residualized) Sales Tax in bin", y = paste0("(residualized)", Y), color = NULL)
#       
#     ggsave(graphout)
#     
#     # Plot the residuals comming from non-0 changes
#     graph.data <- graph.data[D.ln_sales_tax !=0,]
#     graphout <- paste0(output.path, "/res by bin/res_",Y, "_bin", bin, "no0change.png")
#     ggplot(graph.data, aes(x = ln_sales_tax.res, y = res, weights = base.sales)) + 
#       stat_binhex(aes(fill=log(..count..)), bins = 50, colour="white") + scale_fill_gradientn(colours=c("yellow","black")) +
#       labs(x = "(residualized) Sales Tax in bin", y = paste0("(residualized)", Y), color = NULL)
#     ggsave(graphout)    
#     
#     
#   }
# 
# }

# for (Y in outcomesFD) {
#   
#   # Run the FE regression with weights
#   formula1 <- as.formula(paste0(
#     Y, " ~ 1 | division_by_module_by_time | 0 | module_by_state"
#   ))
#   res1 <- felm(formula = formula1, data = all_pi,
#                weights = all_pi$base.sales)
#   
#   # Residualize
#   all_pi$res <- res1$residuals
#   
#   # Plot the residuals by bin
#   for (bin in unique(all_pi$init_tax_bin)) {
#     
#     ## Full residuals
#     graph.data <- all_pi[init_tax_bin == bin,]
#     graphout <- paste0(output.path, "/res by bin/res_",Y, "_bin", bin,".png")
#     ggplot(graph.data, aes(x = D.ln_sales_tax.res, y = res, weights = base.sales)) + 
#       stat_binhex(aes(fill=log(..count..)), bins = 50, colour="white") + scale_fill_gradientn(colours=c("yellow","black")) +
#       labs(x = "(residualized) Delta Sales Tax in bin", y = paste0("(residualized)", Y), color = NULL)
#     
#     ggsave(graphout)
#     
#     # Plot the residuals comming from non-0 changes
#     graph.data <- graph.data[D.ln_sales_tax !=0,]
#     graphout <- paste0(output.path, "/res by bin/res_",Y, "_bin", bin, "no0change.png")
#     ggplot(graph.data, aes(x = D.ln_sales_tax.res, y = res, weights = base.sales)) + 
#       stat_binhex(aes(fill=log(..count..)), bins = 50, colour="white") + scale_fill_gradientn(colours=c("yellow","black")) +
#       labs(x = "(residualized) Delta Sales Tax in bin", y = paste0("(residualized)", Y), color = NULL)
#     ggsave(graphout)    
#     
#     
#   }
#   
# }

#### Plot binscatters instead of hexagonal heatmaps ------------------

# Minor change: divide the mean sales tax for the model in levels
all_pi[, m.ln_sales_tax := mean(ln_sales_tax), by = .(store_by_module)]
all_pi[, init_tax_bin_m := floor(m.ln_sales_tax/0.0125) + 1 ]


for (Y in outcomes) {

  # Run the FE regression with weights
  formula1 <- as.formula(paste0(
    Y, " ~ 1 | division_by_module_by_time + store_by_module | 0 | module_by_state"
  ))
  res1 <- felm(formula = formula1, data = all_pi,
               weights = all_pi$base.sales)

  # Residualize
  all_pi$res <- res1$residuals

  # Plot the residuals by bin
  for (bin in sort(unique(all_pi$init_tax_bin))) {

    ## Full residuals
    graph.data <- all_pi[init_tax_bin_m == bin,]
    # identify quantile (50)
    graph.data[ , quartile := cut(ln_sales_tax.res,
                            breaks = quantile(ln_sales_tax.res, probs = seq(0, 1, by = 1/30), weights = base.sales, na.rm = T), 
                            labels = 1:30, right = FALSE)]
    # collapse by quartile
    graph.data <- graph.data[ , .(ln_sales_tax.res = median(ln_sales_tax.res, na.rm = T), 
                    res = weighted.mean(res, w = base.sales, na.rm = T)), by = .(quartile)]

    graphout <- paste0(output.path, "/res by bin scatter/res_",Y, "_bin", bin, "_full.png")
    ggplot(graph.data, aes(x = ln_sales_tax.res , y = res)) +
      geom_point() + labs(x = "(residualized) Sales Tax in bin", y = paste0("(residualized) ", Y), color = NULL)

    ggsave(graphout)

    # Plot the residuals comming from non-0 changes
    graph.data <- all_pi[D.ln_sales_tax !=0 & init_tax_bin_m == bin,]
    graph.data[ , quartile := cut(ln_sales_tax.res,
                                  breaks = quantile(ln_sales_tax.res, probs = seq(0, 1, by = 1/15), weights = base.sales, na.rm = T), 
                                  labels = 1:15, right = FALSE)]
    # collapse by quartile
    graph.data <- graph.data[ , .(ln_sales_tax.res = median(ln_sales_tax.res, na.rm = T), 
                                  res = weighted.mean(res, w = base.sales, na.rm = T)), by = .(quartile)]
    
    
    graphout <- paste0(output.path, "/res by bin scatter/res_",Y, "_bin", bin, "_no0change.png")
    ggplot(graph.data, aes(x = ln_sales_tax.res , y = res)) +
      geom_point() + labs(x = "(residualized) Sales Tax in bin", y = paste0("(residualized) ", Y), color = NULL)
    ggsave(graphout)


  }

}

for (Y in outcomesFD) {

  # Run the FE regression with weights
  formula1 <- as.formula(paste0(
    Y, " ~ 1 | division_by_module_by_time | 0 | module_by_state"
  ))
  res1 <- felm(formula = formula1, data = all_pi,
               weights = all_pi$base.sales)

  # Residualize
  all_pi$res <- res1$residuals

  # Plot the residuals by bin
  for (bin in sort(-unique(all_pi$init_tax_bin))) {

    ## Full residuals
    graph.data <- all_pi[init_tax_bin == bin,]
    # identify quantile (50)
    graph.data[ , quartile := cut(D.ln_sales_tax.res,
                                  breaks = quantile(D.ln_sales_tax.res, probs = seq(0, 1, by = 1/30), weights = base.sales, na.rm = T), 
                                  labels = 1:30, right = FALSE)]
    # collapse by quartile
    graph.data <- graph.data[ , .(D.ln_sales_tax.res = median(D.ln_sales_tax.res, na.rm = T), 
                    res = weighted.mean(res, w = base.sales, na.rm = T)), by = .(quartile)]
    
    graphout <- paste0(output.path, "/res by bin scatter/res_",Y, "_bin", bin, "_full.png")
    ggplot(graph.data, aes(x = D.ln_sales_tax.res , y = res)) +
      geom_point() + labs(x = "(residualized) Delta Sales Tax in bin", y = paste0("(residualized) ", Y), color = NULL)
    
    ggsave(graphout)
    
    # Plot the residuals comming from non-0 changes
    graph.data <- all_pi[D.ln_sales_tax !=0 & init_tax_bin == bin,]
    graph.data[ , quartile := cut(D.ln_sales_tax.res,
                                  breaks = quantile(D.ln_sales_tax.res, probs = seq(0, 1, by = 1/15), weights = base.sales, na.rm = T), 
                                  labels = 1:15, right = FALSE)]
    # collapse by quartile
    graph.data <- graph.data[ , .(D.ln_sales_tax.res = median(D.ln_sales_tax.res, na.rm = T),
                                  res = weighted.mean(res, w = base.sales, na.rm = T)), by = .(quartile)]
    
    
    graphout <- paste0(output.path, "/res by bin scatter/res_",Y, "_bin", bin, "_no0change.png")
    ggplot(graph.data, aes(x = D.ln_sales_tax.res , y = res)) +
      geom_point() + labs(x = "(residualized) Delta Sales Tax in bin", y = paste0("(residualized) ", Y), color = NULL)
    ggsave(graphout)


  }

}
