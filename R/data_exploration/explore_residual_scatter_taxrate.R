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

#### Run loop ----------------------
for (Y in outcomes) {
  
  # Run the FE regression with weights
  formula1 <- as.formula(paste0(
    Y, " ~ 1 | division_by_module_by_time + store_by_module | 0 | module_by_state"
  ))
  res1 <- felm(formula = formula1, data = all_pi,
               weights = all_pi$base.sales)
  
  # Residualize
  all_pi$res <- res1$residuals
  
  # Plot the residuals
  graphout <- paste0(output.path, "/res_",Y, ".png")
  ggplot(all_pi, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50)
  ggsave(graphout)
  
  # Zoom in at y-axis
  graphout <- paste0(output.path, "/res_",Y, "_zoom.png")
  ggplot(all_pi, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50) + coord_cartesian(ylim=c(-1,1))
  ggsave(graphout)  
  
  # Plot the residuals comming from non-0 changes
  graph.data <- all_pi[D.ln_sales_tax !=0,]
  graphout <- paste0(output.path, "/res_",Y, "no0change.png")
  ggplot(graph.data, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50)
  ggsave(graphout)
  
  # Zoom in at y-axis
  graphout <- paste0(output.path, "/res_",Y, "no0change_zoom.png")
  ggplot(graph.data, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50) + coord_cartesian(ylim=c(-1,1))
  ggsave(graphout)    
  
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
  
  # Plot the residuals
  graphout <- paste0(output.path, "/res_",Y, ".png")
  ggplot(all_pi, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50)
  ggsave(graphout)
  
  # Zoom in at y-axis
  graphout <- paste0(output.path, "/res_",Y, "_zoom.png")
  ggplot(all_pi, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50) + coord_cartesian(ylim=c(-1,1))
  ggsave(graphout) 

  # Plot the residuals comming from non-0 changes
  graph.data <- all_pi[D.ln_sales_tax !=0,]
  graphout <- paste0(output.path, "/res_",Y, "no0change.png")
  ggplot(graph.data, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50)
  ggsave(graphout)
  
  # Zoom in at y-axis
  graphout <- paste0(output.path, "/res_",Y, "no0change_zoom.png")
  ggplot(graph.data, aes(x = ln_sales_tax, y = res, weights = base.sales)) + 
    stat_binhex(bins = 50) + coord_cartesian(ylim=c(-1,1))
  ggsave(graphout)  
}

