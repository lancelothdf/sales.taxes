#' Compute de-meaned price distributions at different quantiles of the sales tax (for which you observe positive tax changes)

library(data.table)
library(futile.logger)
library(lfe)
library(multcomp)
library(ggplot2)



setwd("/project2/igaarder")


## input filepaths -----------------------------------------------
#' This data is the same as all_goods_pi_path, except it has 2015-2016 data as well.
data.semester <- "Data/Nielsen/semester_nielsen_data.csv"
data.year <- "Data/Nielsen/yearly_nielsen_data.csv"


## output filepaths ----------------------------------------------
output.results.file <- "Data/allpol_semesterly_levels.csv"
output.path <- "../../home/slacouture/NLP"

### Set up Semester Data ---------------------------------
all_pi <- fread(data.semester)

# Create de-meaned prices
all_pi[, n.ln_cpricei2 := ln_cpricei2 - mean(ln_cpricei2, weight = base.sales), by = .(module_by_time)]

# take the sample that comes only from changes
all_pi <- all_pi[D.ln_sales_tax != 0, ]

# Add quantile of ln_sales_tax column
all_pi <- all_pi[!is.na(ln_sales_tax)][, quantile := cut(ln_sales_tax,
                                                         breaks = quantile(ln_sales_tax, probs = seq(0, 1, by = 1/5), na.rm = T, weight = base.sales),
                                                         labels = 1:5, right = FALSE)]
all_pi <- all_pi[!is.na(quantile)]

##### Plot the kernel densities --------------------------
graphout <- paste0(output.path,"/norm_prices_by_quant_salestax.png")

# Plot - zoom in between -0.7 to 0.7
ggplot(all_pi, aes(x=n.ln_cpricei2, color=quantile)) +
  geom_density() +
  labs(x = "Normalized (log) Price", y = "K-Density", title = "Density by Sales Taxes") +
  scale_x_continuous(limits = c(-0.5,0.5), breaks = seq(-0.5, 0.5, 0.2))

ggsave(graphout)

#### Plot the CDF of the kernel densities -------------------

# Split the data by group and calculate the smoothed cumulative density for each group
dens = split(all_pi, all_pi$quantile) %>% 
  map_df(function(d) {
    dens = density(d$n.ln_cpricei2, from=-1, to=1)
    data.frame(x=dens$x, y=dens$y, cd=cumsum(dens$y)/sum(dens$y), group=d$quantile[1])
  })

graphout <- paste0(output.path,"/norm_prices_by_quant_salestax_cdf.png")
ggplot() +
  geom_line(data=dens, aes(x, cd, colour=group)) +
  theme_classic() +
  labs(x = "Normalized (log) Price", y = "Cumulative K-Density", 
       title = "CDF by Sales Taxes Quantiles")+
  scale_x_continuous(limits = c(-0.3,0.3), breaks = seq(-0.3, 0.3, 0.1))
ggsave(graphout)
