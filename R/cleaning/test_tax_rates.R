##### Quarterly
### Make quarterly tax rates
sales.test4 <- fread(sales_data_path)
tax.data <- fread(tax_rate_data_path)
taxability.data <- fread(taxability_data_path)
expanded.data <- fread(expanded_data_path)

### Make expanded dataset mergeable
expanded.data$year<-as.numeric(str_sub(expanded.data$date, -4, -1)) #Create year variable
expanded.data$month<-rep(seq(1,12,1), nrow(expanded.data)/12)  # Create month variable
expanded.data$state_tax<-expanded.data$sales_tax_rate/100

### Subset Data
sales.test<-sales.test4[sales.test4$year<2011,]
tax.test<-tax.data[tax.data$year<2011,]
taxability.test<-taxability.data[taxability.data$year<2011,]
expanded.test<-expanded.data[expanded.data$year<2011,]

### Keep only relevant variables
sales.test <- sales.test[, c("store_code_uc", "product_module_code", "year", "month", "sales", "nweeks", "fips_state", "fips_county")]
taxability.test <- taxability.test[, c("year", "month", "fips_state", "product_module_code", "taxability", "reduced_rate")]
expanded.test <- expanded.test[, c("year", "month", "fips_state", "state_tax")]
tax.test <- tax.test[, c("year", "month", "fips_state", "fips_county", "sales_tax", "state_tax")]

### Merge Data
sales.test2 <- merge(sales.test, taxability.test, by = c("fips_state", "product_module_code", "year", "month"), all.x = T)
sales.test3 <- merge(sales.test2, tax.test, by = c("fips_state", "fips_county", "year", "month"), all = T)
sales.test4 <- merge(sales.test3, expanded.test, by = c("fips_state", "year", "month"), all = T)
sales.test4[, state_tax := ifelse(year==2006 | year==2007, state_tax.y, state_tax.x)]
sales.test4 <- sales.test4[,-c("state_tax.x", "state_tax.y")]

## Create sales weights
sales.test4[, sales := sales*nweeks]
sales.test4[, quarter := ceiling(month/3)]

### Generate New Tax Variables for 2006/07 & 2015/16
data_2008q1<-sales.test4[sales.test4$year==2008 & sales.test4$quarter==1,c("year","quarter","store_code_uc","sales_tax")]
data_2008q1$total_tax_2008q1<-as.numeric(data_2008q1$sales_tax)
data_2008q1 <- data_2008q1[, list(sales_tax = mean(sales_tax), total_tax_2008q1=mean(total_tax_2008q1)), by = .(store_code_uc, year, quarter)] # Collapse by utc/year/quarter
data_2008q1<-data_2008q1[,-c("year","quarter","sales_tax")]
sales.test4 <- merge(sales.test4, data_2008q1, by=c("store_code_uc"), all.x=T)
sales.test4[, loc2008q1 := ifelse(year==2006 | year==2007, total_tax_2008q1-state_tax, NA)]
sales.test4[, sales_tax := ifelse(year==2006 | year==2007, loc2008q1+state_tax, sales_tax)]


## Create tax rates
sales.test4[, sales_tax := 1 + sales_tax]
sales.test4[, reduced_rate := 1 + reduced_rate]
sales.test4[, sales_tax := ifelse(taxability == 0, 1, sales_tax)]
sales.test4[, sales_tax := ifelse(taxability == 2, NA, sales_tax)]
sales.test4[, sales_tax := ifelse(is.na(reduced_rate) == F, reduced_rate, sales_tax)]


### Keep Observations with sales tax reading
sales.test4 <- sales.test4[is.na(sales.test4$sales_tax)==F,]

# Create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## Collapse
sales.test_collapse <- sales.test4[, list(sales_tax = mean(sales_tax), sales_tax_wtd = weighted.mean(sales_tax, w = sales), taxability = getmode(taxability)), by = .(store_code_uc, product_module_code, year, quarter)]

#sales.test4 <- sales.test4[, list(sales_tax = mean(sales_tax), sales_tax_wtd = weighted.mean(sales_tax, w = sales), taxability = mode(taxability)), by = .(store_code_uc, product_module_code, year, quarter)]
sales.test4[, taxability := ifelse(taxability == 2, NA, taxability)]
