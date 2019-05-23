/*
Replicate analysis in Table 4 of Kroft, Laliberte, Leal-Vizcaino, and
Notowidigdo (2017). Specifically, replicate the analysis in Panel A,
but instead of estimating with a "variety" outcome, estimate on
quantity (log(sales) - log(price)).
*/

cd /project2/igaarder
log using Code/stata_test.log, replace

* prep the data
import delimited using Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv
keep if year >= 2008 & year <= 2014 & cpricei !=. & sales_tax !=.

* take logs
gen ln_cpricei = ln(cpricei)
gen ln_sales_tax = ln(sales_tax)
gen ln_quantity = ln(sales) - ln(pricei)

* get sales weights
gen sales_if_basetime = sales * (year == 2008 & quarter == 1)
bys store_code_uc product_module_code: egen base_sales = sum(sales_if_basetime)

keep store_code_uc product_module_code fips_state fips_county year quarter sales ///
       ln_cpricei ln_sales_tax ln_quantity base_sales

* balance on store-module level
bys store_code_uc product_module_code: egen N = count(cpricei)
keep if N == (2014 - 2007) * 4

* calculate expenditure shares
bys store_code_uc product_module_code: egen total_sales = sum(sales)
gen expend_share = sales / total_sales
gen ln_expend_share = ln(expend_share)

* prep some other vars for the regression
egen yr_quarter = group(year quarter)
egen store_module = group(store_code_uc product_module_code)
egen state_by_module = group(fips_state product_module_code)
gen linear_time = year * 4 + quarter
egen module_by_time = group(year quarter product_module_code)

* run analysis on price
reghdfe ln_cpricei ln_sales_tax product_module_code#c.linear_time [aweight=base_sales], ///
    absorb(yr_quarter store_module) cluster(state_by_module)
	
log close
