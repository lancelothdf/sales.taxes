/*
Replicate analysis in Table 4 of Kroft, Laliberte, Leal-Vizcaino, and
Notowidigdo (2017). Specifically, replicate the analysis in Panel A,
but instead of estimating with a "variety" outcome, estimate on
quantity (log(sales) - log(price)).
*/
cd /project2/igaarder

cap log close
log using Code/stata_test.log, replace

ssc install ftools
ssc install reghdfe

/************************ Prep *************************/

import delimited using Data/Nielsen/price_quantity_indices_allitems_2006-2016_notaxinfo.csv, clear
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
bys store_code_uc product_module_code: egen N = count(ln_cpricei)
keep if N == (2014 - 2007) * 4

* calculate expenditure shares
bys store_code_uc product_module_code: egen total_sales = sum(sales)
gen expend_share = sales / total_sales
gen ln_expend_share = ln(expend_share)

/***** TEMPORARY: SUBSET DATA TO BE 2 YRS (FOR DEBUGGING) ********/
keep if year == 2008 | year == 2009
/*****************************************************************/

* prep some other vars for the regression
egen yr_quarter = group(year quarter)
egen store_module = group(store_code_uc product_module_code)
egen state_by_module = group(fips_state product_module_code)
gen linear_time = year * 4 + quarter
egen module_by_time = group(year quarter product_module_code)

* display some counts for the table
count
bys product_module_code: gen nprods = _n == 1
count if nprods
bys store_code_uc: gen nstores = _n == 1
count if nstores
bys fips_county fips_state: gen ncounties = _n == 1
count if ncounties
bys year quarter: gen nquarters = _n == 1
count if nquarters
bys fips_county fips_state product_module_code: gen ncountymods = _n == 1
count if ncountymods

/************************ Analysis *************************/

* run analysis on price
reghdfe ln_cpricei ln_sales_tax product_module_code#c.linear_time [aweight=base_sales], ///
    absorb(yr_quarter store_module) cluster(state_by_module)
	
reghdfe ln_cpricei ln_sales_tax [aweight=base_sales], ///
    absorb(module_by_time store_module) cluster(state_by_module)
	
* run analysis on quantity
reghdfe ln_quantity ln_sales_tax product_module_code#c.linear_time [aweight=base_sales], ///
    absorb(yr_quarter store_module) cluster(state_by_module)
	
reghdfe ln_quantity ln_sales_tax [aweight=base_sales], ///
    absorb(module_by_time store_module) cluster(state_by_module)
	
* run analysis on expenditure share
reghdfe ln_expend_share ln_sales_tax product_module_code#c.linear_time [aweight=base_sales], ///
    absorb(yr_quarter store_module) cluster(state_by_module)
	
reghdfe ln_expend_share ln_sales_tax [aweight=base_sales], ///
    absorb(module_by_time store_module) cluster(state_by_module)
	
log close
