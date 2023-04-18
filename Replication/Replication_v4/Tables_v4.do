* --------------------------------- *
* ----		Sales Taxes 		--- *
* - 	 Export LaTeX tables  	  - *
* --------------------------------- *

*** Set up ----------
global mainfolder "/project2/igaarder/Data"
global inputs "$mainfolder/Replication"
global outputfolder "$inputs/figsandtabs"

cap {
	net from http://www.stata-journal.com/software/sj12-4
	net install sg97_5
}
** Table 1: Descriptive statistics on sales tax changes in the US (2008-2014) **
{
** Open Data of tax lavels
mat drop _all
use "$mainfolder/county_monthly_tax_rates_w_pop.dta", clear

*Create Matrix to export
matrix table = J(7, 7, .)

* Capture stats
local r = 1
foreach x of var city_tax county_tax state_tax sales_tax {

	local ++r
	qui sum `x' [fw = population], d
	matrix table[`r', 1] = r(mean) * 100
	matrix table[`r', 2] = r(sd) * 100
	matrix table[`r', 3] = r(min) * 100
	matrix table[`r', 4] = r(p25) * 100
	matrix table[`r', 5] = r(p50) * 100
	matrix table[`r', 6] = r(p75) * 100
	matrix table[`r', 7] = r(max) * 100

}
** Open Tax base
import delimited "$mainfolder/sales_taxability_state_panel.csv", clear
* Sum across time 
collapse (sum) sales, by(fips_state taxability)
bys fips_state: egen tot_sales = sum(sales)
gen share_sales = sales/tot_sales
qui sum share_sales if taxability == 1, d
matrix table[7, 1] = r(mean) * 100
matrix table[7, 2] = r(sd) * 100
matrix table[7, 3] = r(min) * 100
matrix table[7, 4] = r(p25) * 100
matrix table[7, 5] = r(p50) * 100
matrix table[7, 6] = r(p75) * 100
matrix table[7, 7] = r(max) * 100


frmttable using "$outputfolder/Tax_levels", replace tex statmat(table) sdec(2) coljust(lcccccccc) hlines(110000101)  ///
ctitle("", "Mean", "SD", "Min", "p25", "Median", "p75", "Max") rtitle("\textit{Tax Rates}"  \ "City"\ "County"\ "State"\ "Total" \ "\textit{Tax Base}" \ "Taxable Sales") ///
fragment
}


** Table 2: Descriptive statistics on sales taxes in the US (pp.; average between 2008-2014).  **
{
** Open Data
mat drop _all
use "$mainfolder/tax_reforms_by_level.dta", clear

*Create Matrix to export
matrix table = J(4, 8, .)

** Extract info By Level
qui levelsof level, local(lev)
local r = 0
local rows
foreach x of local lev {
	
	local ++r
	qui sum tax_change if level == "`x'"
	local N = r(N)
	qui sum tax_change if level == "`x'" & tax_change > 0
	matrix table[`r', 1] = `N'
	local Npos = r(N)
	matrix table[`r', 2] = (`Npos'/`N') * 100
	qui sum tax_change if level == "`x'" & tax_change > 0 [fw = population], d
	matrix table[`r', 3] = r(mean)* 100
	matrix table[`r', 4] = r(max)* 100
	matrix table[`r', 5] = (r(p75) - r(p25)) * 100
	qui sum tax_change if level == "`x'"  & tax_change < 0 [fw = population], d
	local Npos = r(N)
	matrix table[`r', 6] = r(mean)* 100
	matrix table[`r', 7] = r(min)* 100
	matrix table[`r', 8] = (r(p75) - r(p25)) * 100
	local name = strproper("`x'")
	local rows = `" `rows' `name' "'
}

** Extract Pooled
local rows `"`rows' "Pooled" "'
local ++r
qui sum tax_change
local N = r(N)
matrix table[`r', 1] = `N'
qui sum tax_change if tax_change > 0
local Npos = r(N)
matrix table[`r', 2] = `Npos'/`N' * 100
qui sum tax_change if tax_change > 0 [fw = population], d
matrix table[`r', 3] = r(mean) * 100
matrix table[`r', 4] = r(max) * 100
matrix table[`r', 5] = (r(p75) - r(p25)) * 100
qui sum tax_change if tax_change < 0 [fw = population], d
local Npos = r(N)
matrix table[`r', 6] = r(mean)* 100
matrix table[`r', 7] = r(min)* 100
matrix table[`r', 8] = (r(p75) - r(p25))* 100

* Name Rows 
mat rownames table = `rows'

** Export table
frmttable using "$outputfolder/Tax_changes", replace tex statmat(table) sdec(0,1,2,2,2,2,2,2) coljust(lcccccccc) hlines(1010011)  ///
ctitle("", "", "", "Increases", "", "", "Decreases", "", "" \ "Level", "N", "\% increases", "Mean", "Max", "IQR", "Mean", "Max", "IQR") multicol(1,4,3;1,7,3) ///
varlabels fragment

}




** Table 3: Reduced-form evidence of the effect of sales taxes on consumer price. **
{
*** Main version: using 2-years
* Create output matrix
mat drop _all
matrix table = J(3,10,.)
matrix stars = J(3,10,0)
* Import Results File (no controls)
import delimited "$inputs/LRdiff_semesterly_main.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_cpricei2" & sample == "all"

* Capture values. Columns 1 and 2
local c = 1
foreach control in region_by_module_by_time division_by_module_by_time {
	
	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag4.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "`control'" 
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "`control'"
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

}
* 
import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_cpricei2" & sample == "all"

* Capture values. Columns 3, 4 and 5
forvalues e=2/4 {

	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag4.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "division_by_module_by_time" & econ == `e'	
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

} 


* Export LaTex table
frmttable using  "$outputfolder/Passthrough_1", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
ctitle("", "(1)", "(2)", "(3)", "(4)", "(5)") coljust(lccccc) hlines(11000001{0}1) fragment ///
rtitle("On-Impact" \ "" \ "6 months" \ "" \ "2 years") ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "", "", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "X", "X", "X" \ "" \"Lagged Econ (1-2)" , "", "", "X", "", "" \ "" \"Lagged Econ (1-3)" , "", "", "", "X", "" \ "" \"Lagged Econ (1-4)" , "", "", "", "", "X" \ "" \"Adj. Rsq", "`r2_1'", "`r2_3'", "`r2_5'", "`r2_7'", "`r2_9'")



*** Version 2: using 1-year
* Create output matrix
mat drop _all
matrix table = J(3,10,.)
matrix stars = J(3,10,0)
* Import Results File (no controls)
import delimited "$inputs/LRdiff_semesterly_main.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_cpricei2" & sample == "all"

* Capture values. Columns 1 and 2
local c = 1
foreach control in region_by_module_by_time division_by_module_by_time {
	
	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag2.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "`control'" 
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "`control'"
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

}
* 
import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_cpricei2" & sample == "all"

* Capture values. Columns 3, 4 and 5
forvalues e=2/4 {

	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag2.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "division_by_module_by_time" & econ == `e'	
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

} 


* Export LaTex table
frmttable using  "$outputfolder/Passthrough_2", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
ctitle("", "(1)", "(2)", "(3)", "(4)", "(5)") coljust(lccccc) hlines(11000001{0}1) fragment ///
rtitle("On-Impact" \ "" \ "6 months" \ "" \ "1 year") ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "", "", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "X", "X", "X" \ "" \"Lagged Econ (1-2)" , "", "", "X", "", "" \ "" \"Lagged Econ (1-3)" , "", "", "", "X", "" \ "" \"Lagged Econ (1-4)" , "", "", "", "", "X" \ "" \"Adj. Rsq", "`r2_1'", "`r2_3'", "`r2_5'", "`r2_7'", "`r2_9'")



*** Version 2: using 18 months
* Create output matrix
mat drop _all
matrix table = J(3,10,.)
matrix stars = J(3,10,0)
* Import Results File (no controls)
import delimited "$inputs/LRdiff_semesterly_main.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_cpricei2" & sample == "all"

* Capture values. Columns 1 and 2
local c = 1
foreach control in region_by_module_by_time division_by_module_by_time {
	
	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag3.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "`control'" 
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "`control'"
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

}
* 
import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_cpricei2" & sample == "all"

* Capture values. Columns 3, 4 and 5
forvalues e=2/4 {

	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag3.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "division_by_module_by_time" & econ == `e'	
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

} 


* Export LaTex table
frmttable using  "$outputfolder/Passthrough_3", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
ctitle("", "(1)", "(2)", "(3)", "(4)", "(5)") coljust(lccccc) hlines(11000001{0}1) fragment ///
rtitle("On-Impact" \ "" \ "6 months" \ "" \ "18 months") ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "", "", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "X", "X", "X" \ "" \"Lagged Econ (1-2)" , "", "", "X", "", "" \ "" \"Lagged Econ (1-3)" , "", "", "", "X", "" \ "" \"Lagged Econ (1-4)" , "", "", "", "", "X" \ "" \"Adj. Rsq", "`r2_1'", "`r2_3'", "`r2_5'", "`r2_7'", "`r2_9'")


}


** Table 4: Reduced-form evidence of the effect of sales taxes on quantity. **
{
** Main version: using 2 -years
* Create output matrix
mat drop _all
matrix table = J(3,10,.)
matrix stars = J(3,10,0)
* Import Results File (no controls)
import delimited "$inputs/LRdiff_semesterly_main.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_quantity3" & sample == "all"


* Capture values. Columns 1 and 2
local c = 1
foreach control in region_by_module_by_time division_by_module_by_time {
	
	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag4.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "`control'"
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "`control'"
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "`control'" 
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

}

* Import Results File. 
import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_quantity3"  & sample == "all"

* Capture values. Columns 3, 4 and 5
forvalues e=2/4 {

	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag4.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "division_by_module_by_time" & econ == `e'
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

} 


* Export LaTex table
frmttable using  "$outputfolder/Quantity_1", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
ctitle("", "(1)", "(2)", "(3)", "(4)", "(5)") coljust(lccccc) hlines(11000001{0}1) fragment ///
rtitle("On-Impact" \ "" \ "6 months" \ "" \ "2 years") ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "", "", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "X", "X", "X" \ "" \"Lagged Econ (1-2)" , "", "", "X", "", "" \ "" \"Lagged Econ (1-3)" , "", "", "", "X", "" \ "" \"Lagged Econ (1-4)" , "", "", "", "", "X" \ "" \"Adj. Rsq", "`r2_1'", "`r2_3'", "`r2_5'", "`r2_7'", "`r2_9'")


** Version 2: using 1 -year
* Create output matrix
mat drop _all
matrix table = J(3,10,.)
matrix stars = J(3,10,0)
* Import Results File (no controls)
import delimited "$inputs/LRdiff_semesterly_main.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_quantity3" & sample == "all"


* Capture values. Columns 1 and 2
local c = 1
foreach control in region_by_module_by_time division_by_module_by_time {
	
	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag2.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "`control'"
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "`control'"
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "`control'" 
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

}

* Import Results File. 
import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_quantity3"  & sample == "all"

* Capture values. Columns 3, 4 and 5
forvalues e=2/4 {

	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag2.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "division_by_module_by_time" & econ == `e'
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

} 


* Export LaTex table
frmttable using  "$outputfolder/Quantity_2", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
ctitle("", "(1)", "(2)", "(3)", "(4)", "(5)") coljust(lccccc) hlines(11000001{0}1) fragment ///
rtitle("On-Impact" \ "" \ "6 months" \ "" \ "1 year") ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "", "", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "X", "X", "X" \ "" \"Lagged Econ (1-2)" , "", "", "X", "", "" \ "" \"Lagged Econ (1-3)" , "", "", "", "X", "" \ "" \"Lagged Econ (1-4)" , "", "", "", "", "X" \ "" \"Adj. Rsq", "`r2_1'", "`r2_3'", "`r2_5'", "`r2_7'", "`r2_9'")



** Version 2: using 18-months
* Create output matrix
mat drop _all
matrix table = J(3,10,.)
matrix stars = J(3,10,0)
* Import Results File (no controls)
import delimited "$inputs/LRdiff_semesterly_main.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_quantity3" & sample == "all"


* Capture values. Columns 1 and 2
local c = 1
foreach control in region_by_module_by_time division_by_module_by_time {
	
	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag3.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "`control'" 
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "`control'"
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "`control'"
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "`control'" 
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

}

* Import Results File. 
import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear

* Keep interest outcome
keep if outcome == "D.ln_quantity3"  & sample == "all"

* Capture values. Columns 3, 4 and 5
forvalues e=2/4 {

	local r = 0
	foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag3.D.ln_sales_tax {
	
		local ++r
		qui sum estimate if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'] = r(mean)
		qui sum clusterse if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		matrix table[`r',`c'+1] = r(mean)
		
		qui sum prt if rn == "`est'" & controls == "division_by_module_by_time" & econ == `e'
		local pval = r(mean)
		matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
		
	}
	qui sum adjrsq if controls == "division_by_module_by_time" & econ == `e'
	local r2_`c' = round(r(mean), 0.001)
	local c = `c' + 2

} 


* Export LaTex table
frmttable using  "$outputfolder/Quantity_3", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
ctitle("", "(1)", "(2)", "(3)", "(4)", "(5)") coljust(lccccc) hlines(11000001{0}1) fragment ///
rtitle("On-Impact" \ "" \ "6 months" \ "" \ "18 months") ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "", "", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "X", "X", "X" \ "" \"Lagged Econ (1-2)" , "", "", "X", "", "" \ "" \"Lagged Econ (1-3)" , "", "", "", "X", "" \ "" \"Lagged Econ (1-4)" , "", "", "", "", "X" \ "" \"Adj. Rsq", "`r2_1'", "`r2_3'", "`r2_5'", "`r2_7'", "`r2_9'")


}



** Table 5: Cross-sectional estimates of the effect of sales taxes. **
{

* Create output matrix
mat drop _all
matrix table = J(1,8,.)
matrix stars = J(1,8,0)
* Import Results File. First Retail and then HH
import delimited "$inputs/LRdiff_cross_sectional_design.csv", clear
keep rn estimate clusterse prt outcome
gen income = "N"
tempfile reta
save `reta', replace
import delimited "$inputs/LRdiff_cross_sectional_design_hh.csv", clear
gen income = "N"
replace income = "Y" if strpos(fe_d, "income")
keep rn estimate clusterse prt outcome income
append using `reta'

* Keep interest outcomes and modify to extract as desired
keep if rn == "avg.ln_sales_tax"
gen var = outcome + "_" + income

* Run loop to extract
local c = 0
foreach x in ln_cpricei2_N ln_sales_N ln_expenditures_N ln_expenditures_Y {

		local ++c
		qui sum estimate if var == "`x'" 
		matrix table[1,`c'] = r(mean)
		qui sum prt if var == "`x'" 
		local pval = r(mean)
		matrix stars[1,`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )

		local ++c
		qui sum clusterse if var == "`x'" 
		matrix table[1,`c'] = r(mean)
		
}

** Export table. 
frmttable using  "$outputfolder/CrossSection", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
ctitle("", "Price", "Sales", "Expenditures", "") rtitle(""\ "") multicol(1,4,2;8,2,2;8,4,2) coljust(lccccc) hlines(1101{0}1) fragment ///
addrows("Module FE", "X", "X", "X", "" \ "" \"Module $ \times$ income FE", "", "", "", "X"\ "" \"Data", "Retailer", "", "Household", "")

*addrows("Module FE", "X", "X", "X", "" \ "" \"Module $ \times$ income FE", "", "", "", "X"\ "" \"Data", "\multicolumn{2}{c}{Retailer}", "\multicolumn{2}{c}{Household}")

}






** Table 7: Reduced-form effects of sales taxes: static model **
{

* Create output matrix
mat drop _all
matrix table = J(3,16,.)
matrix stars = J(3,16,0)
* Import Results File
import delimited "$inputs/LR_TWFE_design.csv", clear
keep if  sample == "non_imp_tax"
* Loop across vars
local col1 = 0
foreach var in w.ln_cpricei2 w.ln_quantity3 w.ln_pricei2 w.ln_sales {

	* Loop across spec
	local col2 = 1
	foreach sp in region_by_module_by_time division_by_module_by_time  {
		
		* Loop Across Var
		local r = 0
		local c = `col1' + `col2'
			foreach est in w.ln_sales_tax F2.w.ln_sales_tax F3.w.ln_sales_tax {
			
			local ++r
			qui sum estimate if rn == "`est'" & outcome == "`var'" &  controls == "`sp'"
			matrix table[`r',`c'] = r(mean)
			qui sum clusterse if rn == "`est'" & outcome == "`var'" & controls == "`sp'"
			matrix table[`r',`c'+1] = r(mean)
			
			qui sum prt if rn == "`est'" & outcome == "`var'"& controls == "`sp'"
			local pval = r(mean)
			matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
			
		}	
		local col2 = `col2' + 2
		
	} 
	local col1 = `col1' + 4
}



* Export LaTex table
frmttable using  "$outputfolder/TWFE_LR_full", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) multicol(1,2,2;1,4,2;1,6,2;1,8,2) fragment ///
ctitle("", "Price", "", "Quantity", "", "Prod. Price", "", "Sales", "" \ "", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)") coljust(lcccccccc) hlines(101000001{0}1)  ///
rtitle("$ \tau_t$" \ "" \ "$ \tau_{t+2}$" \ "" \ "$ \tau_{t+3}$") ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "X", "", "X", "", "X", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "", "X", "", "X", "", "X" )

* Produce a short version
mat table = table[1,1..8]
mat stars = stars[1,1..8]

frmttable using  "$outputfolder/TWFE_LR_short", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) multicol(1,2,2;1,4,2) fragment ///
ctitle("", "Price", "", "Quantity" \ "", "(1)", "(2)", "(3)", "(4)") coljust(lcccc) hlines(101{0}1)  ///
rtitle("$ \tau$" \ "" ) addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "X", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "", "X")

** We now produce a new short version where we also include the sample splitting by median and the IV estimate

* First capture the remaining information
tempfile boot_res
import delimited "$inputs/Demand_iv_sat_initial_price_semester_boot_r.csv", clear

* create group indicator
sort iter outcome controls ngroups lev
by iter outcome controls ngroups: gen group = _n

* reshape so that we can capture the IV
keep estimate outcome controls group ngroups iter
replace outcome = "_p" if outcome == "w.ln_cpricei2"
replace outcome = "_q" if outcome == "w.ln_quantity3"
reshape wide estimate, i(iter controls group ngroups) j(outcome) string

* produce the IV
gen estimate_iv = estimate_q/estimate_p

* Compute the bootsrtrapped SE
preserve
	keep if iter > 0
	collapse (sd) estimate_*, by(controls group ngroups)
	foreach x in p q iv {
		rename estimate_`x' se_`x'
	}
	save `boot_res'
restore
* merge bootstrap results
keep if iter == 0
merge 1:1 controls group ngroups using `boot_res'
drop _merge iter

* reshape back to long again
reshape long estimate_ se_, i(controls ngroups group) j(out) string


** Capture the desired estimates
* IV full sample
foreach sp in group_region_by_module_by_time group_division_by_module_by_time  {
	qui sum estimate_ if out == "iv" & ngroups == 1 & controls == "`sp'"
	local es = r(mean)
	qui sum se_ if out == "iv" & ngroups == 1 & controls == "`sp'"
	local se = r(mean)
	mat table = (table , `es', `se')
	local pval = 1 - normal(abs(`es'/`se'))
	
	matrix stars = (stars, (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 ), 0)
}

* Estimate for splitted sample
keep if ngroups == 2
cap mat drop table2 stars2
matrix table2 = J(2,12,.)
matrix stars2 = J(2,12,0)

* Loop across outcomes
local col1 = 0
foreach var in p q iv {

	* Loop across spec
	local col2 = 1
	foreach sp in group_region_by_module_by_time group_division_by_module_by_time  {
		
		* Loop Across group
		local r = 0
		local c = `col1' + `col2'
		forvalues g =1/2 {
			
			local ++r
			qui sum estimate_ if group == `g' & out == "`var'" &  controls == "`sp'"
			local es = r(mean)
			matrix table2[`r',`c'] = r(mean)
			qui sum se_ if group == `g' & out == "`var'" & controls == "`sp'"
			local se = r(mean)
			matrix table2[`r',`c'+1] = r(mean)
			local pval = 1 - normal(abs(`es'/`se'))
			matrix stars2[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
			
		}	
		local col2 = `col2' + 2
		
	} 
	local col1 = `col1' + 4
}

cap mat drop alltab allstar
mat alltab = (table \ table2)
mat allstar = (stars \ stars2)

** Export table
frmttable using  "$outputfolder/TWFE_LR_short_all", replace tex statmat(alltab) sub(1) sdec(3) ann(allstar) asymbol(*, **, ***) multicol(1,2,2;1,4,2;1,6,2) fragment ///
ctitle("", "Price", "", "Quantity", "", "Demand ($ \hat{\beta}^d$)" \ "", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)") coljust(lcccccc) hlines(101010101)  ///
rtitle("Full Sample" \ "" \ "Below median $ p_{it-1}^c$" \ "" \ "Above median $ p_{it-1}^c$" \ "" ) ///
addrows("Reg $\times$ Mod $\times$ Time" , "X", "", "X", "", "X", "" \ "" \"Div $\times$ Mod $\times$ Time" , "", "X", "", "X", "", "X")

}


** Table 9: Average across states of the MVPF of three reforms to the sales tax rate. ***

{
* Import Results File
import delimited "$inputs/average_nationwide_extrapolation.csv", clear
keep if sigma == 1 & theta == 0
replace est = "LB" if est == "" // to capture linear cases properly

** produce panels
foreach scv in "Original" "No Tax" "plus 5 Tax" {
	* Create output matrix
	mat drop _all
	matrix table = J(3,5,.)
	local rn = 0 
	foreach kv in 1 2 8 {
		local ++rn
		local cn = 1
		forvalues lv =1/2 {
			if (`kv'>= `lv') {
				qui sum value if est == "LB" & sc == "`scv'" & k == `kv' & l == `lv'
				mat table[`rn',`cn'] = r(mean)
				local ++cn
				if (`kv' > `lv') {
					qui sum value if est == "UB" & sc == "`scv'"  & k == `kv' & l == `lv'
					mat table[`rn',`cn'] = r(mean)
				}
			}
			local cn = `cn' + 2
		}
		
	}
	local tabti
	local let
	if ("`scv'" == "Original")  {
		local tabti "\it{Panel A: Marginal change in tax}"
		local let "A"
	}		
	if ("`scv'" == "No Tax") {
		local tabti "\it{Panel B: Non-marginal change - from no tax to current tax}"
		local let "B"
	}		
	if ("`scv'" == "plus 5 Tax") {
		local tabti "\it{Panel C: Non-marginal change - 5pp increase in tax}"
		local let "C"
	}	

	
	frmttable using  "$outputfolder/Table_welfare_av_`let'", replace tex statmat(table) sdec(3) ///
	multicol(1,1,6;2,2,2;2,5,2;4,2,2;4,5,2;5,5,2) fragment coljust(lccccc) hlines(1101{0}1)  ///
	ctitle("`tabti'", "", "", "", "" \ "" "$ L^d =1$", "", "", "$ L^d =2$", "" \ "", "LB", "UB", "", "LB", "UB") ///
	rtitle("$ K^d=1$" \ "$ K^d=2$" \ "$ K^d=8$") 
}
}


** Table 9A: Robustness - Average across states of the MVPF of three reforms to the sales tax rate under imperfect competition ***
{
* Import Results File
import delimited "$inputs/average_nationwide_extrapolation.csv", clear
keep if sigma == 1 & theta != 0 
replace est = "LB" if est == "" // to capture linear cases properly
* Create output matrix (can produce this one directly)
mat drop _all
matrix table = J(24,5,.)
* Round theta to capture correctly
gen thetatxt = "0" + string(round(theta, 0.00001))
levelsof thetatxt, local(levt)

local tabti
local scn = 0
foreach scv in "Original" "No Tax" "plus 5 Tax" {
	
	** Capture row titles
	if ("`scv'" == "Original")  	local tabti 	`" `tabti' "\it{Panel A: Marginal change in tax}" \ "" "'
	if ("`scv'" == "No Tax")		local tabti    	`" `tabti' \ "\it{Panel B: Non-marginal change - from no tax to current tax}" \ "" "'
	if ("`scv'" == "plus 5 Tax") 	local tabti  	`" `tabti' \ "\it{Panel C: Non-marginal change - 5pp increase in tax}" \ "" "'

	
	
	* Create output matrix
	local rn = 8*`scn' + 2
	local ++scn
	forvalues lv = 1/2 {
		foreach kv in 1 2 8 {
			local ++rn
			if (`kv'>= `lv') {
				local tabti  	`" `tabti' \ "$ L^d = `lv'$, $ K = `kv'$" "'
				local cn = 1
				foreach th of local levt {
					qui sum value if est == "LB" & sc == "`scv'" & k == `kv' & l == `lv' & thetatxt == "`th'"
					mat table[`rn',`cn'] = r(mean)
					local ++cn
					if (`kv' > `lv') {
						qui sum value if est == "UB" & sc == "`scv'"  & k == `kv' & l == `lv' & thetatxt == "`th'"
						mat table[`rn',`cn'] = r(mean)
					}
					local cn = `cn' + 2
				}
			}
			else local tabti  	`" `tabti' \ "" "'
		}
		
	}
	
	
}

frmttable using  "$outputfolder/Table_welfare_av_rob_elas", replace tex statmat(table) sdec(3) ///
multicol(1,2,2;1,5,2;3,1,6;5,2,2;5,5,2;9,2,2;9,5,2;11,1,6;13,2,2;13,5,2;17,2,2;17,5,2;19,1,6;21,2,2;21,5,2;25,2,2;25,5,2) fragment coljust(lccccc) hlines(101100000110000011000001)  ///
ctitle("", "$ \varepsilon^S \to \infty$ \& $ \theta = `: word 1 of `levt''$", "", "", "$ \varepsilon^S =1$ \& $ \theta = `: word 2 of `levt''$", "" \ "", "LB", "UB", "", "LB", "UB") ///
rtitle(`tabti') 
}





** Table 9B: Robustness - Average across states of the MVPF of three reforms to the sales tax rate under imperfect competition ***
{
* Import Results File
import delimited "$inputs/average_nationwide_extrapolation.csv", clear
keep if theta == 0 & sigma !=.75
replace est = "LB" if est == "" // to capture linear cases properly
* Create output matrix (can produce this one directly)
mat drop _all
matrix table = J(24,8,.)
* Round sigma to capture correctly
gen sigmatxt =  string(sigma)
replace sigmatxt = "0" + sigmatxt if length(sigmatxt) > 1
levelsof sigmatxt, local(levt)
* flip order of local to produce as we want
local levs
foreach l of local levt {
	local levs `" `l' `levs' "'
}
di 	`"`levs'"'

local tabti
local scn = 0
foreach scv in "Original" "No Tax" "plus 5 Tax" {
	
	** Capture row titles
	if ("`scv'" == "Original")  	local tabti 	`" `tabti' "\it{Panel A: Marginal change in tax}" \ "" "'
	if ("`scv'" == "No Tax")		local tabti    	`" `tabti' \ "\it{Panel B: Non-marginal change - from no tax to current tax}" \ "" "'
	if ("`scv'" == "plus 5 Tax") 	local tabti  	`" `tabti' \ "\it{Panel C: Non-marginal change - 5pp increase in tax}" \ "" "'

	
	
	* Create output matrix
	local rn = 8*`scn' + 2
	local ++scn
	forvalues lv = 1/2 {
		foreach kv in 1 2 8 {
			local ++rn
			if (`kv'>= `lv') {
				local tabti  	`" `tabti' \ "$ L^d = `lv'$, $ K = `kv'$" "'
				local cn = 1
				foreach s of local levs {
					qui sum value if est == "LB" & sc == "`scv'" & k == `kv' & l == `lv' & sigmatxt == "`s'"
					mat table[`rn',`cn'] = r(mean)
					local ++cn
					if (`kv' > `lv') {
						qui sum value if est == "UB" & sc == "`scv'"  & k == `kv' & l == `lv' & sigmatxt == "`s'"
						mat table[`rn',`cn'] = r(mean)
					}
					local cn = `cn' + 2
				}
			}
			else local tabti  	`" `tabti' \ "" "'
		}
		
	}
	
	
}

frmttable using  "$outputfolder/Table_welfare_av_rob_sal", replace tex statmat(table) sdec(3) ///
multicol(1,2,2;1,5,2;1,8,2;3,1,9;5,2,2;5,5,2;5,8,2;9,2,2;9,5,2;9,8,2;11,1,9;13,2,2;13,5,2;13,8,2;17,2,2;17,5,2;17,8,2;19,1,9;21,2,2;21,5,2;21,8,2;25,2,2;25,5,2;25,8,2) ///
fragment coljust(lcccccccc) hlines(101100000110000011000001)  ///
ctitle("", "$ \sigma = `: word 1 of `levs''$", "", "", "$ \sigma = `: word 2 of `levs''$", "", "", "$ \sigma = `: word 3 of `levs''$", "" \ "", "LB", "UB", "", "LB", "UB", "", "LB", "UB") ///
rtitle(`tabti') 
}













** Table 16: Robustness of the Passthrough estimates to a diferent definition of the price index. **
* Normalized at -1
{
* Create output matrix
mat drop _all
matrix table = J(2,16,.)
matrix stars = J(2,16,0)
* Import Results File
tempfile dat
import delimited "$inputs/LRdiff_semesterly_main.csv", clear
drop econ
gen econ = 0
save `dat', replace

import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear
append using `dat'

* Keep interest outcome
keep if (outcome == "D.ln_cpricei2" | outcome ==  "D.ln_cpricei" )  & sample == "all"

* Loop across time
local col1 = 0
foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag3.D.ln_sales_tax cumul.lag4.D.ln_sales_tax {

	* Loop across spec
	local col2 = 1
	foreach sp in 0 3 {
		
		* Loop Across Var
		local r = 0
		local c = `col1' + `col2'
		foreach var in D.ln_cpricei2 D.ln_cpricei {
			
			local ++r
			qui sum estimate if rn == "`est'" & outcome == "`var'" & econ == `sp' & controls == "division_by_module_by_time"
			matrix table[`r',`c'] = r(mean)
			qui sum clusterse if rn == "`est'" & outcome == "`var'" & econ == `sp' & controls == "division_by_module_by_time"
			matrix table[`r',`c'+1] = r(mean)
			
			qui sum prt if rn == "`est'" & outcome == "`var'"& econ == `sp' & controls == "division_by_module_by_time"
			local pval = r(mean)
			matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
			
		}	
		local col2 = `col2' + 2
		
	} 
	local col1 = `col1' + 4
}



* Export LaTex table
frmttable using  "$outputfolder/Passthrough_Robust_1", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
coljust(lccccc) hlines(1010001{0}1) multicol(1,2,2;1,4,2;1,6,2;1,8,2) ///
ctitle("", "On-Impact", "", "6 months", "", "18 months", "", "2 years", "" \"", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)") ///
rtitle("Main" \ "" \ "Laspeyres" \ "" ) ///
addrows("Div $\times$ Mod $\times$ Time" , "X", "X", "X", "X", "X", "X", "X", "X" \ "" \"Lagged Econ (1-3)" , "", "X", "", "X", "", "X", "", "X") fragment


}



** Table 17: Robustness of the Passthrough estimates to a different definition of the quantity index. **
* Normalized at -1
{
* Create output matrix
mat drop _all
matrix table = J(3,16,.)
matrix stars = J(3,16,0)
* Import Results File
tempfile dat
import delimited "$inputs/LRdiff_semesterly_main.csv", clear
drop econ
gen econ = 0
save `dat', replace

import delimited "$inputs/LRdiff_semesterly_w_econ.csv", clear
append using `dat'

* Keep interest outcome
keep if (outcome == "D.ln_quantity3" | outcome ==  "D.ln_quantity2" | outcome ==  "D.ln_quantity")  & sample == "all"

* Loop across time
local col1 = 0
foreach est in cumul.lag0.D.ln_sales_tax cumul.lag1.D.ln_sales_tax cumul.lag3.D.ln_sales_tax cumul.lag4.D.ln_sales_tax {

	* Loop across spec
	local col2 = 1
	foreach sp in 0 3 {
		
		* Loop Across Var
		local r = 0
		local c = `col1' + `col2'
		foreach var in D.ln_quantity3 D.ln_quantity2 D.ln_quantity {
			
			local ++r
			qui sum estimate if rn == "`est'" & outcome == "`var'" & econ == `sp' & controls == "division_by_module_by_time"
			matrix table[`r',`c'] = r(mean)
			qui sum clusterse if rn == "`est'" & outcome == "`var'" & econ == `sp' & controls == "division_by_module_by_time"
			matrix table[`r',`c'+1] = r(mean)
			
			qui sum prt if rn == "`est'" & outcome == "`var'"& econ == `sp' & controls == "division_by_module_by_time"
			local pval = r(mean)
			matrix stars[`r',`c'] = (`pval'  <= 0.1 ) + (`pval'  <= 0.05 ) + (`pval'  <= 0.01 )
			
		}	
		local col2 = `col2' + 2
		
	} 
	local col1 = `col1' + 4
}



* Export LaTex table
frmttable using  "$outputfolder/Quantity_Robust_1", replace tex statmat(table) sub(1) sdec(3) ann(stars) asymbol(*, **, ***) ///
coljust(lccccc) hlines(101000001{0}1) multicol(1,2,2;1,4,2;1,6,2;1,8,2) fragment ///
ctitle("", "On-Impact", "", "6 months", "", "18 months", "", "2 years", "" \"", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)") ///
rtitle("Main" \ "" \ "Laspeyres " \ "" \ "log(sales)-log(price)")  ///
addrows("Div $\times$ Mod $\times$ Time" , "X", "X", "X", "X", "X", "X", "X", "X" \ "" \"Lagged Econ (1-3)" , "", "X", "", "X", "", "X", "", "X")


}

