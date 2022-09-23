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


frmttable using "$outputfolder/Tax_levels", replace tex statmat(table) sdec(3) coljust(lcccccccc) hlines(110000101)  ///
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
frmttable using "$outputfolder/Tax_changes", replace tex statmat(table) sdec(0,2,3,3,3,3,3,3) coljust(lcccccccc) hlines(1010011)  ///
ctitle("", "", "", "Increases", "", "", "Decreases", "", "" \ "Level", "N", "\% increases", "Mean", "Max", "IQR", "Mean", "Max", "IQR") multicol(1,4,3;1,7,3) ///
varlabels fragment

}




** Table 3: Reduced-form evidence of the effect of sales taxes on consumer price. **
{

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


}


** Table 4: Reduced-form evidence of the effect of sales taxes on quantity. **
{

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
ctitle("", "Price", "Sales", "Expenditures", "") rtitle(""\ "") multicol(1,4,2) coljust(lccccc) hlines(1101{0}1) fragment ///
addrows("Module FE", "X", "X", "X", "" \ "" \"Module $ \times$ income FE", "", "", "", "X"\ "" \"Data", "\multicolumn{2}{c}{Retailer}", "\multicolumn{2}{c}{Household}")


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


}


** Tables 9





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

