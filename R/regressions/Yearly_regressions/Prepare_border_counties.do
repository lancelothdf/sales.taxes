/* Clean county border data */
clear all
set more off



clear
import delimited /project2/igaarder/Data/county_adjacency.txt

drop v1 v3
rename v2 fips_county1
rename v4 fips_county2

replace fips_county1 = fips_county1[_n-1] if fips_county1[_n] == . & fips_county1[_n-1] != .

drop if fips_county1 == fips_county2 /**/


/* Generate a border ID */
sort fips_county1 fips_county2 
gen bordindx_temp = _n

sort fips_county2 fips_county1
gen bordindx_temp2 = _n

egen bordindx = rowmin(bordindx_temp bordindx_temp2)
drop bordindx_temp bordindx_temp2

rename fips_county1 fips_county
sort fips_county


duplicates drop bordindx, force
rename fips_county fips_county1
reshape long fips_county, i(bordindx) j(version)

sort fips_county bordindx
export delimited using "/project2/igaarder/Data/border_counties.csv", replace
