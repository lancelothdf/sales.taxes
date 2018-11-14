#' Identify tax-exempted (or tax-reduced) goods
#'
#' @description \code{identify_exemptions} identifies tax-exempt goods and marks
#'     their tax rates as zero.
#'
#'

identify_exemptions <- function(sales_data){
  # Step 1: import the module exemptions file (wide or long)
  module_exemptions <- read.dta13("/project2/igaarder/Data/Nielsen/Module_exemptions_by_state.dta")
  # From there, if wide, convert to long
  # Match based on state and product code
  sales_data[, tax_status := ]
  gen tax_status = taxable /* 0 is always exempt; 1 is always taxable; 2 different
  but always constant rate; 3 status change; 4 different and changing */
    /** Will be used in event studies **/
    replace sales_tax = 0.02 if taxable == 2 &amp; fips_state == 5 &amp; year &lt;= 2010
  replace taxable = 1 if taxable == 2 &amp; fips_state == 5 &amp; year &lt;= 2010
  replace sales_tax = 0.015 if taxable == 2 &amp; fips_state == 5 &amp; year &gt;= 2011
  replace taxable = 1 if taxable == 2 &amp; fips_state == 5 &amp; year &gt;= 2011
  replace tax_status = 4 if taxable == 2 &amp; fips_state == 5
  replace taxable = 0 if taxable == 2 &amp; fips_state == 8 &amp; (year &lt;= 2009 | (year ==
                                                                                        2010 &amp; quarter == 1))
  replace taxable = 1 if taxable == 2 &amp; fips_state == 8 &amp; (year &gt;= 2011 | (year ==
                                                                                        2010 &amp; quarter &gt;= 2))
  replace tax_status = 3 if taxable == 2 &amp; fips_state == 8
  replace sales_tax = 0.01 if taxable == 2 &amp; fips_state == 17
  replace taxable = 1 if taxable == 2 &amp; fips_state == 17
  replace tax_status = 2 if taxable == 2 &amp; fips_state == 17
  replace sales_tax = 0.01 if taxable == 3 &amp; fips_state == 17 &amp; (year == 2008 | (year
                                                                                         == 2009 &amp; quarter &lt; 4))
  replace taxable = 1 if taxable == 3 &amp; fips_state == 17 &amp; (year == 2008 | (year ==
                                                                                      2009 &amp; quarter &lt; 4))
  replace taxable = 1 if taxable == 3 &amp; fips_state == 17 &amp; (year &gt;= 2010 | (year ==
                                                                                         2009 &amp; quarter == 4))
  replace tax_status = 4 if taxable == 3 &amp; fips_state == 17
  replace taxable = 0 if taxable == 2 &amp; fips_state == 23 &amp; (year &lt;= 2012 | (year ==
                                                                                         2013 &amp; quarter &lt;= 3))
  replace taxable = 1 if taxable == 2 &amp; fips_state == 23 &amp; (year &gt;= 2014 | (year ==
                                                                                         2013 &amp; quarter == 4))
  replace tax_status = 3 if taxable == 2 &amp; fips_state == 23
  replace sales_tax = 0.01225 if taxable == 2 &amp; fips_state == 29
  replace taxable = 1 if taxable == 2 &amp; fips_state == 29
  replace tax_status = 4 if taxable == 2 &amp; fips_state == 29
  replace sales_tax = 0.02 if taxable == 2 &amp; fips_state == 37
  replace taxable = 1 if taxable == 2 &amp; fips_state == 37
  replace tax_status = 4 if taxable == 2 &amp; fips_state == 37
  replace taxable = 0 if taxable == 2 &amp; fips_state == 44 &amp; (year &lt;= 2010 | (year ==
                                                                                         2011 &amp; quarter &lt;= 3))
  replace taxable = 1 if taxable == 2 &amp; fips_state == 44 &amp; (year &gt;= 2012 | (year ==
                                                                                         2011 &amp; quarter == 4))
  replace tax_status = 3 if taxable == 2 &amp; fips_state == 44

  replace sales_tax = 0.0525 if taxable == 2 &amp; fips_state == 47 &amp; (year &lt;= 2012 |
                                                                             (year == 2013 &amp; quarter &lt;= 2))
  replace taxable = 1 if taxable == 2 &amp; fips_state == 47 &amp; (year &lt;= 2012 | (year ==
                                                                                         2013 &amp; quarter &lt;= 2))
  replace sales_tax = 0.05 if taxable == 2 &amp; fips_state == 47 &amp; (year &gt;= 2014 | (year
                                                                                            == 2013 &amp; quarter &gt;= 3))
  replace taxable = 1 if taxable == 2 &amp; fips_state == 47 &amp; (year &gt;= 2014 | (year ==
                                                                                         2013 &amp; quarter &gt;= 3))
  replace tax_status = 4 if taxable == 2 &amp; fips_state == 47
  replace sales_tax = 0.03 if taxable == 2 &amp; fips_state == 49
  replace taxable = 1 if taxable == 2 &amp; fips_state == 49
  replace tax_status = 4 if taxable == 2 &amp; fips_state == 49
  replace sales_tax = 0.015 if taxable == 2 &amp; fips_state == 51
  replace taxable = 1 if taxable == 2 &amp; fips_state == 51
  replace tax_status = 4 if taxable == 2 &amp; fips_state == 51
  replace taxable = 0 if taxable == 2 &amp; fips_state == 53 &amp; (year &lt;= 2009 | (year ==
                                                                                         2010 &amp; quarter &lt;= 2) | year &gt;= 2011)
  replace taxable = 1 if taxable == 2 &amp; fips_state == 53 &amp; (year == 2010 &amp; quarter
                                                                    &gt;= 3)
  replace tax_status = 3 if taxable == 2 &amp; fips_state == 53
  replace sales_tax = 0.04 if taxable == 2 &amp; fips_state == 54 &amp; (year == 2008 &amp;
                                                                         quarter &lt;= 2)
  replace taxable = 1 if taxable == 2 &amp; fips_state == 54 &amp; (year == 2008 &amp; quarter
                                                                    &lt;= 2)
  replace sales_tax = 0.03 if taxable == 2 &amp; fips_state == 54 &amp; ((year == 2008 &amp;
                                                                          quarter &gt;= 3) | (year &gt;= 2009 &amp; year &lt;= 2011))
  replace taxable = 1 if taxable == 2 &amp; fips_state == 54 &amp; ((year == 2008 &amp; quarter
                                                                     &gt;= 3) | (year &gt;= 2009 &amp; year &lt;= 2011))
  replace sales_tax = 0.02 if taxable == 2 &amp; fips_state == 54 &amp; (year == 2012 &amp;
                                                                         quarter &lt;= 2)
  replace taxable = 1 if taxable == 2 &amp; fips_state == 54 &amp; (year == 2012 &amp; quarter
                                                                    &lt;= 2)
  replace sales_tax = 0.01 if taxable == 2 &amp; fips_state == 54 &amp; ((year == 2012 &amp;
                                                                          quarter &gt;= 3) | (year == 2013 &amp; quarter &lt;= 2))
  replace taxable = 1 if taxable == 2 &amp; fips_state == 54 &amp; ((year == 2012 &amp; quarter
                                                                     &gt;= 3) | (year == 2013 &amp; quarter &lt;= 2))
  replace taxable = 0 if taxable == 2 &amp; fips_state == 54 &amp; ((year == 2013 &amp; quarter
                                                                     &gt;= 3) | year &gt;= 2014)
  replace tax_status = 4 if taxable == 2 &amp; fips_state == 54
}
