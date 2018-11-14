#' Test sales.taxes functions.
#'
library(data.table)
scanner_data <- data.table(expand.grid(state_id = 1:2, month = 1:2,
                                       store_code_uc = 1:2, product_module_code = 1:2
                                       ))
# scanner_data[, quantity := round(runif(nrow(scanner_data), 1, 20), 1)]
# scanner_data[, sales := round(runif(nrow(scanner_data), 10, 40), 2)]
quantity <- c(17.6, 2.1, 11.1, 7.6, 13.2, 1.8, 16.2, 19.4, 14.2, 14.1, 5.2, 1.7,
              16.1, 3.7, 19.2, 18.7)
sales = c(17.09, 20.12, 31.13, 35.99, 14.38, 33.21, 38.28, 21.97, 36.69, 38,
          11.07, 10.33, 36.8, 11.76, 36, 23.73)
scanner_data[, sales := sales]
scanner_data[, quantity := quantity]
scanner_data[, year := 2010]

# test combine_scanner_data
