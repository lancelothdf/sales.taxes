### R file to compare pure "diff-in-diff" to pure "event-study" designs --- Outcome: Sales

### Start from John's file for price indices and just apply to Sales (Author: Lancelot Henry de Frahan -- But really just a modification of John Bonney's file)
##

library(data.table)
library(lfe)

fdt <- data.table(expand.grid(store_code_uc = 1:100,
                              product_module_code = 1:100,
                              year = 2008:2014,
                              quarter = 1:4))
fdt[, sales := rnorm(nrow(fdt), 10000, 1000)]
fdt[, cpricei := rnorm(nrow(fdt), 100, 10)]
fdt[, tax.change := sample(0:1, nrow(fdt), replace = T, prob = c(.98, .02))]

fdt <- fdt[product_module_code == 8]
fdt[, year.qtr := year + (quarter - 1) / 4]

setkey(fdt, store_code_uc, year.qtr)

fdt[, ever.change := max(tax.change), by = .(store_code_uc)]
fdt[, changed := cummax(tax.change), by = .(store_code_uc)]
fdt[ever.change == 1, change_onset_time := min(year.qtr[changed == 1]), by = .(store_code_uc)]
fdt[ever.change == 0, change_onset_time := Inf]
fdt[, event_time := (year.qtr - change_onset_time) * 4]



## need to have interaction terms across all cohorts (except untreated),
## across all relative times (except t=-2).

new.var.names <- NULL
for (cohort in seq(2009, 2013.75, .25)) {
  for (tt_event in setdiff(((seq(2008, 2014.75, .25) - cohort) * 4), -2)) {
    cohort.name <- paste0("cohort", cohort,
                          ifelse(tt_event < 0, "m", "p"), abs(tt_event))
    fdt[, (cohort.name) := as.integer(change_onset_time == cohort) *
          as.integer(event_time == tt_event)]
    new.var.names <- c(new.var.names, cohort.name)
  }
}

felm_formula_input <- paste(new.var.names, collapse = "+")
felm_formula <- paste0("cpricei ~ ", felm_formula_input, " | store_code_uc + year.qtr | 0 | cluster_var")
# TODO: what do we want to cluster on?

est <- felm(felm_formula, data = fdt, weights = fdt[[sales]],
            nostats = FALSE, keepX = FALSE, keepCX = FALSE, psdef = FALSE, kclass = FALSE)


###############################################
######### below is copied from ES packages

ES_data[, ref_onset_ref_event_time := .GRP, by = list(ref_onset_time, ref_event_time)]
ES_data[, unit_sample := .GRP, by = list(get(onset_time_var), catt_specific_sample, ref_onset_time)]
ES_data[, cluster_on_this := .GRP, by = cluster_vars]

discrete_covar_formula_input = NA
cont_covar_formula_input = NA

start_cols <- copy(colnames(ES_data))


for (h in min(ES_data$ref_onset_time):max(ES_data$ref_onset_time)) {
  for (r in setdiff(min(ES_data[ref_onset_time == h]$ref_event_time):max(ES_data[ref_onset_time == h]$ref_event_time), omitted_event_time)) {
    var <- sprintf("ref_onset_time%s_catt%s", h, r)
    ES_data[, (var) := as.integer(ref_onset_time == h & ref_event_time == r & get(onset_time_var) == h)]
  }
}

new_cols <- setdiff(colnames(ES_data), start_cols)
new_cols_used <- gsub("\\-", "lead", new_cols) # "-" wreaks havoc otherwise, syntactically
setnames(ES_data, new_cols, new_cols_used)

# Should now be able to combine all of the above in a natural way
felm_formula_input <- paste(new_cols_used, collapse = "+")


  felm_formula_input = paste(na.omit(c(felm_formula_input, cont_covar_formula_input)), collapse = " + ")
  fe_formula = paste(na.omit(c("unit_sample + ref_onset_ref_event_time", discrete_covar_formula_input)), collapse = " + ")

est <- felm(as.formula(paste0(eval(outcomevar), " ~ ", felm_formula_input, " | ", fe_formula," | 0 | cluster_on_this")),
            data = ES_data, weights = ES_data[[reg_weights]],
            nostats = FALSE, keepX = FALSE, keepCX = FALSE, psdef = FALSE, kclass = FALSE
)



ES_data <- NULL
gc()

results <- as.data.table(summary(est, robust = TRUE)$coefficients, keep.rownames = TRUE)
est <- NULL
gc()
results[!grepl("ref\\_onset\\_time", rn), e := min_onset_time]
results[, rn := gsub("lead", "-", rn)]
for (c in min_onset_time:(max_onset_time - 1)) {
  results[grepl(sprintf("ref\\_onset\\_time%s", c), rn), e := c]
  results[grepl(sprintf("ref\\_onset\\_time%s", c), rn), rn := gsub(sprintf("ref\\_onset\\_time%s\\_et", c), "et", rn)]
  results[grepl(sprintf("ref\\_onset\\_time%s", c), rn), rn := gsub(sprintf("ref\\_onset\\_time%s\\_catt", c), "catt", rn)]
}
results[grepl("et", rn), event_time := as.integer(gsub("et", "", rn))]
results[grepl("catt", rn), event_time := as.integer(gsub("catt", "", rn))]
results[grepl("et", rn), rn := "event_time"]
results[grepl("catt", rn), rn := "catt"]
results <- results[rn == "catt"]


setnames(results, c("e"), onset_time_var)
setnames(results, c("Estimate", "Cluster s.e."), c("estimate", "cluster_se"))
setnames(results,"Pr(>|t|)","pval")
results[, pval := round(pval,8)]

