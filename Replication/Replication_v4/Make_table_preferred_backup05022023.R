
rm(list = ls())
setwd("/Users/lancelot/Documents/Sales Taxes/")

library(data.table)
library(lfe)
library(readxl)
library(Hmisc)
library(statar)
library(sandwich)
library(lmtest)
library(ivreg)
library(dplyr)
library(broom)
library(stringr)
library(extrafont)
library(RColorBrewer)
library(latex2exp)
library(ggplot2)


# Font: Need to install TTFs of latin modern roman
loadfonts()
fontsize <- 16


#### Make Main Table without IV
data.main <- fread("Data/LR_Diff_design.csv")
data.main <- data.main[outcome %in% c("DL.ln_cpricei2", "DL.ln_quantity3") & sample == "non_imp_tax_strong"]

data.controls <- fread("Data/LRDiff_semesterly_w_econ.csv")
data.controls <- data.controls[outcome %in% c("DL.ln_cpricei2", "DL.ln_quantity3") & rn == "DL.ln_sales_tax" & sample == "non_imp_tax_strong"]


#c1 <- c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.", "Pr(>|t|)")], digits = 2)), "x", "", "", "", data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.", "Pr(>|t|)")]$`adj.Rsq`)
c5 <- as.data.frame(c("coef1", "se1", "coef2", "se2", "region", "division", "econ"))
c1 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", ""))
c2 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", ""))
c3 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", "$\\checkmark$"))
c4 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", "$\\checkmark$"))
table1 <- as.data.frame(c(c5, c1, c2, c3, c4))
names(table1) <- c("rows", "c1", "c2", "c3", "c4")

setDT(table1)
for(c in c("c1", "c2", "c3", "c4")) {
  
  #table1[, (c) := ifelse(rows %in% c("coef1", "coef2"), paste0("\\textbf{", table1[[c]], "}"), table1[[c]])]
  table1[, (c) := ifelse(rows %in% c("se1", "se2"), paste0("(", table1[[c]], ")"), table1[[c]])]
  
}

table1[, c4 := ifelse(rows == "se1", paste0(c4, " \\\\"), c4)]
table1[, c4 := ifelse(rows == "se2", paste0(c4, " \\\\ \\hline"), c4)]

table1[, rows := ifelse(rows == "coef1", "Consumer Price", rows)]
table1[, rows := ifelse(rows == "coef2", "Quantity", rows)]
table1[, rows := ifelse(rows %in% c("se1", "se2"), "", rows)]
table1[, rows := ifelse(rows == "region", "Reg $\\times$ Mod $\\times$ Time FE", rows)]
table1[, rows := ifelse(rows == "division", "Div $\\times$ Mod $\\times$ Time FE", rows)]
table1[, rows := ifelse(rows == "econ", "Econ controls", rows)]


# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{ccccc}",
            "\\hline",
            " & (1) & (2) & (3) & (4) \\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline \\\\",
            "\\end{tabular}%}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
body <- apply(table1, 1, function(x) paste(x, collapse = " & "))
body <- paste(body, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, body, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_main_LR.tex")



######################################
######################################

#### Make main table with IVs
## Panel 1: first-stage and reduced-forms
data.main <- fread("Data/LR_Diff_design.csv")
data.main <- data.main[outcome %in% c("DL.ln_cpricei2", "DL.ln_pricei2", "DL.ln_quantity3") & sample == "non_imp_tax_strong"]

data.controls <- fread("Data/LRDiff_semesterly_w_econ.csv")
data.controls <- data.controls[outcome %in% c("DL.ln_cpricei2", "DL.ln_quantity3", "DL.ln_pricei2") & rn == "DL.ln_sales_tax" & sample == "non_imp_tax_strong"]

# Need to add rows with producer price for data.controls
data.controls2 <- data.controls[outcome %in% c("DL.ln_cpricei2") & rn == "DL.ln_sales_tax" & sample == "non_imp_tax_strong"]
data.controls2 <- data.controls2[ , Estimate := Estimate - 1]
data.controls2 <- data.controls2[ , outcome := "DL.ln_pricei2"]
data.controls <- rbind(data.controls, data.controls2)

#c1 <- c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.", "Pr(>|t|)")], digits = 2)), "x", "", "", "", data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.", "Pr(>|t|)")]$`adj.Rsq`)
c5 <- as.data.frame(c("coef1", "se1", "coef2", "se2", "coef3", "se3"))
c1 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_pricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c2 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_pricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c3 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_pricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c4 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_pricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
table1 <- as.data.frame(c(c5, c1, c2, c3, c4))
names(table1) <- c("rows", "c1", "c2", "c3", "c4")

##
setDT(table1)
for(c in c("c1", "c2", "c3", "c4")) {
  
  #table1[, (c) := ifelse(rows %in% c("coef1", "coef2"), paste0("\\textbf{", table1[[c]], "}"), table1[[c]])]
  table1[, (c) := ifelse(rows %in% c("se1", "se2"), paste0("(", table1[[c]], ")"), table1[[c]])]
  
}

table1[, c4 := ifelse(rows == "se1", paste0(c4, " \\\\"), c4)]
table1[, c4 := ifelse(rows == "se2", paste0(c4, " \\\\"), c4)]
table1[, c4 := ifelse(rows == "se3", paste0(c4, " \\\\ \\hline"), c4)]

table1[, rows := ifelse(rows == "coef1", "Consumer Price ($p^{c}$)", rows)]
table1[, rows := ifelse(rows == "coef2", "Producer Price ($p$)", rows)]
table1[, rows := ifelse(rows == "coef3", "Quantity ($q$)", rows)]
table1[, rows := ifelse(rows %in% c("se1", "se2", "se3"), "", rows)]


## Panel 2: IVs
data.IV <- fread("Data/IV_Dem_Sup.csv")
data.IV <- data.IV[outcome %in% c("Demand", "Inverse Supply") & n.groups == 1]

data.IV.controls <- fread("Data/IV_Dem_Sup_controls.csv")
data.IV.controls <- data.IV.controls[outcome %in% c("Demand", "Inverse Supply") & rn %in% c("`DL.ln_cpricei2(fit)`", "`DL.ln_quantity3(fit)`") & n.groups == 1]

c5 <- as.data.frame(c("coef1", "se1", "coef2", "se2", "region", "division", "econ"))
c1 <- as.data.frame(c(t(round(data.IV[outcome == "Demand" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV[outcome == "Inverse Supply" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", ""))
c2 <- as.data.frame(c(t(round(data.IV[outcome == "Demand" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV[outcome == "Inverse Supply" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", ""))
c3 <- as.data.frame(c(t(round(data.IV.controls[outcome == "Demand" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV.controls[outcome == "Inverse Supply" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", "$\\checkmark$"))
c4 <- as.data.frame(c(t(round(data.IV.controls[outcome == "Demand" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV.controls[outcome == "Inverse Supply" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)),  "", "$\\checkmark$", "$\\checkmark$"))
table2 <- as.data.frame(c(c5, c1, c2, c3, c4))
names(table2) <- c("rows", "c1", "c2", "c3", "c4")

setDT(table2)
for(c in c("c1", "c2", "c3", "c4")) {
  
  #table2[, (c) := ifelse(rows %in% c("coef1", "coef2"), paste0("\\textbf{", table2[[c]], "}"), table2[[c]])]
  table2[, (c) := ifelse(rows %in% c("se1", "se2"), paste0("(", table2[[c]], ")"), table2[[c]])]
  
}

table2[, c4 := ifelse(rows == "se1", paste0(c4, " \\\\"), c4)]
table2[, c4 := ifelse(rows == "se2", paste0(c4, " \\\\ \\hline"), c4)]

table2[, rows := ifelse(rows == "coef1", "Demand ($\\frac{\\partial q^{d}}{\\partial p^{c}}$)", rows)]
table2[, rows := ifelse(rows == "coef2", "Inverse Supply ($1/\\frac{\\partial q^{s}}{\\partial p}$)", rows)]
table2[, rows := ifelse(rows %in% c("se1", "se2"), "", rows)]
table2[, rows := ifelse(rows == "region", "Reg $\\times$ Mod $\\times$ Time FE", rows)]
table2[, rows := ifelse(rows == "division", "Div $\\times$ Mod $\\times$ Time FE", rows)]
table2[, rows := ifelse(rows == "econ", "Econ controls", rows)]


## Make the table
# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{lcccc}",
            "\\hline",
            " & (1) & (2) & (3) & (4) \\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline \\\\",
            "\\end{tabular}%}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
body1 <- apply(table1, 1, function(x) paste(x, collapse = " & "))
body1 <- paste(body1, "\\\\")
body2 <- apply(table2, 1, function(x) paste(x, collapse = " & "))
body2 <- paste(body2, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, "\\textbf{OLS} & \\multicolumn{4}{c}{} \\\\", "\\multicolumn{5}{c}{} \\\\", body1, "\\textbf{IV} & \\multicolumn{4}{c}{} \\\\", "\\multicolumn{5}{c}{} \\\\", body2, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_main_LR_IV.tex")




####### Now Robustness to different price and quantity indices

#### Have quantity and price in the same table 
#### Make table 
data.main <- fread("Data/LR_Diff_design.csv")
data.main <- data.main[outcome %in% c("DL.ln_cpricei", "DL.ln_cpricei2") & sample == "non_imp_tax_strong"]

data.controls <- fread("Data/LRDiff_semesterly_w_econ.csv")
data.controls <- data.controls[outcome %in% c("DL.ln_cpricei", "DL.ln_cpricei2") & rn == "DL.ln_sales_tax" & sample == "non_imp_tax_strong"]

c5 <- as.data.frame(c("coef1", "se1", "coef2", "se2"))
c1 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_cpricei" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c2 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_cpricei" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c3 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_cpricei" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c4 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_cpricei" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
table1 <- as.data.frame(c(c5, c1, c2, c3, c4))
names(table1) <- c("rows", "c1", "c2", "c3", "c4")

setDT(table1)
for(c in c("c1", "c2", "c3", "c4")) {
  
  #table1[, (c) := ifelse(rows %in% c("coef1", "coef2"), paste0("\\textbf{", table1[[c]], "}"), table1[[c]])]
  table1[, (c) := ifelse(rows %in% c("se1", "se2"), paste0("(", table1[[c]], ")"), table1[[c]])]
  
}

table1[, c4 := ifelse(rows == "se1", paste0(c4, " \\\\"), c4)]
table1[, c4 := ifelse(rows == "se2", paste0(c4, " \\\\ \\hline"), c4)]

table1[, rows := ifelse(rows == "coef1", "Consumer Price (Main Index)", rows)]
table1[, rows := ifelse(rows == "coef2", "Consumer Price (Laspeyres)", rows)]
table1[, rows := ifelse(rows %in% c("se1", "se2"), "", rows)]



## Quantity
#### 
data.main <- fread("Data/LR_Diff_design.csv")
data.main <- data.main[outcome %in% c("DL.ln_quantity", "DL.ln_quantity2", "DL.ln_quantity3") & sample == "non_imp_tax_strong"]

data.controls <- fread("Data/LRDiff_semesterly_w_econ.csv")
data.controls <- data.controls[outcome %in% c("DL.ln_quantity", "DL.ln_quantity2", "DL.ln_quantity3") & rn == "DL.ln_sales_tax" & sample == "non_imp_tax_strong"]

c5 <- as.data.frame(c("coef1", "se1", "coef2", "se2", "region", "division", "econ"))
c1 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", ""))
c2 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", ""))
c3 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", "$\\checkmark$"))
c4 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", "$\\checkmark$"))
table2 <- as.data.frame(c(c5, c1, c2, c3, c4))
names(table2) <- c("rows", "c1", "c2", "c3", "c4")

setDT(table2)
for(c in c("c1", "c2", "c3", "c4")) {
  
  #table2[, (c) := ifelse(rows %in% c("coef1", "coef2"), paste0("\\textbf{", table2[[c]], "}"), table2[[c]])]
  table2[, (c) := ifelse(rows %in% c("se1", "se2"), paste0("(", table2[[c]], ")"), table2[[c]])]
  
}

table2[, c4 := ifelse(rows == "se1", paste0(c4, " \\\\"), c4)]
table2[, c4 := ifelse(rows == "se2", paste0(c4, " \\\\ \\hline"), c4)]

table2[, rows := ifelse(rows == "coef1", "Quantity (Main Index)", rows)]
table2[, rows := ifelse(rows == "coef2", "Quantity (log Sales - log Price)", rows)]
table2[, rows := ifelse(rows %in% c("se1", "se2"), "", rows)]
table2[, rows := ifelse(rows == "region", "Reg $\\times$ Mod $\\times$ Time FE", rows)]
table2[, rows := ifelse(rows == "division", "Div $\\times$ Mod $\\times$ Time FE", rows)]
table2[, rows := ifelse(rows == "econ", "Econ controls", rows)]


#Combine panel1 and panel2
table1 <- rbind(table1, table2)

# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{lcccc}",
            "\\hline",
            " & (1) & (2) & (3) & (4) \\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline",
            "\\end{tabular} %}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
body <- apply(table1, 1, function(x) paste(x, collapse = " & "))
body <- paste(body, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, body, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_price_quantity_index_combined_LR.tex")




############ Spillovers
data.spill <- fread("Data/LRDiff_semesterly_spillovers.csv")

spill.DL <- data.spill[rn == "DL.ln_statutory_tax" & subsample == "all_taxexempt" & outcome %in% c("DL.ln_cpricei2", "DL.ln_quantity3")]
spill.DL <- spill.DL[sample == "non_imp_tax_strong"]

spill.controls <- fread("Data/LRDiff_semesterly_spillovers_controls.csv")
spill.controls <- spill.controls[rn == "DL.ln_statutory_tax" & subsample == "all_taxexempt" & outcome %in% c("DL.ln_cpricei2", "DL.ln_quantity3")]
spill.controls <- spill.controls[sample == "non_imp_tax_strong"]


#### Make table
c5 <- as.data.frame(c("coef1", "se1", "coef2", "se2", "region", "division", "econ"))
c1 <- as.data.frame(c(t(round(spill.DL[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(spill.DL[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", ""))
c2 <- as.data.frame(c(t(round(spill.DL[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(spill.DL[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", ""))
c3 <- as.data.frame(c(t(round(spill.controls[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(spill.controls[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", "$\\checkmark$"))
c4 <- as.data.frame(c(t(round(spill.controls[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(spill.controls[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", "$\\checkmark$"))
table1 <- as.data.frame(c(c5, c1, c2, c3, c4))
names(table1) <- c("rows", "c1", "c2", "c3", "c4")

setDT(table1)
for(c in c("c1", "c2", "c3", "c4")) {
  
  #table1[, (c) := ifelse(rows %in% c("coef1", "coef2"), paste0("\\textbf{", table1[[c]], "}"), table1[[c]])]
  table1[, (c) := ifelse(rows %in% c("se1", "se2"), paste0("(", table1[[c]], ")"), table1[[c]])]
  
}

table1[, c4 := ifelse(rows == "se1", paste0(c4, " \\\\"), c4)]
table1[, c4 := ifelse(rows == "se2", paste0(c4, " \\\\ \\hline"), c4)]

table1[, rows := ifelse(rows == "coef1", "Consumer Price (Expt goods)", rows)]
table1[, rows := ifelse(rows == "coef2", "Quantity (Expt goods)", rows)]
table1[, rows := ifelse(rows %in% c("se1", "se2"), "", rows)]
table1[, rows := ifelse(rows == "region", "Reg $\\times$ Mod $\\times$ Time FE", rows)]
table1[, rows := ifelse(rows == "division", "Div $\\times$ Mod $\\times$ Time FE", rows)]
table1[, rows := ifelse(rows == "econ", "Econ controls", rows)]


# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{lcccc}",
            "\\hline",
            " & (1) & (2) & (3) & (4) \\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline",
            "\\end{tabular} %}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
body <- apply(table1, 1, function(x) paste(x, collapse = " & "))
body <- paste(body, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, body, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_spillovers.tex")




########################### Non-linearities
### Full sample
#data.main <- fread("Data/LR_Diff_design.csv")
#data.main <- data.main[outcome %in% c("DL.ln_cpricei2", "DL.ln_quantity3") & sample == "non_imp_tax_strong"]

### Subsample IVs
data.sub2 <- fread("Data/IV_subsamples_initprice.csv")
data.sub2 <- data.sub2[n.groups %in% c(1,2)]




### Divide sample based on p^c_it-1 
# First panel: Full Data
rows <- as.data.frame(c("coef", "se"))
c1 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time" & n.groups == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c2 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time" & n.groups == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c3 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time" & n.groups == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c4 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time" & n.groups == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c5 <- as.data.frame(c(t(round(data.sub2[outcome == "IV" & controls == "region_by_module_by_time" & n.groups == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c6 <- as.data.frame(c(t(round(data.sub2[outcome == "IV" & controls == "division_by_module_by_time" & n.groups == 1, c("Estimate", "Cluster s.e.")], digits = 3))))


panel1 <- as.data.frame(c(rows, c1, c2, c3, c4, c5, c6))
names(panel1) <- c("rows", "c1", "c2", "c3", "c4", "c5", "c6")


# Second panel: lagged prices below median
rows <- as.data.frame(c("coef", "se"))
c1 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time" & n.groups == 2 & group == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c2 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time" & n.groups == 2 & group == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c3 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time" & n.groups == 2 & group == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c4 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time" & n.groups == 2 & group == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c5 <- as.data.frame(c(t(round(data.sub2[outcome == "IV" & controls == "region_by_module_by_time" & n.groups == 2 & group == 1, c("Estimate", "Cluster s.e.")], digits = 3))))
c6 <- as.data.frame(c(t(round(data.sub2[outcome == "IV" & controls == "division_by_module_by_time" & n.groups == 2 & group == 1, c("Estimate", "Cluster s.e.")], digits = 3))))


panel2 <- as.data.frame(c(rows, c1, c2, c3, c4, c5, c6))
names(panel2) <- c("rows", "c1", "c2", "c3", "c4", "c5", "c6")


# Third panel: lagged prices above median
rows <- as.data.frame(c("coef", "se", "region", "division"))
c1 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time" & n.groups == 2 & group == 2, c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", ""))
c2 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time" & n.groups == 2 & group == 2, c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$"))
c3 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time" & n.groups == 2 & group == 2, c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", ""))
c4 <- as.data.frame(c(t(round(data.sub2[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time" & n.groups == 2 & group == 2, c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$"))
c5 <- as.data.frame(c(t(round(data.sub2[outcome == "IV" & controls == "region_by_module_by_time" & n.groups == 2 & group == 2, c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", ""))
c6 <- as.data.frame(c(t(round(data.sub2[outcome == "IV" & controls == "division_by_module_by_time" & n.groups == 2 & group == 2, c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$"))


panel3 <- as.data.frame(c(rows, c1, c2, c3, c4, c5, c6))
names(panel3) <- c("rows", "c1", "c2", "c3", "c4", "c5", "c6")


### Combine panels into a latex table
setDT(panel1)
setDT(panel2)
setDT(panel3)
for(c in c("c1", "c2", "c3", "c4", "c5", "c6")) {
  
  #panel1[, (c) := ifelse(rows == "coef", paste0("\\textbf{", panel1[[c]], "}"), panel1[[c]])]
  panel1[, (c) := ifelse(rows == "se", paste0("(", panel1[[c]], ")"), panel1[[c]])]
  
  #panel2[, (c) := ifelse(rows == "coef", paste0("\\textbf{", panel2[[c]], "}"), panel2[[c]])]
  panel2[, (c) := ifelse(rows == "se", paste0("(", panel2[[c]], ")"), panel2[[c]])]
  
  #panel3[, (c) := ifelse(rows == "coef", paste0("\\textbf{", panel3[[c]], "}"), panel3[[c]])]
  panel3[, (c) := ifelse(rows == "se", paste0("(", panel3[[c]], ")"), panel3[[c]])]
  
}

panel3[, c6 := ifelse(rows == "se", paste0(c6, " \\\\ \\hline"), c6)]

panel1[, rows := ""]

panel2[, rows := ifelse(rows == "coef", "Below median $p^{c}_{it}$", "")]
panel3[, rows := ifelse(rows == "coef", "Above median $p^{c}_{it}$", rows)]
panel3[, rows := ifelse(rows == "region", "Reg $\\times$ Mod $\\times$ Time FE", rows)]
panel3[, rows := ifelse(rows == "division", "Div $\\times$ Mod $\\times$ Time FE", rows)]
panel3[, rows := ifelse(rows == "se", "", rows)]


# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{lcccccc}",
            "\\hline",
            " & \\multicolumn{2}{c}{Price} & \\multicolumn{2}{c}{Quantity} & \\multicolumn{2}{c}{IV} \\\\",
            " & (1) & (2) & (3) & (4) & (5) & (6)\\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline",
            "\\end{tabular} %}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
body1 <- apply(panel1, 1, function(x) paste(x, collapse = " & "))
body1 <- paste(body1, "\\\\")

body2 <- apply(panel2, 1, function(x) paste(x, collapse = " & "))
body2 <- paste(body2, "\\\\")

body3 <- apply(panel3, 1, function(x) paste(x, collapse = " & "))
body3 <- paste(body3, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, " \\textbf{Full Sample} & \\multicolumn{6}{c}{} \\\\", body1, "\\hline \\\\", " \\textbf{Subsamples} & \\multicolumn{6}{c}{} \\\\ \\\\", body2, "\\\\", body3, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_nonlinearities_DL_pt_1.tex")





###### Cross-sectional design
data <- fread("Data/LRdiff_cross_sectional_design_hh.csv")
data <- data[rn == "avg.ln_sales_tax"]

rows <- as.data.frame(c("coef", "se", "group", "income"))
c1 <- as.data.frame(c(t(round(data[FE_d == "group_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", ""))
c2 <- as.data.frame(c(t(round(data[FE_d == "income_by_group_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$"))

table1 <- as.data.frame(c(rows, c1, c2))
names(table1) <- c("rows", "c1", "c2")

setDT(table1)
for(c in c("c1", "c2")) {
  
  #panel1[, (c) := ifelse(rows == "coef", paste0("\\textbf{", panel1[[c]], "}"), panel1[[c]])]
  table1[, (c) := ifelse(rows == "se", paste0("(", table1[[c]], ")"), table1[[c]])]
  
}

table1[, c2 := ifelse(rows == "se", paste0(c2, "\\\\ \\hline \\\\"), c2)]
table1[, rows := ifelse(rows == "group", "Product Group FE", rows)]
table1[, rows := ifelse(rows == "income", "Product Group $\\times$ Income FE", rows)]
table1[, rows := ifelse(rows %in% c("se", "coef"), "", rows)]

# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{lcc}",
            "\\hline",
            " & \\multicolumn{2}{c}{Expenditures} \\\\",
            " & (1) & (2) \\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("Data & \\multicolumn{2}{c}{Household Panel Data} \\\\",
            "\\hline",
            "\\end{tabular} %}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
table1 <- apply(table1, 1, function(x) paste(x, collapse = " & "))
table1 <- paste(table1, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, table1, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_cross_section_hh.tex")



####### Make table for conduct parameter under different assumptions about salience
#### Using division X module X FEs
data.division <- fread("Data/salience_conduct_parameter_at_p_division.csv")
data.region <- fread("Data/salience_conduct_parameter_at_p_region.csv")

## Keep Collapse to average across prices and keep only desired specifications
av.theta.division <- data.division[, .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
av.theta.region <- data.region[, .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]

av.theta.division <- av.theta.division[K == 2 & es.val == Inf & sigma %in% c(0.25, 0.5, 0.75, 1)]
av.theta.region <- av.theta.region[K == 2 & es.val == Inf & sigma %in% c(0.25, 0.5, 0.75, 1)]


table1 <- as.data.frame(rbind(t(round(av.theta.division[, c("sigma", "theta")], digits = 2)), t(round(av.theta.region[, "theta"], digits = 2))))
table1 <- as.data.frame(c(as.data.frame(c("", "", "")), table1, as.data.frame(c("sigma", "Div", "Reg"))))
names(table1) <- c("V1", "V2", "V3", "V4", "V5", "rows")

setDT(table1)
for(c in c("V2", "V3", "V4", "V5")) {
  
  table1[, (c) := ifelse(rows == "sigma", paste0("$\\boldsymbol{ \\sigma =", table1[[c]], "}$"), table1[[c]])]
  
}

#table1[, rows := ifelse(rows == "sigma", paste0(V5, "\\\\ \\hline"), V5)]
table1[, V1 := ifelse(rows == "Div", "\\multirow{2}{*}{$\\boldsymbol{\\theta =}$}", V1)]

table1[, rows := ifelse(rows == "sigma", "\\textbf{Specification} \\\\ \\hline", rows)]
table1[, rows := ifelse(rows == "Div", "Div. FE", rows)]
table1[, rows := ifelse(rows == "Reg", "Reg. FE", rows)]


# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{lccccc}",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline",
            "\\end{tabular} %}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
table1 <- apply(table1, 1, function(x) paste(x, collapse = " & "))
table1 <- paste(table1, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, table1, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_theta.tex")

