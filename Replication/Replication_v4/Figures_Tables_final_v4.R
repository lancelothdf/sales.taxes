
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



########## TABLE 3: Reduced-form effects (and IV) in the long-run (2-year differences)
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
c6 <- as.data.frame(c("", "", "", "", "", ""))
c1 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_pricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c2 <- as.data.frame(c(t(round(data.main[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_pricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.main[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c3 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_pricei2" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
c4 <- as.data.frame(c(t(round(data.controls[outcome == "DL.ln_cpricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_pricei2" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.controls[outcome == "DL.ln_quantity3" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3))))
table1 <- as.data.frame(c(c5, c6, c1, c2, c3, c4))
names(table1) <- c("rows", "c6", "c1", "c2", "c3", "c4")

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
c6 <- as.data.frame(c("", "", "", "", "", "", ""))
c1 <- as.data.frame(c(t(round(data.IV[outcome == "Demand" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV[outcome == "Inverse Supply" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", ""))
c2 <- as.data.frame(c(t(round(data.IV[outcome == "Demand" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV[outcome == "Inverse Supply" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "", "$\\checkmark$", ""))
c3 <- as.data.frame(c(t(round(data.IV.controls[outcome == "Demand" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV.controls[outcome == "Inverse Supply" & controls == "region_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), "$\\checkmark$", "", "$\\checkmark$"))
c4 <- as.data.frame(c(t(round(data.IV.controls[outcome == "Demand" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)), t(round(data.IV.controls[outcome == "Inverse Supply" & controls == "division_by_module_by_time", c("Estimate", "Cluster s.e.")], digits = 3)),  "", "$\\checkmark$", "$\\checkmark$"))
table2 <- as.data.frame(c(c5, c6, c1, c2, c3, c4))
names(table2) <- c("rows", "c6", "c1", "c2", "c3", "c4")

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
            "\\begin{tabularx}{\\textwidth}{lXXXXX}",
            "\\hline",
            " & & (1) & (2) & (3) & (4) \\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline \\\\",
            "\\end{tabularx}%}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Create the table body
body1 <- apply(table1, 1, function(x) paste(x, collapse = " & "))
body1 <- paste(body1, "\\\\")
body2 <- apply(table2, 1, function(x) paste(x, collapse = " & "))
body2 <- paste(body2, "\\\\")

# Combine the header, body, and footer into a single string
table_str <- c(header, "\\textbf{OLS} & \\multicolumn{5}{c}{} \\\\", "\\multicolumn{6}{c}{} \\\\", body1, "\\textbf{IV} & \\multicolumn{5}{c}{} \\\\", "\\multicolumn{6}{c}{} \\\\", body2, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/table_main_LR_IV.tex")



####### APPENDIX TABLE: ROBUSTNESS TO PRICE/QUANTITY INDEX
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
            "\\begin{tabularx}{\textwidth}{lCCCC}",
            "\\hline",
            " & (1) & (2) & (3) & (4) \\\\",
            "\\hline \\\\")

# Define the table footer
footer <- c("\\hline",
            "\\end{tabularx} %}",
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



############# TABLE 4: EVIDENCE FOR NON-LINEARITIES
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



########### APPENDIX TABLE: CROSS-SECTIONAL HOUSEHOLD-LEVEL DESIGN
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




######### TABLE 5: Conduct parameter under different assumptions about salience
####### Make table for conduct parameter under different assumptions about salience
#### Using division X module X FEs
#data.division <- fread("Data/salience_conduct_parameter_at_p_division.csv")
data.region <- fread("Data/salience_conduct_parameter_at_p_region.csv")

## Keep Collapse to average across prices and keep only desired specifications
#av.theta.division <- data.division[, .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
av.theta.region <- data.region[, .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]

#av.theta.division <- av.theta.division[K == 2 & es.val == Inf & sigma %in% c(0.25, 0.5, 0.75, 1)]
av.theta.region <- av.theta.region[K == 2 & es.val == Inf & sigma %in% c(0.25, 0.5, 0.75, 1)]


table1 <- as.data.frame(rbind(c(0.25, 0.50, 0.75, 1.00), t(round(av.theta.region[, "theta"], digits = 3))))
table1 <- as.data.frame(c(as.data.frame(c("sigma", "")), table1))
#table1 <- as.data.frame(c(as.data.frame(c("sigma", "Div", "Reg")), as.data.frame(c("", "", "")), table1))
names(table1) <- c("rows", "V1", "V2", "V3", "V4")


### NEED TO UPDATE BELOW
setDT(table1)
for(c in c("V2", "V3", "V4")) {
  
  #table1[, (c) := ifelse(rows == "sigma", paste0("$\\boldsymbol{ \\sigma =", table1[[c]], "}$"), table1[[c]])]
  table1[, (c) := ifelse(rows == "sigma", paste0("$\\boldsymbol{", table1[[c]], "}$"), table1[[c]])]
  
}

table1[, V5 := ifelse(rows == "sigma", paste0(V5, "\\\\ & \\multicolumn{4}{c}{} & \\\\ \\hline & \\multicolumn{4}{c}{} & "), V5)]

table1[, V1 := ifelse(rows == "sigma", paste0(V1, "$\\boldsymbol{\\sigma =}$"), V1)]
table1[, V1 := ifelse(rows == "Div", "\\multirow{2}{*}{$\\boldsymbol{\\theta =}$}", V1)]

table1[, rows := ifelse(rows == "sigma", "\\textbf{Specification}", rows)]
table1[, rows := ifelse(rows == "Div", "Div. FE", rows)]
table1[, rows := ifelse(rows == "Reg", "Reg. FE", rows)]


# Define the table header
header <- c("%\\begin{table}[htb]",
            "%\\centering",
            "%\\resizebox{\\textwidth}{!}{",
            "\\begin{tabular}{| c | ccccc |}",
            "\\hline & \\multicolumn{4}{c}{} & \\\\")

# Define the table footer
footer <- c("& \\multicolumn{4}{c}{} & \\\\",
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
writeLines(table_str, "Figures_preferred_v4/table_theta.tex")




###################### FIGURE 1: Distributed-lag model estimates for price and quantity
# Open data
data <- fread("Data/LRdiff_semesterly_main.csv")

# Keep only cummulative effects and 
data <- data[str_detect(rn, "cumul") & sample == "all"]

# Capture time of event
data[, tt_event := (1:.N) - 6, by = .(outcome, controls)]

# Keep from -4 to 4
data <- data[abs(tt_event) <= 5 & tt_event <= 3]
data <- data[ , tt_event := tt_event + 1]


# Change names to produce plot
setnames(data, old = c("Estimate", "Cluster s.e."), new = c("estimate", "se"))

# Make Se =0 if NA (base time)
data[, se:= ifelse(is.na(se),0,se) ]


###### Division_by_module_by_time
### (a) Dynamic response of consumer price to sales taxes

gg <- ggplot(data = data[outcome == "D.ln_cpricei2" & controls == "division_by_module_by_time"], 
             mapping = aes(x = tt_event)) +
  geom_line(aes(y = estimate), size = 0.8, alpha = 0.5) +
  geom_line(aes(y = estimate + 1.645 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  geom_line(aes(y = estimate - 1.645 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-0.5, 1.5), breaks = seq(-0.5, 1.5, 0.25)) +
  scale_x_continuous(breaks = seq(-4, 4, 1), labels = seq(-24,24,6)) +
  labs(x = "Months Relative to Event", y = "Elasticity to Tax") +
  geom_hline(yintercept = 0, color = "red", linetype = "55") +
  geom_vline(xintercept =  0.5, color = "black", alpha = .8) +
  theme(legend.position = "none",
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size   # Modify Y axis label font size
ggsave("Figures_preferred_v4/F1_prices.png",
       height = 3, width = 6.5)



### (b) Dynamic response of quantity to sales taxes
gg <- ggplot(data = data[outcome == "D.ln_quantity3" & controls == "division_by_module_by_time"], 
             mapping = aes(x = tt_event)) +
  geom_line(aes(y = estimate), size = 0.8, alpha = 0.5) +
  geom_line(aes(y = estimate + 1.645 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  geom_line(aes(y = estimate - 1.645 * se), alpha = 0.5, size = 0.8, linetype = "55") +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(limits = c(-1.75, 0.5), breaks = seq(-1.75, 0.5, 0.25)) +
  scale_x_continuous(breaks = seq(-4, 4, 1), labels = seq(-24,24,6)) +
  labs(x = "Months Relative to Event", y = "Elasticity to Tax") +
  geom_hline(yintercept = 0, color = "red", linetype = "55") +
  geom_vline(xintercept =  0.5, color = "black", alpha = .8) + 
  theme(legend.position = "none",
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size
ggsave("Figures_preferred_v4/F1_quantities.png",
       height = 3, width = 6.5)

rm(data, gg)



##################### FIGURE 4: Evidence for non-linearity in demand (compare region and division)
### (a) IV of demand for each of 2 quintiles of the distribution of lagged prices.
# Open data - Only 2 quantiles
data.sub2 <- fread("Data/IV_subsamples_initprice.csv")
data.sub2 <- data.sub2[outcome == "IV"]

# Change names to produce plot
setnames(data.sub2, old = c("Estimate", "Cluster s.e."), new = c("estimate", "se"))

# Subset to presented case
data.sub2 <- data.sub2[n.groups == 2]


## Recover corect s.e.s
data.sub2[, ll90.norm := estimate - 1.645*se]
data.sub2[, ul90.norm := estimate + 1.645*se]

data.sub2 <- data.sub2[, c("group", "estimate", "se", "ll90.norm", "ul90.norm", "controls")]

labels <- paste0("Q", order(unique(data.sub2$group), decreasing = TRUE))

# Keep only specification with 
data.sub2 <- data.sub2[controls == "region_by_module_by_time"]

# Produce plots
# both FEs
gg <- ggplot(data = data.sub2, 
             mapping = aes(x = group, y = estimate)) +
  geom_point(size = 2.2, alpha = .8, position = position_dodge(width = 0.3), color = "black") +
  geom_errorbar(mapping = aes(ymax = ul90.norm,
                              ymin = ll90.norm),
                width = .3, position = position_dodge(width = 0.3)) +
  theme_bw(base_size = fontsize) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin=0.0, ymax=Inf), fill = "black", alpha = 0.05) +
  scale_y_continuous(limits = c(-1, 0.5), breaks = seq(-1, 0.5, 0.5)) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = seq(1, 2, 1), labels = labels) +
  labs(x = "Initial Price Level Quantile", y = "IV Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)
ggsave("Figures_preferred_v4/F2_Subsample_IVs_boot_FEs_2Q.png",
       height = 3, width = 6.5)

rm(data.sub2, gg)


### (b) Distribution of current prices in each of 2 quintiles of the distribution of lagged prices
# (b) Distribution of current prices in each quintiles of the distribution of lagged prices
data <- fread("Data/Emp_price_subsamples_initprice.csv")

# tag FE as forgot to do so
data[, nr := seq_along(.I), by = c("quantile", "n.groups", "d.lp", "treated", "w")]
data[, controls := ifelse(nr == 1, "region_by_module_by_time", "division_by_module_by_time")]

# identify data and steps
data <- data[n.groups == 2 & controls == "region_by_module_by_time" & is.na(treated)]
step <- (max(data$log.n.p) - min(data$log.n.p))/1500
data[, dens.n.log.p := dens.n.log.p/(100*step)]

# Compute the CDFs
data <- data[order(log.n.p, quantile, w)]
data[, CDF.n.log.p := cumsum(dens.n.log.p), by = .(quantile, w)] 
data[, CDF.n.log.p := CDF.n.log.p/max(CDF.n.log.p, na.rm=T), by = .(quantile, w)] # divided into max due to approximation (floor)

# Version 1: bases.sales weighted
gg <- ggplot(data[w == "base.sales"], 
             aes(x = log.n.p, y = CDF.n.log.p, linetype = factor(quantile))) + 
  geom_line(color = "black") +  # Set color to black for both lines
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.25,0.25,0.125)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_linetype_manual(values = c("solid", "dashed")) +  # Solid and dashed lines
  scale_color_brewer(palette="Set1") +
  labs(x = "Log(price)", y = "CDF", linetype = "Quantile") +  # Removed color legend
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)
ggsave("Figures_preferred_v4/F2_Prices_CDF_Subsamples_2Q.png",
       height = 3.5, width = 6.5)


# Version 1: ``cohort-corrected'' bases.sales weighted
gg <- ggplot(data[w == "base.sales.q"], 
             aes(x = log.n.p, y = CDF.n.log.p, linetype = factor(quantile))) + 
  geom_line(color = "black") +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.25,0.25,0.125)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_linetype_manual(values = c("solid", "dashed")) +  # Solid and dashed lines
  scale_color_brewer(palette="Set1") +
  labs(x = "Log(price)", y = "CDF", color = "Quantile") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)
ggsave("Figures_preferred_v4/F2_Prices_CDF_Subsamples_corrw_2Q.png",
       height = 3.5, width = 6.5)

rm(data, gg, step)




###################### FIGURE 5: Evidence for non-linearities in the Supply curve
###########
#### Figure on non-linearities in (inverse) Supply
##### Figure #: Reduced form evidence of non-linearities in the (inverse supply) curve. -----

### (a) IV of demand for each of 2 quintiles of the distribution of lagged prices.
# Open data - Only 2 quantiles
data.sub2 <- fread("Data/IV_invsup_subsamples_initprice.csv")
data.sub2 <- data.sub2[outcome == "IV"]

# Change names to produce plot
setnames(data.sub2, old = c("Estimate", "Cluster s.e."), new = c("estimate", "se"))

# Subset to presented case
data.sub2 <- data.sub2[n.groups == 2]


## Recover corect s.e.s
data.sub2[, ll90.norm := estimate - 1.645*se]
data.sub2[, ul90.norm := estimate + 1.645*se]

data.sub2 <- data.sub2[, c("group", "estimate", "se", "ll90.norm", "ul90.norm", "controls")]

labels <- paste0("Q", order(unique(data.sub2$group), decreasing = TRUE))

# Keep only specification with 
data.sub2 <- data.sub2[controls == "region_by_module_by_time"]

# Produce plots
# both FEs
maxval <- max(data.sub2$ul90.norm)

gg <- ggplot(data = data.sub2, 
             mapping = aes(x = group, y = estimate)) +
  geom_point(size = 2.2, alpha = .8, position = position_dodge(width = 0.3), color = "black") +
  geom_errorbar(mapping = aes(ymax = ul90.norm,
                              ymin = ll90.norm),
                width = .3, position = position_dodge(width = 0.3)) +
  theme_bw(base_size = fontsize) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin=-Inf, ymax=0.0), fill = "black", alpha = 0.05) +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = c(seq(-0.5, 0.5, 0.5),maxval), labels = round(c(seq(-0.5, 0.5, 0.5),maxval), digits = 1),
                     sec.axis = sec_axis(~ ., 
                                         name = "Implied Elast. of Supply", breaks = c(0.5, maxval, 0.0), 
                                         labels = c("2", "10", TeX('$+\\infty$'))
                     )) +
  scale_x_continuous(limits = c(0.5, 2.5), breaks = seq(1, 2, 1), labels = labels) +
  labs(x = "Initial Producer Price Level Quantile", y = "IV Estimate", color = NULL) +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #        text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)
ggsave("Figures_preferred_v4/F2_Subsample_invsup_IVs_boot_FEs_2Q.png",
       height = 3, width = 6.5)

rm(data.sub2, gg)



### (b) Distribution of current prices in each of 2 quintiles of the distribution of lagged producer prices
# (b) Distribution of current prices in each quintiles of the distribution of lagged producer prices
data <- fread("Data/Emp_prod_price_subsamples_initprice.csv")

# tag FE as forgot to do so
data[, nr := seq_along(.I), by = c("quantile", "n.groups", "d.lp", "treated", "w")]
data[, controls := ifelse(nr == 1, "region_by_module_by_time", "division_by_module_by_time")]

# identify data and steps
data <- data[n.groups == 2 & controls == "region_by_module_by_time" & is.na(treated)]
step <- (max(data$log.n.p) - min(data$log.n.p))/1500
data[, dens.n.log.p := dens.n.log.p/(100*step)]

# Compute the CDFs
data <- data[order(log.n.p, quantile, w)]
data[, CDF.n.log.p := cumsum(dens.n.log.p), by = .(quantile, w)] 
data[, CDF.n.log.p := CDF.n.log.p/max(CDF.n.log.p, na.rm=T), by = .(quantile, w)] # divided into max due to approximation (floor)

# Version 1: bases.sales weighted
gg <- ggplot(data[w == "base.sales"], 
             aes(x = log.n.p, y = CDF.n.log.p, linetype = factor(quantile))) + 
  geom_line(color = "black") +  # Set color to black for both lines
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.25,0.25,0.125)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_linetype_manual(values = c("solid", "dashed")) +  # Solid and dashed lines
  scale_color_brewer(palette="Set1") +
  labs(x = "Log(producer price)", y = "CDF", linetype = "Quantile") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        # text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size))
ggsave("Figures_preferred_v4/F2_Producer_Prices_CDF_Subsamples_2Q.png",
       height = 3.5, width = 6.5)


# Version 1: ``cohort-corrected'' bases.sales weighted
gg <- ggplot(data[w == "base.sales.q"], 
             aes(x = log.n.p, y = CDF.n.log.p, linetype = factor(quantile))) + 
  geom_line(color = "black") + # Set color to black for both lines
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.25,0.25,0.125)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  scale_linetype_manual(values = c("solid", "dashed")) +  # Solid and dashed lines
  scale_color_brewer(palette="Set1") +
  labs(x = "Log(producer price)", y = "CDF", color = "Quantile") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size))
ggsave("Figures_preferred_v4/F2_Producer_Prices_CDF_Subsamples_corrw_2Q.png",
       height = 3.5, width = 6.5)

rm(data, gg, step)




############### APPENDIX FIGURE: POINT IDENTIFIED DEMAND AND ELASTICITY 
########  Plot Estimated Demand and Elasticity under K=L=2
##### Figure #: Estimated demand function  and elasticity under K^d = L^d = 2. -------

#### First prepare data for Division FEs
## Add options to plots
min.price <- -0.15
max.price <- 0.15

data <- fread("Data/Demand_theta_sat_initial_price_semester_boot_r.csv")

## Keep 2 quintiles case, div x module x time FE 
#data <- data[n.groups == 2 & controls == "group_division_by_module_by_time"]

## Keep interest variables to dcast data
#data <- data[, c("beta_n", "beta_hat", "iter")]

## Extract boot data
#data.0 <- data[iter == 0]
#data.boot <- data[iter != 0]
### dcast betas
#data.boot <- dcast(data.boot, iter ~ beta_n,  fun=sum, value.var = c("beta_hat"))
#setnames(data.boot, old = c("0", "1", "2"), new = paste0("beta_",0:2))

### calculate variances and covariances
#data.covs <- data.boot[, .(mean0 = mean(beta_0), mean1 = mean(beta_1), mean2 = mean(beta_2),
#                           var0 = (sd(beta_0))^2, var1 = (sd(beta_1))^2, 
#                           var2 = (sd(beta_2))^2,
#                           cov01 = cov(beta_0, beta_1), cov02 = cov(beta_0, beta_2),
#                           cov12 = cov(beta_2, beta_1))]
#rm(data.boot)


#p <- seq(min.price, max.price, 0.005)

### Estimations across p
#beta_hat <- data.0[["beta_hat"]]
#rm(data.0, data.boot)

#demand <- rep(beta_hat[1], length(p))
#elasticity <- rep(0, length(p))
#s.e.elas <- rep(0, length(p))
#s.e.dmd <- rep(data.covs[["var0"]], length(p))
## Calculate Demand, Elas and s.e.s
#for (i in 1:2) {
  
##  demand <- demand + beta_hat[i+1]*p^i
#  elasticity <- elasticity + i*beta_hat[i+1]*p^(i-1)
  
#  s.e.dmd <- s.e.dmd + (p^(2*i))*(data.covs[[paste0("var",i)]]) + 2*(p^i)*(data.covs[[paste0("cov0",i)]])
#  s.e.elas <- s.e.elas + (i^2)*(p^(2*(i-1)))*(data.covs[[paste0("var",i)]])
#  for (j in i:2) {
#    if (i != j) {
#      s.e.dmd <- s.e.dmd + 2*(p^(i+j))*(data.covs[[paste0("cov",i,j)]])
#      s.e.elas <- s.e.elas + 2*i*j*(p^(i+j-2))*(data.covs[[paste0("cov",i,j)]])
#    }
#  }
#}
#s.e.dmd <- (s.e.dmd)^(1/2)
#s.e.elas <- (s.e.elas)^(1/2)
## Save
#data <- data.table(p, demand, elasticity, s.e.dmd, s.e.elas)
#rm(data.covs, s.e.dmd, p, demand, elasticity, s.e.elas, beta_hat)

##### (a) Estimated demand function
#data[, ll := demand - 1.96 * s.e.dmd]
#data[, ul := demand + 1.96 * s.e.dmd]
#data[, ll90 := demand - 1.645 * s.e.dmd]
#data[, ul90 := demand + 1.645 * s.e.dmd]
#data[, FE := "Division FE"]

#data.division <- data


### Region FEs
data <- fread("Data/Demand_theta_sat_initial_price_semester_boot_r.csv")

# Keep 2 quintiles case, div x module x time FE 
data <- data[n.groups == 2 & controls == "group_region_by_module_by_time"]

# Keep interest variables to dcast data
data <- data[, c("beta_n", "beta_hat", "iter")]

# Extract boot data
data.0 <- data[iter == 0]
data.boot <- data[iter != 0]
## dcast betas
data.boot <- dcast(data.boot, iter ~ beta_n,  fun=sum, value.var = c("beta_hat"))
setnames(data.boot, old = c("0", "1", "2"), new = paste0("beta_",0:2))

## calculate variances and covariances
data.covs <- data.boot[, .(mean0 = mean(beta_0), mean1 = mean(beta_1), mean2 = mean(beta_2),
                           var0 = (sd(beta_0))^2, var1 = (sd(beta_1))^2, 
                           var2 = (sd(beta_2))^2,
                           cov01 = cov(beta_0, beta_1), cov02 = cov(beta_0, beta_2),
                           cov12 = cov(beta_2, beta_1))]
rm(data.boot)


p <- seq(min.price, max.price, 0.005)

## Estimations across p
beta_hat <- data.0[["beta_hat"]]
rm(data.0, data.boot)

demand <- rep(beta_hat[1], length(p))
elasticity <- rep(0, length(p))
s.e.elas <- rep(0, length(p))
s.e.dmd <- rep(data.covs[["var0"]], length(p))
# Calculate Demand, Elas and s.e.s
for (i in 1:2) {
  
  demand <- demand + beta_hat[i+1]*p^i
  elasticity <- elasticity + i*beta_hat[i+1]*p^(i-1)
  
  s.e.dmd <- s.e.dmd + (p^(2*i))*(data.covs[[paste0("var",i)]]) + 2*(p^i)*(data.covs[[paste0("cov0",i)]])
  s.e.elas <- s.e.elas + (i^2)*(p^(2*(i-1)))*(data.covs[[paste0("var",i)]])
  for (j in i:2) {
    if (i != j) {
      s.e.dmd <- s.e.dmd + 2*(p^(i+j))*(data.covs[[paste0("cov",i,j)]])
      s.e.elas <- s.e.elas + 2*i*j*(p^(i+j-2))*(data.covs[[paste0("cov",i,j)]])
    }
  }
}
s.e.dmd <- (s.e.dmd)^(1/2)
s.e.elas <- (s.e.elas)^(1/2)
# Save
data <- data.table(p, demand, elasticity, s.e.dmd, s.e.elas)
rm(data.covs, s.e.dmd, p, demand, elasticity, s.e.elas, beta_hat)

#### (a) Estimated demand function
data[, ll := demand - 1.96 * s.e.dmd]
data[, ul := demand + 1.96 * s.e.dmd]
data[, ll90 := demand - 1.645 * s.e.dmd]
data[, ul90 := demand + 1.645 * s.e.dmd]
data[, FE := "Region FE"]


#data <- rbind(data, data.division)


gg <- ggplot(data = data, aes(x = p, y = demand)) +
  geom_line() +
  theme_bw(base_size = fontsize) +
  #geom_ribbon(data = data, aes(ymax = ul, ymin = ll, fill = factor(FE)), alpha = 0.2) +
  geom_ribbon(data = data, aes(ymax = ul90, ymin = ll90), alpha = 0.4, colour = NA, show.legend = FALSE) +
  scale_y_continuous(limits = c(7.5, 8), breaks = seq(7.5, 8, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Price", y = "Estimated Demand", color = NULL) +
  theme(legend.position = "bottom",
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))

ggsave("Figures_preferred_v4/F3_Point_Demand_2_FEs.png",
       height = 3, width = 6.5)


######### ELASTICITY
# Plot Estimation: Elasticity
data[, ll := elasticity - 1.96 * s.e.elas]
data[, ul := elasticity + 1.96 * s.e.elas]

data[, ll90 := elasticity - 1.645 * s.e.elas]
data[, ul90 := elasticity + 1.645 * s.e.elas]

gg <- ggplot(data = data, aes(x = p, y = elasticity)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  #geom_ribbon(data = data, aes(ymax = ul, ymin = ll, fill = factor(FE)), alpha = 0.2) +
  geom_ribbon(data = data, aes(ymax = ul90, ymin = ll90), alpha = 0.4, colour = NA, show.legend = FALSE) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Price", y = "Estimated Elasticity", color = NULL) +
  theme(legend.position = "bottom",
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F3_Point_Elasticity_2_FEs.png",
       height = 3, width = 6.5)

rm(gg, min.price, max.price)




############# APPENDIX FIGURE: Robustness to controls for demographics
##### Figure #: Estimates of non-linearities in the demand curve are robust to allowing for potential heterogeneity in elasticities along observables. ------
## Region FEs
# Open estimated demand file
estimates <- fread("Data/Demand_theta_robust_demog_initial_price_semester.csv")

# Average 
estimates <- estimates[het != "Het.Sample", .(beta_hat = weighted.mean(beta_hat, w = P.het.d)), 
                       by = .(beta_n, n.groups, controls, het, n.het.g)]

# Open the estimates in het sample
full.estimates <- fread("Data/Demand_theta_robust_demog_initial_price_semester.csv")
full.estimates <- full.estimates[het  == "Het.Sample", ][, n.het.g := 1]


# Price sequence to plot
price <- seq(-0.15, 0.15, 0.01)

## Options to generate plots
demographics <- c("av_hh_income_trips", 'per_bachelor_25_trips', 'per_black_trips', 'per65_trips')

#Keep same range as other plots to make comparisons
data <- data[abs(p)<=0.15]

for (dem in demographics) {
  full.estimates[, het := dem]
  outfile <- paste0("Figures_preferred_v4/SF3_Het_elas_quad_", dem, "_region.png")
  
  # Capture data that we will use
  case.data <- rbind(estimates[n.groups == "2" & controls == "group_region_by_module_by_time" & het == dem],
                     full.estimates[n.groups == "2" & controls == "group_region_by_module_by_time"], fill = T) 
  
  # Loop across number of groups of heterogeneity. Estimate elasticities
  data.plot <- data.table(NULL)
  for (m in 1:5) {
    
    betas <- case.data[n.het.g == m][["beta_hat"]]
    elas <- rep(0, length(price))
    for (k in 2:3) {
      elas <- elas + (k-1)*(betas[k])*price^(k-2)
    }
    
    data.plot <- rbind(data.plot, data.table(price, elas, m))
    
  }      
  
  ## Plot
  gg <- ggplot(data = data.plot, mapping = aes(x = price, y = elas, color = factor(m))) +
    geom_line(size = 1.1, alpha = .5) +
    theme_bw(base_size = fontsize) +
    scale_y_continuous(limits = c(-1.3, 0.5), breaks = round(seq(-1.2, 0.4, 0.4),2)) +
    scale_x_continuous(breaks = round(seq(-0.15, 0.15, 0.05),2)) +
    labs(x = "Price", y = "Elasticity", color = "M") +
    scale_color_brewer(palette="Set1") +
    theme(legend.position = "bottom",
          #text = element_text(family = "Garamond"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
  ggsave(outfile,
         height = 120, width = 200, units = "mm")
  
}


## Division FEs
# Open estimated demand file
estimates <- fread("Data/Demand_theta_robust_demog_initial_price_semester.csv")

# Average 
estimates <- estimates[het != "Het.Sample", .(beta_hat = weighted.mean(beta_hat, w = P.het.d)), 
                       by = .(beta_n, n.groups, controls, het, n.het.g)]

# Open the estimates in het sample
full.estimates <- fread("Data/Demand_theta_robust_demog_initial_price_semester.csv")
full.estimates <- full.estimates[het  == "Het.Sample", ][, n.het.g := 1]


# Price sequence to plot
price <- seq(-0.15, 0.15, 0.01)

## Options to generate plots
demographics <- c("av_hh_income_trips", 'per_bachelor_25_trips', 'per_black_trips', 'per65_trips')

#Keep same range as other plots to make comparisons
data <- data[abs(p)<=0.15]

for (dem in demographics) {
  full.estimates[, het := dem]
  outfile <- paste0("Figures_preferred_v4/SF3_Het_elas_quad_", dem, "_division.png")
  
  # Capture data that we will use
  case.data <- rbind(estimates[n.groups == "2" & controls == "group_division_by_module_by_time" & het == dem],
                     full.estimates[n.groups == "2" & controls == "group_division_by_module_by_time"], fill = T) 
  
  # Loop across number of groups of heterogeneity. Estimate elasticities
  data.plot <- data.table(NULL)
  for (m in 1:5) {
    
    betas <- case.data[n.het.g == m][["beta_hat"]]
    elas <- rep(0, length(price))
    for (k in 2:3) {
      elas <- elas + (k-1)*(betas[k])*price^(k-2)
    }
    
    data.plot <- rbind(data.plot, data.table(price, elas, m))
    
  }      
  
  ## Plot
  gg <- ggplot(data = data.plot, mapping = aes(x = price, y = elas, color = factor(m))) +
    geom_line(size = 1.1, alpha = .5) +
    theme_bw(base_size = fontsize) +
    scale_y_continuous(limits = c(-1.3, 0.5), breaks = round(seq(-1.2, 0.4, 0.4),2)) +
    scale_x_continuous(breaks = round(seq(-0.15, 0.15, 0.05),2)) +
    labs(x = "Price", y = "Elasticity", color = "M") +
    scale_color_brewer(palette="Set1") +
    theme(legend.position = "bottom",
          #text = element_text(family = "Garamond"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
  ggsave(outfile,
         height = 120, width = 200, units = "mm")
  
}



############### FIGURE 3: Bounds around elasticity of demand (for division fixed effects) + robustness appendix figure using region fixed effects
##### Figure #: Estimated bounds on the point elasticity of demand and the demand function. -----

## With Region FEs
data <- fread("Data/partial_point_results_region.csv")

#Keep same range as other plots to make comparisons
data <- data[abs(p)<=0.15]

### (a) Bounds on the point elasticity of demand using 1 linear IV (Ld = 1).
gg <- ggplot(data[L==1 & K %in% c(2,3,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = elas.up)) +
  geom_line(aes(y = elas.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_elas_L1_region.png",
       height = 3.5, width = 6.5)


### (b) Bounds on the point elasticity of demand using 2 linear IVs (Ld = 2).
gg <- ggplot(data[L==2 & K %in% c(4,5,6,7,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = elas.up)) +
  geom_line(aes(y = elas.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_elas_L2_region.png",
       height = 3.5, width = 6.5)




### (c) Bounds on demand when using 1 linear IV (Ld = 1).
gg <- ggplot(data[L==1 & K %in% c(2,3,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = dd.up)) +
  geom_line(aes(y = dd.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Log(quantity)", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_dd_L1_region.png",
       height = 3.5, width = 6.5)


### (d) Bounds on demand when using 2 linear IVs (Ld = 2).
gg <- ggplot(data[L==2 & K %in% c(4,5,6,7,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = dd.up)) +
  geom_line(aes(y = dd.down)) +
  theme_bw(base_size = 16) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Log(quantity)", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_dd_L2_region.png",
       height = 3.5, width = 6.5)

rm(data, gg)


## With Division FEs
data <- fread("Data/partial_point_results_division.csv")

#Keep same range as other plots to make comparisons
data <- data[abs(p)<=0.15]

### (a) Bounds on the point elasticity of demand using 1 linear IV (Ld = 1).
gg <- ggplot(data[L==1 & K %in% c(2,3,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = elas.up)) +
  geom_line(aes(y = elas.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_elas_L1_division.png",
       height = 3.5, width = 6.5)


### (b) Bounds on the point elasticity of demand using 2 linear IVs (Ld = 2).
gg <- ggplot(data[L==2 & K %in% c(3,4,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = elas.up)) +
  geom_line(aes(y = elas.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Elasticity", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_elas_L2_division.png",
       height = 3.5, width = 6.5)




### (c) Bounds on demand when using 1 linear IV (Ld = 1).
gg <- ggplot(data[L==1 & K %in% c(2,3,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = dd.up)) +
  geom_line(aes(y = dd.down)) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Log(quantity)", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_dd_L1_division.png",
       height = 3.5, width = 6.5)


### (d) Bounds on demand when using 2 linear IVs (Ld = 2).
gg <- ggplot(data[L==2 & K %in% c(3,4,5,8)], aes(x = p, color = factor(K))) +
  geom_line(aes(y = dd.up)) +
  geom_line(aes(y = dd.down)) +
  theme_bw(base_size = 16) +
  scale_x_continuous(limits = c(-0.15, 0.15), breaks = seq(-0.15, 0.15, 0.05), 
                     labels = scales::number_format(accuracy = 0.01)) +
  scale_color_brewer(palette="YlOrRd", direction = -1) +
  labs(x = "Log(price)", y = "Log(quantity)", color = "K") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size)))
ggsave("Figures_preferred_v4/F4_bounds_dd_L2_division.png",
       height = 3.5, width = 6.5)

rm(data, gg)



##################################### TABLE 6: Bounds around MVPF and AVPF of three reforms -- L = 1 and L = 2
## Welfare Nationwide - Region FE
#data <- fread("./average_nationwide_extrapolation_region.csv")
#data.3 <- fread("./average_nationwide_extrapolation_region_smallv2.csv")
data.region <- fread("Data/average_nationwide_extrapolation_region_small.csv")
#data.division <- fread("Data/average_nationwide_extrapolation_division.csv")

setDT(data.region)


# Define the table header
header1 <- c("%\\begin{table}[htb]",
             "\\centering",
             "%\\resizebox{0.7\\textwidth}{!}{",
             "\\begin{tabularx}{\\textwidth}{lCCCCC}",
             "\\hline\\noalign{\\smallskip}",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel A: Marginal change in tax}}}  \\\\[1em]",
             " & \\multicolumn{2}{c}{\\footnotesize{$L^{d}=1$}} & & \\multicolumn{2}{c}{\\footnotesize{$L^{d}=2$}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

# Fill in the body
data.region1 <- data.region[sigma == 1 & L == 1 & theta == 0,]
data.region2 <- data.region[sigma == 1 & L == 2 & theta == 0,]


row1 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "Original" & K == 1,]$value, digits = 3), "} & & "), "\\multicolumn{2}{c}{.} \\\\", sep = "")
row2 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "Original" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 2 & est == "UB",]$value, digits = 3), " & &"), "\\multicolumn{2}{c}{.} \\\\", sep = "")
row3 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "Original" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 4 & est == "UB",]$value, digits = 3), " & &"), paste0(round(data.region2[sc == "Original" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row4 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "Original" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 8 & est == "UB",]$value, digits = 3), " & &"), paste0(round(data.region2[sc == "Original" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")


header2 <- c("\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel B: Non-marginal change - from no tax to current tax}}}  \\\\[1em]",
             " & \\multicolumn{2}{c}{\\footnotesize{$L^{d}=1$}} & & \\multicolumn{2}{c}{\\footnotesize{$L^{d}=2$}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

row5 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "No Tax" & K == 1,]$value, digits = 3), "} & & "), "\\multicolumn{2}{c}{.}", "\\\\", sep = "")
row6 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "No Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 2 & est == "UB",]$value, digits = 3), " & &"), "\\multicolumn{2}{c}{.}", "\\\\", sep = "")
row7 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "No Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 4 & est == "UB",]$value, digits = 3), " & &"), paste0(round(data.region2[sc == "No Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row8 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "No Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 8 & est == "UB",]$value, digits = 3), " & &"), paste0(round(data.region2[sc == "No Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")

header3 <- c("\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel C: Non-marginal change - 5pp increase in tax}}}  \\\\[1em]",
             " & \\multicolumn{2}{c}{\\footnotesize{$L^{d}=1$}} & & \\multicolumn{2}{c}{\\footnotesize{$L^{d}=2$}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

row9 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "plus 5 Tax" & K == 1,]$value, digits = 3), "} & & "), "\\multicolumn{2}{c}{.}", "\\\\", sep = "")
row10 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 2 & est == "UB",]$value, digits = 3), " & &"), "\\multicolumn{2}{c}{.}", "\\\\", sep = "")
row11 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 4 & est == "UB",]$value, digits = 3), " & &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row12 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 8 & est == "UB",]$value, digits = 3), " & &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")


# Define the table footer
footer <- c("\\hline \\\\",
            "\\end{tabularx}%}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Combine the header, body, and footer into a single string
table_str <- c(header1, row1, row2, row3, row4, header2, row5, row6, row7, row8, header3, row9, row10, row11, row12, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/Table_welfare_av_all.tex")





################### FIGURE 6: Conduct parameters consistent with the data (+ robustness to salience and region/division FEs)
##### Figure #: Set of pairs (\varepsilon^s, \theta) that are consistent with the estimated demand function and passthrough. ------
#### Using region X module X FEs
data <- fread("Data/salience_conduct_parameter_at_p_region.csv")

# Compute mean among negative values
#av.theta <- data[q1 < 0 , .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
av.theta <- data[, .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
# Capture elasticity of demand under perfect salience
ed <- 0.453 # Added manually from full sample estimates

# Create the value of the interest parameter (not e^s but e^s/(e^s+e^d)))  

av.theta[, x := es.val/(es.val+ed)]
av.theta[, x := ifelse(is.nan(x),1,x)]


# Plot original paper figure
elas <- -0.453 ## From IV
#avg.tax <- 0.06232442 ## From data
avg.tax <- 0.072 ## Population weighted average tax rate across states

theta.at.inf <- av.theta[sigma==1 & is.infinite(es.val) & K==2, mean(theta)] ## Extracted operatively
mvpf.at.inf <- ((1 + avg.tax + theta.at.inf)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas)
theta.at.4 <- av.theta[sigma==1 & (es.val > 4 & es.val < 4.05) & K==2, mean(theta)] ## Extracted operatively
mvpf.at.4 <- ((1 + avg.tax + theta.at.4)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas)

gg <- ggplot(av.theta[sigma==1 & K==2], mapping = aes(x = x, y = theta)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = fontsize) +
  geom_rect(aes(xmin=0, xmax=4/(4+ed), ymin=theta.at.4, ymax=Inf), fill = "red", alpha = 0.05) +
  geom_rect(aes(xmin=4/(4+ed), xmax = 1, ymin = theta.at.inf, ymax = theta.at.4), fill = "green", alpha = 0.05) +
  geom_vline(xintercept = 4/(4+ed), linetype = "dashed") + 
  labs(x = TeX('$\\frac{\\epsilon_S}{\\epsilon_S + \\epsilon_D}$'), 
       y = TeX("Av. $\\theta$")) +
  scale_y_continuous(limits = c(-0.02, 0.5), breaks = seq(0, 0.5, 0.1),
                     sec.axis = sec_axis(~ ((1 + avg.tax + .)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas), 
                                         name = "MVPF", breaks = c(1, seq(1.2, 1.5, 0.1),mvpf.at.inf, mvpf.at.4), 
                                         labels = c("1", "1.2", "", "1.4", "", 
                                                    round(mvpf.at.inf, digits = 3), 
                                                    round(mvpf.at.4, digits = 3))
                     )) +
  theme(legend.position = "bottom",
        # text = element_text(family = "Garamond"), # have to comment out for it produces a weird error
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("Figures_preferred_v4/F5_average_theta_sigma1_K2_region.png",
       height = 120, width = 200, units = "mm")  


### Triplets (\theta, \varepsilon_s, sigma) that are consistent with the data.

## To be modified

# Plot
gg <- ggplot(av.theta[K==2 & sigma %in% c(0.25, 0.3, 0.5, 0.75, 1)], 
             mapping = aes(x = x, y = theta, color = factor(sigma))) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = fontsize) +
  labs(x = TeX('$\\frac{\\epsilon_S}{\\epsilon_S + \\epsilon_D}$'), 
       y = TeX("Av. $\\theta$"), 
       color = TeX("$\\sigma$  ")) +
  scale_color_brewer(palette="RdYlBu", direction = -1) +
  # geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=Inf), color = "gray", fill = "gray3", alpha = 0.02) +
  # geom_rect(aes(xmin=0, xmax=1/(1+ed), ymin=0.0666, ymax=1), color = "brown3", fill = "brown3", alpha = 0.015) +
  # geom_rect(aes(xmin=1/(1+ed), xmax = 1, ymin = 0, ymax = 0.0666), color = "green", fill = "green", alpha = 0.015) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 4/(4+ed), linetype = "dashed") + 
  theme(legend.position = "bottom",
        legend.margin = unit(0, "mm"),
        # text = element_text(family = "Garamond"), # have to comment out for it produces a weird error
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("Figures_preferred_v4/SF5_average_theta_sigmaall_K2_region.png",
       height = 120, width = 200, units = "mm")

rm(gg, av.theta)



#### Using division X module X FEs
data <- fread("Data/salience_conduct_parameter_at_p_division.csv")

# Compute mean among negative values
#av.theta <- data[q1 < 0 , .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
av.theta <- data[, .(theta = mean(theta, na.rm = T)), by = .(sigma, es.val, K)]
# Capture elasticity of demand under perfect salience
ed <- 0.503 # Added manually from full sample estimates

# Create the value of the interest parameter (not e^s but e^s/(e^s+e^d)))  

av.theta[, x := es.val/(es.val+ed)]
av.theta[, x := ifelse(is.nan(x),1,x)]


# Plot original paper figure
elas <- -0.503 ## From IV
#avg.tax <- 0.06232442 ## From data
avg.tax <- 0.072 ## Population weighted average tax rate across states

theta.at.inf <- av.theta[sigma==1 & is.infinite(es.val) & K==2, mean(theta)] ## Extracted operatively
mvpf.at.inf <- ((1 + avg.tax + theta.at.inf)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas)
theta.at.4 <- av.theta[sigma==1 & (es.val > 4 & es.val < 5) & K==2, mean(theta)] ## Extracted operatively
mvpf.at.4 <- ((1 + avg.tax + theta.at.4)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas)

gg <- ggplot(av.theta[sigma==1 & K==2], mapping = aes(x = x, y = theta)) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = fontsize) +
  geom_rect(aes(xmin=0, xmax=4/(4+ed), ymin=theta.at.4, ymax=Inf), fill = "red", alpha = 0.05) +
  geom_rect(aes(xmin=4/(4+ed), xmax = 1, ymin=theta.at.inf, ymax = theta.at.4), fill = "green", alpha = 0.05) +
  geom_vline(xintercept = 4/(4+ed), linetype = "dashed") + 
  labs(x = TeX('$\\frac{\\epsilon_S}{\\epsilon_S + \\epsilon_D}$'), 
       y = TeX("Av. $\\theta$")) +
  scale_y_continuous(limits = c(-0.02, 0.5), breaks = seq(0, 0.5, 0.1),
                     sec.axis = sec_axis(~ ((1 + avg.tax + .)/(1+avg.tax))/(1+(avg.tax/(1+avg.tax))*elas), 
                                         name = "MVPF", breaks = c(1, seq(1.2, 1.5, 0.1),mvpf.at.inf, mvpf.at.4), 
                                         labels = c("1", "1.2", "", "1.4", "", 
                                                    round(mvpf.at.inf, digits = 3), 
                                                    round(mvpf.at.4, digits = 3))
                     )) +
  theme(legend.position = "bottom",
        # text = element_text(family = "Garamond"), # have to comment out for it produces a weird error
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("Figures_preferred_v4/F5_average_theta_sigma1_K2_division.png",
       height = 120, width = 200, units = "mm")  


### Triplets (\theta, \varepsilon_s, sigma) that are consistent with the data.

## To be modified

# Plot
gg <- ggplot(av.theta[K==2 & sigma %in% c(0.25, 0.3, 0.5, 0.75, 1)], 
             mapping = aes(x = x, y = theta, color = factor(sigma))) +
  geom_line() +
  geom_point() +
  theme_bw(base_size = fontsize) +
  labs(x = TeX('$\\frac{\\epsilon_S}{\\epsilon_S + \\epsilon_D}$'), 
       y = TeX("Av. $\\theta$"), 
       color = TeX("$\\sigma$  ")) +
  scale_color_brewer(palette="RdYlBu", direction = -1) +
  # geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=Inf), color = "gray", fill = "gray3", alpha = 0.02) +
  # geom_rect(aes(xmin=0, xmax=1/(1+ed), ymin=0.0666, ymax=1), color = "brown3", fill = "brown3", alpha = 0.015) +
  # geom_rect(aes(xmin=1/(1+ed), xmax = 1, ymin = 0, ymax = 0.0666), color = "green", fill = "green", alpha = 0.015) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 4/(4+ed), linetype = "dashed") + 
  theme(legend.position = "bottom",
        legend.margin = unit(0, "mm"),
        # text = element_text(family = "Garamond"), # have to comment out for it produces a weird error
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
ggsave("Figures_preferred_v4/SF5_average_theta_sigmaall_K2_division.png",
       height = 120, width = 200, units = "mm")

rm(gg, av.theta)





########################## APPENDIX FIGURE: Point identification under imperfect salience
##### Figure #: Point identification under imperfect salience ------

## Region by Module By Time FEs
data <- fread("Data/Demand_theta_sat_initial_price_semester_salience.csv")

## Add options to plot
min.price <- -0.15
max.price <- 0.15

p <- seq(min.price, max.price, 0.005)

## Estimations across p
data.plot <- data.table(NULL)
for (sig in c(0.25, 0.5, 0.75, 1)) {
  beta_hat <- data[controls == "region_by_module_by_time" & sigma == sig & n.groups == 2, ][["beta_hat"]]
  
  elasticity <- rep(0, length(p))
  # Calculate Demand, Elas and s.e.s
  for (i in 1:2) {
    
    elasticity <- elasticity + i*beta_hat[i+1]*p^(i-1)
  }
  response <- elasticity*sig
  
  # Save
  data.plot <- rbind(data.plot, data.table(p, elasticity, response, sigma = sig))
}

# Elasticity
ggplot(data.plot, aes(x = p, y = elasticity, color = factor(sigma))) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(limits = c(-4, 2), breaks = seq(-4, 2, 0.5), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Price", y = "Estimated Price Elasticity", color = TeX("$\\sigma$")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size))))
ggsave("Figures_preferred_v4/F5a_Elasticity_bysigma_K2_region.png",
       height = 3, width = 6.5)

# Response
ggplot(data.plot, aes(x = p, y = response, color = factor(sigma))) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(limits = c(-1.5, 1), breaks = seq(-1.5, 1, 0.25), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Price", y = "Estimated Tax Response", color = TeX("$\\sigma$")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size))))
ggsave("Figures_preferred_v4/F5a_Response_bysigma_K2_region.png",
       height = 3, width = 6.5)





## Division by Module By Time FEs
data <- fread("Data/Demand_theta_sat_initial_price_semester_salience.csv")

## Add options to plot
min.price <- -0.15
max.price <- 0.15

p <- seq(min.price, max.price, 0.005)

## Estimations across p
data.plot <- data.table(NULL)
for (sig in c(0.25, 0.5, 0.75, 1)) {
  beta_hat <- data[controls == "division_by_module_by_time" & sigma == sig & n.groups == 2, ][["beta_hat"]]
  
  elasticity <- rep(0, length(p))
  # Calculate Demand, Elas and s.e.s
  for (i in 1:2) {
    
    elasticity <- elasticity + i*beta_hat[i+1]*p^(i-1)
  }
  response <- elasticity*sig
  
  # Save
  data.plot <- rbind(data.plot, data.table(p, elasticity, response, sigma = sig))
}

# Elasticity
ggplot(data.plot, aes(x = p, y = elasticity, color = factor(sigma))) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(limits = c(-4, 2), breaks = seq(-4, 2, 0.5), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Price", y = "Estimated Price Elasticity", color = TeX("$\\sigma$")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size))))
ggsave("Figures_preferred_v4/F5a_Elasticity_bysigma_K2_division.png",
       height = 3, width = 6.5)

# Response
ggplot(data.plot, aes(x = p, y = response, color = factor(sigma))) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red", linetype = "55", alpha = .8) +
  theme_bw(base_size = fontsize) +
  scale_x_continuous(limits = c(min.price, max.price), breaks = seq(min.price, max.price, 0.05), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(limits = c(-1.5, 1), breaks = seq(-1.5, 1, 0.25), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Price", y = "Estimated Tax Response", color = TeX("$\\sigma$")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.x = element_text(size = 8),   # Modify X axis numbers (tick labels) font size
        axis.text.y = element_text(size = 8))    # Modify Y axis numbers (tick labels) font size))))
ggsave("Figures_preferred_v4/F5a_Response_bysigma_K2_division.png",
       height = 3, width = 6.5)




################ FIGURE 7: Heterogeneity in MVPF across States - show individual estimates
# Prepare tax and prices data to use for plots
data.st <- fread("./Data/extraction_state_binned_tax.csv")
data.st <- data.st[, .(tau = weighted.mean(tau, eta_m),
                       p_m = weighted.mean(p_m, eta_m)),
                   by = (fips_state)]

# merge in state initials
data.st <- merge(data.st, fread("./Data/fips_states.csv"), by = c("fips_state"))


# Create a combined variable to ensure unique ordering
data.st <- data.st[order(tau, p_m)]  # Sort by tau, then p_m
data.st[, tau_state := paste(tau, state, sep = "_")]  # Create unique combination of tau and state

# Order staes
# By price
data.st[, state.ord := factor(p_m,
                              levels = c(data.st$p_m[order(data.st$p_m)]),
                              labels = c(data.st$state[order(data.st$p_m)]), 
                              ordered = T)]

# By tax
#data.st[, state.ord.t := factor(tau, 
#                                levels = c(data.st$tau[order(data.st$tau)]),
#                                labels = c(data.st$state[order(data.st$tau)]), 
#                                ordered = T)]
data.st[, state.ord.t := factor(tau_state, 
                                 levels = unique(data.st$tau_state),  # Unique combinations
                                 labels = data.st$state,  # Corresponding state names
                                 ordered = TRUE)]



### Marginal changes
# Prepare MVPF results to plot them
data.all <- fread("./Data/state_welfare_extrapolation_marginal_region.csv")
#data.all <- data.all[K==8 & sigma==1 & theta == 0 & L == 1,] ## Check that we have all estimates
#data.all <- data.all[K==2 & sigma==1 & theta == 0 & L == 1,]


# Drop duplicates that we may have left
data.all <- data.all[!duplicated(data.all[, c('est', 'L', 'K', 'sigma', 'theta', 'state')]),]
# dcast to have min and max
data.all <- dcast(data.all, state + theta + sigma + K + L ~ est, value.var = 'value')


# Merge state attributes
setnames(data.all, "state", "fips_state")
data.all <- merge(data.all, data.st, by = c("fips_state"))



# Order states
# By price
data.all[, state.ord := factor(p_m, 
                               levels = c(data.st$p_m[order(data.st$p_m)]),
                               labels = c(data.st$state[order(data.st$p_m)]), 
                               ordered = T)]
# By tax
#data.all[, state.ord.t := factor(tau, 
#                                 levels = c(data.st$tau[order(data.st$tau)]),
#                                 labels = c(data.st$state[order(data.st$tau)]), 
#                                 ordered = T)]

# Create the ordered factor in data.all
data.all[, state.ord.t := factor(tau_state, 
                                 levels = unique(data.st$tau_state),  # Unique combinations
                                 labels = data.st$state,  # Corresponding state names
                                 ordered = TRUE)]


### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K %in% c(2,8) & L== 1 & sigma==1 & theta == 0],
                 aes(ymin = LB, ymax = UB, color = factor(K)), 
                 position = position_dodge(width = 0.6), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$K^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.y = element_text(size = 8),    # Modify Y axis numbers (tick labels) font size)))
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("Figures_preferred_v4/F7_mvpf_state_marg_sigma1_theta0_byp.png",
       height = 2.7, width = 6.5)  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K %in% c(2,8) & L == 1 & sigma==1 & theta == 0],
                 aes(ymin = LB, ymax = UB, color = factor(K)), 
                 position = position_dodge(width = 0.6), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States"), color = TeX("$K^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.y = element_text(size = 8),    # Modify Y axis numbers (tick labels) font size)))
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("Figures_preferred_v4/F7_mvpf_state_marg_sigma1_theta0_byt.png",
       height = 2.7, width = 6.5)  



### Non-marginal changes:  No tax
# Prepare MVPF results to plot them
data.all <- fread("./Data/state_welfare_extrapolation_nonmarginal_region.csv")
#data.all <- data.all[K==8 & sigma==1 & theta == 0 & L == 1 & sc == "No Tax",] ## Check that we have all estimates
#data.all <- data.all[K==2 & sigma==1 & theta == 0 & L == 1 & sc == "No Tax",] ## Check that we have all estimates
#data.all <- data.all[K==8 & sigma==1 & theta == 0 & L == 1 & sc == "plus 5 Tax",] ## Check that we have all estimates
#data.all <- data.all[K==2 & sigma==1 & theta == 0 & L == 1 & sc == "plus 5 Tax",] ## Check that we have all estimates
#data.all <- data.all[order(state)]

# Drop duplicates that we may have left
data.all <- data.all[!duplicated(data.all[, c('est', 'L', 'K', 'sigma', 'theta', 'state', 'sc')]),]
# dcast to have min and max
data.all <- dcast(data.all, state + theta + sigma + K + L + sc ~ est, value.var = 'value')


# Merge state attributes
setnames(data.all, "state", "fips_state")
data.all <- merge(data.all, data.st, by = c("fips_state"))



# Order states
# By price
data.all[, state.ord := factor(p_m, 
                               levels = c(data.st$p_m[order(data.st$p_m)]),
                               labels = c(data.st$state[order(data.st$p_m)]), 
                               ordered = T)]
# By tax
#data.all[, state.ord.t := factor(tau, 
#                                 levels = c(data.st$tau[order(data.st$tau)]),
#                                 labels = c(data.st$state[order(data.st$tau)]), 
#                                 ordered = T)]
data.all[, state.ord.t := factor(tau_state, 
                                 levels = unique(data.st$tau_state),  # Unique combinations
                                 labels = data.st$state,  # Corresponding state names
                                 ordered = TRUE)]


#### No tax

### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K %in% c(2,8) & L == 1 & sigma==1 & theta == 0 & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(K)), 
                 position = position_dodge(width = 0.6), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$K^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.y = element_text(size = 8),    # Modify Y axis numbers (tick labels) font size)))
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("Figures_preferred_v4/F7_mvpf_state_nonmarg0_sigma1_theta0_byp.png",
       height = 2.7, width = 6.5)  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K %in% c(2,8) & L == 1 & sigma==1 & theta == 0  & sc == "No Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(K)), 
                 position = position_dodge(width = 0.6), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States"), color = TeX("$K^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.y = element_text(size = 8),    # Modify Y axis numbers (tick labels) font size)))
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("Figures_preferred_v4/F7_mvpf_state_nonmarg0_sigma1_theta0_byt.png",
       height = 2.7, width = 6.5)  



### Non-marginal change: Plus 5 tax
#### Plus 5 tax

### sigma = 1, theta = 0 case

# Plot, ordering by price 
ggplot(data = NULL, aes(x = state.ord)) +
  geom_linerange(data = data.all[K %in% c(2,8) & L == 1 & sigma==1 & theta == 0 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(K)), 
                 position = position_dodge(width = 0.6), size = 1.5) +
  geom_point(data = data.st, aes(y = 0.7*(1.5 + p_m))) +
  geom_text(data = data.st, aes(y = 0.7*(1.5 + p_m), label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(./0.7-1.5), name="Average Price")
  ) +
  labs(x = TeX("States"), color = TeX("$K^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.y = element_text(size = 8),    # Modify Y axis numbers (tick labels) font size)))
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("Figures_preferred_v4/F7_mvpf_state_nonmarg5_sigma1_theta0_byp.png",
       height = 2.7, width = 6.5)  


# Plot, ordering by tax
ggplot(data = NULL, aes(x = state.ord.t)) +
  geom_linerange(data = data.all[K %in% c(2,8) & L == 1 & sigma==1 & theta == 0 & sc == "plus 5 Tax"],
                 aes(ymin = LB, ymax = UB, color = factor(K)), 
                 position = position_dodge(width = 0.6), size = 1.5) +
  geom_point(data = data.st, aes(y = 1 + tau)) +
  geom_text(data = data.st, aes(y = 1+tau, label = state), vjust=-1, size = 2) +
  theme_bw(base_size = fontsize) +
  scale_y_continuous(
    # Features of the first axis
    name = "MVPF",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~(.-1)*100, name="Average Tax rate (%)")
  ) +
  labs(x = TeX("States"), color = TeX("$K^d$") ) +
  coord_cartesian(ylim = c(1,1.15)) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),   # Modify legend text font size
        legend.title = element_text(size = 8), # Modify legend title font size
        #text = element_text(family = "Garamond"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5),
        axis.title.x = element_text(size = 9),  # Modify X axis label font size
        axis.title.y = element_text(size = 9),   # Modify Y axis label font size
        axis.text.y = element_text(size = 8),    # Modify Y axis numbers (tick labels) font size)))
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("Figures_preferred_v4/F7_mvpf_state_nonmarg5_sigma1_theta0_byt.png",
       height = 2.7, width = 6.5)  





################# APPENDIX FIGURE: Summarize variation in bounds around MVPF across states
## A. Open interest estimates
marginal <- fread("./Data/state_welfare_extrapolation_marginal_region.csv")
nonmarginal <- fread("./Data/state_welfare_extrapolation_nonmarginal_region.csv")

## B. Clean individually all estimates
# B1 Maginal
# Drop duplicates that we may have left
marginal <- marginal[!duplicated(marginal[, c('est', 'L', 'K', 'sigma', 'theta', 'state')]),]
marginal[, sc:= "marginal"]
marginal <- marginal[, -c("it", "s", "attempt")]
# B2 
# Drop duplicates that we may have left
nonmarginal <- nonmarginal[!duplicated(nonmarginal[, c('est', 'L', 'K', 'sigma', 'theta', 'state', 'sc')]),]
nonmarginal <- nonmarginal[, -c("it", "s", "attempt", "ConsChck")]

## C. Put together all estimates
estimates <- rbind(marginal, nonmarginal)
rm(marginal, nonmarginal)

## D. Compute p10, p90 and median in each scenario. Only K=8, L=1
summary <- estimates[K == 8 & L == 1, .(p10 = quantile(value, 0.1),
                                        median = median(value),
                                        mean = mean(value),
                                        p90 = quantile(value, 0.9)), by = .(sigma, theta, sc, est)]


# Melt to plot together
## Label Scenarii
summary[theta == 0 & sigma ==1, scenario := 1]
summary[(theta > 0.05 & theta < 0.06) & sigma ==1, scenario := 2]
summary[theta == 0 & sigma ==0.5, scenario := 3]

summary <- summary[!is.na(scenario)][,-c("sigma", "theta")]

##### Print some actual numbers to report in the paper
#temp.summary <- estimates[K == 8 & L == 1, .(min = min(value), p10 = quantile(value, 0.1),
#                                        median = median(value),
#                                        mean = mean(value),
#                                        p90 = quantile(value, 0.9), max = max(value)), by = .(sigma, theta, sc, est)]

#temp.summary[theta == 0 & sigma ==1, scenario := 1]
#temp.summary[(theta > 0.05 & theta < 0.06) & sigma ==1, scenario := 2]
#temp.summary[theta == 0 & sigma ==0.5, scenario := 3]

#temp.summary <- temp.summary[!is.na(scenario)][,-c("sigma", "theta")]
#temp.summary <- temp.summary[scenario == 1,]


# Melt and clean
summary <- melt(summary, id.vars = c("scenario", "est", "sc"), variable.name = "estimate")
summary[, estimate := as.integer(factor(estimate, levels = c("p10", "median", "p90", "mean")))]
summary[, est := factor(est, levels = c("LB", "UB"))]
summary[, sc := factor(sc, levels = c("No Tax", "marginal", "plus 5 Tax"), labels = c("Statu quo", "Marginal Change", "5pp Increase"))]


summary[, estimate := ifelse(estimate == 4, 5, estimate)]

# Plot
for (s in unique(summary$scenario)) {
  
  ggplot(summary[scenario == s], aes(estimate, value, fill = est)) +
    facet_grid(. ~ sc) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_vline(xintercept = 4, color = "red") +
    labs(y = NULL, x = NULL, fill = NULL) +
    coord_cartesian(ylim = c(1,1.125)) +
    scale_x_continuous(breaks = c(1:5), 
                       labels=c("p10", "median", "p90", "", "mean")) +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom",
          legend.margin = unit(0, "mm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(colour = "black", linetype = "dotted", size = 0.5))
  ggsave(paste0("Figures_preferred_v4/Heterogeneity_welfare_K8L1_", s,".png"), height = 4, width = 9)
  
  
}




##################################### TABLE 11 - ROBUSTNESS - IMPERFECT COMPETITION: Bounds around MVPF and AVPF of three reforms -- L = 1 
## Welfare Nationwide - Division FE
#data <- fread("./average_nationwide_extrapolation_region.csv")
#data.3 <- fread("./average_nationwide_extrapolation_region_smallv2.csv")
data.region <- fread("Data/average_nationwide_extrapolation_region_small.csv")

#data.division <- fread("Data/average_nationwide_extrapolation_division.csv")
#data.division2 <- fread("Data/average_nationwide_extrapolation_division_small.csv")
#data.division3 <- fread("Data/average_nationwide_extrapolation_division_smallv2.csv")
#data.division4 <- fread("Data/average_nationwide_extrapolation_division_smallv3.csv")
#data.division5 <- fread("Data/average_nationwide_extrapolation_division_smallv5.csv")

#data.division <- rbind(data.division, data.division2, data.division3, data.division4, data.division5)

setDT(data.region)
#setDT(data.division)

## Drop duplicates that we may have left
#data.division <- data.division[!duplicated(data.division[, c('sc', 'L', 'K', 'sigma', 'theta', 'est')]),]


# Define the table header
header1 <- c("%\\begin{table}[htb]",
             "\\centering",
             "%\\resizebox{0.7\\textwidth}{!}{",
             "\\begin{tabularx}{\\textwidth}{lCCCCC}",
             "\\hline\\noalign{\\smallskip}",
             "\\multicolumn{6}{c}{\\footnotesize{$L^{d} = 1$}} \\\\",
             "\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel A: Marginal change in tax}}} \\\\",
             " & \\multicolumn{2}{c}{\\footnotesize{Baseline ($\\theta = 0$})} & & \\multicolumn{2}{c}{\\footnotesize{Imperfect Competition ($\\theta = 0.002$)}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

# Fill in the body
data.region2 <- data.region[sigma == 1 & L == 1 & (theta >= 0.002 & theta <= 0.003),]
data.region1 <- data.region[sigma == 1 & L == 1 & (theta == 0),]
#data.division <- data.division[sigma == 1 & L == 1 & (theta >= 0.02 & theta <= 0.03),]

row1 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "Original" & K == 1,]$value, digits = 3), "} &"), " &", paste0("\\multicolumn{2}{c}{", round(data.region2[sc == "Original" & K == 1,]$value, digits = 3), "}"), "\\\\", sep = "")
row2 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "Original" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 2 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "Original" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 2 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row3 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "Original" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 4 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "Original" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row4 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "Original" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 8 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "Original" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")


header2 <- c("\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel B: Non-marginal change - from no tax to current tax}}}  \\\\",
             " & \\multicolumn{2}{c}{\\footnotesize{Baseline ($\\theta = 0$})} & & \\multicolumn{2}{c}{\\footnotesize{Imperfect Competition ($\\theta = 0.002$)}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

row5 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "No Tax" & K == 1,]$value, digits = 3), "} &"), " &", paste0("\\multicolumn{2}{c}{", round(data.region2[sc == "No Tax" & K == 1,]$value, digits = 3), "}"), "\\\\", sep = "")
row6 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "No Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 2 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "No Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 2 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row7 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "No Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 4 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "No Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row8 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "No Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 8 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "No Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")

header3 <- c("\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel C: Non-marginal change - 5pp increase in tax}}}  \\\\",
             " & \\multicolumn{2}{c}{\\footnotesize{Baseline ($\\theta = 0$})} & & \\multicolumn{2}{c}{\\footnotesize{Imperfect Competition ($\\theta = 0.002$)}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

row9 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "plus 5 Tax" & K == 1,]$value, digits = 3), "} &"), " &", paste0("\\multicolumn{2}{c}{", round(data.region2[sc == "plus 5 Tax" & K == 1,]$value, digits = 3), "}"), "\\\\", sep = "")
row10 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 2 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "plus 5 Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 2 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row11 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 4 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "plus 5 Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row12 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 8 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "plus 5 Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")


# Define the table footer
footer <- c("\\hline \\\\",
            "\\end{tabularx}%}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Combine the header, body, and footer into a single string
table_str <- c(header1, row1, row2, row3, row4, header2, row5, row6, row7, row8, header3, row9, row10, row11, row12, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/Table_welfare_av_rob_competition_all.tex")



##################################### TABLE 12: ROBUSTNESS - SALIENCE: Bounds around MVPF and AVPF of three reforms -- L = 1 
## Welfare Nationwide - Division FE
data.region <- fread("Data/average_nationwide_extrapolation_region_small.csv")

setDT(data.region)

# Define the table header
header1 <- c("%\\begin{table}[htb]",
             "\\centering",
             "%\\resizebox{0.7\\textwidth}{!}{",
             "\\begin{tabularx}{\\textwidth}{lCCCCC}",
             "\\hline\\noalign{\\smallskip}",
             "\\multicolumn{6}{c}{\\footnotesize{$L^{d} = 1$}} \\\\",
             "\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel A: Marginal change in tax}}} \\\\",
             " & \\multicolumn{2}{c}{\\footnotesize{Baseline ($\\sigma = 1$})} & & \\multicolumn{2}{c}{\\footnotesize{Low Salience ($\\sigma = 0.25$)}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

# Fill in the body
data.region1 <- data.region[theta == 0 & L == 1 & sigma == 1,]
data.region2 <- data.region[theta == 0 & L == 1 & sigma == 0.25,]


#row1 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "Original" & K == 1,]$value, digits = 3), "} &"), " &", paste0("\\multicolumn{2}{c}{", round(data.region2[sc == "Original" & K == 1,]$value, digits = 3), "}"), "\\\\", sep = "")
row2 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "Original" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 2 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "Original" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 2 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row3 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "Original" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 4 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "Original" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row4 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "Original" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "Original" & K == 8 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "Original" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "Original" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")


header2 <- c("\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel B: Non-marginal change - from no tax to current tax}}}  \\\\",
             " & \\multicolumn{2}{c}{\\footnotesize{Baseline ($\\sigma = 1$})} & & \\multicolumn{2}{c}{\\footnotesize{Low Salience ($\\sigma = 0.25$)}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

#row5 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "No Tax" & K == 1,]$value, digits = 3), "} &"), " &", paste0("\\multicolumn{2}{c}{", round(data.region2[sc == "No Tax" & K == 1,]$value, digits = 3), "}"), "\\\\", sep = "")
row6 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "No Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 2 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "No Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 2 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row7 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "No Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 4 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "No Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row8 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "No Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "No Tax" & K == 8 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "No Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "No Tax" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")

header3 <- c("\\hline",
             "\\multicolumn{6}{c}{ \\footnotesize{\\textbf{Panel C: Non-marginal change - 5pp increase in tax}}}  \\\\",
             " & \\multicolumn{2}{c}{\\footnotesize{Baseline ($\\sigma = 1$})} & & \\multicolumn{2}{c}{\\footnotesize{Low Salience ($\\theta = 0.25$)}} \\\\",
             "\\cline{2-3} \\cline{5-6} \\\\",
             " & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} & & \\footnotesize{\\textbf{Lower-Bound}} & \\footnotesize{\\textbf{Upper-Bound}} \\\\",
             "\\hline")

#row9 <- paste0("$K^{d}=1$ &", paste0("\\multicolumn{2}{c}{", round(data.region1[sc == "plus 5 Tax" & K == 1,]$value, digits = 3), "} &"), " &", paste0("\\multicolumn{2}{c}{", round(data.region2[sc == "plus 5 Tax" & K == 1,]$value, digits = 3), "}"), "\\\\", sep = "")
row10 <- paste0("$K^{d}=2$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 2 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "plus 5 Tax" & K == 2 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 2 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row11 <- paste0("$K^{d}=4$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 4 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "plus 5 Tax" & K == 4 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 4 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")
row12 <- paste0("$K^{d}=8$ &", paste0(round(data.region1[sc == "plus 5 Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region1[sc == "plus 5 Tax" & K == 8 & est == "UB",]$value, digits = 3), " &"), " &", paste0(round(data.region2[sc == "plus 5 Tax" & K == 8 & est == "LB",]$value, digits = 3), " &"), paste0(round(data.region2[sc == "plus 5 Tax" & K == 8 & est == "UB",]$value, digits = 3)), "\\\\", sep = "")


# Define the table footer
footer <- c("\\hline \\\\",
            "\\end{tabularx}%}",
            "%\\caption{xx}",
            "%\\label{tab:main_DL}",
            "%\\end{table}")

# Combine the header, body, and footer into a single string
table_str <- c(header1, row2, row3, row4, header2, row6, row7, row8, header3, row10, row11, row12, footer)

# Write the table string to a file
writeLines(table_str, "Figures_preferred_v4/Table_welfare_av_rob_salience_all.tex")






