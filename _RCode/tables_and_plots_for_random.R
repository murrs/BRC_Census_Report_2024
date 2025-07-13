source("make_plot_data.R")
source("make_table_data.R")
source("additional_functions.R")
source("read_census_data.R")

library(data.table)
library(survey)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
library(weights)
library(Cairo)

virgin <- makePlotData(varName = "Was this your first time visiting Black Rock City?", 
                       varNameTable,
                       designs = list(design13, design14, design15, 
                                      design16, design17, design18, 
                                      design19, design22, design23),
                       years = c(2013:2019, 2022:2023), 
                       levels = list(c("no", "yes"), c("no", "yes"),
                                     c("no", "yes"), c("no", "yes"),
                                     c("no", "yes"), c("no", "yes"),
                                     c("no", "yes"), c(FALSE, TRUE),
                                     c(FALSE, TRUE)),
                       labels = c("Not virgin", "Virgin"),
                       labelOrder = c("Virgin", "Not virgin"))


virginPlot <- ggplot(virgin[virgin$labels == "Virgin",], aes(x = year, y = est)) +
  geom_line(linewidth = 1.5, color = "#EA008B", alpha = 0.7) +
  geom_point(size = 2.25, color = "#EA008B") +
  # geom_errorbar(aes(ymin = lower, ymax = upper), color = "black", width = 0.2,
  #               linewidth = 1.25) +
  theme_bw(13) +
  theme(panel.grid.minor = element_blank()) +
  # scale_color_brewer(type = "seq", palette = 11) +
  scale_x_continuous(breaks = c(2013:2019, 2022:2023), 
                     labels = c(2013:2019, 2022:2023)) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  labs(x = "", y = "Burning Man Participants (%)")

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\virgin", type = "SVG", units = "in", width = 8, height = 5)
virginPlot
dev.off()


virginTable <- makeTableData(virgin)
virginTable <- virginTable[1,] |>
  matrix(nrow = 1) |>
  as.table()
rownames(virginTable) = "Virgin"
colnames(virginTable) = c(2013:2019, 2022:2023)
fwrite(virginTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\virgin_table.csv")



agePlot <- ggplot(census23[, c("age", "weights")], aes(x = age, y = after_stat(density), weight = weights)) + 
  geom_histogram(fill = "#EA008B", color = "black") +
  theme_bw() +
  labs(x = "Age when arriving on playa in 2023", y = "Density")

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\age", type = "SVG", units = "in", width = 8, height = 5)
agePlot
dev.off()

medianAge <- c(svyquantile(~age, design23, 0.5)[[1]][1],
               svyquantile(~age, design22, 0.5)[[1]][1],
               svyquantile(~age, design19, 0.5)[[1]][1],
               svyquantile(~age, design18, 0.5)[[1]][1],
               svyquantile(~age, design17, 0.5)[[1]][1],
               svyquantile(~age, design16, 0.5)[[1]][1],
               svyquantile(~age, design15, 0.5)[[1]][1],
               svyquantile(~age, design14, 0.5)[[1]][1],
               32) |>
  rev() |>
  matrix(nrow = 1)
row.names(medianAge) <- "Median Age"
colnames(medianAge) <- as.character(c(2013:2019, 2022:2023))
fwrite(virginTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\median_age_table.csv")




gender23.male <- svyciprop(~grepl("(?<!([Ff]e))[Mm]ale", currentGender, perl = TRUE), 
                          design23)
gender23.male <- data.table(est = as.numeric(gender23.male), 
                           lower = attr(gender23.male, "ci")[1],
                           upper = attr(gender23.male, "ci")[2],
                           level = "male",
                           year = 2023,
                           labels = factor("Male", 
                                           levels = c("Male", "Female", 
                                                      "Other identity")))
gender23.female <- svyciprop(~grepl("[Ff]emale", currentGender, perl = TRUE), 
                            design23)
gender23.female <- data.table(est = as.numeric(gender23.female), 
                             lower = attr(gender23.female, "ci")[1],
                             upper = attr(gender23.female, "ci")[2],
                             level = "female",
                             year = 2023,
                             labels = factor("Female", 
                                             levels = c("Male", "Female", 
                                                        "Other identity")))
gender23.other <- svyciprop(~I(!grepl("[Mm]ale", currentGender, perl = TRUE)), 
                           design23)
gender23.other <- data.table(est = as.numeric(gender23.other), 
                            lower = attr(gender23.other, "ci")[1],
                            upper = attr(gender23.other, "ci")[2],
                            level = "male",
                            year = 2023,
                            labels = factor("Other identity", 
                                            levels = c("Male", "Female", 
                                                       "Other identity")))
gender23 <- rbind(gender23.male, gender23.female, gender23.other)

gender22.male <- svyciprop(~grepl("(?<!([Ff]e))[Mm]ale", currentGender, perl = TRUE), 
                           design22)
gender22.male <- data.table(est = as.numeric(gender22.male), 
                            lower = attr(gender22.male, "ci")[1],
                            upper = attr(gender22.male, "ci")[2],
                            level = "male",
                            year = 2022,
                            labels = factor("Male", 
                                            levels = c("Male", "Female", 
                                                       "Other identity")))
gender22.female <- svyciprop(~grepl("[Ff]emale", currentGender, perl = TRUE), 
                             design22)
gender22.female <- data.table(est = as.numeric(gender22.female), 
                              lower = attr(gender22.female, "ci")[1],
                              upper = attr(gender22.female, "ci")[2],
                              level = "female",
                              year = 2022,
                              labels = factor("Female", 
                                              levels = c("Male", "Female", 
                                                         "Other identity")))
gender22.other <- svyciprop(~I(!grepl("[Mm]ale", currentGender, perl = TRUE)), 
                            design22)
gender22.other <- data.table(est = as.numeric(gender22.other), 
                             lower = attr(gender22.other, "ci")[1],
                             upper = attr(gender22.other, "ci")[2],
                             level = "male",
                             year = 2022,
                             labels = factor("Other identity", 
                                             levels = c("Male", "Female", 
                                                        "Other identity")))
gender22 <- rbind(gender22.male, gender22.female, gender22.other)

gender <- makePlotData(varName = "What is your current gender?", varNameTable,
                       designs = list(design13, design14, design15, 
                                      design16, design17, design18, 
                                      design19),
                       years = c(2013:2019), 
                       levels = rep(list(c("female", "fluid", "male")), times = 7),
                       labels = c("Female", "Other identity", "Male"),
                       labelOrder = c("Male", "Female", "Other identity"))
gender <- gender[!is.na(gender$est),]
gender <- rbind(gender, gender22, gender23)

genderPlot <- ggplot(gender, aes(x = year, y = est, color = labels)) +
  geom_line(linewidth = 1.5, alpha = 0.7) +
  geom_point(size = 2.25) +
  theme_bw(13) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(2013:2019, 2022:2023), 
                     labels = c(2013:2019, 2022:2023)) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  scale_color_manual(values = c("#EA008B", "#554149", "#BDA5AD")) +
  labs(x = "", y = "Burning Man participants (%)", 
       color = "Current gender")

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\gender", 
      type = "SVG", units = "in", width = 8, height = 5)
genderPlot
dev.off()

genderTable <- makeTableData(gender)
fwrite(genderTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\gender_table.csv")



genderID <- makePlotData(varName = "What is your current gender?", varNameTable,
                         designs = list(design23),
                         years = c(2023), 
                         levels = list(c("Female", "Male", "Gender queer", 
                                         "Cisgender male", "Cisgender female",
                                         "Transgender female/Trans female",
                                         "Non-binary",
                                         "Transgender male/ Trans male",
                                         "Prefer to self-describe",
                                         "Two-spirit")),
                         labels = c("Female", "Male", "Gender queer", 
                                    "Cisgender male", "Cisgender female",
                                    "Transgender female", "Non-binary", 
                                    "Transgender male", "Other", "Two-spirit"),
                         labelOrder = c("Male", "Female", "Cisgender male",
                                        "Cisgender female", 
                                        "Transgender female", "Non-binary",
                                        "Gender queer", "Transgender male", 
                                        "Other", "Two-spirit"))

genderIDPlot <- ggplot(genderID, aes(x = labels, y = est)) +
  geom_bar(stat = "identity", fill = "#EA008B") +
  scale_y_continuous(labels = percent) +
  labs(x = "Gender identity", y = "Burning Man participants in 2023 (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\gender_ID", 
      type = "SVG", units = "in", width = 8, height = 5)
genderIDPlot
dev.off()

genderIDTable <- makeTableData(genderID)
fwrite(genderIDTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\gender_ID_table.csv")


pi.lt25.23 <- customPlotDat(c("No income", "Less than $7500", 
                              "$7,500-$14,999", "$15,000-$24,999"),
                            design = design23, "personalIncome",
                            2023, "Less than $25,000")
pi.25to50.23 <- customPlotDat(c("$25,000-$34,999", "$35,000-$49,999"),
                              design = design23, "personalIncome", 2023,
                              "$25,000-$49,999")
pi.50to100.23 <- customPlotDat(c("$50,000-$74,999", "$75,000-$99,999"),
                               design = design23, "personalIncome",
                               2023, "$50,000-$99,999")
pi.100to300.23 <- customPlotDat(c("$100,000-$149,999", "$150,000-$299,000"),
                                design = design23, "personalIncome", 2023,
                                "$100,000-$299,999")
pi.gt300.23 <- customPlotDat("$300,000 or more", design = design23,
                             "personalIncome", 2023, "$300,000 or more")
pi.23 <- rbind(pi.lt25.23, pi.25to50.23, pi.50to100.23, pi.100to300.23,
               pi.gt300.23)


pi.lt25.22 <- customPlotDat(c("No income", "Less than $7500", 
                              "$7,500-$14,999", "$15,000-$24,999"),
                            design = design22, "personalIncome",
                            2022, "Less than $25,000")
pi.25to50.22 <- customPlotDat(c("$25,000-$34,999", "$35,000-$49,999"),
                              design = design22, "personalIncome", 2022,
                              "$25,000-$49,999")
pi.50to100.22 <- customPlotDat(c("$50,000-$74,999", "$75,000-$99,999"),
                               design = design22, "personalIncome",
                               2022, "$50,000-$99,999")
pi.100to300.22 <- customPlotDat(c("$100,000-$149,999", "$150,000-$299,000"),
                                design = design22, "personalIncome", 2022,
                                "$100,000-$299,999")
pi.gt300.22 <- customPlotDat("$300,000 or more", design = design22,
                             "personalIncome", 2022, "$300,000 or more")
pi.22 <- rbind(pi.lt25.22, pi.25to50.22, pi.50to100.22, pi.100to300.22,
               pi.gt300.22)

pi.lt25.19 <- customPlotDat(c("None", "Less_than_7500US", 
                              "_7500_14999US", "_15000_24999US"),
                            design = design19, "incomep",
                            2019, "Less than $25,000")
pi.25to50.19 <- customPlotDat(c("_25000_34999US", "_35000_49999US"),
                              design = design19, "incomep", 2019,
                              "$25,000-$49,999")
pi.50to100.19 <- customPlotDat(c("_50000_74999US", "_75000_99999US"),
                               design = design19, "incomep",
                               2019, "$50,000-$99,999")
pi.100to300.19 <- customPlotDat(c("_100k_149999US", "_150k_299999US"),
                                design = design19, "incomep", 2019,
                                "$100,000-$299,999")
pi.gt300.19 <- customPlotDat("_300kUS_or_more", design = design19,
                             "incomep", 2019, "$300,000 or more")
pi.19 <- rbind(pi.lt25.19, pi.25to50.19, pi.50to100.19, pi.100to300.19,
               pi.gt300.19)

pi.lt25.18 <- customPlotDat(c("None", "Less_than_7500US", 
                              "_7500_14999US", "_15000_24999US"),
                            design = design18, "incomep",
                            2018, "Less than $25,000")
pi.25to50.18 <- customPlotDat(c("_25000_34999US", "_35000_49999US"),
                              design = design18, "incomep", 2018,
                              "$25,000-$49,999")
pi.50to100.18 <- customPlotDat(c("_50000_74999US", "_75000_99999US"),
                               design = design18, "incomep",
                               2018, "$50,000-$99,999")
pi.100to300.18 <- customPlotDat(c("_100k_149999US", "_150k_299999US"),
                                design = design18, "incomep", 2018,
                                "$100,000-$299,999")
pi.gt300.18 <- customPlotDat("_300kUS_or_more", design = design18,
                             "incomep", 2018, "$300,000 or more")
pi.18 <- rbind(pi.lt25.18, pi.25to50.18, pi.50to100.18, pi.100to300.18,
               pi.gt300.18)

pi.lt25.17 <- customPlotDat(c("None", "Less_than_7500US", 
                              "_7500_14999US", "_15000_24999US"),
                            design = design17, "incomep",
                            2017, "Less than $25,000")
pi.25to50.17 <- customPlotDat(c("_25000_34999US", "_35000_49999US"),
                              design = design17, "incomep", 2017,
                              "$25,000-$49,999")
pi.50to100.17 <- customPlotDat(c("_50000_74999US", "_75000_99999US"),
                               design = design17, "incomep",
                               2017, "$50,000-$99,999")
pi.100to300.17 <- customPlotDat(c("_100k_149999US", "_150k_299999US"),
                                design = design17, "incomep", 2017,
                                "$100,000-$299,999")
pi.gt300.17 <- customPlotDat("_300kUS_or_more", design = design17,
                             "incomep", 2017, "$300,000 or more")
pi.17 <- rbind(pi.lt25.17, pi.25to50.17, pi.50to100.17, pi.100to300.17,
               pi.gt300.17)

pi.lt25.16 <- customPlotDat(c("None", "Less_than_7500US", 
                              "_7500_14999US", "_15000_24999US"),
                            design = design16, "incomep",
                            2016, "Less than $25,000")
pi.25to50.16 <- customPlotDat(c("_25000_34999US", "_35000_49999US"),
                              design = design16, "incomep", 2016,
                              "$25,000-$49,999")
pi.50to100.16 <- customPlotDat(c("_50000_74999US", "_75000_99999US"),
                               design = design16, "incomep",
                               2016, "$50,000-$99,999")
pi.100to300.16 <- customPlotDat(c("_100k_149999US", "_150k_299999US"),
                                design = design16, "incomep", 2016,
                                "$100,000-$299,999")
pi.gt300.16 <- customPlotDat("_300kUS_or_more", design = design16,
                             "incomep", 2016, "$300,000 or more")
pi.16 <- rbind(pi.lt25.16, pi.25to50.16, pi.50to100.16, pi.100to300.16,
               pi.gt300.16)

pi.lt25.15 <- customPlotDat(c("None", "Less_than_7500US", 
                              "_7500_14999US", "_15000_24999US"),
                            design = design15, "incomep",
                            2015, "Less than $25,000")
pi.25to50.15 <- customPlotDat(c("_25000_34999US", "_35000_49999US"),
                              design = design15, "incomep", 2015,
                              "$25,000-$49,999")
pi.50to100.15 <- customPlotDat(c("_50000_74999US", "_75000_99999US"),
                               design = design15, "incomep",
                               2015, "$50,000-$99,999")
pi.100to300.15 <- customPlotDat(c("_100k_149999US", "_150k_299999US"),
                                design = design15, "incomep", 2015,
                                "$100,000-$299,999")
pi.gt300.15 <- customPlotDat("_300kUS_or_more", design = design15,
                             "incomep", 2015, "$300,000 or more")
pi.15 <- rbind(pi.lt25.15, pi.25to50.15, pi.50to100.15, pi.100to300.15,
               pi.gt300.15)

pi.allYears <- rbind(pi.23, pi.22, pi.19, pi.18, pi.17, pi.16, pi.15)
pi.allYears$labels <- factor(pi.allYears$labels, 
                             levels = c("Less than $25,000",
                                        "$25,000-$49,999",
                                        "$50,000-$99,999",
                                        "$100,000-$299,999",
                                        "$300,000 or more"))

piPlot <- ggplot(pi.allYears, aes(x = year, y = est, color = labels)) +
  geom_line(linewidth = 1.5, alpha = 0.85) +
  geom_point(size = 2.25) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), color = "black", width = 0.2,
  #               linewidth = 1) +
  theme_bw(13) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(2013:2019, 2022), labels = c(2013:2019, 2022)) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  scale_color_manual(values = c("#EA008B", "#d460c7", "#bc8ce0", "#b0aedc", 
                                "#c3c4c6")) +
  labs(x = "", y = "Burning Man participants (%)", color = "Personal income (USD)")

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\personal_income", 
      type = "SVG", units = "in", width = 8, height = 5)
piPlot
dev.off()

personalIncome <- makePlotData(varName = "What was your personal income in 2022 before taxes? Give your best estimate. Please do not include your spouse's income as your own.", 
                               varNameTable,
                               designs = list(design15, design16, design17, design18,
                                              design19, design22, design23),
                               years = c(2015:2019, 2022:2023), 
                               levels = c(rep(list(c("_100k_149999US", 
                                                     "_15000_24999US", 
                                                     "_150k_299999US", 
                                                     "_25000_34999US",  
                                                     "_300kUS_or_more", 
                                                     "_35000_49999US", 
                                                     "_50000_74999US", 
                                                     "_7500_14999US", 
                                                     "_75000_99999US", 
                                                     "Less_than_7500US", 
                                                     "None")), times  = 5),
                                          rep(list(c("$100,000-$149,999", "$15,000-$24,999", 
                                                 "$150,000-$299,000", "$25,000-$34,999",
                                                 "$300,000 or more", "$35,000-$49,999",
                                                 "$50,000-$74,999", "$7,500-$14,999",
                                                 "$75,000-$99,999", "Less than $7500",
                                                 "No income")), times = 2)),
                               labels = c("$100,000-$149,999", "$15,000-$24,999", 
                                          "$150,000-$299,000", "$25,000-$34,999",
                                          "$300,000 or more", "$35,000-$49,999",
                                          "$50,000-$74,999", "$7,500-$14,999",
                                          "$75,000-$99,999", "Less than $7,500",
                                          "No income"),
                               labelOrder = c("No income", "Less than $7,500", 
                                              "$7,500-$14,999", "$15,000-$24,999", 
                                              "$25,000-$34,999", "$35,000-$49,999",
                                              "$50,000-$74,999", "$75,000-$99,999",
                                              "$100,000-$149,999", "$150,000-$299,000",
                                              "$300,000 or more"))



piTable <- makeTableData(personalIncome)
fwrite(piTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\personal_income_table.csv")


ethnoracial23 <- makePlotData("Which category best describes your race?",
                              varNameTable = varNameTable,
                              designs = list(design23),
                              years = 2023,
                              levels = TRUE,
                              labels = c("Asian", "Native American", 
                                         "Hispanic/Latino", 
                                         "White (non-hispanic)",
                                         "Middle Eastern or North African",
                                         "Black (non-Hispanic)", "Other"),
                              labelOrder = c("White (non-hispanic)",
                                             "Hispanic/Latino",
                                             "Asian",
                                             "Middle Eastern or North African",
                                             "Black (non-Hispanic)",
                                             "Native American", "Other"))
ethnoracial22 <- makePlotData("Which category best describes your race?",
                              varNameTable = varNameTable,
                              designs = list(design22),
                              years = 2022,
                              levels = TRUE,
                              labels = c("Asian", "Native American", 
                                         "Hispanic/Latino", 
                                         "White (non-hispanic)",
                                         "Middle Eastern or North African",
                                         "Black (non-Hispanic)", "Other"),
                              labelOrder = c("White (non-hispanic)",
                                             "Hispanic/Latino",
                                             "Asian",
                                             "Middle Eastern or North African",
                                             "Black (non-Hispanic)",
                                             "Native American", "Other"))
ethnoracial17to19 <- makePlotData("ethnoracial 2013 through 2019",
                                  varNameTable = varNameTable,
                                  designs = list(design17, design18, design19),
                                  years = 2017:2019,
                                  levels = rep(list(c("asian", "black", "hispanic", 
                                                      "middleeastern", "native", 
                                                      "other_or_mix", "white")),
                                               times = 3),
                                  labels = c("Asian", "Black (non-Hispanic)", 
                                             "Hispanic/Latino", 
                                             "Middle Eastern or North African",
                                             "Native American", 
                                             "Other", "White (non-hispanic)"),
                                  labelOrder = c("White (non-hispanic)",
                                                 "Hispanic/Latino",
                                                 "Asian",
                                                 "Middle Eastern or North African",
                                                 "Black (non-Hispanic)",
                                                 "Native American", "Other"))

ethnoracial13to16 <- makePlotData("ethnoracial 2013 through 2019",
                                  varNameTable = varNameTable,
                                  designs = list(design13, design14, design15,
                                                 design16),
                                  years = 2013:2016,
                                  levels = rep(list(c("asian", "black", "hispanic", 
                                                      "native", "other_or_mix", 
                                                      "white")), times = 4),
                                  labels = c("Asian", "Black (non-Hispanic)", 
                                             "Hispanic/Latino", 
                                             "Native American", 
                                             "Other", "White (non-hispanic)"),
                                  labelOrder = c("White (non-hispanic)",
                                                 "Hispanic/Latino",
                                                 "Asian",
                                                 "Black (non-Hispanic)",
                                                 "Native American", "Other"))
ethnoracial13to16 <- rbind(ethnoracial13to16,
                           data.frame(est = NA, lower = NA, upper = NA,
                                      level = "middleeastern",
                                      year = 2013:2016,
                                      labels = factor("Middle Eastern or North African",
                                                      levels = c("White (non-hispanic)",
                                                                 "Hispanic/Latino",
                                                                 "Asian",
                                                                 "Middle Eastern or North African",
                                                                 "Black (non-Hispanic)",
                                                                 "Native American", "Other"))))

ethnoracialPlot <- ggplot(ethnoracial23, aes(x = labels, y = est)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#EA008B") +
  scale_y_continuous(labels = percent) +
  labs(x = "Ethnoracial background", y = "Burning Man participants in 2022 (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\ethnoracial", 
      type = "SVG", units = "in", width = 8, height = 5)
ethnoracialPlot
dev.off()

ethnoracialTable <- makeTableData(rbind(ethnoracial22, ethnoracial17to19, ethnoracial13to16))
fwrite(ethnoracialTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\ethnoracial_table.csv")


personOfColor <- makePlotData("Do you consider yourself to be a person of color?",
                              varNameTable = varNameTable,
                              designs = list(design13, design14, design15,
                                             design16, design17, design18,
                                             design19, design22, design23),
                              years = c(2013:2019, 2022:2023),
                              levels = c("No", "Sometimes", "Yes"),
                              labels = c("No", "Sometimes", "Yes"),
                              labelOrder = 1:3)

pocPlot <- ggplot(personOfColor, aes(x = year, y = est, fill = labels)) +
  geom_area(alpha = 0.65) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(2013:2019, 2022:2023), labels = c(2013:2019, 2022:2023)) +
  scale_fill_manual(values = c("#EA008B", "#bc8ce0", "#c3c4c6")[c(3,2,1)]) +
  labs(x = "", y = "Burning Man participants (%)", fill = "Do you consider \n yourself a \n person of color?") +
  theme_bw()

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\person_of_color_ID", 
      type = "SVG", units = "in", width = 8, height = 5)
pocPlot
dev.off()

pocTable <- makeTableData(personOfColor)
fwrite(pocTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\person_of_color_ID_table.csv")



ticketSource <- makePlotData("Where did you get your ticket for the event this year?",
                             varNameTable = varNameTable,
                             designs = list(design23, design22),
                             years = c(2022:2023),
                             levels = rep(list(c("2021 Invitation to the Future Ticket",
                                             "Burner Express Bus Plus",
                                             "FOMO sale",
                                             "Gifted ticket",
                                             "Main sale or OMG sale",
                                             "No ticket",
                                             "Other",
                                             "Someone I know",
                                             "Staff or volunteer credential",
                                             "STEP",
                                             "Stewards sales",
                                             "Stranger",
                                             "Third party reseller",
                                             "Ticket Aid/Low Income Ticket Program")), times = 2),
                             labels = c("2021 Invitation to the \n Future Ticket",
                                        "Burner Express Bus Plus",
                                        "FOMO sale",
                                        "Gifted ticket",
                                        "Main sale or OMG sale",
                                        "No ticket",
                                        "Other",
                                        "Someone I know",
                                        "Staff or volunteer \n credential",
                                        "STEP",
                                        "Stewards sales",
                                        "Stranger",
                                        "Third party reseller",
                                        "Ticket Aid/Low Income \n Ticket Program"),
                             labelOrder = c(7,9,1,2,6,4,3,13,8,11,12,14,10,5))

ticketSourcePlot <- ggplot(ticketSource[ticketSource$year == 2023,], aes(x = labels, y = est)) +
  geom_bar(stat = "identity", fill = "#EA008B") +
  scale_y_continuous(labels = percent) +
  labs(x = "Ticket source", y = "Burning Man participants in 2023 (%)") +
  theme_bw() +
  coord_flip()

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\ticket_source", 
      type = "SVG", units = "in", width = 8, height = 5)
ticketSourcePlot
dev.off()

ticketSourceTable <- makeTableData(ticketSource)[14:1,]
fwrite(ticketSourceTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\ticket_source_table.csv")


ticketCost <- makePlotData("Did you pay face value for your ticket?",
                           varNameTable = varNameTable,
                           designs = list(design14, design15, design16,
                                          design17, design18, design19,
                                          design22, design23),
                           years = c(2014:2019, 2022:2023),
                           levels = c(rep(list(c("Facevalue", "Gift",
                                                 "IDK", "Less_than_facevalue",
                                                 "More_than_facevalue",
                                                 "Other")), times = 6),
                                      rep(list(c("Face value", "Gift/did not pay",
                                             "Don\'t know", 
                                             "Less than face value",
                                             "More than face value",
                                             "Other")), times = 2)),
                           labels = c("Face value", "Gift or did not pay",
                                      "I don\'t know", 
                                      "Less than face value",
                                      "More than face value",
                                      "Other"),
                           labelOrder = c(5, 1, 4, 2, 3, 6))

ticketCostPlot <- ggplot(ticketCost, aes(x = year, y = est, color = labels)) +
  geom_line(linewidth = 1.5, alpha = 0.7) +
  geom_point(size = 2.25) +
  theme_bw(13) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(2014:2019, 2022:2023), labels = c(2014:2019, 2022:2023)) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  scale_color_manual(values = c("#EA008B", "#BDA5AD", "#FF7457", "#A07200", 
                                "#FFCF50", "#554149")) +
  labs(x = "", y = "Burning Man Partcipants (%)", color = "Amount paid")

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\ticket_cost", 
      type = "SVG", units = "in", width = 8, height = 5)
ticketCostPlot
dev.off()

ticketCostTable <- makeTableData(ticketCost)
fwrite(ticketCostTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\ticket_cost_table.csv")

ticketCostChange <- data.table(level = ticketCost$labels[1:6],
                               est2022 = ticketCost$est[ticketCost$year == 2022],
                               est2023 = ticketCost$est[ticketCost$year == 2023])
ticketCostChange$estDif <- ticketCostChange$est2023 - ticketCostChange$est2022
ticketCostChange$positive <- ifelse(ticketCostChange$estDif > 0, ticketCostChange$estDif, 0)
ticketCostChange$negative <- ifelse(ticketCostChange$estDif < 0, ticketCostChange$estDif, 0)

ticketCostChange2 <- data.table(level = ticketCostChange$level,
                                change = c(ticketCostChange$positive,
                                           ticketCostChange$negative),
                                direction = rep(c("Positive", "Negative"),
                                                each = nrow(ticketCostChange)))

costChangePlot <- ggplot(ticketCostChange2, aes(x = level, y = change, fill = direction)) + 
  geom_bar(stat="identity", position="identity", show.legend = FALSE) +
  theme_bw(13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(labels = percent, limits = c(-0.025, 0.025)) +
  scale_fill_manual(values = c("#EA008B", "#BDA5AD")[2:1]) +
  labs(x = "Ticket cost", y = "Change in response from 2022 to 2023")

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\ticket_cost_change", 
      type = "SVG", units = "in", width = 8, height = 5)
costChangePlot
dev.off()


generate <- makePlotData("How did you generate electricity in your camp?",
                         varNameTable = varNameTable,
                         designs = list(design13, design14, design15,
                                        design16, design17, design18,
                                        design19, design22, design23),
                         years = c(2013:2019, 2022:2023),
                         levels = c(rep(list(c("yes")), times = 7),
                                    rep(list(c(TRUE)), times = 2)),
                         labels = c("I didn\'t use any power",
                                    "I used batteries",
                                    "I used solar power",
                                    "I used wind power",
                                    "I used my vehicle\'s generator",
                                    "I used my camp\'s generator",
                                    "I used another camp\'s power",
                                    "I used the BRC grid",
                                    "Other"),
                         labelOrder = c(9,4,7,1,8,5,3,2,6))

generatePlot <- ggplot(generate[generate$year == 2023,], aes(x = labels, y = est)) +
  geom_bar(stat = "identity", fill = "#EA008B") +
  scale_y_continuous(labels = percent) +
  labs(x = "Electricity generation", y = "Burning Man participants in 2022 (%)") +
  theme_bw() +
  coord_flip()

Cairo(file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\electricity_generation", 
      type = "SVG", units = "in", width = 8, height = 5)
generatePlot
dev.off()

generateTable <- makeTableData(generate)[9:1,]

fwrite(generateTable, file = "C:\\Users\\ashev\\Documents\\census\\report_2023\\things_for_random\\electricity_generation_table.csv")
