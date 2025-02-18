library(data.table)

raw <- fread("20240125014405-SurveyExport.csv")
rawNames <- names(raw)
optionMatches <- regexpr("(.*)(?=\\:)", rawNames, perl = TRUE)
optionMatchesText <- regmatches(rawNames, optionMatches)
rawOptions <- optionMatches
rawOptions[rawOptions != -1] <- optionMatchesText
rawOptions[rawOptions == -1] <- ""
rawNames <- gsub("(.*)\\:", "", rawNames)

out <- data.table(question = rawNames, option = rawOptions, varname2023 = "",
                  varname2022 = "", varname2019 = "", varname2018 = "",
                  varname2017 = "", varname2016 = "", varname2015 = "",
                  varname2014 = "", varname2013 = "")

out.old <- fread("question_name_xwalk_2022.tsv", sep = "\t")
out$varname2023 <- out.old$varname2022[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2022 <- out.old$varname2022[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2019 <- out.old$varname2019[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2018 <- out.old$varname2018[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2017 <- out.old$varname2017[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2016 <- out.old$varname2016[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2015 <- out.old$varname2015[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2014 <- out.old$varname2014[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]
out$varname2013 <- out.old$varname2013[match(paste(out$question, out$option), 
                                             paste(out.old$question, 
                                                   out.old$option))]

fwrite(out, sep = "\t", file = "question_name_xwalk_2023.tsv")

