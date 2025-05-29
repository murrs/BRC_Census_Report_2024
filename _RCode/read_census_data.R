#### Code to read in and prep all years of data
# Edit this file to your local file paths
# Don't pull request changes to this file

library(data.table)
library(survey)

datadir <- "C:\\Users\\ashev\\Documents\\census\\report_2024\\BRC_Census_Report_2024\\_data\\"

census13 <- fread(paste0(datadir, "\\Census2013Fulltab.tsv"), sep = "\t", na.strings = c("", "NA"))
census14 <- fread(paste0(datadir, "\\Census2014Fulltab.tsv"), sep = "\t", na.strings = c("", "NA"))
census15 <- fread(paste0(datadir, "\\Clean2015CensusFulltabJul2018.tsv"), sep = "\t", na.strings = c("", "NA"))
census16 <- fread(paste0(datadir, "\\Census2016Fulltab.tsv"), sep = "\t", na.strings = c("", "NA"))
census17 <- fread(paste0(datadir, "\\Clean2017CensusFulltabMar2018.tsv"), sep = "\t", na.strings = c("", "NA"))
census18 <- fread(paste0(datadir, "\\Clean2018CensusFulltabApr2019.tsv"), sep = "\t", na.strings = c("", "NA"))
census19 <- fread(paste0(datadir, "\\Clean2019CensusFulltabJan2019.tsv"), sep = "\t", na.strings = c("", "NA"))
census22 <- fread(paste0(datadir, "\\census2022_cleaned_weighted.tsv"), sep = "\t", na.strings = c("", "NA"))
census23 <- fread(paste0(datadir, "\\census2023_cleaned_weighted.tsv"), sep = "\t", na.strings = c("", "NA"))
census24 <- fread(paste0(datadir, "\\census2024_cleaned_weighted.tsv"), sep = "\t", na.strings = c("", "NA"))

design13 <- svydesign(ids = ~id, weights = ~weight, data = census13)
design14 <- svydesign(ids = ~id, weights = ~weightbmorg, data = census14)
design15 <- svydesign(ids = ~id, weights = ~weightbmorg1, data = census15)
design16 <- svydesign(ids = ~id, weights = ~weightbmorg1, data = census16)
design17 <- svydesign(ids = ~id, weights = ~weightbfarrival, data = census17)
design18 <- svydesign(ids = ~id, weights = ~weightbfarrival, data = census18)
design19 <- svydesign(ids = ~id, weights = ~weightbfarrival, data = census19)
design22 <- svydesign(ids = ~responseID, weights = ~weights, data = census22)
design23 <- svydesign(ids = ~responseID, weights = ~weights, data = census23)
design24 <- svydesign(ids = ~responseID, weights = ~weights, data = census24)

varNameTable <- fread(paste0(datadir, "\\question_name_xwalk_2024.tsv"), sep = "\t")




