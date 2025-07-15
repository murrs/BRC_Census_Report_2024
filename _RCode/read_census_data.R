#### Code to read in and prep all years of data
#
# create a file called config.yml and add the path to your data
# directory (with a trailing slash) like this for Mac or linux:
#
# default:
#   datadir: /Users/matto/Documents/census_data/
#
# or if on Windows:
#
# default:
#   datadir: "C:\Users\ashev\Documents\census\report_2023\BRC_Census_Report_2023\_data\"

library(data.table)
library(survey)

datadir <- config::get("datadir")

census13 <- fread(paste0(datadir, "Census2013Fulltab.tsv"), sep = "\t", na.strings = c("", "NA"))
census14 <- fread(paste0(datadir, "Census2014Fulltab.tsv"), sep = "\t", na.strings = c("", "NA"))
census15 <- fread(paste0(datadir, "Clean2015CensusFulltabJul2018.tsv"), sep = "\t", na.strings = c("", "NA"))
census16 <- fread(paste0(datadir, "Census2016Fulltab.tsv"), sep = "\t", na.strings = c("", "NA"))
census17 <- fread(paste0(datadir, "Clean2017CensusFulltabMar2018.tsv"), sep = "\t", na.strings = c("", "NA"))
census18 <- fread(paste0(datadir, "Clean2018CensusFulltabApr2019.tsv"), sep = "\t", na.strings = c("", "NA"))
census19 <- fread(paste0(datadir, "Clean2019CensusFulltabJan2019.tsv"), sep = "\t", na.strings = c("", "NA"))
census22 <- fread(paste0(datadir, "census2022_cleaned_weighted.tsv"), sep = "\t", na.strings = c("", "NA"))
census23 <- fread(paste0(datadir, "census2023_cleaned_weighted.tsv"), sep = "\t", na.strings = c("", "NA"))
census24 <- fread(paste0(datadir, "census2024_cleaned_weighted.tsv"), sep = "\t", na.strings = c("", "NA"))


design13 <- svydesign(ids = ~1, weights = ~weight, data = census13)
design14 <- svydesign(ids = ~1, weights = ~weightbmorg, data = census14)
design15 <- svydesign(ids = ~1, weights = ~weightbmorg1, data = census15)
design16 <- svydesign(ids = ~1, weights = ~weightbmorg1, data = census16)
design17 <- svydesign(ids = ~1, weights = ~weightbfarrival, data = census17)
design18 <- svydesign(ids = ~1, weights = ~weightbfarrival, data = census18)
design19 <- svydesign(ids = ~1, weights = ~weightbfarrival, data = census19)
design22 <- svydesign(ids = ~1, weights = ~weights, data = census22)
design23 <- svydesign(ids = ~1, weights = ~weights, data = census23)
design24 <- svydesign(ids = ~1, weights = ~weights, data = census24)


varNameTable <- fread(paste0(datadir, "question_name_xwalk_2024.tsv"), sep = "\t")

