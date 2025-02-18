library(data.table)
library(survey)
library(anesrake)

#Conversation with random, sonder, and carly
# Awesome, so Total Population = BxB + 1.05(BxA) + Gate
# "1392 total passengers flew in with BxA. There were also approximately 
#    300 general aviation pilots and passengers that came in via 88NV. Those 
#    numbers are less precise, as we don't specifically capture the count for 
#    passengers that come in via GA, but it's roughly an average of 0.5 per 
#    pilot."
# Cool, so airport should be a total of 1392 BxA + 300 pilots + 150 passengers = 1842

#Read in random samplle data, create age categorical variable
randomSample2023 <- fread("allRandomSamples2023.tsv", sep = "\t")
randomSample2023[, age := 2023 - `What is the year of your birth (1)`]
randomSample2023[, ageCat5 := ifelse(between(age, 0, 29), "0-29",
                                     ifelse(between(age, 30, 39), "30-39",
                                            ifelse(between(age, 40, 49), "40-49",
                                                   ifelse(between(age, 50, 59), "50-59",
                                                          ifelse(age >= 60, "60+", NA)))))]
randomSample2023[, ID := paste0(`Shift nb`, `Lane`, `Clicker`, `Person numerator`)]
# randomSample2023[, `Sampling site` := factor(ifelse(`Sampling site` == 1, "BxB",
#                                                     ifelse(`Sampling site` == 2, "Airport",
#                                                            ifelse(`Sampling site` == 3, "Gate", NA))),
#                                              levels = c("Gate", "BxB", "Airport"))]
randomSample2023 <- randomSample2023[!is.na(`Sampling site`)]
# randomSample2022 <- fread("C:\\Users\\ashev\\Documents\\census\\report_2022\\allRandomSamples2022.tsv", sep = "\t")
#Read in cleaned online survey data
online2023 <- fread("cleaned2023.tsv", sep = "\t")
online2023 <- do.call(data.table, lapply(online2023, function(x){
  ifelse(x == "", NA, x)}))

#Create "population" data sets for raking based off random sample
totPop <- 77100
bxbPop <- 7349
airportPop <- 1842
gatePop <- totPop - bxbPop - airportPop
randomSample2023[, entryPop := ifelse(`Sampling site` == 1, bxbPop,
                                      ifelse(`Sampling site` == 2, airportPop,
                                             ifelse(`Sampling site` == 3, gatePop, NA)))]


randomSample2023[, nbburns := ifelse(((`Number of burns (TOTAL YEARS) (4b)` == 1) | 
                                        (is.na(`Number of burns (TOTAL YEARS) (4b)`) &
                                           !is.na(`What was the first language you learned (5)`))),
                                     "Virgin",
                                     ifelse(`Number of burns (TOTAL YEARS) (4b)` == 2, "1",
                                            ifelse(`Number of burns (TOTAL YEARS) (4b)` == 3, "2",
                                                   ifelse((`Number of burns (TOTAL YEARS) (4b)` >= 4) &
                                                            (`Number of burns (TOTAL YEARS) (4b)` <= 5), "3-4",
                                                          ifelse((`Number of burns (TOTAL YEARS) (4b)` >= 6) &
                                                                   (`Number of burns (TOTAL YEARS) (4b)` <= 8), "5-7",
                                                                 ifelse(`Number of burns (TOTAL YEARS) (4b)` >= 9, "8+", NA))))))]
randomSample2023[, nbburns := factor(nbburns, levels = c("Virgin", "1", "2", "3-4", "5-7", "8+"))]

randomSample2023[, gender := ifelse(`What is your current gender (3)` == 1, "Female",
                                    ifelse(`What is your current gender (3)` == 2, "Male",
                                           ifelse((`What is your current gender (3)` == 3) |
                                                    (`What is your current gender (3)` == 7) |
                                                    (`What is your current gender (3)` == 9), "Other", NA)))]
randomSample2023[, gender := factor(gender, levels = c("Female", "Male", "Other"))]

randomSample2023[, ageCat5 := factor(ageCat5, levels = c("0-29", "30-39",
                                                         "40-49", "50-59",
                                                         "60+"))]

randomSample2023[, english := ifelse(`What was the first language you learned (5)` == 1, "English",
                                     ifelse(`What was the first language you learned (5)` != 1, "Other language", NA))]
randomSample2023[, english := factor(english, levels = c("English", "Other language"))]

randomSample2023[, foreign := ifelse((`Where do you usually reside (2)` >= 1) &
                   (`Where do you usually reside (2)` <= 3), "US",
                 ifelse((`Where do you usually reside (2)` >= 4) &
                          (`Where do you usually reside (2)` <= 6), "Other country", NA))]
randomSample2023[, foreign := factor(foreign, levels = c("US", "Other country"))]

randomSample2023[, party := ifelse(`Are you eligible to vote (6a)` == 2, "Not eligible",
                                   ifelse(`With which political party are you currently affiliated (6c)` == 1, "Democrat",
                                          ifelse(`With which political party are you currently affiliated (6c)` == 2, "Green",
                                                 ifelse(`With which political party are you currently affiliated (6c)` == 3, "Libertarian",
                                                        ifelse(`With which political party are you currently affiliated (6c)` == 4, "Republican",
                                                               ifelse(`With which political party are you currently affiliated (6c)` == 5, "Other",
                                                                      ifelse(`With which political party are you currently affiliated (6c)` == 6, "None", NA)))))))]
randomSample2023[, party := factor(party, levels = c("Not eligible", "Democrat",
                                                     "Green", "Libertarian", 
                                                     "Republican", "Other", 
                                                     "None"))]

randomSampleDesign <- svydesign(~ID, strata = ~`Sampling site`, 
                                data = randomSample2023,
                                fpc = ~entryPop)


nbburns.target <- prop.table(svytable(~nbburns, randomSampleDesign))
gender.target <- prop.table(svytable(~gender, randomSampleDesign))
age.target <- prop.table(svytable(~ageCat5, randomSampleDesign))
english.target <- prop.table(svytable(~english, randomSampleDesign))
foreign.target <- prop.table(svytable(~foreign, randomSampleDesign))
gender.target <- prop.table(svytable(~gender, randomSampleDesign))
party.target <- prop.table(svytable(~party, randomSampleDesign))



#Create online survey variables for raking
online2023[, early.ipf := ifelse(grepl("((pre)|(Before))", firstArrivedBRC), "Early", 
                                 ifelse(grepl("[A-Za-z0-9]+", firstArrivedBRC), "Not early", NA))]
online2023[, early.ipf := as.factor(early.ipf)]

online2023[, nbburns.ipf := ifelse(nburns == 1, "Virgin",
                                   ifelse(nburns == 2, "1",
                                          ifelse(nburns == 3, "2",
                                                 ifelse(between(nburns, 4, 5), "3-4",
                                                        ifelse(between(nburns, 6, 8), "5-7",
                                                               ifelse(nburns >= 9, "8+", NA))))))]
online2023[, nbburns.ipf := as.factor(nbburns.ipf)]

online2023[, gender.ipf := ifelse(grepl("(?<!(e))([Mm]ale)", currentGender, perl = TRUE), "Male",
                                  ifelse(grepl("[Ff]emale", currentGender), "Female",
                                         ifelse(grepl("[A-Z]+", currentGender), "Other", NA)))]
online2023[, gender.ipf := as.factor(gender.ipf)]

online2023[, age.ipf := ifelse(between(age, 0, 29), "0-29",
                               ifelse(between(age, 30, 39), "30-39",
                                      ifelse(between(age, 40, 49), "40-49",
                                             ifelse(between(age, 50, 59), "50-59",
                                                    ifelse(age >= 60, "60+", NA)))))]
online2023[, age.ipf := as.factor(age.ipf)]

online2023[, english.ipf := ifelse(firstLanguage == "English", "English",
                                   ifelse(grepl("[A-Z]+", firstLanguage), "Other language", NA))]
online2023[, english.ipf := as.factor(english.ipf)]

online2023[, foreign.ipf := ifelse(reside %in% c("Within California", 
                                                 "Within Nevada", 
                                                 "Other location within U.S."),
                                   "US", ifelse(grepl("[A-Z]+", reside),
                                                "Other country", NA))]
online2023[, foreign.ipf := as.factor(foreign.ipf)]

online2023[, party.ipf := ifelse(eligibleVoteUS == "no", "Not eligible",
                                 ifelse(politicalParty == "Democratic Party", "Democrat",
                                        ifelse(politicalParty == "Green Party", "Green",
                                               ifelse(politicalParty == "Libertarian Party", "Libertarian",
                                                      ifelse(politicalParty == "Republican Party", "Republican",
                                                             ifelse(politicalParty == "Other US Party", "Other",
                                                                    ifelse(politicalParty == "None or unaffiliated", "None", NA)))))))]
online2023[, party.ipf := as.factor(party.ipf)]



rake2023 <- anesrake(inputter = list(#early.ipf = early.target,
                                     nbburns.ipf = nbburns.target,
                                     gender.ipf = gender.target,
                                     age.ipf = age.target,
                                     english.ipf = english.target,
                                     foreign.ipf = foreign.target,
                                     party.ipf = party.target),
                     dataframe = online2023,
                     caseid = online2023$responseID,
                     type = "nolim",
                     cap = 5)

online2023$weights <- rake2023$weightvec
fwrite(online2023, file = "census2023_cleaned_weighted.tsv", sep = "\t")
