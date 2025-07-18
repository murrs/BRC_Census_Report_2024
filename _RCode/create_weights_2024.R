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
randomSample2024 <- fread("allRandomSamples2024.tsv", sep = "\t")
randomSample2024[, age := `What is your age as of today (1)`]
randomSample2024[, ageCat5 := ifelse(between(age, 0, 29), "0-29",
                                     ifelse(between(age, 30, 39), "30-39",
                                            ifelse(between(age, 40, 49), "40-49",
                                                   ifelse(between(age, 50, 59), "50-59",
                                                          ifelse(age >= 60, "60+", NA)))))]
randomSample2024[, ID := 1:nrow(randomSample2024)]
# randomSample2023[, `Sampling site` := factor(ifelse(`Sampling site` == 1, "BxB",
#                                                     ifelse(`Sampling site` == 2, "Airport",
#                                                            ifelse(`Sampling site` == 3, "Gate", NA))),
#                                              levels = c("Gate", "BxB", "Airport"))]
randomSample2024 <- randomSample2024[!is.na(`Sampling site`)]
# randomSample2022 <- fread("C:\\Users\\ashev\\Documents\\census\\report_2022\\allRandomSamples2022.tsv", sep = "\t")
#Read in cleaned online survey data
online2024 <- fread("cleaned2024.tsv", sep = "\t")
online2024 <- do.call(data.table, lapply(online2024, function(x){
  ifelse(x == "", NA, x)}))

#Create "population" data sets for raking based off random sample
totPop2023 <- 77100
bxbPop2023 <- 7349
airportPop2023 <- 1842
gatePop2023 <- totPop2023 - bxbPop2023 - airportPop2023

totPop <- 72000
bxbPop <- 6254
airportPop <- 1093 + 228
gatePop <- totPop - bxbPop - airportPop



randomSample2024[, entryPop := ifelse(`Sampling site` == 1, bxbPop,
                                      ifelse(`Sampling site` == 2, airportPop,
                                             ifelse(`Sampling site` == 3, gatePop, NA)))]


randomSample2024[, nbburns := ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 1,
                                     "Virgin",
                                     ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 2, "2",
                                            ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 3, "3",
                                                   ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 4, "4-5",
                                                          ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 5, "6-7",
                                                                 ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 6, "8-11",
                                                                        ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 7, "12-15",
                                                                               ifelse(`How many times have you attended Burning Man at Black Rock City (5)` == 8, "16-19",
                                                                                      ifelse(`How many times have you attended Burning Man at Black Rock City (5)`, "20+", NA)))))))))]
randomSample2024[, nbburns := factor(nbburns, levels = c("Virgin", "2", "3", "4-5", "6-7", "8-11", "12-15", "16-19", "20+"))]

randomSample2024[, gender := ifelse(`What is your current gender (4)` == 1, "Female",
                                    ifelse(`What is your current gender (4)` == 2, "Male",
                                           ifelse((`What is your current gender (4)` == 3) |
                                                    (`What is your current gender (4)` == 4) |
                                                    (`What is your current gender (4)` == 5), "Other", NA)))]
randomSample2024[, gender := factor(gender, levels = c("Female", "Male", "Other"))]

randomSample2024[, ageCat5 := factor(ageCat5, levels = c("0-29", "30-39",
                                                         "40-49", "50-59",
                                                         "60+"))]

randomSample2024[, nativeAmerican := `Native American or Alaskan (7)`]
randomSample2024[, asian := `Asian (7)`]
randomSample2024[, black := `Black or African American (7)`]
randomSample2024[, hawaiiPacIslander := `Native Hawaiian or Other Pacific Islander (7)`]
randomSample2024[, white := `White/Caucasian (7)`]
randomSample2024[, midEasternNorAfrican := `Middle Eastern or North African (7)`]
randomSample2024[, hispanic := `Hispanic or Latino (7)`]
randomSample2024[, other := `Other race (7)`]

randomSample2024[, poc := grepl("[12]", `Do you consider yourself to be a person of color (6)`)]

randomSample2024[, vote := ifelse(`Did you vote in the last election in your country (3)` == 1, TRUE,
                                  ifelse(`Did you vote in the last election in your country (3)` %in% c(2, 3), FALSE, NA))]


randomSample2024[, reside := ifelse(`Where do you usually reside (2)` == 1, "Nevada (US)",
                                     ifelse(`Where do you usually reside (2)` == 2, "California (US)",
                                            ifelse(`Where do you usually reside (2)` == 3, "Other State (US)",
                                                   ifelse(`Where do you usually reside (2)` == 4, "Canada",
                                                          ifelse(`Where do you usually reside (2)` == 5, "Other Country", NA)))))]

randomSample2024[, reside := factor(reside, levels = c("Nevada (US)",
                                                         "California (US)",
                                                         "Other State (US)",
                                                         "Canada",
                                                         "Other Country"))]

randomSample2024[, education := ifelse(grepl("5", randomSample2024$`What education have you completed (9)`), "Graduate Degree",
                                       ifelse(grepl("[234]", randomSample2024$`What education have you completed (9)`), "College",
                                              ifelse(grepl("6", randomSample2024$`What education have you completed (9)`), "Vocaional", "Other")))]
randomSample2024[, education := factor(education, levels = c("College", "Graduate", "Vocational", "Other"))]


randomSample2024[, ticket := ifelse(grepl("[156]", randomSample2024$`In which sale did you purchase your ticket (10)`), "Standard ticket",
                                    ifelse(grepl("4", randomSample2024$`In which sale did you purchase your ticket (10)`), "Steward",
                                    ifelse(grepl("2", randomSample2024$`In which sale did you purchase your ticket (10)`), "FOMO",
                                           ifelse(grepl("3", randomSample2024$`In which sale did you purchase your ticket (10)`), "Ticket aid",
                                                  ifelse(grepl("9", randomSample2024$`In which sale did you purchase your ticket (10)`),"Staff", 
                                                         ifelse(!is.na(randomSample2024$`In which sale did you purchase your ticket (10)`), "Other", NA))))))]
randomSample2024[, ticket := factor(ticket, levels = c("Standard ticket", "Steward", "FOMO", "Ticket aid", "Staff", "Other"))]


randomSampleDesign <- svydesign(~1, strata = ~`Sampling site`, 
                                data = randomSample2024,
                                fpc = ~entryPop)


nbburns.target <- prop.table(svytable(~nbburns, randomSampleDesign))
gender.target <- prop.table(svytable(~gender, randomSampleDesign))
age.target <- prop.table(svytable(~ageCat5, randomSampleDesign))
white.target <- prop.table(svytable(~white, randomSampleDesign))
black.target <- prop.table(svytable(~black, randomSampleDesign))
hispanic.target <- prop.table(svytable(~hispanic, randomSampleDesign))
asian.target <- prop.table(svytable(~asian, randomSampleDesign))
poc.target <- prop.table(svytable(~poc, randomSampleDesign))
vote.target <- prop.table(svytable(~vote, randomSampleDesign))
reside.target <- prop.table(svytable(~reside, randomSampleDesign))
education.target <- prop.table(svytable(~education, randomSampleDesign))
ticket.target <- prop.table(svytable(~ticket, randomSampleDesign))


#Create online survey variables for raking
#c("Virgin", "1", "2", "3", "4-5", "6-7", "8-11", "12-15", "16-19", "20+")
online2024[, nbburns.ipf := ifelse(numberBurns == 1, "Virgin",
                                   ifelse(numberBurns == 2, "2",
                                          ifelse(numberBurns == 3, "3",
                                                 ifelse(between(numberBurns, 4, 5), "4-5",
                                                        ifelse(between(numberBurns, 6, 7), "6-7",
                                                               ifelse(between(numberBurns, 8, 11), "8-11",
                                                                      ifelse(between(numberBurns, 12, 15), "12-15",
                                                                             ifelse(between(numberBurns, 16, 19), "16-19",
                                                                                    ifelse(numberBurns >= 20, "20+", NA)))))))))]
online2024[, nbburns.ipf := factor(nbburns.ipf, levels = c("Virgin", "2", "3", "4-5", "6-7", "8-11", "12-15", "16-19", "20+"))]

online2024[, gender.ipf := factor(currentGender, levels = c("Female", "Male", "Other"))]

online2024[, age.ipf := ifelse(between(age, 0, 29), "0-29",
                               ifelse(between(age, 30, 39), "30-39",
                                      ifelse(between(age, 40, 49), "40-49",
                                             ifelse(between(age, 50, 59), "50-59",
                                                    ifelse(age >= 60, "60+", NA)))))]
online2024[, age.ipf := factor(age.ipf, levels = c("0-29", "30-39",
                                                 "40-49", "50-59",
                                                 "60+"))]

online2024[, white.ipf := ethnoracial.white]
online2024[, black.ipf := ethnoracial.black]
online2024[, hispanic.ipf := ethnoracial.hispanic]
online2024[, asian.ipf := ethnoracial.asian]

online2024[, poc.ipf := personOfColor %in% c("Sometimes", "Yes")]

online2024[, reside.ipf := ifelse(reside == "Within California", "California (US)",
                                  ifelse(reside == "Within Nevada", "Nevada (US)",
                                         ifelse(reside == "Other location within U.S.", "Other State (US)",
                                                ifelse(reside == "In Canada", "Canada",
                                                       ifelse(reside == "Other", "Other Country", NA)))))]
online2024[, reside.ipf := factor(reside.ipf, levels = c("Nevada (US)",
                                                           "California (US)",
                                                           "Other State (US)",
                                                           "Canada",
                                                           "Other Country"))]

online2024[, education.ipf := ifelse(grepl("Graduate", education), "Graduate Degree",
                                           ifelse(grepl("(Bach)|(Assoc)", education), "College",
                                                  ifelse(grepl("Tech", education), "Vocational", "Other")))]
online2024[, education.ipf := factor(education.ipf, levels = c("College", "Graduate", "Vocational", "Other"))]


# online2024[, vote.ipf := ]
online2024[, ticket.ipf := ifelse(ticketSource %in% c("Main sale or OMG sale",
                                                      "Burner Express Bus Plus", 
                                                      "STEP"), "Standard ticket",
                                  ifelse(ticketSource == "Stewards sales", "Steward",
                                  ifelse(ticketSource == "FOMO Sale", "FOMO",
                                  ifelse(ticketSource == "Staff or volunteer credential", "Staff",
                                         ifelse(ticketSource == "Ticket Aid/Low Income Ticket Program", "Ticket aid",
                                                ifelse(!is.na(ticketSource), "Other", NA))))))]
online2024[, ticket.ipf := factor(ticket.ipf, levels = c("Standard ticket", "Steward", "FOMO", "Ticket aid", "Staff", "Other"))]


rake2024 <- anesrake(inputter = list(#early.ipf = early.target,
  nbburns.ipf = nbburns.target,
  gender.ipf = gender.target,
  age.ipf = age.target,
  white.ipf = white.target[2:1],
  black.ipf = black.target[2:1],
  hispanic.ipf = hispanic.target[2:1],
  asian.ipf = asian.target[2:1],
  poc.ipf = poc.target[2:1],
  reside.ipf = reside.target,
  ticket.ipf = ticket.target),
  dataframe = online2024,
  caseid = online2024$responseID,
  type = "nolim",
  cap = 5)

online2024$weights <- rake2024$weightvec
fwrite(online2024, file = "census2024_cleaned_weighted.tsv", sep = "\t")
