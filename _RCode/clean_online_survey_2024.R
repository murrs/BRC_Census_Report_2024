library(data.table)

raw <- fread("20250113011309-SurveyExport.csv")
raw <- do.call(data.table, lapply(raw, function(x) {ifelse(x == "", NA, x)}))
varxwalk <- fread("question_name_xwalk_2024.tsv", sep = "\t")
names(raw) = varxwalk$varname2024[1:ncol(raw)]
raw <- raw[,-163]

#Number of burns
attends <- apply(raw, 1, function(x){
  attendVars <- grepl("^attend\\.([0-9]{4})", names(x))
  sum(!is.na(x[attendVars]))
})
correction <- as.numeric(!is.na(raw$attend.1990.baker) & 
                           !is.na(raw$attend.1990.blackrock))
nbburns <- attends - correction
raw[, numberBurns := nbburns]
raw[, virgin2 := (numberBurns == 1) & (attend.2024 == 2024)]

#Eligibility: First time filling out census/did not go in 2024/Complete status
#             and responded to age with age at least 18 years old.
raw <- raw[(raw$virgin != 3) &  
             (raw$status == "Complete") & 
             (raw$numberBurns > 0) &
             (!is.na(raw$age) & raw$age >= 18) &
             ((raw$virgin == 1) == raw$virgin2),]

#Age when arrived in Black Rock City (no cleaning needed)

#Virgin
raw$virgin <- raw$virgin == 1

#Year born (no cleaning needed)

#Current gender
currentGenderSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Female",
         "2" = "Male",
         "4" = "Other")
}
raw[, currentGender := sapply(raw$currentGender, currentGenderSwitch)]


#Plan to return to BRC
planToReturnSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes, definitely in 2025",
         "2" = "Yes, not in 2025 but definitely in the next 5 years",
         "3" = "Maybe in the future if possible",
         "4" = "Not likely",
         "5" = "No, definitely not")
}
raw[, planToReturn := sapply(raw$planToReturn, planToReturnSwitch)]


#Where do you reside?
resideSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Within Nevada",
         "2" = "Within California",
         "3" = "Other location within U.S.",
         "4" = "In Canada",
         "5" = "Other")
}
raw[, reside := sapply(reside, resideSwitch)]

#Reside ZIP/Canadian Post Code
#TODO write check for valid ZIP and post code

#Income
incomeSwitch.pi  <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "I had no personal income",
         "2" = "Less than $7,500 USD",
         "3" = "$7,500-$14,999 USD",
         "4" = "$15,000-$24,999 USD",
         "5" = "$25,000-$34,999 USD",
         "6" = "$35,000-$49,999 USD",
         "7" = "$50,000-$74,999 USD",
         "8" = "$75,000-$99,999 USD",
         "9" = "$100,000-$149,999 USD",
         "10" = "$150,000-$299,999 USD",
         "11" = "$300,000-$399,999 USD",
         "34" = "$400,000-$499,999 USD",
         "35" = "$500,000-$749,999 USD",
         "36" = "$750,000-$999,999 USD",
         "37" = "$1,000,000-$4,999,999 USD",
         "38" = "$5,000,000-$9,999,999 USD",
         "39" = "$10,000,000 USD or more")
}

incomeSwitch.hi  <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "I had no household income",
         "2" = "Less than $7,500 USD",
         "3" = "$7,500-$14,999 USD",
         "4" = "$15,000-$24,999 USD",
         "5" = "$25,000-$34,999 USD",
         "6" = "$35,000-$49,999 USD",
         "7" = "$50,000-$74,999 USD",
         "8" = "$75,000-$99,999 USD",
         "9" = "$100,000-$149,999 USD",
         "10" = "$150,000-$299,999 USD",
         "11" = "$300,000-$399,999 USD",
         "12" = "$400,000-$499,999 USD",
         "13" = "$500,000-$749,999 USD",
         "14" = "$750,000-$999,999 USD",
         "15" = "$1,000,000-$4,999,999 USD",
         "16" = "$5,000,000-$9,999,999 USD",
         "17" = "$10,000,000 USD or more")
}


raw[, personalIncome := sapply(personalIncome, incomeSwitch.pi)]
raw[, householdIncome := sapply(householdIncome, incomeSwitch.hi)]

#Education
educationSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "12" = "Elementary school or equivalent",
         "13" = "High school diploma or equivalent",
         "15" = "Associate's degree or equivalent",
         "16" = "Bachelor's degree or equivalent",
         "17" = "Graduate degree (Master's, Doctorate, or equivalent) or equivalent",
         "18" = "Technical/vocational certification or equivalent",
         "19" = "Other",
         "20" = "None of the above")
}
raw[, education := sapply(education, educationSwitch)]

#First Language
firstLanguageSwitch  <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "English",
         "2" = "Arabic",
         "3" = "Chinese",
         "4" = "Danish",
         "5" = "Dutch",
         "6" = "Farsi",
         "7" = "French",
         "8" = "German",
         "9" = "Hebrew",
         "10" = "Hindi",
         "11" = "Italian",
         "12" = "Japanese",
         "13" = "Norwegian",
         "14" = "Polish",
         "15" = "Portuguese",
         "16" = "Punjabi",
         "17" = "Romanian",
         "18" = "Russian",
         "19" = "Spanish",
         "20" = "Swedish",
         "21" = "Tagalog",
         "22" = "Urdu",
         "23" = "Other")
}

raw[, firstLanguage := sapply(firstLanguage, firstLanguageSwitch)]

#Ethnoracial
raw[, ethnoracial.asian := !is.na(ethnoracial.asian)]
raw[, ethnoracial.nativeAmerican := !is.na(ethnoracial.nativeAmerican)]
raw[, ethnoracial.pacificIslander := !is.na(ethnoracial.pacificIslander)]
raw[, ethnoracial.hispanic := !is.na(ethnoracial.hispanic)]
raw[, ethnoracial.white := !is.na(ethnoracial.white)]
raw[, ethnoracial.black := !is.na(ethnoracial.black)]
raw[, ethnoracial.middleEasternNorthAfrican := !is.na(ethnoracial.middleEasternNorthAfrican)]
raw[, ethnoracial.selfDescribe := !is.na(ethnoracial.selfDescribe)]

#Person of color
personOfColorSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "2" = "No",
         "3" = "Sometimes")
}
raw[, personOfColor := sapply(personOfColor, personOfColorSwitch)]

#Eligible to vote in the US
raw[, eligibleVoteUS := ifelse(eligibleVoteUS == 1, "yes", "no")]

#Voted in election...
raw[, voteUS.2022 := ifelse(is.na(voteUS.2022), "no", "yes")]
raw[, voteUS.2020 := ifelse(is.na(voteUS.2020), "no", "yes")]
raw[, voteUS.2018 := ifelse(is.na(voteUS.2018), "no", "yes")]
raw[, voteUS.2016 := ifelse(is.na(voteUS.2016), "no", "yes")]
# raw[, voteUS.2014 := ifelse(is.na(voteUS.2014), "no", "yes")]
# raw[, voteUS.2012 := ifelse(is.na(voteUS.2012), "no", "yes")]
raw[, voteUS.none := ifelse(is.na(voteUS.none), "no", "yes")]
raw[, voteUS.timesVoted := ifelse(voteUS.none == "yes", 0, 
                                  (voteUS.2022 == "yes") +
                                    (voteUS.2020 == "yes") +
                                    (voteUS.2018 == "yes") +
                                    (voteUS.2016 == "yes"))]
raw[, voteUS.timesVoted.eligible := ifelse(eligibleVoteUS == "no", NA, 
                                           ifelse(voteUS.none == "yes" &
                                                    eligibleVoteUS == "yes", 0,
                                                  (voteUS.2022 == "yes") +
                                                    (voteUS.2020 == "yes") +
                                                    (voteUS.2018 == "yes") +
                                                    (voteUS.2016 == "yes")))]

#TODO check key
#Political party
politicalPartySwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Democratic Party",
         "2" = "Green Party",
         "3" = "Libertarian Party",
         "4" = "Republican Party",
         "6" = "Other US Party",
         "7" = "None, I am nonpartisan",
         "8" = "None or unaffiliated")
}
raw[, politicalParty := sapply(politicalParty, politicalPartySwitch)]

#Political views
politicalViewsSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Anarchist",
         "2" = "Conservative",
         "3" = "Green",
         "4" = "Liberal",
         "5" = "Libertarian",
         "6" = "Progressive",
         "7" = "Socialist",
         "8" = "Centrist or moderate",
         "9" = "Other",
         "10" = "None, I consider myself to be apolitical")
}
raw[, politicalViews := sapply(politicalViews, politicalViewsSwitch)]

#Sexual orientation
sexualOrientationSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Heterosexual or straight",
         "2" = "Gay, lesbian, or homosexual",
         "3" = "Bisexual or pansexual",
         "4" = "Bicurious or heteroflexible",
         "6" = "Demi-sexual or aesexual",
         # "6" = "Asexual",
         "7" = "Refuse labels",
         "8" = "Other")
}
raw[, sexualOrientation := sapply(sexualOrientation, sexualOrientationSwitch)]



# #Romantic orientation
# romanticOrientationSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "Hetero-romantic",
#          "2" = "Homo-romantic",
#          "3" = "Bi-romantic or Pan-romantic",
#          "4" = "Demi-romantic",
#          "5" = "Aromantic",
#          "6" = "Can\'t label",
#          "7" = "Other")
# }
# raw[, romanticOrientation := sapply(romanticOrientation, romanticOrientationSwitch)]

#Gender
raw[, gender.transperson := !is.na(gender.transperson)]
raw[, gender.queer := !is.na(gender.queer)]
raw[, gender.nonConforming := !is.na(gender.nonConforming)]
raw[, gender.questioning := !is.na(gender.questioning)]
raw[, gender.twoSpirit := !is.na(gender.twoSpirit)]
raw[, gender.notMatchBirth := !is.na(gender.notMatchBirth)]

#Partners
romanticPartnersSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes and we are married",
         "2" = "Yes and we are not married",
         "3" = "No",
         "4" = "It\'s complicated")
}
raw[, romanticPartners := sapply(romanticPartners, romanticPartnersSwitch)]

#Relationship
# raw[, relationship.monogamous := !is.na(relationship.monogamous)]
# raw[, relationship.monogamish := !is.na(relationship.monogamish)]
# raw[, relationship.polyamourous := !is.na(relationship.polyamourous)]
# raw[, relationship.nonConsensualNonMonogomous := !is.na(relationship.nonConsensualNonMonogomous)]
# raw[, relationship.serialMonogomous := !is.na(relationship.serialMonogomous)]
# raw[, relationship.polyfidelity := !is.na(relationship.polyfidelity)]
# raw[, relationship.polygamous := !is.na(relationship.polygamous)]
# raw[, relationship.unicornPoly := !is.na(relationship.unicornPoly)]
# raw[, relationship.soloPoly := !is.na(relationship.soloPoly)]
# raw[, relationship.powerDynamic := !is.na(relationship.powerDynamic)]
# raw[, relationship.open := !is.na(relationship.open)]
# raw[, relationship.ethicalNonMonogomous := !is.na(relationship.ethicalNonMonogomous)]
# raw[, relationship.dontAskDontTell := !is.na(relationship.dontAskDontTell)]
# raw[, relationship.anarchy := !is.na(relationship.anarchy)]
# raw[, relationship.other := !is.na(relationship.other)]

#Sexuality
# raw[, sexuality.kinkster := !is.na(sexuality.kinkster)]
# raw[, sexuality.swinger := !is.na(sexuality.swinger)]
# raw[, sexuality.sexWorker := !is.na(sexuality.sexWorker)]
# raw[, sexuality.grayDemiSexual := !is.na(sexuality.grayDemiSexual)]
# raw[, sexuality.queer := !is.na(sexuality.queer)]
# raw[, sexuality.sexLoveAddict := !is.na(sexuality.sexLoveAddict)]
# raw[, sexuality.openCurious := !is.na(sexuality.openCurious)]
# raw[, sexuality.other := !is.na(sexuality.other)]

#Spirituality
spiritualitySwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Religious",
         "2" = "Spiritual, but not religious",
         "3" = "Atheist",
         "4" = "Agnostic",
         "5" = "Deist",
         "6" = "Don\'t know")
}
raw[, spirituality := sapply(spirituality, spiritualitySwitch)]

# #Religion
# religionSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "No religion",
#          "2" = "Protestant",
#          "3" = "Catholic",
#          "4" = "Other Christian",
#          "5" = "Jewish",
#          "6" = "Buddhist",
#          "7" = "Pastafarian",
#          "8" = "Pagan",
#          "9" = "Muslim",
#          "10" = "Hindu",
#          "11" = "Unitarian",
#          "13" = "Discordian",
#          "14" = "Other",
#          "15" = "Multiple religions")
# }
# raw[, religion := sapply(religion, religionSwitch)]

#Additional religion
# raw[, moreReligion.protestant := !is.na(moreReligion.protestant)]
# raw[, moreReligion.catholic := !is.na(moreReligion.catholic)]
# raw[, moreReligion.christianOther := !is.na(moreReligion.christianOther)]
# raw[, moreReligion.jewish := !is.na(moreReligion.jewish)]
# raw[, moreReligion.buddhist := !is.na(moreReligion.buddhist)]
# raw[, moreReligion.pastafarian := !is.na(moreReligion.pastafarian)]
# raw[, moreReligion.pagan := !is.na(moreReligion.pagan)]
# raw[, moreReligion.muslim := !is.na(moreReligion.muslim)]
# raw[, moreReligion.hindu := !is.na(moreReligion.hindu)]
# raw[, moreReligion.unitarian := !is.na(moreReligion.unitarian)]
# raw[, moreReligion.discordian := !is.na(moreReligion.discordian)]
# raw[, moreReligion.other := !is.na(moreReligion.other)]


#Attend regional
regionalAttendSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes, I have attended at least one Regional Burning Man Event",
         "2" = "Yes, and I volunteered with a Regional Burning Man Event",
         "3" = "No, but I am on an email list for Regional Burners",
         "4" = "No, I am not connected to my Regional Burning Man community")
}
raw[, regionalAttend := sapply(regionalAttend, regionalAttendSwitch)]

#Regional involvment
raw[, regionalInvolved.production := !is.na(regionalInvolved.production)]
raw[, regionalInvolved.staffVolunteer := !is.na(regionalInvolved.staffVolunteer)]
raw[, regionalInvolved.medical := !is.na(regionalInvolved.medical)]
raw[, regionalInvolved.art := !is.na(regionalInvolved.art)]
raw[, regionalInvolved.themeCamp := !is.na(regionalInvolved.themeCamp)]
raw[, regionalInvolved.vendor := !is.na(regionalInvolved.vendor)]
raw[, regionalInvolved.performance := !is.na(regionalInvolved.performance)]
raw[, regionalInvolved.donor := !is.na(regionalInvolved.donor)]
raw[, regionalInvolved.supporter := !is.na(regionalInvolved.supporter)]
raw[, regionalInvolved.other := !is.na(regionalInvolved.other)]
raw[, regionalInvolved.notInvolved := !is.na(regionalInvolved.notInvolved)]

#Media managed by Burning Man
mediaSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Never",
         "2" = "Rarely",
         "3" = "Often")
}

raw[, mediaInfoBM.JRS := sapply(mediaInfoBM.JRS, mediaSwitch)]
raw[, mediaInfoBM.website := sapply(mediaInfoBM.website, mediaSwitch)]
raw[, mediaInfoBM.journal := sapply(mediaInfoBM.journal, mediaSwitch)]
raw[, mediaInfoBM.hive := sapply(mediaInfoBM.hive, mediaSwitch)]
raw[, mediaInfoBM.ePlaya := sapply(mediaInfoBM.ePlaya, mediaSwitch)]
raw[, mediaInfoBM.socialMedia := sapply(mediaInfoBM.socialMedia, mediaSwitch)]

#Media not managed by Burning Man
raw[, mediaInfoNotBM.campsSocialMedia := sapply(mediaInfoNotBM.campsSocialMedia, mediaSwitch)]
raw[, mediaInfoNotBM.regionalNewsletter := sapply(mediaInfoNotBM.regionalNewsletter, mediaSwitch)]
raw[, mediaInfoNotBM.regionalWebsite := sapply(mediaInfoNotBM.regionalWebsite, mediaSwitch)]
raw[, mediaInfoNotBM.regionalDiscussionList := sapply(mediaInfoNotBM.regionalDiscussionList, mediaSwitch)]
raw[, mediaInfoNotBM.otherSocialMedia := sapply(mediaInfoNotBM.otherSocialMedia, mediaSwitch)]
raw[, mediaInfoNotBM.otherWebBlogPod := sapply(mediaInfoNotBM.otherWebBlogPod, mediaSwitch)]
raw[, mediaInfoNotBM.otherDiscussionList := sapply(mediaInfoNotBM.otherDiscussionList, mediaSwitch)]
raw[, mediaInfoNotBM.wordOfMouth := sapply(mediaInfoNotBM.wordOfMouth, mediaSwitch)]

#Most valuable information coming from Burning Man Project
raw[, mostValuableInfo.BRCEvents := !is.na(mostValuableInfo.BRCEvents)]
raw[, mostValuableInfo.bayAreaEvents := !is.na(mostValuableInfo.bayAreaEvents)]
raw[, mostValuableInfo.citiesWorldEvents := !is.na(mostValuableInfo.citiesWorldEvents)]
raw[, mostValuableInfo.regionals := !is.na(mostValuableInfo.regionals)]
raw[, mostValuableInfo.volunteer := !is.na(mostValuableInfo.volunteer)]
raw[, mostValuableInfo.art := !is.na(mostValuableInfo.art)]
raw[, mostValuableInfo.BRCPreparation := !is.na(mostValuableInfo.BRCPreparation)]
raw[, mostValuableInfo.camps := !is.na(mostValuableInfo.camps)]
raw[, mostValuableInfo.photos := !is.na(mostValuableInfo.photos)]
raw[, mostValuableInfo.storiesAboutBurners := !is.na(mostValuableInfo.storiesAboutBurners)]
raw[, mostValuableInfo.burningManNonProfit := !is.na(mostValuableInfo.burningManNonProfit)]
raw[, mostValuableInfo.RIDE := !is.na(mostValuableInfo.RIDE)]
raw[, mostValuableInfo.sustainability := !is.na(mostValuableInfo.sustainability)]
raw[, mostValuableInfo.other := !is.na(mostValuableInfo.other)]


#BMIR
raw[, BMIR.listenedOnline := !is.na(BMIR.listenedOnline)]
raw[, BMIR.listenedMobile := !is.na(BMIR.listenedMobile)]
raw[, BMIR.listenedRadio := !is.na(BMIR.listenedRadio)]
raw[, BMIR.loudspeakers := !is.na(BMIR.loudspeakers)]
raw[, BMIR.dontKnowBMIR := !is.na(BMIR.dontKnowBMIR)]
raw[, BMIR.didntListen := !is.na(BMIR.didntListen)]

#BMIR Useful
BMIRUsefulSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Always",
         "2" = "Often",
         "3" = "Sometimes",
         "4" = "Never")
}
raw[, BMIRUseful := sapply(BMIRUseful, BMIRUsefulSwitch)]

#GARS
raw[, GARS.insideBRC := !is.na(GARS.insideBRC)]
raw[, GARS.onWayToBRCOnly := !is.na(GARS.onWayToBRCOnly)]
raw[, GARS.exodusOnly := !is.na(GARS.exodusOnly)]
raw[, GARS.onWaytoBRCandExodus := !is.na(GARS.onWaytoBRCandExodus)]
raw[, GARS.Gerlach := !is.na(GARS.Gerlach)]

#BMIR Useful
GARSUsefulSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Always",
         "2" = "Often",
         "3" = "Sometimes",
         "4" = "Never")
}
raw[, GARSUseful := sapply(GARSUseful, GARSUsefulSwitch)]

#Non-profit programs
raw[, nonProfitPrograms.BWB := ifelse(nonProfitPrograms.BWB == 1, "Yes",
                                      ifelse(nonProfitPrograms.BWB == 2, "Heard of it",
                                             ifelse(nonProfitPrograms.BWB == 3, "No", NA)))]
raw[, nonProfitPrograms.BMArts := ifelse(nonProfitPrograms.BMArts == 1, "Yes",
                                         ifelse(nonProfitPrograms.BMArts == 2, "Heard of it",
                                                ifelse(nonProfitPrograms.BMArts == 3, "No", NA)))]
raw[, nonProfitPrograms.blackRockLabs := ifelse(nonProfitPrograms.blackRockLabs == 1, "Yes",
                                                ifelse(nonProfitPrograms.blackRockLabs == 2, "Heard of it",
                                                       ifelse(nonProfitPrograms.blackRockLabs == 3, "No", NA)))]
raw[, nonProfitPrograms.regionalNetwork := ifelse(nonProfitPrograms.regionalNetwork == 1, "Yes",
                                                  ifelse(nonProfitPrograms.regionalNetwork == 2, "Heard of it",
                                                         ifelse(nonProfitPrograms.regionalNetwork == 3, "No", NA)))]
raw[, nonProfitPrograms.flyRanch := ifelse(nonProfitPrograms.flyRanch == 1, "Yes",
                                           ifelse(nonProfitPrograms.flyRanch == 2, "Heard of it",
                                                  ifelse(nonProfitPrograms.flyRanch == 3, "No", NA)))]
raw[, nonProfitPrograms.philosophicalCenter := ifelse(nonProfitPrograms.philosophicalCenter == 1, "Yes",
                                                      ifelse(nonProfitPrograms.philosophicalCenter == 2, "Heard of it",
                                                             ifelse(nonProfitPrograms.philosophicalCenter == 3, "No", NA)))]
raw[, nonProfitPrograms.HIVE := ifelse(nonProfitPrograms.HIVE == 1, "Yes",
                                       ifelse(nonProfitPrograms.HIVE == 2, "Heard of it",
                                              ifelse(nonProfitPrograms.HIVE == 3, "No", NA)))]

#Ten principles
#TODO Identify people who skipped this question
raw[, tenPrinciplesFrequent.radicalInclusion := !is.na(tenPrinciplesFrequent.radicalInclusion)]
raw[, tenPrinciplesFrequent.gifting := !is.na(tenPrinciplesFrequent.gifting)]
raw[, tenPrinciplesFrequent.decommodification := !is.na(tenPrinciplesFrequent.decommodification)]
raw[, tenPrinciplesFrequent.radicalSelfReliance := !is.na(tenPrinciplesFrequent.radicalSelfReliance)]
raw[, tenPrinciplesFrequent.radicalSelfExpression := !is.na(tenPrinciplesFrequent.radicalSelfExpression)]
raw[, tenPrinciplesFrequent.communalEffort := !is.na(tenPrinciplesFrequent.communalEffort)]
raw[, tenPrinciplesFrequent.civicResponsibility := !is.na(tenPrinciplesFrequent.civicResponsibility)]
raw[, tenPrinciplesFrequent.leaveNoTrace := !is.na(tenPrinciplesFrequent.leaveNoTrace)]
raw[, tenPrinciplesFrequent.participation := !is.na(tenPrinciplesFrequent.participation)]
raw[, tenPrinciplesFrequent.immediacy := !is.na(tenPrinciplesFrequent.immediacy)]

raw[, tenPrinciplesDifficult.radicalInclusion := !is.na(tenPrinciplesDifficult.radicalInclusion)]
raw[, tenPrinciplesDifficult.gifting := !is.na(tenPrinciplesDifficult.gifting)]
raw[, tenPrinciplesDifficult.decommodification := !is.na(tenPrinciplesDifficult.decommodification)]
raw[, tenPrinciplesDifficult.radicalSelfReliance := !is.na(tenPrinciplesDifficult.radicalSelfReliance)]
raw[, tenPrinciplesDifficult.radicalSelfExpression := !is.na(tenPrinciplesDifficult.radicalSelfExpression)]
raw[, tenPrinciplesDifficult.communalEffort := !is.na(tenPrinciplesDifficult.communalEffort)]
raw[, tenPrinciplesDifficult.civicResponsibility := !is.na(tenPrinciplesDifficult.civicResponsibility)]
raw[, tenPrinciplesDifficult.leaveNoTrace := !is.na(tenPrinciplesDifficult.leaveNoTrace)]
raw[, tenPrinciplesDifficult.participation := !is.na(tenPrinciplesDifficult.participation)]
raw[, tenPrinciplesDifficult.immediacy := !is.na(tenPrinciplesDifficult.immediacy)]

tenPrinciplesImportantSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Very important",
         "2" = "Important",
         "3" = "Moderately important",
         "4" = "Slightly important",
         "5" = "Not important",
         "6" = "I don\'t know")   
}
raw[, tenPrinciplesImportant := sapply(tenPrinciplesImportant, tenPrinciplesImportantSwitch)]

tenPrinciplesEssentialSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Very important",
         "2" = "Important",
         "3" = "Moderately important",
         "4" = "Slightly important",
         "5" = "Not important")
}
raw[, tenPrinciplesEssential := sapply(tenPrinciplesEssential, tenPrinciplesEssentialSwitch)]

tenPrinciplesDailyLifeSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes, I try to practice these principles in my daily life",
         "2" = "Yes, I have modeled my business or professional activity on these
principles",
         "3" = "No, I am not",
         "4" = "Not applicable")
}

raw[, tenPrinciplesDailyLife := sapply(tenPrinciplesDailyLife, tenPrinciplesDailyLifeSwitch)]

#Volunteering
raw[, volunteerLastYear.BRC := !is.na(volunteerLastYear.BRC)]
raw[, volunteerLastYear.regionals := !is.na(volunteerLastYear.regionals)]
raw[, volunteerLastYear.BWB := !is.na(volunteerLastYear.BWB)]
raw[, volunteerLastYear.flyRanch := !is.na(volunteerLastYear.flyRanch)]
raw[, volunteerLastYear.art := !is.na(volunteerLastYear.art)]
raw[, volunteerLastYear.political := !is.na(volunteerLastYear.political)]
raw[, volunteerLastYear.schools := !is.na(volunteerLastYear.schools)]
raw[, volunteerLastYear.environment := !is.na(volunteerLastYear.environment)]
raw[, volunteerLastYear.health := !is.na(volunteerLastYear.health)]
raw[, volunteerLastYear.rights := !is.na(volunteerLastYear.rights)]
raw[, volunteerLastYear.poverty := !is.na(volunteerLastYear.poverty)]
raw[, volunteerLastYear.religious := !is.na(volunteerLastYear.religious)]
raw[, volunteerLastYear.community := !is.na(volunteerLastYear.community)]
raw[, volunteerLastYear.deeds := !is.na(volunteerLastYear.deeds)]
raw[, volunteerLastYear.other := !is.na(volunteerLastYear.other)]

#Donation
raw[, donation.BRC := !is.na(donation.BRC)]
raw[, donation.regionals := !is.na(donation.regionals)]
raw[, donation.BWB := !is.na(donation.BWB)]
raw[, donation.flyRanch := !is.na(donation.flyRanch)]
raw[, donation.art := !is.na(donation.art)]
raw[, donation.political := !is.na(donation.political)]
raw[, donation.schools := !is.na(donation.schools)]
raw[, donation.environment := !is.na(donation.environment)]
raw[, donation.health := !is.na(donation.health)]
raw[, donation.rights := !is.na(donation.rights)]
raw[, donation.poverty := !is.na(donation.poverty)]
raw[, donation.religious := !is.na(donation.religious)]
raw[, donation.community := !is.na(donation.community)]
raw[, donation.deeds := !is.na(donation.deeds)]
raw[, donation.other := !is.na(donation.other)]


#Amount donated to Burning Man
donationAmountSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "$1-$20",
         "2" = "$21-$100",
         "3" = "$101-$500",
         "4" = "$501-$1,000",
         "5" = "$1,001-$5,000",
         "6" = "$5,001-$10,000",
         "7" = "over $10,000")
}
raw[, donation.amount := sapply(donation.amount, donationAmountSwitch)]

#Volunteer hours
#TODO

#Inspired to volunteer
inspiredVolunteerSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "2" = "No",
         "3" = "Maybe")
}
raw[, inspiredVolunteer := sapply(inspiredVolunteer, inspiredVolunteerSwitch)]



#Inspired skills
raw[, inspiredSkill.artCreation := !is.na(inspiredSkill.artCreation)]
raw[, inspiredSkill.performance := !is.na(inspiredSkill.performance)]
raw[, inspiredSkill.physical := !is.na(inspiredSkill.physical)]
raw[, inspiredSkill.leadership := !is.na(inspiredSkill.leadership)]
raw[, inspiredSkill.projectManagment := !is.na(inspiredSkill.projectManagment)]
raw[, inspiredSkill.community := !is.na(inspiredSkill.community)]
raw[, inspiredSkill.construction := !is.na(inspiredSkill.construction)]
raw[, inspiredSkill.electrical := !is.na(inspiredSkill.electrical)]
raw[, inspiredSkill.carpentryMetal := !is.na(inspiredSkill.carpentryMetal)]
raw[, inspiredSkill.graphicDesign := !is.na(inspiredSkill.graphicDesign)]
raw[, inspiredSkill.mediation := !is.na(inspiredSkill.mediation)]
raw[, inspiredSkill.political := !is.na(inspiredSkill.political)]
raw[, inspiredSkill.emotional := !is.na(inspiredSkill.emotional)]
raw[, inspiredSkill.selfCare := !is.na(inspiredSkill.selfCare)]
raw[, inspiredSkill.survival := !is.na(inspiredSkill.survival)]
raw[, inspiredSkill.other := !is.na(inspiredSkill.other)]

#Skills useful in the default world
skillsUsefulDefaultWorldSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Very useful",
         "2" = "Somewhat useful",
         "3" = "May be at some point",
         "4" = "Not at all")
}
raw[, skillsUsefulDefaultWorld := sapply(skillsUsefulDefaultWorld, skillsUsefulDefaultWorldSwitch)]

#Using skills outsides BRC
raw[, skillsUsing.creatingForMyself := !is.na(skillsUsing.creatingForMyself)]
raw[, skillsUsing.creatingForHome := !is.na(skillsUsing.creatingForHome)]
raw[, skillsUsing.teaching := !is.na(skillsUsing.teaching)]
raw[, skillsUsing.creatingToBringToBRC := !is.na(skillsUsing.creatingToBringToBRC)]
raw[, skillsUsing.notUsing := !is.na(skillsUsing.notUsing)]

#Skills impacting life outside BRC
raw[, skillsImpacting.newRelationships := !is.na(skillsImpacting.newRelationships)]
raw[, skillsImpacting.moreFulfilled := !is.na(skillsImpacting.moreFulfilled)]
raw[, skillsImpacting.notImpacting := !is.na(skillsImpacting.notImpacting)]

#Identify as a burner
identifyAsBurnerSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "2" = "No",
         "3" = "Sort of")
}
raw[, identifyAsBurner := sapply(identifyAsBurner, identifyAsBurnerSwitch)]

#Year round activities
# yearRoundActivitiesSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "Yes",
#          "2" = "No",
#          "3" = "I don\'t care")
# }
# raw[, yearRoundActivities := sapply(yearRoundActivities, yearRoundActivitiesSwitch)]

#Donate $1,000
cleanDonate1k <- function(x){
  if(is.na(x)){return(NA)}
  if(grep("[0-9]+", x) == 1){
    y <- as.numeric(gsub("[^0-9]", "", x))
    if(y >= 0 & y <= 1000){
      return(y)
    }
    else{return(NA)}
  }
  else{return(NA)}
}
raw[, donate1k.publicArtUS := sapply(donate1k.publicArtUS, cleanDonate1k)]
raw[, donate1k.publicArtAbroad := sapply(donate1k.publicArtAbroad, cleanDonate1k)]
raw[, donate1k.disasterRelief := sapply(donate1k.disasterRelief, cleanDonate1k)]
raw[, donate1k.planInfrastructureBRC := sapply(donate1k.planInfrastructureBRC, cleanDonate1k)]
raw[, donate1k.lowIncomeTickets := sapply(donate1k.lowIncomeTickets, cleanDonate1k)]
raw[, donate1k.artBRC := sapply(donate1k.artBRC, cleanDonate1k)]
raw[, donate1k.multimediaBRC := sapply(donate1k.multimediaBRC, cleanDonate1k)]
raw[, donate1k.regionals := sapply(donate1k.regionals, cleanDonate1k)]
raw[, donate1k.RIDE := sapply(donate1k.RIDE, cleanDonate1k)]
raw[, donate1k.sustainability := sapply(donate1k.sustainability, cleanDonate1k)]
raw[, donate1k.other := sapply(donate1k.other, cleanDonate1k)]

donate1k.allMissing <- is.na(raw$donate1k.publicArtUS) &
  is.na(raw$donate1k.publicArtAbroad) &
  is.na(raw$donate1k.disasterRelief) & 
  is.na(raw$donate1k.planInfrastructureBRC) &
  is.na(raw$donate1k.lowIncomeTickets) &
  is.na(raw$donate1k.artBRC) &
  is.na(raw$donate1k.multimediaBRC) &
  is.na(raw$donate1k.regionals) &
  is.na(raw$donate1k.RIDE) &
  is.na(raw$donate1k.sustainability) &
  is.na(raw$donate1k.other)

donate1k.sums <- rowSums(cbind(raw$donate1k.publicArtUS,
                               raw$donate1k.publicArtAbroad,
                               raw$donate1k.disasterRelief,
                               raw$donate1k.planInfrastructureBRC,
                               raw$donate1k.lowIncomeTickets,
                               raw$donate1k.artBRC,
                               raw$donate1k.multimediaBRC,
                               raw$donate1k.regionals,
                               raw$donate1k.RIDE,
                               raw$donate1k.sustainability,
                               raw$donate1k.other), na.rm = TRUE)

raw$donate1k.publicArtUS <- ifelse(donate1k.allMissing | 
                                     (donate1k.sums < 100), NA,
                                   ifelse(is.na(raw$donate1k.publicArtUS), 0,
                                          (raw$donate1k.publicArtUS / 
                                             donate1k.sums) * 1000))
raw$donate1k.publicArtAbroad <- ifelse(donate1k.allMissing | 
                                         (donate1k.sums < 100), NA,
                                       ifelse(is.na(raw$donate1k.publicArtAbroad), 0,
                                              (raw$donate1k.publicArtAbroad / 
                                                 donate1k.sums) * 1000))
raw$donate1k.disasterRelief <- ifelse(donate1k.allMissing | 
                                        (donate1k.sums < 100), NA,
                                      ifelse(is.na(raw$donate1k.disasterRelief), 0,
                                             (raw$donate1k.disasterRelief / 
                                                donate1k.sums) * 1000))
raw$donate1k.planInfrastructureBRC <- ifelse(donate1k.allMissing | 
                                               (donate1k.sums < 100), NA,
                                             ifelse(is.na(raw$donate1k.planInfrastructureBRC), 0,
                                                    (raw$donate1k.planInfrastructureBRC / 
                                                       donate1k.sums) * 1000))
raw$donate1k.lowIncomeTickets <- ifelse(donate1k.allMissing | 
                                          (donate1k.sums < 100), NA,
                                        ifelse(is.na(raw$donate1k.lowIncomeTickets), 0,
                                               (raw$donate1k.lowIncomeTickets / 
                                                  donate1k.sums) * 1000))
raw$donate1k.artBRC <- ifelse(donate1k.allMissing | 
                                (donate1k.sums < 100), NA,
                              ifelse(is.na(raw$donate1k.artBRC), 0,
                                     (raw$donate1k.artBRC / 
                                        donate1k.sums) * 1000))
raw$donate1k.multimediaBRC <- ifelse(donate1k.allMissing | 
                                       (donate1k.sums < 100), NA,
                                     ifelse(is.na(raw$donate1k.multimediaBRC), 0,
                                            (raw$donate1k.multimediaBRC / 
                                               donate1k.sums) * 1000))
raw$donate1k.regionals <- ifelse(donate1k.allMissing | 
                                   (donate1k.sums < 100), NA,
                                 ifelse(is.na(raw$donate1k.regionals), 0,
                                        (raw$donate1k.regionals / 
                                           donate1k.sums) * 1000))
raw$donate1k.RIDE <- ifelse(donate1k.allMissing | 
                              (donate1k.sums < 100), NA,
                            ifelse(is.na(raw$donate1k.RIDE), 0,
                                   (raw$donate1k.RIDE / 
                                      donate1k.sums) * 1000))
raw$donate1k.sustainability <- ifelse(donate1k.allMissing | 
                                        (donate1k.sums < 100), NA,
                                      ifelse(is.na(raw$donate1k.sustainability), 0,
                                             (raw$donate1k.sustainability / 
                                                donate1k.sums) * 1000))
raw$donate1k.other <- ifelse(donate1k.allMissing | 
                               (donate1k.sums < 100), NA,
                             ifelse(is.na(raw$donate1k.other), 0,
                                    (raw$donate1k.other / 
                                       donate1k.sums) * 1000))


#First arrived at BRC
firstArrivedBRCSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Before Wed 8/21 (pre-event)",
         "2" = "Wed 8/21 (pre-event)",
         "3" = "Thu 8/22 (pre-event)",
         "4" = "Fri 8/23 (pre-event)",
         "5" = "Sat 8/24 (pre-event)",
         "6" = "Sun 8/25",
         "7" = "Mon 8/26",
         "8" = "Tue 8/27",
         "9" = "Wed 8/28",
         "10" = "Thu 8/29",
         "11" = "Fri 8/30",
         "12" = "Sat 8/31",
         "13" = "Sun 9/01",
         "14" = "Mon 9/02",
         "15" = "Tue 9/03 (post-event)",
         "16" = "After Tue 9/03 (post-event)")
}
raw[, firstArrivedBRC := sapply(firstArrivedBRC, firstArrivedBRCSwitch)]

#Planned to leave BRC
plannedLeaveBRCSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Before Sun 8/25 (pre-event)",
         "2" = "Sun 8/25",
         "3" = "Mon 8/26",
         "4" = "Tue 8/27",
         "5" = "Wed 8/28",
         "6" = "Thu 8/29",
         "7" = "Fri 8/30",
         "8" = "Sat 8/31",
         "9" = "Sun 9/01",
         "10" = "Mon 9/02",
         "11" = "Tue 9/03 (post-event)",
         "12" = "After Tue 9/03 (post-event)")
}
raw[, plannedLeaveBRC := sapply(plannedLeaveBRC, plannedLeaveBRCSwitch)]


#Actually Left BRC
leftBRCSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Before Sun 8/25 (pre-event)",
         "2" = "Sun 8/25",
         "3" = "Mon 8/26",
         "4" = "Tue 8/27",
         "5" = "Wed 8/28",
         "6" = "Thu 8/29",
         "7" = "Fri 8/30",
         "8" = "Sat 8/31",
         "9" = "Sun 9/01",
         "10" = "Mon 9/02",
         "11" = "Tue 9/03 (post-event)",
         "12" = "After Tue 9/03 (post-event)")
}
raw[, leftBRC := sapply(leftBRC, leftBRCSwitch)]

#How did you arrive at BRC
howArriveBRCSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Gate in a vehicle",
         "2" = "Gate in a Burner Express Bus",
         "3" = "Gate with another shuttle service",
         "4" = "Point 1",
         "5" = "BRC airport",
         "6" = "Other")
}
raw[, howArriveBRC := sapply(howArriveBRC, howArriveBRCSwitch)]

#How did you depart BRC
howDepartBRCSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Gate in the same vehicle",
         "2" = "Gate in a different vehicle",
         "3" = "Gate in a Burner Express Bus",
         "4" = "Gate with another shuttle service",
         "5" = "Point 1",
         "6" = "BRC airport",
         "7" = "Other")
}
raw[, howDepartBRC := sapply(howDepartBRC, howDepartBRCSwitch)]

#Fly to BRC
flyToBRCSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "2" = "No")
}
raw[, flyToBRC := sapply(flyToBRC, flyToBRCSwitch)]

#Final Arrival airport
arrivalAirportSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Reno",
         "2" = "Sacramento",
         "3" = "San Francisco",
         "4" = "Oakland",
         "5" = "Las Vegas",
         "6" = "Los Angeles",
         "7" = "Salt Lake City",
         "8" = "Other")
}
raw[, arrivalAirport := sapply(arrivalAirport, arrivalAirportSwitch)]

#Number of people in vehicle


#Number of people under age 13


#Number of people 14 to 17


#Vehicle Type
vehicleTypeSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Motorcycle",
         "2" = "Car",
         "3" = "SUV",
         "4" = "Pick-up truck",
         "6" = "Mini-van",
         "7" = "Full-size van",
         "8" = "Moving truck or box truck",
         "9" = "Semi-truck towing trailer",
         "10" = "Bus",
         "11" = "RV",
         "12" = "Other")
}
raw[, vehicleType := sapply(vehicleType, vehicleTypeSwitch)]

#Vehicle Towing
vehicleTowingSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "No trailer",
         "2" = "Small",
         "3" = "Medium",
         "4" = "Large",
         "5" = "Very large",
         "6" = "Don\'t remember")
}
raw[, vehicleTowing := sapply(vehicleTowing, vehicleTowingSwitch)]

#Vehicle miles

#Identify which unit is mentioned first in text (some people give two 
#  numbers and units)
miRegex <- grep("[Mm][Ii][Ll][Ee]", raw$vehicleMiles)
kmRegex <- grep("([Kk][Mm])|([Kk]ilometer)", raw$vehicleMiles)
unitMiles <- ((miRegex > 0) & (kmRegex == -1)) | 
  ((miRegex > 0) & (kmRegex > 0) & (miRegex < kmRegex))
unitKM <- ((kmRegex > 0) & (miRegex == -1)) | 
  ((kmRegex > 0) & (miRegex > 0) & (kmRegex < miRegex))

#Pull first number given in text
raw[, vehicleMiles := gsub(",", "", vehicleMiles)]
raw$vehicleMiles[!is.na(raw$vehicleMiles)] <- as.numeric(regmatches(raw$vehicleMiles, regexpr("^([0-9]+)", raw$vehicleMiles)))
# raw[, vehicleMiles := as.numeric(regmatches(vehicleMiles, regexpr("^([0-9]+)", vehicleMiles)))]
raw$vehicleMiles[raw$vehiclMiles >= 100000] <- NA

#vehicle miles unit
vehicleMiles.unitSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "miles",
         "2" = "KM")
}
raw[, vehicleMiles.unit := sapply(vehicleMiles.unit, vehicleMiles.unitSwitch)]
raw$vehicleMiles.unit[unitMiles] <- "miles"
raw$vehicleMiles.unit[unitKM] <- "KM"

#Route to BRC
routeToBRCSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "12" = "From Reno, NV",
         "13" = "From Las Vegas, NV",
         "14" = "From San Francisco/Sacramento to RENO to BRC via I-80",
         "15" = "From Washington/Oregon/Northern California on Route 447",
         "16" = "From Los Angeles / San Diego DIRECT to BRC via US-395",
         "17" = "From Los Angeles / San Diego to RENO to BRC via US-395",
         "18" = "From Central or Southern California via US-6/US-95",
         "19" = "From Southern California to Las Vegas to BRC via I-15",
         "20" = "From the Utah border via I-80",
         "21" = "From the Arizona border via US-93/US-95",
         "22" = "Different route",
         "23" = "Don\'t know")
}
raw[, routeToBRC := sapply(routeToBRC, routeToBRCSwitch)]

#Vehicle fuel type
vehicleFuelTypeSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Regular gasoline",
         "2" = "Premium gasoline",
         "3" = "Diesel",
         "4" = "Bio-diesel or other bio-fuel",
         "5" = "Sunshine (solar powered)",
         "6" = "Battery powered",
         "7" = "Hydrogen fuel cell",
         "8" = "I don\'t know")
}
raw[, vehicleFuelType := sapply(vehicleFuelType, vehicleFuelTypeSwitch)]

#MPG
cleanMPG = function(mpg, mpgIndicator, fuelType){
  if(fuelType %in% c("Regular gasoline", "Premium gasoline", "Diesel",
                     "Bio-diesel or other bio-fuel")){
    if(as.numeric(mpgIndicator) == 1 & !is.na(mpg)){
      return(as.numeric(mpg))
    }
    else{return(NA)}
  }
  else{return(NA)}
}
vehicleMPG.cleaned = apply(raw[, c("vehicleFuelType", "vehicleMPG", "vehicleMPG.writeIn")],
                           1, function(x){cleanMPG(x[[3]], x[[2]], x[[1]])})
raw[, vehicleMPG := vehicleMPG.cleaned]
raw[, vehicleMPG.writeIn := NULL]


# vehicleMPGSourceSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "Miles and the gallons for entire trip",
#          "2" = "Miles and the gallons for part of trip",
#          "3" = "Vehicle\'s trip computer",
#          "4" = "MPG value for similar vehicles",
#          "5" = "Know what vehicle gets",
#          "6" = "EPA fuel economy rating for vehicle",
#          "7" = "Asked someone",
#          "8" = "Guessed")
# }
# raw[, vehicleMPGSource := sapply(vehicleMPGSource, vehicleMPGSourceSwitch)]

# #BXB Route
# raw[, BXBRoute.SFToBRC := !is.na(BXBRoute.SFToBRC)]
# raw[, BXBRoute.RenoToBRC := !is.na(BXBRoute.RenoToBRC)]
# raw[, BXBRoute.BRCToSF := !is.na(BXBRoute.BRCToSF)]
# raw[, BXBRoute.BRCToReno := !is.na(BXBRoute.BRCToReno)]
# 
# #BXB Camp
# BXBCampSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "No, I did not camp in Burner Express Camping",
#          "2" = "Yes, and I would camp there again",
#          "3" = "Yes, but I would not camp there again",
#          "4" = "I don\'t know")
# }
# raw[, BXBCamp := sapply(BXBCamp, BXBCampSwitch)]
# 
# #Shuttle in BRC
# shuttleBRCSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "Yes",
#          "2" = "No",
#          "3" = "Don\'t know")
# }
# raw[, shuttleBRC := sapply(shuttleBRC, shuttleBRCSwitch)]
# 
# #BXB Reason
# raw[, BXBReason.cost := !is.na(BXBReason.cost)]
# raw[, BXBReason.notDrive := !is.na(BXBReason.notDrive)]
# raw[, BXBReason.avoidTraffic := !is.na(BXBReason.avoidTraffic)]
# raw[, BXBReason.reduceCongestion := !is.na(BXBReason.reduceCongestion)]
# raw[, BXBReason.previousExperience := !is.na(BXBReason.previousExperience)]
# raw[, BXBReason.other := !is.na(BXBReason.other)]
# 
# #BXB Ticket Price
# BXBTicketPriceSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "Underpriced",
#          "2" = "Overpriced",
#          "3" = "Just right")
# }
# raw[, BXBTicketPrice := sapply(BXBTicketPrice, BXBTicketPriceSwitch)]

#Gerlach Stops
raw[, gerlachStops.BMOffice := !is.na(gerlachStops.BMOffice)]
raw[, gerlachStops.360 := !is.na(gerlachStops.360)]
raw[, gerlachStops.oasis := !is.na(gerlachStops.oasis)]
raw[, gerlachStops.desertClub := !is.na(gerlachStops.desertClub)]
raw[, gerlachStops.highRockPizza := !is.na(gerlachStops.highRockPizza)]
raw[, gerlachStops.granitePoint := !is.na(gerlachStops.granitePoint)]
raw[, gerlachStops.minersClub := !is.na(gerlachStops.minersClub)]
raw[, gerlachStops.brunos := !is.na(gerlachStops.brunos)]
raw[, gerlachStops.joes := !is.na(gerlachStops.joes)]
raw[, gerlachStops.gas := !is.na(gerlachStops.gas)]
raw[, gerlachStops.roadsideFood := !is.na(gerlachStops.roadsideFood)]
raw[, gerlachStops.roadsideGoods := !is.na(gerlachStops.roadsideGoods)]
raw[, gerlachStops.friendsOfBlackRock := !is.na(gerlachStops.friendsOfBlackRock)]
raw[, gerlachStops.other := !is.na(gerlachStops.other)]
raw[, gerlachStops.didNotStop := !is.na(gerlachStops.didNotStop)]

allGerlachStopsNA <- rowSums(raw[, .SD, .SDcols = names(raw) %like% "gerlachStops"][,-16]) == 0
raw[allGerlachStopsNA, (names(raw)[grepl("gerlachStops", names(raw))]) := NA]

#Public Lands Stops
raw[, publicLands.camping := !is.na(publicLands.camping)]
raw[, publicLands.hiking := !is.na(publicLands.hiking)]
raw[, publicLands.boating := !is.na(publicLands.boating)]
raw[, publicLands.hotSprings := !is.na(publicLands.hotSprings)]
raw[, publicLands.windSailing := !is.na(publicLands.windSailing)]
raw[, publicLands.burningManOnly := !is.na(publicLands.burningManOnly)]
raw[, publicLands.other := !is.na(publicLands.other)]

allPublicLandsNA <- rowSums(raw[, .SD, .SDcols = names(raw) %like% "publicLands"][,-8]) == 0
raw[allPublicLandsNA, (names(raw)[grepl("publicLands", names(raw))]) := NA]

#Return to Black Rock Desert outside of Burning Man
returnBlackRockSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "2" = "No",
         "3" = "Maybe")
}
raw[, returnBlackRock := sapply(returnBlackRock, returnBlackRockSwitch)]

#Ticket source
#TODO CHECK KEY, ASSUMING NO KIDS TICKETS
ticketSourceSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "12" = "Main sale or OMG sale",
         "2" = "Stewards sales",
         "13" = "FOMO Sale",
         "3" = "Ticket Aid/Low Income Ticket Program",
         "16" = "Burner Express Bus Plus",
         "14" = "Staff or volunteer credential",
         "15" = "Gifted ticket",
         "8" = "Kid\'s Ticket",
         "4" = "STEP",
         "5" = "Someone I know",
         "6" = "A stranger",
         "7" = "Third party reseller",
         "9" = "No ticket",
         "10" = "Other")
}
raw[, ticketSource := sapply(ticketSource, ticketSourceSwitch)]

#Face value ticket
ticketFaceValueSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Less than face value",
         "2" = "Face value",
         "3" = "More than face value",
         "4" = "Gift/did not pay",
         "5" = "Don\'t know",
         "6" = "Other")
}
raw[, ticketFaceValue := sapply(ticketFaceValue, ticketFaceValueSwitch)]

#Money spent to go to BRC excluding ticket
raw[, spendToGoToBRC := as.numeric(gsub("\\$", "", raw$spendToGoToBRC))]
raw$spendToGoToBRC[raw$spendToGoToBRC > 10000000] = NA

#Money spent in Nevada
raw[, spendNevada.fuel := as.numeric(gsub("\\$", "", raw$spendNevada.fuel))]
raw$spendNevada.fuel[raw$spendNevada.fuel > 1000000] = NA
raw[, spendNevada.food := as.numeric(gsub("\\$", "", raw$spendNevada.food))]
raw$spendNevada.food[raw$spendNevada.food > 1000000] = NA
raw[, spendNevada.lodging := as.numeric(gsub("\\$", "", raw$spendNevada.lodging))]
raw$spendNevada.lodging[raw$spendNevada.loding > 1000000] = NA
raw[, spendNevada.survival := as.numeric(gsub("\\$", "", raw$spendNevada.survival))]
raw$spendNevada.survival[raw$spendNevada.survival > 1000000] = NA
raw[, spendNevada.fun := as.numeric(gsub("\\$", "", raw$spendNevada.fun))]
raw$spendNevada.fun[raw$spendNevada.fun > 1000000] = NA

#Visit Parks or Recreation Areas
visitParksRecreationSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "No",
         "2" = "Not sure",
         "3" = "One place",
         "4" = "More than one place")
}
raw[, visitParksRecreation := sapply(visitParksRecreation, visitParksRecreationSwitch)]

#Sort waste at camp
wasteSortSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "2" = "Someone else did",
         "4" = "Did not sort waste",
         "5" = "Don\'t know")
}
raw[, wasteSort := sapply(wasteSort, wasteSortSwitch)]

# raw[, wasteSort.burnables := !is.na(wasteSort.burnables)]
# raw[, wasteSort.recyclables := !is.na(wasteSort.recyclables)]
# raw[, wasteSort.batteriesElectronics := !is.na(wasteSort.batteriesElectronics)]
# raw[, wasteSort.compostables := !is.na(wasteSort.compostables)]
# raw[, wasteSort.reusables := !is.na(wasteSort.reusables)]
# raw[, wasteSort.other := !is.na(wasteSort.other)]

#Waste taken after leaving BRC
raw[, wasteTaken.collectionOnPlaya := !is.na(wasteTaken.collectionOnPlaya)]
raw[, wasteTaken.GGID := !is.na(wasteTaken.GGID)]
raw[, wasteTaken.exodusProgram := !is.na(wasteTaken.exodusProgram)]
raw[, wasteTaken.stationLandfill := !is.na(wasteTaken.stationLandfill)]
raw[, wasteTaken.paidDumpster := !is.na(wasteTaken.paidDumpster)]
raw[, wasteTaken.friendFamilyDumpster := !is.na(wasteTaken.friendFamilyDumpster)]
raw[, wasteTaken.foundDumpster := !is.na(wasteTaken.foundDumpster)]
raw[, wasteTaken.leftOnRoadPlaya := !is.na(wasteTaken.leftOnRoadPlaya)]
raw[, wasteTaken.home := !is.na(wasteTaken.home)]
raw[, wasteTaken.BXB := !is.na(wasteTaken.BXB)]
raw[, wasteTaken.someoneElse := !is.na(wasteTaken.someoneElse)]
raw[, wasteTaken.other := !is.na(wasteTaken.other)]

#Number of people in camp
numberPeopleInCampSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "1",
         "2" = "2",
         "3" = "3-5",
         "4" = "6-9",
         "5" = "10-19",
         "6" = "20-29",
         "7" = "30-49",
         "8" = "50-99",
         "9" = "100-149",
         "10" = "150-199",
         "11" = "200 or more")
}
raw[, numberPeopleInCamp := sapply(numberPeopleInCamp, numberPeopleInCampSwitch)]

#Number family members
numberFamilySwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "0",
         "2" = "1",
         "3" = "2",
         "4" = "3",
         "5" = "4",
         "6" = "5",
         "7" = "more than 5")
}
raw[, numberFamily := sapply(numberFamily, numberFamilySwitch)]


#Ever brought children
broughtChildrenSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes, and I would again",
         "2" = "Yes, but I would not again",
         "3" = "No, but I would",
         "4" = "No, and I would never")
}
raw[, broughtChildren := sapply(broughtChildren, broughtChildrenSwitch)]


#Stay in an RV
# RVStayedSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "Yes and pumped",
#          "2" = "Yes and not pumped",
#          "3" = "Yes and don\'t know if pumped",
#          "4" = "No",
#          "5" = "Not sure")
# }
# raw[, RVStayed := sapply(RVStayed, RVStayedSwitch)]

#Number of adults in the RV
# RVNumberAdultsSwitch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "1",
#          "2" = "2",
#          "3" = "3",
#          "4" = "4",
#          "5" = "5",
#          "6" = "6",
#          "7" = "7",
#          "8" = "8",
#          "9" = "9",
#          "10" = "10+")
# }
# raw[, RVNumberAdults := sapply(RVNumberAdults, RVNumberAdultsSwitch)]



# #Restrict age
# restrictAge18Switch <- function(x){
#   if(is.na(x)){return(NA)}
#   switch(as.character(x),
#          "1" = "No, all ages",
#          "2" = "Yes, only 18+",
#          "3" = "No opinion")
# }
# raw[, restrictAge18 := sapply(restrictAge18, restrictAge18Switch)]

#Ride a bike
bikeRideSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "4" = "No",
         "2" = "I don\'t know")
}
raw[, bikeRide := sapply(bikeRide, bikeRideSwitch)]

#Whose bike
bikeWhoseSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "My own I brought to Black Rock City",
         "2" = "Someone else's I borrowed with their consent",
         "3" = "Someone else's I borrowed without their consent",
         "4" = "A yellow bike",
         "5" = "Other")
}
raw[, bikeWhose := sapply(bikeWhose, bikeWhoseSwitch)]

#Bike needed repair
bikeNeededRepairSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes, I was able to get it fixed",
         "2" = "Yes, I was unable to get it fixed",
         "3" = "No",
         "4" = "Other")
}
raw[, bikeNeededRepair := sapply(bikeNeededRepair, bikeNeededRepairSwitch)]


#Bike how repair
bikeHowRepairSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "I did it myself",
         "2" = "By someone I know",
         "3" = "By a stranger (not affiliated with an identified bike repair camp)",
         "5" = "By an identified bike repair camp",
         "6" = "Other")
}
raw[, bikeHowRepair := sapply(bikeHowRepair, bikeHowRepairSwitch)]

#Electricity
raw[, electricity.didntUse := !is.na(electricity.didntUse)]
raw[, electricity.batteries := !is.na(electricity.batteries)]
raw[, electricity.solar := !is.na(electricity.solar)]
raw[, electricity.wind := !is.na(electricity.wind)]
raw[, electricity.vehicleGenerator := !is.na(electricity.vehicleGenerator)]
raw[, electricity.campGenerator := !is.na(electricity.campGenerator)]
raw[, electricity.anotherCamp := !is.na(electricity.anotherCamp)]
raw[, electricity.grid := !is.na(electricity.grid)]
raw[, electricity.other := !is.na(electricity.other)]

#Vehicle generator fuel type
generatorFuelTypeVehicleSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Gasoline",
         "2" = "Diesel",
         "3" = "Biodiesel",
         "4" = "Propane",
         "5" = "Other",
         "6" = "Don\'t know")
}
raw[, generatorFuelTypeVehicle := sapply(generatorFuelTypeVehicle, generatorFuelTypeVehicleSwitch)]

#Camp generator fuel type
generatorFuelTypeCampSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Gasoline",
         "2" = "Diesel",
         "3" = "Biodiesel",
         "4" = "Propane",
         "5" = "Other",
         "6" = "Don\'t know",
         "7" = "Multiple types")
}
raw[, generatorFuelTypeCamp := sapply(generatorFuelTypeCamp, generatorFuelTypeCampSwitch)]

#Camp generator output
generatorOutputCampSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "<1kW",
         "2" = "1kW-<2kW",
         "3" = "2kW-<4kW",
         "4" = "4kW-<10kW",
         "5" = "10kW-<20kW",
         "6" = "20kW-<40kW",
         "7" = "40kW+",
         "8" = "Don\'t know")
}
raw[, generatorOutputCamp := sapply(generatorOutputCamp, generatorOutputCampSwitch)]

#Camp street
campStreetSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "A",
         "2" = "B",
         "3" = "C",
         "4" = "D",
         "5" = "E",
         "6" = "F",
         "7" = "G",
         "8" = "H",
         "9" = "I",
         "10" = "J",
         "11" = "K",
         "13" = "Esplanade",
         "14" = "Rod\'s Road",
         "15" = "Center Camp Plaza",
         "16" = "Walk-in camping",
         "17" = "DPW Depot",
         "18" = "ESD",
         "19" = "Airport")
}
raw[, campStreet := sapply(campStreet, campStreetSwitch)]

#Camp radial
campRadialSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "2:00",
         "2" = "2:30",
         "3" = "3:00",
         "4" = "3:30",
         "5" = "4:00",
         "6" = "4:30",
         "7" = "5:00",
         "8" = "5:30",
         "9" = "6:00",
         "10" = "6:30",
         "11" = "7:00",
         "12" = "7:30",
         "13" = "8:00",
         "14" = "8:30",
         "15" = "9:00",
         "16" = "9:30",
         "17" = "10:00")
}
raw[, campRadial := sapply(campRadial, campRadialSwitch)]

#Camp placed
campPlacedSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Yes",
         "2" = "No",
         "3" = "Don\'t know")
}
raw[, campPlaced := sapply(campPlaced, campPlacedSwitch)]

#Camp location
raw[, campLocation.attractions := !is.na(campLocation.attractions)]
raw[, campLocation.playaConditions := !is.na(campLocation.playaConditions)]
raw[, campLocation.friends := !is.na(campLocation.friends)]
raw[, campLocation.space := !is.na(campLocation.space)]
raw[, campLocation.neighbors := !is.na(campLocation.neighbors)]
raw[, campLocation.sameLastYear := !is.na(campLocation.sameLastYear)]
raw[, campLocation.differentLastYear := !is.na(campLocation.differentLastYear)]
raw[, campLocation.quiet := !is.na(campLocation.quiet)]
raw[, campLocation.parties := !is.na(campLocation.parties)]
raw[, campLocation.dontKnow := !is.na(campLocation.dontKnow)]

#Notice sustainability
raw[, sustainabilityNotice.solarMan := !is.na(sustainabilityNotice.solarMan)]
raw[, sustainabilityNotice.fewerGenerators := !is.na(sustainabilityNotice.fewerGenerators)]
raw[, sustainabilityNotice.moreSolarBattery := !is.na(sustainabilityNotice.moreSolarBattery)]
raw[, sustainabilityNotice.solarTrailers := !is.na(sustainabilityNotice.solarTrailers)]
raw[, sustainabilityNotice.campImpact := !is.na(sustainabilityNotice.campImpact)]
raw[, sustainabilityNotice.artImpact := !is.na(sustainabilityNotice.artImpact)]
raw[, sustainabilityNotice.artThemes := !is.na(sustainabilityNotice.artThemes)]
raw[, sustainabilityNotice.education := !is.na(sustainabilityNotice.education)]
raw[, sustainabilityNotice.conversations := !is.na(sustainabilityNotice.conversations)]
raw[, sustainabilityNotice.moop := !is.na(sustainabilityNotice.moop)]
raw[, sustainabilityNotice.mutantVehicle := !is.na(sustainabilityNotice.mutantVehicle)]
raw[, sustainabilityNotice.other := !is.na(sustainabilityNotice.other)]
raw[, sustainabilityNotice.nothing := !is.na(sustainabilityNotice.nothing)]

#Address sustainability
raw[, sustainabilityAddress.carbonFootprint := !is.na(sustainabilityAddress.carbonFootprint)]
raw[, sustainabilityAddress.carbonOffset := !is.na(sustainabilityAddress.carbonOffset)]
raw[, sustainabilityAddress.container := !is.na(sustainabilityAddress.container)]
raw[, sustainabilityAddress.createContent := !is.na(sustainabilityAddress.createContent)]
raw[, sustainabilityAddress.greenCamp := !is.na(sustainabilityAddress.greenCamp)]
raw[, sustainabilityAddress.carpool := !is.na(sustainabilityAddress.carpool)]
raw[, sustainabilityAddress.BXB := !is.na(sustainabilityAddress.BXB)]
raw[, sustainabilityAddress.pooledResources := !is.na(sustainabilityAddress.pooledResources)]
raw[, sustainabilityAddress.HUBS := !is.na(sustainabilityAddress.HUBS)]
raw[, sustainabilityAddress.renewableEnergy := !is.na(sustainabilityAddress.renewableEnergy)]
raw[, sustainabilityAddress.wasteReduction := !is.na(sustainabilityAddress.wasteReduction)]
raw[, sustainabilityAddress.foodWaste := !is.na(sustainabilityAddress.foodWaste)]
raw[, sustainabilityAddress.greyWater := !is.na(sustainabilityAddress.greyWater)]
raw[, sustainabilityAddress.plastic := !is.na(sustainabilityAddress.plastic)]
raw[, sustainabilityAddress.packaging := !is.na(sustainabilityAddress.packaging)]
raw[, sustainabilityAddress.wasteDisposal := !is.na(sustainabilityAddress.wasteDisposal)]
raw[, sustainabilityAddress.helpedOthers := !is.na(sustainabilityAddress.helpedOthers)]
raw[, sustainabilityAddress.recycleCamp := !is.na(sustainabilityAddress.recycleCamp)]
raw[, sustainabilityAddress.leaveNoTrace := !is.na(sustainabilityAddress.leaveNoTrace)]
raw[, sustainabilityAddress.other := !is.na(sustainabilityAddress.other)]
raw[, sustainabilityAddress.nothing := !is.na(sustainabilityAddress.nothing)]

#How important is sustainability
sustainabilityImportantSwitch <- function(x){
  if(is.na(x)){return(NA)}
  switch(as.character(x),
         "1" = "Essential",
         "2" = "Very important",
         "3" = "Somewhat important",
         "4" = "Not important")
}
raw[, sustainabilityImportant := sapply(sustainabilityImportant, sustainabilityImportantSwitch)]

#RIDE
raw[, RIDE.diverseCitizenry := !is.na(RIDE.diverseCitizenry)]
raw[, RIDE.themeCamps := !is.na(RIDE.themeCamps)]
raw[, RIDE.art := !is.na(RIDE.art)]
raw[, RIDE.accessible := !is.na(RIDE.accessible)]
raw[, RIDE.conversations := !is.na(RIDE.conversations)]
raw[, RIDE.other := !is.na(RIDE.other)]
raw[, RIDE.nothing := !is.na(RIDE.nothing)]

#Diversity
raw[, diverse.newBIPOC := !is.na(diverse.newBIPOC)]
raw[, diverse.financialAssistance := !is.na(diverse.financialAssistance)]
raw[, diverse.RIDE := !is.na(diverse.RIDE)]
raw[, diverse.events := !is.na(diverse.events)]
raw[, diverse.peopleOfColorborhood := !is.na(diverse.peopleOfColorborhood)]
raw[, diverse.queerborhood := !is.na(diverse.queerborhood)]
raw[, diverse.offPlaya := !is.na(diverse.offPlaya)]
raw[, diverse.accessbile := !is.na(diverse.accessbile)]
raw[, diverse.conversations := !is.na(diverse.conversations)]
raw[, diverse.other := !is.na(diverse.other)]
raw[, diverse.nothing := !is.na(diverse.nothing)]

#Census
raw[, census.emailLink := !is.na(census.emailLink)]
raw[, census.wordOfMouth := !is.na(census.wordOfMouth)]
raw[, census.knownForYears := !is.na(census.knownForYears)]
raw[, census.signExodus := !is.na(census.signExodus)]
raw[, census.sampled := !is.na(census.sampled)]
raw[, census.fieldNotes := !is.na(census.fieldNotes)]
raw[, census.BMIR := !is.na(census.BMIR)]
raw[, census.burnerProfile := !is.na(census.burnerProfile)]
raw[, census.sticker := !is.na(census.sticker)]
raw[, census.journal := !is.na(census.journal)]
raw[, census.jackrabbit := !is.na(census.jackrabbit)]
raw[, census.emailVolunteer := !is.na(census.emailVolunteer)]
raw[, census.signCamp := !is.na(census.signCamp)]
raw[, census.socialMedia := !is.na(census.socialMedia)]
raw[, census.decompression := !is.na(census.decompression)]
raw[, census.other := !is.na(census.other)]

#Remove administrative variables
raw[, timeStarted := NULL]
raw[, dateSubmitted := NULL]
raw[, status := NULL]
raw[, legacyComments := NULL]
raw[, comments := NULL]
raw[, language := NULL]
raw[, referer := NULL]
raw[, sessionID := NULL]
raw[, userAgent := NULL]
raw[, tags := NULL]
raw[, authorization := NULL]
# raw[, additionalAuthorization := NULL]

fwrite(raw, file = "cleaned2024.tsv", sep = "\t")