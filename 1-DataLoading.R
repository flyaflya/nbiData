# Code prepared for Journal of Infrastructure Systems 
# @article{bridgeDeckTBA,
#   title={Bayesian Survival Analysis for US Concrete Highway Bridge Decks},
#   author={Fleischhacker, Adam and Ghonima, Omar and Schumacher, Thomas},
#   journal={Journal of infrastructure systems},
#   volume={UNDER REVIEW},
#   number={UNDER REVIEW},
#   pages={UNDER REVIEW},
#   year={UNDER REVIEW},
#   publisher={American Society of Civil Engineers}
# }
#install.packages("openxlsx", dependencies = TRUE)
library("openxlsx")
library("tidyverse")

##read in the raw data from the supplied Excel file
rawDF = read.xlsx(xlsxFile = "./NBI.xlsx",
                  startRow = 4, 
                  colNames = FALSE, 
                  cols = c(1,2,3,4,5,8,9,13,16,17,19,20:42))

##supply colimn names                           
names(rawDF) = c("structNo","countyID","maintRespCode","functClass","yearBuilt","deckType","structMat","deckProt","ADTT","climaticRegion","seaWaterDist",paste0("y",as.character(1992:2014)))

###make unique bridge identifier
rawDF2 = rawDF %>% distinct() %>% mutate(rowNum = row_number()) %>% mutate(bridgeID = paste0(yearBuilt,"-",countyID,"-",structNo,"-",rowNum)) %>% distinct()

##make the data tidy
bridgeDF = rawDF2 %>% select(bridgeID, countyID, maintRespCode, deckType, ADTT, climaticRegion, functClass, yearBuilt, deckType, structMat, deckProt, seaWaterDist) %>%
  mutate(countyID = as.character(countyID),
         maintRespCode = as.character(maintRespCode),
         deckType = as.character(deckType),
         ADTT = as.character(ADTT),
         climaticRegion = as.character(climaticRegion),
         functClass = as.character(functClass),
         yearBuilt = as.integer(yearBuilt),
         deckType = as.character(deckType),
         structMat = as.character(structMat),
         deckProt = as.character(deckProt))

##make bridge rating data
observationDF = rawDF2 %>%
  select(bridgeID, y1992:y2014) %>% mutate_all(as.character)  %>%
  gather(y1992:y2014, key = "year", value = "rating") %>%
  mutate(year = as.numeric(substr(year,start = 2,stop = 5)), rating = as.numeric(rating)) %>%
  group_by(bridgeID) %>%
  arrange(bridgeID,year) %>%
  mutate(rating = ifelse(is.nan(rating),NA,rating))#change NAN's to bad data

#identify censored observations using 999 code
observationDF2 = observationDF %>%
  mutate(rateDiff = rating - lag(rating),
         revRateDiff = lead(rating)-rating,
         rateDiffCode = ifelse(is.na(rateDiff)|is.na(revRateDiff)|revRateDiff>0,999,rateDiff),
         altRating = ifelse(is.na(rating),999,rating)) %>%
  mutate(ratingGrp=cumsum(c(1, diff(altRating)!=0)))

##get TICR for bridges -- extracting possible predictors
observationDF3 = observationDF2 %>%
  group_by(bridgeID, rating, ratingGrp) %>%
  summarize(maxRateDiff = max(rateDiffCode), TICR = n()) %>%
  filter(!is.na(rating)) %>% left_join(bridgeDF) %>%
  mutate(eventObserved = ifelse(maxRateDiff == 999, 0, 1)) %>%
  select(bridgeID, rating, TICR, eventObserved, climaticRegion,maintRespCode,deckType,ADTT,functClass, yearBuilt, deckType, structMat, deckProt, seaWaterDist) %>%
  ungroup() %>%
  mutate(rating = as.character(rating))

###save to file  
save(observationDF3, file = "output/bridgeData.RData")

  
