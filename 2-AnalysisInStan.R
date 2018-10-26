## This formats and fits the NBI data with Stan
## Stan model may require multiple hours to run

load("output/bridgeData.RData")

library(rstan)
library(tidyverse)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Eliminate rare CR / deckProt Ratings
observationDF4 = observationDF3 %>% filter(rating != "0") %>% filter(rating != "1") %>% filter(rating != "2") %>% filter(climaticRegion %in% c("2","3","4","5","6","7","8","9","10")) %>% filter(as.numeric(deckProt) <=9)

## Create Factors for Explanatory Variables
observationDF4$rating = factor(observationDF4$rating, levels=c("7","3","4","5","6","8","9"))
observationDF4$climaticRegion = factor(observationDF4$climaticRegion, levels=c("5","2","3","4","6","7","8","9","10"))
observationDF4$maintRespCode = factor(observationDF4$maintRespCode, levels=c("1","2","3","4","26","31"))
observationDF4$log10ADTT = log(as.numeric(observationDF4$ADTT)+1,base = 10)
observationDF4$structMat = factor(observationDF4$structMat, levels=c("3","1","2","4","5","6"))
observationDF4$nearSea = as.character(ifelse(observationDF4$seaWaterDist < 3,1,0)) 

## Compile explantory variables in design matrix
ratingMatrixDF = as.data.frame(model.matrix(~ rating + climaticRegion + maintRespCode + deckType + log10ADTT + functClass + structMat + deckProt + nearSea, observationDF4))
designMatrixDF = data.frame(TICR = observationDF4$TICR, eventObserved = observationDF4$eventObserved, stringsAsFactors = FALSE) %>% 
  cbind(ratingMatrixDF) %>% select(-3)


# set.seed(123)  Uncomment to create smaller sample for testing
designMatrixDF = designMatrixDF #%>% sample_n(100000)

# split censored and uncensored observations
survObsDF = designMatrixDF %>% filter(eventObserved == 1) 

censObsDF = designMatrixDF %>% filter(eventObserved == 0) 

# create list of data to be passed to Stan
bridge_dat = list(
  Nobs = nrow(survObsDF),
  Ncen = nrow(censObsDF),
  M_pred = ncol(designMatrixDF) - as.integer(2), #intercept is handled my mu
  yobs = survObsDF$TICR,
  ycen = censObsDF$TICR,
  Xobs_pred = survObsDF %>% select(3:ncol(survObsDF)) %>% as.matrix(),
  Xcen_pred = censObsDF %>% select(3:ncol(censObsDF)) %>% as.matrix()
)

# generate intelligent defaults for Stan
gen_inits <- function() {
  list(
    alpha_raw = 1/10+0.01*rnorm(1),
    beta_pred_raw = 0.01*rnorm(ncol(designMatrixDF) - as.integer(2)),
    mu = rnorm(1)
  )
}

# Create Stan fit object *** takes long time ***
fit <- stan(file="TICRSurvivalModel.stan",data=bridge_dat, seed = 123, init = gen_inits)

# Add names for coefficients
coeffNames = c(paste0("Beta_",names(designMatrixDF[3:ncol(designMatrixDF)])))
names(fit)[1:(ncol(designMatrixDF)-2)] = coeffNames

# Save image for subsequent usage
save.image("output/postStanFit.RData")

