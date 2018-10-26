## Create Plots

load("output/postStanFit.RData")

library(knitr)
library(broom)
library(rethinking)
###function that takes posterior draws of coefficeints and creates 95% pct Interval as string
getHazRateInt = function(vectorX) {
    qLow = quantile(exp(vectorX), probs = 0.025)
    qHigh = quantile(exp(vectorX), probs = 0.975)
    z = paste0(round(qLow, digits = 3)," - ",round(qHigh, digits = 3))
    return(z)
  }

##create interval 
df_of_draws <- as.data.frame(fit)

hazardDF = data.frame(variable = c("Condition Rating",
                                   "--- Rating 3",
                                   "--- Rating 4",
                                   "--- Rating 5",
                                   "--- Rating 6",
                                   "--- Rating 7",
                                   "--- Rating 8",
                                   "--- Rating 9",
                                   "Climatic Region",
                                   "--- Region 2",
                                   "--- Region 3",
                                   "--- Region 4",
                                   "--- Region 5",
                                   "--- Region 6",
                                   "--- Region 7",
                                   "--- Region 8",
                                   "--- Region 9",
                                   "--- Region 10",
                                   "Maintenance Responsibility",
                                   "--- Code 1",
                                   "--- Code 2",
                                   "--- Code 3",
                                   "--- Code 4",
                                   "--- Code 26",
                                   "--- Code 31",
                                   "Deck Structure Type",
                                   "--- Code 1",
                                   "--- Code 2",
                                   "ADTT",
                                   "--- ADTT (log base 10)",
                                   "Functional Class of Inventory Route",
                                   "--- Code 1",
                                   "--- Code 2",
                                   "Structure Type",
                                   "--- Code 1",
                                   "--- Code 2",
                                   "--- Code 3",
                                   "--- Code 4",
                                   "--- Code 5",
                                   "--- Code 6",
                                   "Deck Protection",
                                   "--- Code 0",
                                   "--- Code 1",
                                   "--- Code 2",
                                   "--- Code 3",
                                   "--- Code 4",
                                   "--- Code 6",
                                   "--- Code 7",
                                   "--- Code 8",
                                   "--- Code 9",
                                   "Distance to Seawater",
                                   "--- Sea Further Than 3 km Away",
                                   "--- Sea Within 3 km"), stringsAsFactors = FALSE)

hazardDF$mean = c("",
                  round(mean(exp(df_of_draws$Beta_rating3)),digits=3),
                  round(mean(exp(df_of_draws$Beta_rating4)),digits=3),
                  round(mean(exp(df_of_draws$Beta_rating5)),digits=3),
                  round(mean(exp(df_of_draws$Beta_rating6)),digits=3),
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_rating8)),digits=3),
                  round(mean(exp(df_of_draws$Beta_rating9)),digits=3),
                  "",
                  round(mean(exp(df_of_draws$Beta_climaticRegion2)),digits=3),
                  round(mean(exp(df_of_draws$Beta_climaticRegion3)),digits=3),
                  round(mean(exp(df_of_draws$Beta_climaticRegion4)),digits=3),
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_climaticRegion6)),digits=3),
                  round(mean(exp(df_of_draws$Beta_climaticRegion7)),digits=3),
                  round(mean(exp(df_of_draws$Beta_climaticRegion8)),digits=3),
                  round(mean(exp(df_of_draws$Beta_climaticRegion9)),digits=3),
                  round(mean(exp(df_of_draws$Beta_climaticRegion10)),digits=3),
                  "",
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_maintRespCode2)),digits=3),
                  round(mean(exp(df_of_draws$Beta_maintRespCode3)),digits=3),
                  round(mean(exp(df_of_draws$Beta_maintRespCode4)),digits=3),
                  round(mean(exp(df_of_draws$Beta_maintRespCode26)),digits=3),
                  round(mean(exp(df_of_draws$Beta_maintRespCode31)),digits=3),
                  "",
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_deckType2)),digits=3),
                  "",
                  round(mean(exp(df_of_draws$Beta_log10ADTT)),digits=3),
                  "",
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_functClass2)),digits=3),
                  "",
                  round(mean(exp(df_of_draws$Beta_structMat1)),digits=3),
                  round(mean(exp(df_of_draws$Beta_structMat2)),digits=3),
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_structMat4)),digits=3),
                  round(mean(exp(df_of_draws$Beta_structMat5)),digits=3),
                  round(mean(exp(df_of_draws$Beta_structMat6)),digits=3),
                  "",
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_deckProt1)),digits=3),
                  round(mean(exp(df_of_draws$Beta_deckProt2)),digits=3),
                  round(mean(exp(df_of_draws$Beta_deckProt3)),digits=3),
                  round(mean(exp(df_of_draws$Beta_deckProt4)),digits=3),
                  round(mean(exp(df_of_draws$Beta_deckProt6)),digits=3),
                  round(mean(exp(df_of_draws$Beta_deckProt7)),digits=3),
                  round(mean(exp(df_of_draws$Beta_deckProt8)),digits=3),
                  round(mean(exp(df_of_draws$Beta_deckProt9)),digits=3),
                  "",
                  "---Referent---",
                  round(mean(exp(df_of_draws$Beta_nearSea1)),digits=3))

hazardDF$pctInterval = c("",
                         getHazRateInt(df_of_draws$Beta_rating3),
                         getHazRateInt(df_of_draws$Beta_rating4),
                         getHazRateInt(df_of_draws$Beta_rating5),
                         getHazRateInt(df_of_draws$Beta_rating6),
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_rating8),
                         getHazRateInt(df_of_draws$Beta_rating9),
                         "",
                         getHazRateInt(df_of_draws$Beta_climaticRegion2),
                         getHazRateInt(df_of_draws$Beta_climaticRegion3),
                         getHazRateInt(df_of_draws$Beta_climaticRegion4),
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_climaticRegion6),
                         getHazRateInt(df_of_draws$Beta_climaticRegion7),
                         getHazRateInt(df_of_draws$Beta_climaticRegion8),
                         getHazRateInt(df_of_draws$Beta_climaticRegion9),
                         getHazRateInt(df_of_draws$Beta_climaticRegion10),
                         "",
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_maintRespCode2),
                         getHazRateInt(df_of_draws$Beta_maintRespCode3),
                         getHazRateInt(df_of_draws$Beta_maintRespCode4),
                         getHazRateInt(df_of_draws$Beta_maintRespCode26),
                         getHazRateInt(df_of_draws$Beta_maintRespCode31),
                         "",
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_deckType2),
                         "",
                         getHazRateInt(df_of_draws$Beta_log10ADTT),
                         "",
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_functClass2),
                         "",
                         getHazRateInt(df_of_draws$Beta_structMat1),
                         getHazRateInt(df_of_draws$Beta_structMat2),
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_structMat4),
                         getHazRateInt(df_of_draws$Beta_structMat5),
                         getHazRateInt(df_of_draws$Beta_structMat6),
                         "",
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_deckProt1),
                         getHazRateInt(df_of_draws$Beta_deckProt2),
                         getHazRateInt(df_of_draws$Beta_deckProt3),
                         getHazRateInt(df_of_draws$Beta_deckProt4),
                         getHazRateInt(df_of_draws$Beta_deckProt6),
                         getHazRateInt(df_of_draws$Beta_deckProt7),
                         getHazRateInt(df_of_draws$Beta_deckProt8),
                         getHazRateInt(df_of_draws$Beta_deckProt9),
                         "",
                         "---Referent---",
                         getHazRateInt(df_of_draws$Beta_nearSea1))

kable(hazardDF, align = c("l","c","c"), caption = "Table XXX: Hazard ratio estimates.")



## Visualizing Survival Curves

library(ggplot2)
library(scales)
library(tidyverse)
library(ggthemes)

plotDF = hazardDF %>% mutate(description = c(codingDF$Description[1:28],"","ADTT = 10",codingDF$Description[29:51]))
plotDF$coefType = ""

for (i in 1:nrow(plotDF)){
  plotDF$coefType[i] = ifelse(substr(plotDF$variable[i],1,1) == "-",
                             plotDF$coefType[i-1],
                             plotDF$variable[i])
}

###add in other adttvalues
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

r = 31###row to start inserting
for (i in 2:4){
  newrow = c("--- ADTT",as.numeric(plotDF[r-1,2])*i,"",paste0("ADTT = ",prettyNum(10^i,big.mark=",", scientific = FALSE)),"ADTT")
  plotDF = insertRow(plotDF,newrow,r-2+i)
}

mu = round(mean(df_of_draws$mu),digits=3)
coefTypes = unique(plotDF$coefType)

plotArray = list()
count = 0  ###store plot number
###start for loop for each coefficient type
for (i in coefTypes){
count=count+1  
###loop over each class of coef type (e.g. condition rating, etc.)
plotDF2 = plotDF %>% filter(substr(variable,1,1) == "-") %>%
  mutate(condition = description) %>%
  mutate(shapeParam = round(mean(df_of_draws$alpha),digits=3)) %>% ##add in ADTT to linear scale on next line
  mutate(linearScale = ifelse(substr(variable,1,8) == "--- ADTT",0,log(as.numeric((plotDF$mean[31]))))) %>%
  mutate(linearScale = linearScale + mu + ifelse(is.na(as.numeric(mean)),log(1),log(as.numeric(mean)))) %>%
  select(coefType,condition,shapeParam,linearScale) %>%
  filter(coefType == i) %>% 
  arrange(linearScale) %>%
  mutate(condition = factor(condition, levels = condition))

###this function
survProb = function(t, alpha, linearScale) {
  exp( -t^alpha * exp(linearScale))
  }

### create plots
plotDF3 = plotDF2 %>% left_join(expand.grid(time = seq(0,20,0.01), condition = plotDF2$condition, stringsAsFactors = FALSE)) %>%
  mutate(survProb = survProb(time, shapeParam, linearScale))

plot1 = ggplot(plotDF3, aes(x = time, y=survProb, color = condition)) + 
  geom_point(size = 0.5) + ggtitle(paste0("Comparison of ",i," Values")) + scale_colour_tableau() +
  theme(axis.title = element_text(size = 9), axis.text = element_text(size = 9), plot.title = element_text(size = 10), legend.justification = c(0,0.5), legend.text = element_text(size = 9), legend.title = element_blank(), panel.grid.major = element_line(colour = "grey"), legend.box.margin=margin(-10,-10,-10,-10)) + ylab("Survival Probability") + xlab("Time in Condition(yrs)") + xlim(0,20) + ylim(0,1) +
  guides(colour = guide_legend(override.aes = list(size=1.5)))
plotArray[[count]] = plot1

}

######end FOR loop

library(cowplot)
plot_grid(plotArray[[1]],plotArray[[2]],align="v", ncol =1)
plot_grid(plotArray[[3]],plotArray[[5]],align="v", ncol =1)
plot_grid(plotArray[[7]],plotArray[[8]],align="v", ncol =1)
plot_grid(plotArray[[4]],plotArray[[6]],plotArray[[9]],align="v", ncol =1)

library(rstan)
library(tidyverse)
library("bayesplot")
posterior <- as.matrix(fit)
colnames(posterior) = gsub("structMat", "strucType", colnames(posterior))
coeffNames = gsub("structMat", "strucType", coeffNames)


plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
plotMCMC = mcmc_areas(posterior, 
           pars = c(coeffNames), 
           prob = 0.95) + plot_title + grid_lines("grey80",0.05)
  
print(plotMCMC)
