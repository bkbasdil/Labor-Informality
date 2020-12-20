##### PREAMBLE ####
#install.packages("VIM")
#install.packages("Amelia")
#install.packages("rdd")
#install.packages("tidyverse")
#install.packages("broom")
#install.packages("writexl")

library(VIM)
library(Amelia)
library(readxl)
library(rdd)
library(ggplot2)
library(tidyverse)
library(broom)
library(xtable)
library(broom)     
library(writexl)
library(dplyr)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

##### SUMMARY STATISTICS ####
#assume the dataset has already been imported
dataset <- index_law_and_order_informality_government_consumption # isim çok uzundu kýsalttým
summary(dataset)

#CORRELATIONS,
dataset.numerical <- dataset[,c(4,5,7,8,9,10)]

correlations <- cor(dataset.numerical, use = "pairwise.complete.obs")
xtable(round(correlations, 2))

#STANDARD DEVIATIONS
sd(dataset$INDEX)

# INFORMALITY
summary(dataset$INFORMALITY)
sd(dataset$INFORMALITY, na.rm =T)

# NA ANALYSIS
length(which(is.na(dataset$`Hiring regulations and minimum wage`)))/length(dataset$`Hiring regulations and minimum wage`)
length(which(is.na(dataset$`Hiring and firing regulations`)))/length(dataset$`Hiring and firing regulations`)
length(which(is.na(dataset$PENNSize)))/length(dataset$PENNSize)
length(which(is.na(dataset$`Centralized collective bargaining`)))/length(dataset$`Centralized collective bargaining`)
length(which(is.na(dataset$`Hours Regulations`)))/length(dataset$`Hours Regulations`)
length(which(is.na(dataset$`Mandated cost of worker dismissal`)))/length(dataset$`Mandated cost of worker dismissal`)

summary(aggr(dataset))
missmap(dataset, main = "Missing Values", col = c("pink", "snow2"))


##### THE INDEX ####

#NUMBER OF NA OBSERVATIONS IN DATA
number.of.null <- matrix(0, ncol = 1, nrow = nrow(dataset.numerical))
for (i in 1:nrow(dataset.numerical)) {
  for (j in 1:ncol(dataset.numerical)) {
    if(is.na(dataset.numerical[i,j])) {
      
      number.of.null[i] <- number.of.null[i] + 1
      
    }
    
  }
}


#NUMBER OF NON-NA OBSERVATIONS IN DATA
number.of.nonna.variables <- 6 - number.of.null

#WEIGHTED INDEX BY THE NUMBER OF AVAILABLE COMPONENTS
weighted.index <- dataset$SUM/number.of.nonna.variables

xtable(summary(lm(weighted.index~ Year, data = dataset)))



##### INFORMALITY ####
plot(x = dataset$Year, y = dataset$INFORMALITY)
abline(lm(dataset$INFORMALITY~dataset$Year), col ="red")
summary(lm(dataset$INFORMALITY~dataset$Year))
xtable(summary(lm(dataset$INFORMALITY~dataset$Year)))


informality.full <- matrix(NA, ncol=3)
colnames(informality.full) <- colnames(informality)
for (i in 1:nrow(informality)) {
  if(!is.na(informality[i,3])) {
    informality.full <- rbind(informality.full, informality[i,])
  }

}
  


#write_xlsx(as.data.frame(informality.full), "C:\\Users\\Kaan\\Desktop\\informality.full.xlsx")
#EXPORT TO EXCEL
#(as.data.frame(total.data), "C:\\Users\\Kaan\\Desktop\\final.data.xlsx")

        

##### DISCONTINUITY ####

rdis.index <- RDestimate(weighted.index ~ Year, data = dataset, cutpoint = 2009)
summary(rdis.index)

rdis.informality <- RDestimate(INFORMALITY~ Year, data = dataset, cutpoint = 2009)
summary(rdis.informality)


#PRINCIPAL COMPONENT ANALYSIS
prcomp(na.omit(dataset.numerical))

#TABLE TO LATEX CODE
xtable(summary(prcomp(na.omit(dataset.numerical))))