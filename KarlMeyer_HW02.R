#09.15.2014
#Karl Meyer
#CMDA 3654
#HW 02
#09.15.2014

#Problem 1:
setwd("C:\\Karl's Work\\CMDA 3654")
load('phsample.RData')

#Problem 2: 
#This data contains personal data including: occupation, level of education, 
#personal income, and many other demographics variables.

#Problem 3:
#the following code makes a subset of data rows matching detailed employment conditions
psub = subset(dpus,with(dpus,(PINCP>1000)&(ESR==1)&   
                          (PINCP<=250000)&(PERNP>1000)&(PERNP<=250000)&
                          (WKHP>=40)&(AGEP>=20)&(AGEP<=50)&
                          (PWGTP1>0)&(COW %in% (1:7))&(SCHL %in% (1:24))))
psub$SEX = as.factor(ifelse(psub$SEX==1,'M','F'))   #reencodes from 1/2 to M/F
psub$SEX = relevel(psub$SEX,'M')   #makes the reference sex M so F encodes a difference from M in models
cowmap <- c("Employee of a private for-profit",
            "Private not-for-profit employee",
            "Local government employee",
            "State government employee",
            "Federal government employee",
            "Self-employed not incorporated",
            "Self-employed incorporated")
psub$COW = as.factor(cowmap[psub$COW])   #reencodes class of worker into a more readable form
psub$COW = relevel(psub$COW,cowmap[1])
#reencode education info into a more readable form and fewer levels 
schlmap = c(   
  rep("no high school diploma",15),
  "Regular high school diploma",
  "GED or alternative credential",
  "some college credit, no degree",
  "some college credit, no degree",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional degree",
  "Doctorate degree")
psub$SCHL = as.factor(schlmap[psub$SCHL])
psub$SCHL = relevel(psub$SCHL,schlmap[1])
dtrain = subset(psub,ORIGRANDGROUP >= 500)
dtest = subset(psub,ORIGRANDGROUP < 500)
summary(dtrain$COW)     #this tabulates the distribution of categories of work

#Problem 4:
url <- "http://www.repole.com/sun4cast/stats/cfb20130907.xml"
dataprob4 <- xmlToDataFrame(url)
