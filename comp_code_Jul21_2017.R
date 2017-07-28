##############################
####The Library of Babel######
##############################
library(foreign)
library(plyr)
library(car)
library(psych)
library(stargazer)
library(xtable)
library(stats)
library(multilevel)
library(texreg)
library(ggplot2)
library(nlme)
library(DataCombine)
#############################
#########DATASETS############
#############################
#begin with WVS....check at each point for dropped observations
##Construct country aggregates for WVS; note that numeric "scores" need to be constructed from the survey responses
WVS<-read.dta("/Users/Kevin/Documents/WVS_Longitudinal_1981_2014_stata_v2015_04_18.dta")
##Note...try to preseve.. 66 cases
##note quirk...S003A gives E/W Germany instead of S003's Germany
#length(unique(WVS$S003[WVS$C008=="3"]))
#####also, something weird with Bosnia...nonetheless, seems a quick rename solves
WVS$S003[WVS$S003=="Bosnia"]<-"Bosnia"
#unique(WVS$S003[WVS$C008=="3"])
#summary(WVS$C008[WVS$S003=="Bosnia"])
####now make a usuable numeric
WVS$C008<-mapvalues(WVS$C008, from = c("It´s leisure that makes life worth living, not work", "2", "3", "4", "Work is what makes life worth living, not leisure"), to = c("1", "2", "3", "4", "5"))
###Begin creation of file with Work Value scores
WVS_Work_Leisure_Scores<-aggregate(as.numeric(as.character(WVS$C008[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"])), by=list(WVS$S003[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"]), FUN=mean, na.rm=TRUE)
##This will be our cross-country work-ethic index: "1:It´s leisure that makes life worth living, not work 2:2 3:3 4:4 5:Work is what makes life worth living, not leisure -5:Missing; Unknown -4:Not asked in survey -3:Not applicable -2:No answer -1:Don´t know
WVS_Work_Leisure_Scores <- rename(WVS_Work_Leisure_Scores, c(Group.1="Country", x="Work_Index"))
WVS_Work_Leisure_Scores<- WVS_Work_Leisure_Scores[order(WVS_Work_Leisure_Scores$Country),]

###create index of four and five responses as proportion
WVS_High<-aggregate(as.numeric(as.character(WVS$C008[WVS$C008=="4"|WVS$C008=="5"])), by=list(WVS$S003[WVS$C008=="4"|WVS$C008=="5"]), FUN=length)
WVS_High <- rename(WVS_High, c(Group.1="Country", x="Four_Five_Work_Index"))
WVS_High<- WVS_High[order(WVS_High$Country),]
###create index of one and two responses as proportion
WVS_Low<-aggregate(as.numeric(as.character(WVS$C008[WVS$C008=="1"|WVS$C008=="2"])), by=list(WVS$S003[WVS$C008=="1"|WVS$C008=="2"]), FUN=length)
WVS_Low <- rename(WVS_Low, c(Group.1="Country", x="One_Two_Work_Index"))
WVS_Low<- WVS_Low[order(WVS_Low$Country),]
#create a grand total
WVS_All<-aggregate(as.numeric(as.character(WVS$C008[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"])), by=list(WVS$S003[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"]), FUN=length)
WVS_All <- rename(WVS_All, c(Group.1="Country", x="All"))
WVS_All<- WVS_All[order(WVS_All$Country),]
#create a median
WVS_Median<-aggregate(as.numeric(as.character(WVS$C008[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"])), by=list(WVS$S003[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"]), FUN=median)
WVS_Median <- rename(WVS_Median, c(Group.1="Country", x="Median"))
WVS_Median<- WVS_Median[order(WVS_Median$Country),]


###Variances
WVS_Variance<-aggregate(as.numeric(as.character(WVS$C008[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"])), by=list(WVS$S003[WVS$C008=="1"|WVS$C008=="2"|WVS$C008=="3"|WVS$C008=="4"|WVS$C008=="5"]), FUN=var)
WVS_Variance <- rename(WVS_Variance, c(Group.1="Country", x="Var"))
WVS_Variance<- WVS_Variance[order(WVS_Variance$Country),]


#now merge all together
WVS_Work_Leisure_Scores<-merge(WVS_Work_Leisure_Scores, WVS_High, by=c("Country"), all = TRUE)
WVS_Work_Leisure_Scores<-merge(WVS_Work_Leisure_Scores, WVS_Low, by=c("Country"), all = TRUE)
WVS_Work_Leisure_Scores<-merge(WVS_Work_Leisure_Scores, WVS_All, by=c("Country"), all = TRUE)
WVS_Work_Leisure_Scores<-merge(WVS_Work_Leisure_Scores, WVS_Median, by=c("Country"), all = TRUE)
WVS_Work_Leisure_Scores<-merge(WVS_Work_Leisure_Scores, WVS_Variance, by=c("Country"), all = TRUE)

WVS_Work_Leisure_Scores$prop_high<-WVS_Work_Leisure_Scores$Four_Five_Work_Index/WVS_Work_Leisure_Scores$All
WVS_Work_Leisure_Scores$prop_low<-WVS_Work_Leisure_Scores$One_Two_Work_Index/WVS_Work_Leisure_Scores$All
WVS_Work_Leisure_Scores$high_prop_low<-WVS_Work_Leisure_Scores$Four_Five_Work_Index/WVS_Work_Leisure_Scores$One_Two_Work_Index
WVS_Work_Leisure_Scores$median_mean<-WVS_Work_Leisure_Scores$Median/WVS_Work_Leisure_Scores$Work_Index
#for future merging
WVS_Work_Leisure_Scores$Country<-revalue(
  WVS_Work_Leisure_Scores$Country,
  c("Bosnia"="Bosnia and Herzegovina", "Kyrgyzstan"="Kyrgyz Republic",
    "South Korea"="Korea, Rep.","Iran"="Iran, Islamic Rep.",
    "Russia"="Russian Federation", "Macedonia"="Macedonia, FYR",
    "Egypt"="Egypt, Arab Rep.","Venezuela"="Venezuela, RB",
    "Czech Rep." = "Czech Republic", "Viet Nam" = "Vietnam",
    "Dominican Rep." = "Dominican Republic", "Slovakia" = "Slovak Republic"))
WVS_Work_Leisure_Scores
#############################
###########INFORMAL##########
#############################
char_data <- read.csv("~/GitHub/SSHRC_Replication_File/case_informal.csv", header = TRUE, stringsAsFactors = F)
num_data <- data.frame(data.matrix(char_data))
numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
final_data <- data.frame(num_data[,numeric_columns], char_data[,!numeric_columns])
final_data<-rename(final_data, c(char_data....numeric_columns.="Country"))
isn_long<-reshape(final_data,
                  varying = c("X1999","X2000","X2001", "X2002", "X2003", "X2004","X2005","X2006", "X2007"),
                  times = c("X1999","X2000","X2001", "X2002", "X2003", "X2004","X2005","X2006", "X2007"),
                  timevar = "year", direction="long", idvar="Country", v.names = "Informal")
isn_long$Country<-revalue(isn_long$Country, c("Bosnia & Herzegovina"="Bosnia and Herzegovina"))
isn_long<-rename(isn_long, c(year="Year"))
#prep Informal Sector for merge
isn_long$Year <- gsub("X", "", paste(isn_long$Year))

#########################
#####Ensure Merge########
#########################
###as revealed below, problem countries...see if this corrects

#added data set for general econ data
international_economic_data<-read.csv("~/GitHub/SSHRC_Replication_File/international_economy.csv", header = TRUE)
international_economic_data$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.<-as.numeric(gsub(",", "", international_economic_data$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.))
international_economic_data$GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.<-as.numeric(gsub(",", "", international_economic_data$GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.))
international_economic_data$GDP.per.capita.growth..annual.....NY.GDP.PCAP.KD.ZG.<-as.numeric(gsub(",", "", international_economic_data$GDP.per.capita.growth..annual.....NY.GDP.PCAP.KD.ZG.))
international_economic_data$GINI.index..World.Bank.estimate...SI.POV.GINI.<-as.numeric(gsub(",", "", international_economic_data$GINI.index..World.Bank.estimate...SI.POV.GINI.))
international_economic_data$GDP..constant.2005.US....NY.GDP.MKTP.KD.<-as.numeric(gsub(",", "", international_economic_data$GDP..constant.2005.US....NY.GDP.MKTP.KD.))
international_economic_data$Population..total..SP.POP.TOTL.<-as.numeric(gsub(",", "", international_economic_data$Population..total..SP.POP.TOTL.))

#####
####summary(as.numeric(international_economic_data$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.))
#international_economic_data_test<-na.omit(international_economic_data[ ,c("Country", "Year", "GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.",
# "GDP..constant.2005.US....NY.GDP.MKTP.KD.", "GDP.per.capita.growth..annual.....NY.GDP.PCAP.KD.ZG.")])

#View(international_economic_data_test)
names(international_economic_data)[names(international_economic_data)=="Time"] <- "Year"
names(international_economic_data)[names(international_economic_data)=="Country.Name"] <- "Country"
#now merge general econ into WVS
WVS_Econ<-merge(WVS_Work_Leisure_Scores, international_economic_data, by = c("Country"), all = TRUE)
#unique(WVS_Informal_Econ$Country)
#test_set<-na.omit(WVS_Econ[ ,c("Country", "Work_Index", "prop_high", "ISO", "Year")])
##Down to Taiwan only missing
#unique(international_economic_data$Country)
#unique(WVS_Work_Leisure_Scores$Country)
#results1 = setdiff(WVS_Work_Leisure_Scores$Country, test_set$Country)
#results1

WVS_Econ_Informal<-merge(WVS_Econ, isn_long, by = c("Country", "Year"), all = TRUE)
#test_set<-na.omit(WVS_Econ_Informal[ ,c("Country", "Work_Index", "prop_high", "ISO", "GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.")])
#note, drops Montenegro, Puerto Rico and Serbia from WVS...these do not exist in Informal
#unique(isn_long$Country)
#unique(WVS_Work_Leisure_Scores$Country)
#results1 = setdiff(WVS_Work_Leisure_Scores$Country, test_set$Country)
#results1
#okay, tax data. this shit is key.
international_tax_data<-read.csv("~/Desktop/Network Data Construction/ICTDGRD_June2016_CentralGeneralMerged/Merged-Table 1.csv")
#steps to merge, requires new names
#head(international_tax_data)
names(international_tax_data)[names(international_tax_data)=="Calendar.year..nearest."] <- "Year"
WVS_Econ_Informal_Tax<-merge(WVS_Econ_Informal, international_tax_data, by = c("Country", "Year"), all = TRUE)
##okay....continue to only drop Taiwan
#test_set<-na.omit(WVS_Econ_Informal_Tax[ ,c("Country", "Work_Index", "prop_high", "GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.", "Income")])
#results1 = setdiff(WVS_Work_Leisure_Scores$Country, test_set$Country)
#results1

##add polity score
polity<-read.csv("~/Desktop/Network Data Construction/Polity/p4v2014.csv")
#head(polity)
polity<-rename(polity, c(year="Year", country="Country"))
polity$Country<-as.factor(polity$Country)

polity$Country<-revalue(polity$Country, c("Bosnia"="Bosnia and Herzegovina", "Kyrgyzstan"="Kyrgyz Republic", "Korea South"="Korea, Rep.",
                                          "Iran"="Iran, Islamic Rep.", "Russia"="Russian Federation", "Macedonia"="Macedonia, FYR", "Egypt"="Egypt, Arab Rep.",
                                          "Venezuela"="Venezuela, RB"))
####NAME Doctor_Who since all of time and space... well...
Doctor_Who<-merge(WVS_Econ_Informal_Tax, polity, by=c("Country", "Year"), all = TRUE)
#okay, appears no option but to drop Puerto Rico due to abscence from polity
#test_set<-na.omit(Doctor_Who[ ,c("Country", "Work_Index", "prop_high", "GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.", "Income", "polity2")])
#results1 = setdiff(WVS_Work_Leisure_Scores$Country, test_set$Country)
#results1

#######In sum, losing two cases in the WVS, Puerto Rico (no polity, but tax)
###and Taiwan (polity, no tax). Only
#possible good side, both are pretty middling in terms of work ethic scores
#add institutional features
electoral<-read.csv("/Users/Kevin/Downloads/DPI2012.csv")
electoral<-rename(electoral, c(countryname="Country", year="Year"))
Doctor_Who<-merge(Doctor_Who, electoral, by = c("Country", "Year"), all = TRUE)

################################
####### The #### Dataverse #####
#########Is now merged #########
################################
########Blimey##################

####################################
#####Create Suite of Variables######
####################################
Doctor_Who$Indiv_inc_prof<-as.numeric(sub("%", "", Doctor_Who$Non.resource.component.of.taxes.on.income..profits..and.capital.gains))
Doctor_Who$Income_Pro<-as.numeric(sub("%", "", Doctor_Who$Income))
Doctor_Who$Sales<-as.numeric(sub("%", "", Doctor_Who$Taxes.on.goods.and.services..of.which.Taxes.on.Sales))
Doctor_Who$Indiv_Sales<-Doctor_Who$Income_Pro/Doctor_Who$Sales
Doctor_Who$Indiv_Sales[Doctor_Who$Indiv_Sales=="Inf"]<-NA
#summary(Doctor_Who$Indiv_Sales)

Doctor_Who$GDP_cap<-as.numeric(Doctor_Who$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.)
Doctor_Who$GINI<-as.numeric(Doctor_Who$GINI.index..World.Bank.estimate...SI.POV.GINI.)
Doctor_Who$Growth<-as.numeric(Doctor_Who$GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.)
Doctor_Who$UE<-as.numeric(Doctor_Who$Unemployment..total....of.total.labor.force...modeled.ILO.estimate...SL.UEM.TOTL.ZS.)


Doctor_Who$agri<-as.numeric(gsub(",", "", Doctor_Who$Employment.in.agriculture....of.total.employment...SL.AGR.EMPL.ZS.))

Doctor_Who$GDP<-as.numeric(Doctor_Who$GDP..constant.2005.US....NY.GDP.MKTP.KD.)
Doctor_Who$Year<-as.numeric(Doctor_Who$Year)
Doctor_Who$Informal<-as.numeric(Doctor_Who$Informal)

#####polity dummy...update to literature recommendation of 6 cutoff
Doctor_Who$polity_dummy<-as.numeric(Doctor_Who$polity2>5)

########
########
#coded up to line 195 for June 14


Doctor_Who$parreg_dummy<-as.numeric(Doctor_Who$parreg>3)
Doctor_Who$prop_high_dummy<-as.numeric(Doctor_Who$prop_high>.5)

####okay, these will be clutch for RDD
###in all case go one year back from the date... then add number of lag... diff is how much higher/lower years forward is from one year back
Doctor_Who$polity_change <- ave(Doctor_Who$polity2 , list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i)))
Doctor_Who$polity_change_four<-as.numeric(Doctor_Who$polity_change>4|as.numeric(Doctor_Who$polity_change<4))
Doctor_Who$polity_change_dum <- ave(Doctor_Who$polity_dummy , list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i)))
Doctor_Who$become_democ<-as.numeric(Doctor_Who$polity_change_dum>0)
Doctor_Who$become_dict<-as.numeric(Doctor_Who$polity_change_dum<0)
Doctor_Who$polity_change_abs<-as.numeric(Doctor_Who$polity_change_dum!=0)
Doctor_Who$polity_change_dum<-as.factor(Doctor_Who$polity_change_dum)
Doctor_Who$dummy_direction_demo<-as.numeric(Doctor_Who$polity_change>0)
Doctor_Who$dummy_direction_dict<-as.numeric(Doctor_Who$polity_change<0)
Doctor_Who$weightings_polity_change<-Doctor_Who$polity_change/20
Doctor_Who$threes<-(1-Doctor_Who$prop_high-Doctor_Who$prop_low)

#add institutional features
electoral<-read.csv("/Users/Kevin/Downloads/DPI2012.csv")
electoral<-rename(electoral, c(countryname="Country", year="Year"))
Doctor_Who<-merge(Doctor_Who, electoral, by = c("Country", "Year"), all = TRUE)





Doctor_Who$one_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 1)))
Doctor_Who$two_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 2)))
Doctor_Who$three_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 3)))
Doctor_Who$four_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 4)))
Doctor_Who$five_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 5)))
Doctor_Who$six_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 6)))
#Doctor_Who$seven_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 7)))
#Doctor_Who$eight_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 8)))
#Doctor_Who$nine_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 9)))
#Doctor_Who$ten_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 10)))
#Doctor_Who$eleven_yr_inc_change <- ave(Doctor_Who$Income_Pro, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 11)))
Doctor_Who$AVEfive_yr_inc_change<-rowMeans(Doctor_Who[,c("one_yr_inc_change",
                                                         "two_yr_inc_change",
                                                         "three_yr_inc_change",
                                                         "four_yr_inc_change", 
                                                         "five_yr_inc_change",
                                                         "six_yr_inc_change")],
                                           na.rm = TRUE)


head(na.omit(Doctor_Who[,c("Country", "Year", "Income_Pro", "three_yr_inc_change")]))

Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_n1_inc_ratio",
                  slideBy = -1)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_n2_inc_ratio",
                  slideBy = -2)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_n3_inc_ratio",
                  slideBy = -3)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_n4_inc_ratio",
                  slideBy = -4)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_n5_inc_ratio",
                  slideBy = -5)
Doctor_Who$AVE_inc_n5years<-rowMeans(Doctor_Who[,c("lead_n1_inc_ratio",
                                                         "lead_n2_inc_ratio",
                                                         "lead_n3_inc_ratio",
                                                         "lead_n4_inc_ratio", 
                                                         "lead_n5_inc_ratio")],
                                           na.rm = TRUE)

Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_1_inc_ratio",
                  slideBy = 1)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
      TimeVar = "Year", GroupVar = "ISO.x",
      NewVar = "lead_2_inc_ratio",
      slideBy = 2)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_3_inc_ratio",
                  slideBy = 3)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_4_inc_ratio",
                  slideBy = 4)
Doctor_Who<-slide(Doctor_Who, Var = "Income_Pro", 
                  TimeVar = "Year", GroupVar = "ISO.x",
                  NewVar = "lead_5_inc_ratio",
                  slideBy = 5)
Doctor_Who$AVE_inc_5years<-rowMeans(Doctor_Who[,c("lead_1_inc_ratio",
                                                   "lead_2_inc_ratio",
                                                   "lead_3_inc_ratio",
                                                   "lead_4_inc_ratio", 
                                                   "lead_5_inc_ratio")],
                                     na.rm = TRUE)

Doctor_Who$Diff_5_bef_aft<-Doctor_Who$AVE_inc_5years-Doctor_Who$AVE_inc_n5years

head(na.omit(Doctor_Who[,c("Country", "Year", "Income_Pro", "lead_2_inc_ratio")]))

###make some averages
#find column numbers
#which( colnames(Doctor_Who)=="six_yr_inc_change" )
##make cross-row ave
#Doctor_Who$AVEfive_yr_inc_change<-rowMeans(Doctor_Who[,174:178],
#                                           na.rm = TRUE)


Doctor_Who$one_yr_inc_change_IS <- ave(Doctor_Who$Indiv_Sales, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 1)))
Doctor_Who$two_yr_inc_change_IS <- ave(Doctor_Who$Indiv_Sales, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 2)))
Doctor_Who$three_yr_inc_change_IS <- ave(Doctor_Who$Indiv_Sales, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 3)))
Doctor_Who$four_yr_inc_change_IS <- ave(Doctor_Who$Indiv_Sales, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 4)))
Doctor_Who$five_yr_inc_change_IS <- ave(Doctor_Who$Indiv_Sales, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 5)))
Doctor_Who$six_yr_inc_change_IS <- ave(Doctor_Who$Indiv_Sales, list(Doctor_Who$Country), FUN=function(i) c(NA,diff(i, lag = 6)))
#which( colnames(Doctor_Who)=="six_yr_inc_change_IS" )
Doctor_Who$AVEfive_yr_inc_changeIS<-rowMeans(Doctor_Who[,186:190],
                                             na.rm = TRUE)

#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT
#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT
#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT
Doctor_Who$Social_Security<-as.numeric(sub("%", "", Doctor_Who$Social.contributions))
#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT
#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT
#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT
#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT
#NOTE: DANGEROUS TO CHANGE CODING ABOVE THIS POINT

summary(lm(Income_Pro ~ polity_dummy + Informal +log(GDP_cap) + parreg_dummy*prop_high, data = Doctor_Who[Doctor_Who$parcomp>0 & Doctor_Who$Year=="2005",]))
summary(lm(Income_Pro ~ polity_dummy + Informal +log(GDP_cap) + parcomp*prop_high, data = Doctor_Who[Doctor_Who$parcomp>0  & Doctor_Who$Year=="2005",]))

summary(lm(Income_Pro ~ log(GDP_cap) + as.factor(regtrans)*prop_high, data = Doctor_Who[Doctor_Who$regtrans>-3 & Doctor_Who$regtrans<4, ]))
summary(as.factor(Doctor_Who$regtrans))
Doctor_Who$xconst
summary(lm(three_yr_inc_change ~ xconst*prop_high, data = Doctor_Who[Doctor_Who$polity_dummy=="0" & Doctor_Who$Year=="2005",]))

summary(lm(Income_Pro ~ polity_dummy + log(GDP_cap) + xconst*prop_high, data = Doctor_Who[Doctor_Who$Year=="2005",]))
summary(lm(Income_Pro ~ polity_dummy + Informal +log(GDP_cap) + parcomp*prop_high, data = Doctor_Who[Doctor_Who$parcomp>0 & Doctor_Who$Year=="2005",]))
summary(lm(Income_Pro ~ polity_dummy + log(GDP_cap) + parreg*prop_high, data = Doctor_Who[Doctor_Who$Year=="2005",]))

summary(lm(Income_Pro ~  log(GDP_cap) + parreg*prop_high, data = Doctor_Who[Doctor_Who$polity_dummy=="1" & Doctor_Who$Year=="2005",]))
boxplot(Income_Pro~parcomp, data = Doctor_Who[Doctor_Who$polity_dummy=="0" & Doctor_Who$Year=="2005",])
plot(Income_Pro~prop_high, data = Doctor_Who[Doctor_Who$polity_dummy=="0" & Doctor_Who$Year=="2005",])
summary(lm(Income_Pro ~ parcomp*prop_high, data = Doctor_Who[Doctor_Who$parcomp>-1 & Doctor_Who$polity_dummy=="1" & Doctor_Who$Year=="2005",]))

summary(lm(Income_Pro ~ polity_dummy + log(GDP_cap) + polcomp*prop_high, data = Doctor_Who[Doctor_Who$Year=="2005",]))
summary(lm(Income_Pro ~ log(GDP_cap) + polity2*prop_high, data = Doctor_Who[Doctor_Who$Year=="2005",]))
summary(lm(Income_Pro ~ log(GDP_cap) + polity_dummy*prop_high, data = Doctor_Who[Doctor_Who$Year=="2005",]))
summary(Doctor_Who$durable)
summary(lme(Income_Pro ~ 1  + prop_high*durable,
            random = ~ 1  + durable|Country, data=Doctor_Who[Doctor_Who$polity_dummy>0, ],
            na.action=na.omit, method="ML", control=list(opt="optim"),
))
Doctor_Who$WE_Grouping<-NA
Doctor_Who$WE_Grouping[Doctor_Who$prop_high<0.3] <- "1"
Doctor_Who$WE_Grouping[Doctor_Who$prop_high>=0.4 & Doctor_Who$prop_high<0.5] <- "2"
Doctor_Who$WE_Grouping[Doctor_Who$prop_high>=0.5 & Doctor_Who$prop_high<0.6] <- "3"
Doctor_Who$WE_Grouping[Doctor_Who$prop_high>=0.8 & Doctor_Who$prop_high<1] <- "4"
Doctor_Who$WE_Grouping<-as.factor(Doctor_Who$WE_Grouping)

ggplot(Doctor_Who, aes(durable, Income_Pro, colour = as.factor(WE_Grouping)))+geom_point()+geom_text(aes(label=ISO.x), size=3)+facet_wrap(~parreg_dummy) + xlim(0,15) +
  geom_smooth(show.legend = TRUE)

ggplot(listing_countries1aaa, aes(prop_high.x, six_yr_inc_change.x), colour = WE_Grouping.x)+geom_point()+facet_wrap(~polity_dummy.x)+scale_color_discrete(name = "Work Ethic Grouping")

plot()
summary(Doctor_Who$polcomp)

Doctor_Who$dem_high<-as.factor(Doctor_Who$polity_dummy==1 & Doctor_Who$prop_high>0.5)

####
####
####
####
####
#Will need to get institutions back in here
polity<-read.csv("/Users/Kevin/Downloads/p4v2014.csv")
#head(polity)
polity<-rename(polity, c(year="Year", country="Country"))
polity$Country<-as.factor(polity$Country)
polity$Country<-revalue(polity$Country, c("Bosnia"="Bosnia and Herzegovina", "Kyrgyzstan"="Kyrgyz Republic", "Korea South"="Korea, Rep.",
                                          "Iran"="Iran, Islamic Rep.", "Russia"="Russian Federation", "Macedonia"="Macedonia, FYR", "Egypt"="Egypt, Arab Rep.",
                                          "Venezuela"="Venezuela, RB"))
####NAME Doctor_Who since all of time and space... well...
Doctor_Who<-merge(WVS_Econ_Informal_Tax, polity, by=c("Country", "Year"), all = TRUE)




##
##
##
##









#o <- rle(Doctor_Who$polity_change_dum)
#sequence <- unlist(sapply(o$lengths, seq))
#Doctor_Who$countzero <- sequence
#Doctor_Who$countzero[Doctor_Who$polity_change_dum != 0] <- 0

###could add a percent difference

Doctor_Who$per_diff_after_6yr<-(Doctor_Who$six_yr_inc_change/Doctor_Who$Income_Pro)
#Doctor_Who$sq_dir_polity<- Doctor_Who$polity_change_dum*sqrt(abs(Doctor_Who$polity_change))
Doctor_Who$abs_dir_polity<- abs(Doctor_Who$polity_change)
#Doctor_Who$exp_dir_polity<- Doctor_Who$polity_change_dum*(Doctor_Who$polity_change^2)
Doctor_Who$cater_polity_d_WE<-NA
Doctor_Who$cater_polity_d_WE[Doctor_Who$polity_change_dum=="0"]<-0
Doctor_Who$cater_polity_d_WE[Doctor_Who$polity_change_dum=="-1"]<-1
Doctor_Who$cater_polity_d_WE[Doctor_Who$polity_change_dum=="1"&Doctor_Who$prop_high<0.35]<-2
Doctor_Who$cater_polity_d_WE[Doctor_Who$polity_change_dum=="1"&Doctor_Who$prop_high>0.35&Doctor_Who$prop_high<0.5]<-3
Doctor_Who$cater_polity_d_WE[Doctor_Who$polity_change_dum=="1"&Doctor_Who$prop_high>0.5]<-4
Doctor_Who$cater_polity_d_WE<-as.factor(Doctor_Who$cater_polity_d_WE)

Doctor_Who$cater_polity_WE<-NA
Doctor_Who$cater_polity_WE[Doctor_Who$polity_change=="0"]<-0
Doctor_Who$cater_polity_WE[Doctor_Who$polity_change=="-1"]<-1
Doctor_Who$cater_polity_WE[Doctor_Who$polity_change=="1"&Doctor_Who$prop_high<0.35]<-2
Doctor_Who$cater_polity_WE[Doctor_Who$polity_change=="1"&Doctor_Who$prop_high>0.35&Doctor_Who$prop_high<0.5]<-3
Doctor_Who$cater_polity_WE[Doctor_Who$polity_change=="1"&Doctor_Who$prop_high>0.5]<-4
Doctor_Who$cater_polity_WE<-as.factor(Doctor_Who$cater_polity_WE)

Doctor_Who$WE_Grouping<-0
Doctor_Who$WE_Grouping[Doctor_Who$prop_high<0.25] <- "1"
Doctor_Who$WE_Grouping[Doctor_Who$prop_high>=0.25 & Doctor_Who$prop_high<0.5] <- "2"
Doctor_Who$WE_Grouping[Doctor_Who$prop_high>=0.5 & Doctor_Who$prop_high<1] <- "3"
Doctor_Who$WE_Grouping<-as.factor(Doctor_Who$WE_Grouping)


Doctor_Who$trip_dumm_dumm<-NA
Doctor_Who$trip_dumm_dumm[Doctor_Who$polity_change_dum=="0"]<-0
Doctor_Who$trip_dumm_dumm[Doctor_Who$polity_change_dum=="-1"]<-1
Doctor_Who$trip_dumm_dumm[Doctor_Who$polity_change_dum=="1" & Doctor_Who$prop_high<0.48]<-2
Doctor_Who$trip_dumm_dumm[Doctor_Who$polity_change_dum=="1" & Doctor_Who$prop_high>0.48]<-3
Doctor_Who$trip_dumm_dumm<-as.factor(Doctor_Who$trip_dumm_dumm)

Doctor_Who$relevel_dumm<-NA
Doctor_Who$relevel_dumm[Doctor_Who$polity_change_dum=="0"]<-0
Doctor_Who$relevel_dumm[Doctor_Who$polity_change_dum=="-1"]<-1
Doctor_Who$relevel_dumm[Doctor_Who$polity_change_dum=="1"]<-2
Doctor_Who$relevel_dumm<-as.factor(Doctor_Who$relevel_dumm)


###need to fix
Doctor_Who$trip_dumm<-NA
Doctor_Who$trip_dumm[Doctor_Who$polity_change<4 & Doctor_Who$polity_change>(-4)]<-0
Doctor_Who$trip_dumm[Doctor_Who$polity_change<(-4)]<-1
Doctor_Who$trip_dumm[Doctor_Who$polity_change<(-4) & Doctor_Who$prop_high<0.5]<-2
Doctor_Who$trip_dumm[Doctor_Who$polity_change>(4) & Doctor_Who$prop_high>0.5]<-3
Doctor_Who$trip_dumm<-as.factor(Doctor_Who$trip_dumm)
summary(Doctor_Who$trip_dumm)


###Doctor_Who$Final_Polity_Dum<-as.numeric(Doctor_Who$polity2>5[Doctor_Who$Year==2000])



######
######
######
######
######
######
######
######
######

####base lm correlation of country averages
country_ave_2<-na.omit(Doctor_Who[, c("Country", "Year", "ISO.x", "Income_Pro", "GDP_cap", "prop_high", "Work_Index", "polity_dummy")])
country_ave_2x<-na.omit(Doctor_Who[, c("Country", "Year", "ISO.x", "Income_Pro", "GDP_cap", "prop_high", "Work_Index", "polity_dummy", "Informal")])
summary(Doctor_Who$Income_Pro)
Doctor_Who$Income_Pro
###appears solid for variety of subsets

#country_ave_2<-subset(country_ave_2, Year>2000 & Year<2005)

country_ave_2I<-aggregate(country_ave_2[, 4:8], list(Country=country_ave_2$ISO.x), mean)
country_ave_2I$polity_dummy2<-as.numeric(country_ave_2I$polity_dummy>.5)
country_ave_2I

ggplot(country_ave_2I, aes(prop_high, Income_Pro))+geom_point()+geom_text(aes(label=Country), size=3)+facet_wrap(~polity_dummy2) +
  geom_smooth(method='lm',formula=y~x, show.legend = TRUE)

#model 3 with interaction term for democracy
#lm_work_2<-lm(Income_Pro~Work_Index*polity_dummy, data = country_ave_2I)
#note, use prop_high
lm_work_2<-lm(Income_Pro~prop_high*polity_dummy, data = country_ave_2I)
summary(lm_work_2)

#first step... GDP/cap = development corrective
lm_work_2a<-lm(Income_Pro~log(GDP_cap)+prop_high*polity_dummy, data = country_ave_2I)
summary(lm_work_2a, correlation = TRUE)

#note:
plot(Income_Pro~log(GDP_cap), data = country_ave_2I)

#now comtemplate a broader set of factors
#country_ave_2I$polity_dummy2<-as.numeric(country_ave_2I$polity_dummy>.5)
country_ave_2aa<-na.omit(Doctor_Who[, c("Country", "Year", "ISO.x", "Income_Pro", "GDP_cap", "prop_high", "GINI", "Informal", "Work_Index", "polity_dummy")])
country_ave_2Ia<-aggregate(country_ave_2aa[, 4:10], list(Country=country_ave_2aa$ISO.x), mean, rm.na = TRUE)
country_ave_2Ia

summary(lm(Income_Pro~prop_high*polity_dummy+log(GDP_cap)+Informal+GINI, data = country_ave_2Ia), correlation = TRUE)

#country_ave_2I$polity_dummy2<-as.numeric(country_ave_2I$polity_dummy>.5)

country_ave_2a$cuts<-0
country_ave_2a$cuts[country_ave_2a$polity_dummy>=0.5 & country_ave_2a$prop_high<0.48]<-1
country_ave_2a$cuts[country_ave_2a$polity_dummy<0.5]<-2
country_ave_2a$cuts[country_ave_2a$polity_dummy>=0.5 & country_ave_2a$prop_high>0.48]<-3
country_ave_2a$cuts<-as.factor(country_ave_2a$cuts)
country_ave_2a$cuts<-as.numeric(country_ave_2a$cuts)
country_ave_2a

#country_ave_2a<-subset(country_ave_2a, Year>1995 & Year<2000)
summary(country_ave_2a$Informal)
country_ave_2Ia<-aggregate(country_ave_2a[, 4:10], list(Country=country_ave_2a$ISO.x), mean)
country_ave_2Ia$polity_dummy2<-as.numeric(country_ave_2Ia$polity_dummy>.5)
country_ave_2Ia
country_ave_2Ia$cuts1<-0
country_ave_2Ia$cuts1[country_ave_2Ia$cuts<1.5]<-1
country_ave_2Ia$cuts1[country_ave_2Ia$cuts>1.5 & country_ave_2Ia$cuts<2.5]<-2
country_ave_2Ia$cuts1[country_ave_2Ia$cuts>2.5]<-3
country_ave_2Ia$cuts1<-as.factor(country_ave_2Ia$cuts1)
country_ave_2Ia
lm_work_2aa<-lm(Income_Pro~log(GDP_cap)+GINI+Informal+prop_high*cuts1, data = country_ave_2Ia)
summary(lm_work_2aa)
country_ave_2Ia
stargazer(lm_work_2, lm_work_2a, lm_work_2aa)

##CAN also try

country_ave_2I$polity_dummy2<-as.numeric(country_ave_2I$polity_dummy>.5)
country_ave_2I$cuts<-0
country_ave_2I$cuts[country_ave_2I$polity_dummy>=0.5 & country_ave_2I$prop_high<0.48]<-1
country_ave_2I$cuts[country_ave_2I$polity_dummy<0.5]<-2
country_ave_2I$cuts[country_ave_2I$polity_dummy>=0.5 & country_ave_2I$prop_high>0.48]<-3
country_ave_2I$cuts<-as.factor(country_ave_2I$cuts)
country_ave_2I$dem_high<-as.factor(country_ave_2I$prop_high>0.50&country_ave_2I$polity_dummy>.5)

lm_work_2aaaa<-lm(Income_Pro~log(GDP_cap)+prop_high*cuts, data = country_ave_2I)
summary(lm_work_2aaaa)

##
DW_all<-subset(Doctor_Who, trip_dumm_dumm!="0")
DW_all<-na.omit(DW_all[, c("Country", "Year", "per_diff_after_6yr", "prop_high", "trip_dumm_dumm", "polity_dummy")])
lm_work_2aaa<-lm(per_diff_after_6yr~prop_high*trip_dumm_dumm, data = DW_all)
summary(lm_work_2aaa)

plot(per_diff_after_6yr~prop_high, data = DW_all, col = as.factor(trip_dumm_dumm))
text(DW_all$prop_high, DW_all$per_diff_after_6yr, DW_all$Country, cex = .6)

legend(x="topright", legend = levels(as.factor(DW_all$trip_dumm_dumm)), col=c("red", "black", "green"), pch=1)
abline(lm(Income_Pro[dummy_direction_dict=="1"]~prop_high[dummy_direction_dict=="1"], data = Doctor_Who_polity_delta_sensitivity11), col = "black")
abline(lm(Income_Pro[dummy_direction_dict=="0" & prop_high< 0.5]~prop_high[dummy_direction_dict=="0" & prop_high< 0.5], data = Doctor_Who_polity_delta_sensitivity11), col = "red")
abline(lm(Income_Pro[dummy_direction_dict=="0"& prop_high > 0.5]~prop_high[dummy_direction_dict=="0" & prop_high> 0.5], data = Doctor_Who_polity_delta_sensitivity11), col = "red")


lm_work_2aaa<-lm(AVEfive_yr_inc_change[trip_dumm_dumm!=0]~prop_high[trip_dumm_dumm!=0]*trip_dumm_dumm[trip_dumm_dumm!=0], data = Doctor_Who)
summary(lm_work_2aaa)


country_ave_3I<-aggregate(country_ave_2[, 3:7], list(Country=country_ave_2$Country, Year = country_ave_2$Year), mean)
lm_work_3<-lme(per_diff_after_6yr ~ 1 + Year + GDP_cap + Work_Index*polity_dummy,
               random = ~ 1 + GDP_cap + Year + Work_Index*polity_dummy|Country, data=country_ave_3I,
               na.action=na.omit, method="ML", control=list(opt="optim"),
               correlation=corAR1())
summary(lm_work_3)


lm_work_3<-lme(per_diff_after_6yr ~ 1 + Year + GDP_cap + prop_high*polity_dummy,
               random = ~ 1 + GDP_cap + Year + polity_dummy|Country, data=country_ave_3I,
               na.action=na.omit, method="ML", control=list(opt="optim"),
               correlation=corAR1())
###

#Category. Dictatorship
DW_dict<-subset(Doctor_Who, trip_dumm_dumm=="1")
DW_dict<-na.omit(DW_dict[, c("Country", "Year", "Income_Pro", "prop_high", "Work_Index", "polity_dummy")])
DW_dict
summary(lm(Income_Pro ~  prop_high, data=DW_dict))
DW_dict<-subset(Doctor_Who, trip_dumm_dumm=="1")
DW_dict<-na.omit(DW_dict[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
summary(lm(per_diff_after_6yr ~  prop_high, data=DW_dict))
plot(per_diff_after_6yr ~  prop_high, data=DW_dict)
text(per_diff_after_6yr ~  prop_high, labels = Country, data=DW_dict)

Doctor_Who$three_yr_inc_change
DW_dict<-subset(Doctor_Who, trip_dumm_dumm=="1")
DW_dict<-na.omit(DW_dict[, c("Country", "Year", "Income_Pro", "three_yr_inc_change", "prop_high", "Work_Index", "polity_dummy")])
summary(lm(three_yr_inc_change ~  prop_high, data=DW_dict))
plot(three_yr_inc_change ~  prop_high, data=DW_dict)
text(three_yr_inc_change ~  prop_high, labels = Country, data=DW_dict)



DW_dict<-subset(Doctor_Who, trip_dumm_dumm=="1")
DW_dict<-na.omit(DW_dict[, c("Country", "Year", "Income_Pro", "prop_high", "Work_Index", "polity_dummy")])
DW_dict
summary(lm(Income_Pro ~  prop_high, data=DW_dict))
DW_dict<-subset(Doctor_Who, trip_dumm_dumm=="1")
DW_dict<-na.omit(DW_dict[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
summary(lm(per_diff_after_6yr ~  prop_high, data=DW_dict))
plot(per_diff_after_6yr ~  prop_high, data=DW_dict)
text(per_diff_after_6yr ~  prop_high, labels = Country, data=DW_dict)


?text
Doctor_Who$per_diff_after_6yr


summary(lme(Income_Pro ~ 1 + prop_high,
            random = ~ 1 |Country, data=DW_dict,
            na.action=na.omit, method="ML", control=list(opt="optim")))


#Category. Democracy and Low





DW_dem_low<-subset(Doctor_Who, trip_dumm_dumm=="2")
DW_dem_low<-na.omit(DW_dem_low[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
summary(lm(per_diff_after_6yr ~  prop_high, data=DW_dem_low))
plot(per_diff_after_6yr ~  prop_high, data=DW_dem_low)
text(per_diff_after_6yr ~  prop_high, labels = Country, data=DW_dem_low)



#Category. Democracy and High
DW_dem_high<-subset(Doctor_Who, trip_dumm_dumm=="3")
DW_dem_high<-na.omit(DW_dem_high[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
summary(lm(per_diff_after_6yr ~  prop_high, data=DW_dem_high))
plot(per_diff_after_6yr ~  prop_high, data=DW_dem_high)
text(per_diff_after_6yr ~  prop_high, labels = Country, data=DW_dem_high)

Doctor_Who$AVEfive_yr_inc_change
DW_dems<-subset(Doctor_Who, trip_dumm_dumm=="2"|trip_dumm_dumm=="3")
per_diff_after_6yr
DW_dems<-na.omit(DW_dems[, c("Country", "Year", "Income_Pro", "AVEfive_yr_inc_change", "prop_high", "Work_Index", "polity_dummy")])
DW_dems
library(rdrobust)
rdrobust(DW_dems$per_diff_after_6yr, DW_dems$prop_high, c = 0.48)


rdrobust(Doctor_Who$Income_Pro, Doctor_Who$prop_high, c = 0.50)

prop_rdd<-RDestimate(Income_Pro~prop_high, data=Doctor_Who, cutpoint = 0.50)
prop_rdd<-RDestimate(AVEfive_yr_inc_change~prop_high, data=DW_dems, cutpoint = 0.50)
plot(prop_rdd)
?RDestimate
summary(prop_rdd)

?rdrobust
library(rddtools)
?rddtools
rprop_rdd <- RDdata(y = DW_dems$six_yr_inc_change, x = DW_dems$prop_high, cutpoint = 0.48)
rdplot(y = DW_dems$six_yr_inc_change, x = DW_dems$prop_high, cutpoint = 0.48)
library(rdd)
?rdd
prop_rdd<-RDestimate(six_yr_inc_change~prop_high, data=DW_dems, cutpoint = 0.48)
summary(prop_rdd)
plot(prop_rdd)



dem_rdd<-subset(Doctor_Who, polity_change>1)
DW_dem_high<-na.omit(DW_dem_high[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
dem_rdd<-na.omit(dem_rdd[,c("prop_high", "per_diff_after_6yr")])
dem_rdd
dict_rdd<-subset(Doctor_Who, polity_change<0)

prop_rdd<-RDestimate(per_diff_after_6yr~prop_high, data=dem_rdd, cutpoint = .49)


######
######
######
######
######
######
######
######
######
#Category. Dictatorship
DW_fours<-subset(Doctor_Who, (Doctor_Who$polity_change>4 | Doctor_Who$polity_change<(-4)| Doctor_Who$polity2=="NA") & Doctor_Who$Year>1975)
DW_fours<-na.omit(DW_fours[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
DW_fours

summary(lm(per_diff_after_6yr ~  prop_high, data=DW_dem_high))
plot(per_diff_after_6yr ~  prop_high, data=DW_dem_high)
text(per_diff_after_6yr ~  prop_high, labels = Country, data=DW_dem_high)


DW_total<-subset(Doctor_Who, (Doctor_Who$polity_change!=0| Doctor_Who$polity2=="NA") & Doctor_Who$Year>1975)
DW_total<-na.omit(DW_total[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
DW_total

Doctor_Who_Sub_Pol_Change_three
DW_dict<-subset(Doctor_Who, trip_dumm_dumm=="1")
DW_dict<-na.omit(DW_dict[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
DW_dict
summary(lm(Income_Pro ~  prop_high, data=DW_dict))
DW_dict<-subset(Doctor_Who, trip_dumm_dumm=="1")
DW_dict<-na.omit(DW_dict[, c("Country", "Year", "Income_Pro", "per_diff_after_6yr", "prop_high", "Work_Index", "polity_dummy")])
summary(lm(per_diff_after_6yr ~  prop_high, data=DW_dict))
plot(per_diff_after_6yr ~  prop_high, data=DW_dict)
text(per_diff_after_6yr ~  prop_high, labels = Country, data=DW_dict)











Doctor_Who_Sub_Pol_Change_three<-subset(Doctor_Who, (Doctor_Who$polity_change>4 | Doctor_Who$polity_change<(-4)| Doctor_Who$polity2=="NA") & Doctor_Who$Year>1975)
####
ggplot(Doctor_Who_Sub_Pol_Change_three, aes(prop_high, AVEfive_yr_inc_change))+geom_point()+facet_wrap(~polity_change_dum)+geom_text(aes(label=Country), size=3)
###
ggplot(Doctor_Who_Sub_Pol_Change_three, aes(prop_high, Income_Pro))+geom_point()+facet_wrap(~polity_change_dum)+geom_text(aes(label=Country), size=3)
###
ggplot(Doctor_Who_Sub_Pol_Change_three, aes(prop_high, two_yr_inc_change))+geom_point()+facet_wrap(~polity_change_dum)+geom_text(aes(label=Country), size=3)

ggplot(Doctor_Who_Sub_Pol_Change_three, aes(prop_high, AVEfive_yr_inc_change))+geom_point()+facet_wrap(~dummy_direction_demo)+geom_text(aes(label=Country), size=3)
###
ggplot(Doctor_Who_Sub_Pol_Change_three, aes(prop_high, Income_Pro))+geom_point()+facet_wrap(~dummy_direction_demo)+geom_text(aes(label=Country), size=3)
###
ggplot(Doctor_Who_Sub_Pol_Change_three, aes(prop_high, per_diff_after_6yr))+geom_point()+facet_wrap(~dummy_direction_demo)+geom_text(aes(label=Country), size=3)
head(Doctor_Who_Sub_Pol_Change_three)







Doctor_Who_low<-Doctor_Who[Doctor_Who$prop_high<0.5,]
summary(lme(per_diff_after_6yr ~ 1 + Year + log(GDP_cap) + prop_high*trip_dumm_dumm,
            random = ~1 + Year  |Country, data=Doctor_Who_low,
            na.action=na.omit, method="ML", control=list(opt="optim")))
summary(lme(per_diff_after_6yr ~ 1 + Year + prop_high*polity_change_four,
            random = ~1 + Year |Country, data=Doctor_Who_low,
            na.action=na.omit, method="ML", control=list(opt="optim")))


summary(lme(AVEfive_yr_inc_change ~ 1 + Year + prop_high*polity_change_abs,
            random = ~1 + Year |Country, data=Doctor_Who_high,
            na.action=na.omit, method="ML", control=list(opt="optim")))
summary(lme(AVEfive_yr_inc_change ~ 1 + Year + prop_high*polity_change_four,
            random = ~1 + Year|Country, data=Doctor_Who_high,
            na.action=na.omit, method="ML", control=list(opt="optim")))

Doctor_Who$prop_high_cutoff
###
###
summary(lme(six_yr_inc_change ~ 1 + Year + prop_high*as.factor(polity_change_dum)*prop_high_cutoff,
            random = ~1 + Year |Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))

summary(lme(six_yr_inc_change ~ 1 + Year + prop_high*as.factor(polity_change_dum)*prop_high_cutoff,
            random = ~1 + Year |Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))

summary(lme(six_yr_inc_change ~ 1 + Year + prop_high*as.factor(polity_change_dum)*prop_high_cutoff,
            random = ~1 + Year |Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))
####new route....
####
##
#

#
summary(lme(six_yr_inc_change ~ 1 + Year + prop_high*trip_dumm_dumm,
            random = ~1 + Year + prop_high*trip_dumm_dumm|Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))
#
#
#
#

summary(lme(six_yr_inc_change ~ 1 + Year + prop_high*trip_dumm_dumm,
            random = ~1 + Year + trip_dumm_dumm |Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))

summary(lme(six_yr_inc_change ~ 1 + Year + prop_high*relevel_dumm,
            random = ~1 + Year + prop_high*trip_dumm_dumm |Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))

summary(lme(AVEfive_yr_inc_change ~ 1 + Year + prop_high*relevel_dumm,
            random = ~1 + Year + prop_high*trip_dumm_dumm |Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))

summary(lme(AVEfive_yr_inc_change ~ 1 + Year + prop_high*trip_dumm_dumm,
            random = ~1 + Year + prop_high*trip_dumm_dumm |Country, data=Doctor_Who,
            na.action=na.omit, method="ML", control=list(opt="optim")))
#
#

#
#########
Doctor_Who_Sub_Pol_Change<-subset(Doctor_Who, (Doctor_Who$polity_change_dum!=0 | Doctor_Who$polity2=="NA") & Doctor_Who$Year>1975)
Doctor_Who_Sub_Pol_Change
#
#
#
summary(lm(AVEfive_yr_inc_change~prop_high*trip_dumm_dumm, data = Doctor_Who_Sub_Pol_Change))
summary(lm(AVEfive_yr_inc_change~prop_high*relevel_dumm, data = Doctor_Who_Sub_Pol_Change))
#
#
#

##########
Doctor_Who_Sub_Pol_Change_alt_meth<-subset(Doctor_Who, (Doctor_Who$polity_change_dum!=0 | Doctor_Who$polity_change_dum=="NA") & Doctor_Who$Year>1975)
unique(Doctor_Who_Sub_Pol_Change_alt_meth$Country)
Doctor_Who_Sub_Pol_Change_alt_meth[,c("Country","Year","polity_change_dum", "Income_Pro", "AVEfive_yr_inc_change", "two_yr_inc_change", "six_yr_inc_change", "prop_high")]
Doctor_Who_Sub_Pol_Change_alt_meth

##########

####
ggplot(Doctor_Who_Sub_Pol_Change, aes(prop_high, AVEfive_yr_inc_change))+geom_point()+facet_wrap(~polity_change_dum)+geom_text(aes(label=Country), size=3)
###
ggplot(Doctor_Who_Sub_Pol_Change, aes(prop_high, Income_Pro))+geom_point()+facet_wrap(~polity_change_dum)+geom_text(aes(label=Country), size=3)
###
ggplot(Doctor_Who_Sub_Pol_Change, aes(prop_high, six_yr_inc_change))+geom_point()+facet_wrap(~polity_change_dum)+geom_text(aes(label=Country), size=3)

ggplot(Doctor_Who_Sub_Pol_Change, aes(prop_high, per_diff_after_6yr))+geom_point()+facet_wrap(~polity_change_dum)+geom_text(aes(label=Country), size=3)


listing_countries<-merge(Doctor_Who, Doctor_Who_Sub_Pol_Change, by = c("Country"), all.y = FALSE)
listing_countries1<-na.omit(listing_countries[, c("Country", "Year.x", "Income_Pro.x", "prop_high.x", "Work_Index.x", "GDP_cap.x", "polity_dummy.x", "cater_polity_WE.x", "WE_Grouping.x", "countzero.x")])
ggplot(listing_countries1, aes(Year.x, Income_Pro.x))+geom_point()+facet_wrap(~Country)
ggplot(listing_countries1, aes(Year.x, Income_Pro.x), colour = WE_Grouping.x)+geom_point()+facet_wrap(~polity_dummy.x)+scale_color_discrete(name = "Work Ethic Grouping")

qplot(countzero.x, Income_Pro.x, data=listing_countries1, geom = c("smooth"), colour=as.factor(WE_Grouping.x))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")+facet_wrap(~polity_dummy.x)+xlim(0, 20)
qplot(Year.x, Income_Pro.x, data=listing_countries1, geom = c("smooth"), colour=as.factor(Country))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")+facet_wrap(~WE_Grouping.x)

qplot(Year.x, Income_Pro.x, data=listing_countries1, geom = c("smooth"), colour=as.factor(WE_Grouping.x))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")+facet_wrap(~Country)

listing_countries1aaa<-na.omit(listing_countries[, c("Country", "Year.x", "six_yr_inc_change.x", "prop_high.x", "Work_Index.x", "GDP_cap.x", "polity_dummy.x", "cater_polity_WE.x", "WE_Grouping.x", "countzero.x")])
ggplot(listing_countries1aaa, aes(prop_high.x, six_yr_inc_change.x), colour = WE_Grouping.x)+geom_point()+facet_wrap(~polity_dummy.x)+scale_color_discrete(name = "Work Ethic Grouping")
Doctor_Who$six_yr_inc_change


##
##now create data set out of these countries... keeping full time series possible
##


#######################
####base lm correlation of country averages
country_ave_2<-na.omit(Doctor_Who[, c("Country", "Year", "Income_Pro", "prop_high", "Work_Index", "GDP_cap", "polity_dummy")])
country_ave_2I<-aggregate(country_ave_2[, 3:7], list(Country=country_ave_2$Country), mean)
#model 3 with interaction term for democracy
lm_work_2<-lm(Income_Pro~GDP_cap+Work_Index*polity_dummy, data = country_ave_2I)
summary(lm_work_2)
stargazer(lm_work_2)
ggplot(country_ave_2I, aes(Work_Index, Income_Pro, color = polity_dummy))+geom_point()+geom_text(aes(label=Country), size=3)
ggplot(country_ave_2I, aes(prop_high, Income_Pro, color = polity_dummy))+geom_point()+geom_text(aes(label=Country), size=3)

country_ave_3I<-aggregate(country_ave_2[, 3:7], list(Country=country_ave_2$Country, Year = country_ave_2$Year), mean)
lm_work_3<-lme(Income_Pro ~ 1 + Year + GDP_cap + Work_Index*polity_dummy,
               random = ~ 1 + GDP_cap + Year + Work_Index*polity_dummy|Country, data=country_ave_3I,
               na.action=na.omit, method="ML", control=list(opt="optim"),
               correlation=corAR1())
summary(lm_work_3)


country_ave_4I<-aggregate(country_ave_2[, 3:6], list(Country=country_ave_2$Country, Year = country_ave_2$Year), mean)
lm_work_4<-lme(Income_Pro ~ 1 + Year + GDP_cap + Work_Index*polity_dummy,
               random = ~ 1 + GDP_cap + Year |Country, data=Doctor_Who,
               na.action=na.omit, method="ML", control=list(opt="optim"),
               correlation=corAR1())
summary(lm_work_4)
###just wait, can I make whether a bump up or down in polity changes
#income tax relative the country's intercept
country_ave_5a<-na.omit(Doctor_Who[, c("Country", "Year", "Income_Pro", "prop_high", "Work_Index", "GDP_cap", "polity_dummy", "polity_change_dum")])
country_ave_5aI<-aggregate(country_ave_5a[, 3:8], list(Country=country_ave_5a$Country, Year = country_ave_5a$Year), mean)
###
lm_work_5a<-lme(Income_Pro ~ 1 + Year + GDP_cap + prop_high*polity_change_dum,
                random = ~ 1 + GDP_cap + Year + polity_change_dum |Country, data=country_ave_5a,
                na.action=na.omit, method="ML", control=list(opt="optim"),
                correlation=corAR1())
summary(lm_work_5a)

country_ave_5aa<-na.omit(Doctor_Who[, c("Country", "Year", "Income_Pro", "prop_high", "Work_Index", "GDP_cap", "polity_dummy", "cater_polity_WE")])
country_ave_5aaI<-aggregate(country_ave_5aa[, 3:8], list(Country=country_ave_5aa$Country, Year = country_ave_5aa$Year), mean)
ggplot(country_ave_5aa, aes(Year, Income_Pro))+geom_point()+facet_wrap(~cater_polity_WE) + geom_smooth(n = 3)

country_ave_6a<-na.omit(Doctor_Who[, c("Country", "ISO.x", "Year", "six_yr_inc_change", "prop_high", "Work_Index", "polity_change_dum", "polity_change", "cater_polity_WE")])
library(ggplot2)

ggplot(country_ave_6a, aes(prop_high, six_yr_inc_change))
ggplot(country_ave_6a, aes(prop_high, six_yr_inc_change))+geom_point()+facet_wrap(~cater_polity_WE) + geom_smooth(span = 1)
ggplot(country_ave_6a, aes(prop_high, six_yr_inc_change))+geom_point()+facet_wrap(~cater_polity_WE) + geom_smooth(span = .1)
ggplot(country_ave_6a, aes(Year, six_yr_inc_change))+geom_point()+facet_wrap(~cater_polity_WE) + geom_smooth(span = .3)

country_ave_6aa<-na.omit(Doctor_Who[, c("Country", "ISO.x", "Year", "AVEfive_yr_inc_change", "prop_high", "Work_Index", "polity_change_dum", "polity_change", "cater_polity_WE")])
ggplot(country_ave_6aa, aes(prop_high, AVEfive_yr_inc_change))+geom_point()+facet_wrap(~cater_polity_WE) + geom_smooth(span = .1)



country_ave_7a<-na.omit(Doctor_Who[, c("Country", "ISO.x", "Year", "Income_Pro", "prop_high", "Work_Index", "polity_change_dum", "polity_change", "cater_polity_WE")])
ggplot(country_ave_7a, aes(prop_high, Income_Pro))+geom_point()+facet_wrap(~cater_polity_WE) + geom_smooth(span = .3)

Doctor_Who_gg_plot<-qplot(prop_high, six_yr_inc_change, data=country_ave_6a, geom_point()+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
                          Doctor_Who_gg_plot
                          
                          ##
                          ##
                          ##
                          ##
                          
                          Doctor_Who$polity_change_dum
                          Doctor_Who_GG_study<-Doctor_Who[Doctor_Who$polity_change_dum!=0 & Doctor_Who$Year>1980,]
                          unique(Doctor_Who_GG_study$Country)
                          Doctor_Who$RDD_Countries<-NA
                          Doctor_Who$RDD_Countries[Doctor_Who$Country==Doctor_Who_GG_study$Country]<-Doctor_Who_GG_study$Country
                          unique(Doctor_Who$RDD_Countries)
                          Doctor_Who$RDD_Countries[Doctor_Who$Country=="Liberia"]
                          
                          #######################
                          
                          Doctor_Who_polity_delta<-Doctor_Who[Doctor_Who$polity_change!=0,]
                          Doctor_Who_polity_delta<-na.omit(Doctor_Who_polity_delta[,c("Country","Year", "Income_Pro", "AVEfive_yr_inc_change", "polity_change", "dummy_direction_dict", "polity_dummy",
                                                                                      "weightings_polity_change", "Work_Index", "high_prop_low", "prop_high")])
                          Doctor_Who_polity_delta$Year_gm<-Doctor_Who_polity_delta$Year-mean(Doctor_Who_polity_delta$Year)
                          Doctor_Who_polity_delta$prop_high_gm<-Doctor_Who_polity_delta$prop_high-mean(Doctor_Who_polity_delta$prop_high)
                          Doctor_Who_polity_delta$polity_change_gm<-Doctor_Who_polity_delta$polity_change-mean(Doctor_Who_polity_delta$polity_change)
                          Doctor_Who_polity_delta$prop_adjusted<-0
                          Doctor_Who_polity_delta$prop_adjusted[Doctor_Who_polity_delta$polity_dummy=="0"]<-Doctor_Who_polity_delta$prop_high[Doctor_Who_polity_delta$polity_dummy=="0"]
                          Doctor_Who_polity_delta$prop_adjusted[Doctor_Who_polity_delta$polity_dummy=="1"]<-(Doctor_Who_polity_delta$prop_high[Doctor_Who_polity_delta$polity_dummy=="1"])^2
                          Doctor_Who_polity_delta$prop_adjusted_gm<-Doctor_Who_polity_delta$prop_adjusted
                          ##plus sensitivity rounds...awesome results when just UE and GDP cap...shows they come to matter little in themselves but strengthen my relations
                          Doctor_Who_polity_delta_sensitivity<-Doctor_Who[Doctor_Who$polity_change!=0,]
                          Doctor_Who_polity_delta_sensitivity<-na.omit(Doctor_Who_polity_delta_sensitivity[,c("Country","Year", "Income_Pro", "AVEfive_yr_inc_change", "polity_change", "dummy_direction_dict", "polity_dummy", "UE", "GDP_cap", "polity_change_dum",
                                                                                                              "weightings_polity_change", "Work_Index", "high_prop_low", "prop_high")])
                          Doctor_Who_polity_delta_sensitivity$Year_gm<-Doctor_Who_polity_delta_sensitivity$Year-mean(Doctor_Who_polity_delta_sensitivity$Year)
                          Doctor_Who_polity_delta_sensitivity$prop_high_gm<-Doctor_Who_polity_delta_sensitivity$prop_high-mean(Doctor_Who_polity_delta_sensitivity$prop_high)
                          Doctor_Who_polity_delta_sensitivity$polity_change_gm<-Doctor_Who_polity_delta_sensitivity$polity_change-mean(Doctor_Who_polity_delta_sensitivity$polity_change)
                          Doctor_Who_polity_delta_sensitivity$prop_adjusted<-0
                          Doctor_Who_polity_delta_sensitivity$prop_adjusted[Doctor_Who_polity_delta_sensitivity$polity_dummy=="0"]<-Doctor_Who_polity_delta_sensitivity$prop_high[Doctor_Who_polity_delta_sensitivity$polity_dummy=="0"]
                          Doctor_Who_polity_delta_sensitivity$prop_adjusted[Doctor_Who_polity_delta_sensitivity$polity_dummy=="1"]<-(Doctor_Who_polity_delta_sensitivity$prop_high[Doctor_Who_polity_delta_sensitivity$polity_dummy=="1"])^2
                          Doctor_Who_polity_delta_sensitivity$prop_adjusted_gm<-Doctor_Who_polity_delta_sensitivity$prop_adjusted
                          
                          
                          
                          
                          ##########################################################
                          #quickly, general LME...good sign...  robus to +/- "UE", "GDP_cap", "GINI"
                          Doctor_Who_polity_delta_gen<-na.omit(Doctor_Who[,c("Country","Year", "Income_Pro", "AVEfive_yr_inc_change", "polity_change", "dummy_direction_dict", "polity_dummy", "UE", "GDP_cap", "GINI",
                                                                             "weightings_polity_change", "Work_Index", "high_prop_low", "prop_high")])
                          Doctor_Who_polity_delta_gen$Year_gm<-Doctor_Who_polity_delta_gen$Year-mean(Doctor_Who_polity_delta_gen$Year)
                          Doctor_Who_polity_delta_gen$prop_high_gm<-Doctor_Who_polity_delta_gen$prop_high-mean(Doctor_Who_polity_delta_gen$prop_high)
                          Doctor_Who_polity_delta_gen$polity_change_gm<-Doctor_Who_polity_delta_gen$polity_change-mean(Doctor_Who_polity_delta_gen$polity_change)
                          Doctor_Who_polity_delta_gen$prop_adjusted<-0
                          Doctor_Who_polity_delta_gen$prop_adjusted[Doctor_Who_polity_delta_gen$polity_dummy=="0"]<-Doctor_Who_polity_delta_gen$prop_high[Doctor_Who_polity_delta_gen$polity_dummy=="0"]
                          Doctor_Who_polity_delta_gen$prop_adjusted[Doctor_Who_polity_delta_gen$polity_dummy=="1"]<-(Doctor_Who_polity_delta_gen$prop_high[Doctor_Who_polity_delta_gen$polity_dummy=="1"])^2
                          ###ISSUE
                          Doctor_Who_polity_delta_gen$prop_adjusted_gm<-Doctor_Who_polity_delta_gen$prop_adjusted
                          
                          
                          
                          
                          ###
                          ####
                          #####
                          #Doctor_Who_polity_delta_gen_IS<-Doctor_Who[Doctor_Who$polity_change!=0,]
                          Doctor_Who_polity_delta_gen_IS<-na.omit(Doctor_Who[,c("Country","Year", "Indiv_Sales", "polity_change", "dummy_direction_dict", "polity_dummy", "GDP_cap",
                                                                                "weightings_polity_change", "Work_Index", "high_prop_low", "prop_high")])
                          
                          Doctor_Who_polity_delta_gen_IS$prop_adjusted<-0
                          Doctor_Who_polity_delta_gen_IS$prop_adjusted[Doctor_Who_polity_delta_gen_IS$polity_dummy=="0"]<-Doctor_Who_polity_delta_gen_IS$prop_high[Doctor_Who_polity_delta_gen_IS$polity_dummy=="0"]
                          Doctor_Who_polity_delta_gen_IS$prop_adjusted[Doctor_Who_polity_delta_gen_IS$polity_dummy=="1"]<-(Doctor_Who_polity_delta_gen_IS$prop_high[Doctor_Who_polity_delta_gen_IS$polity_dummy=="1"])^2
                          Doctor_Who_polity_delta_gen_IS$Indiv_Saleslog<-log(Doctor_Who_polity_delta_gen_IS$Indiv_Sales)
                          
                          
                          summary(lme(Indiv_Saleslog ~ 1 + Year + prop_adjusted*polity_change + GDP_cap,
                                      random = ~1 + Year + polity_change + GDP_cap|Country, data=Doctor_Who_polity_delta_gen_IS,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          
                          ####XCONST is a curious indicator too
                          
                          
                          summary(lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_adjusted_gm*polity_change_gm,
                                      random = ~1 + Year_gm |Country, data=Doctor_Who_polity_delta_gen,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          
                          
                          summary(lme(AVEfive_yr_inc_change ~ 1 + Year_gm + UE + GDP_cap + GINI + prop_adjusted_gm*polity_change_gm,
                                      random = ~1 + Year_gm + UE + GDP_cap + polity_change_gm |Country, data=Doctor_Who_polity_delta_gen,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          Doctor_Who_polity_delta_gen<-na.omit(Doctor_Who[,c("Country","Year", "Income_Pro", "AVEfive_yr_inc_change", "polity_change", "dummy_direction_dict", "polity_dummy",
                                                                             "weightings_polity_change", "Work_Index", "high_prop_low", "prop_high")])
                          Doctor_Who_polity_delta_gen$Year_gm<-Doctor_Who_polity_delta_gen$Year-mean(Doctor_Who_polity_delta_gen$Year)
                          Doctor_Who_polity_delta_gen$prop_high_gm<-Doctor_Who_polity_delta_gen$prop_high-mean(Doctor_Who_polity_delta_gen$prop_high)
                          Doctor_Who_polity_delta_gen$polity_change_gm<-Doctor_Who_polity_delta_gen$polity_change-mean(Doctor_Who_polity_delta_gen$polity_change)
                          Doctor_Who_polity_delta_gen$prop_adjusted<-0
                          Doctor_Who_polity_delta_gen$prop_adjusted[Doctor_Who_polity_delta_gen$polity_dummy=="0"]<-Doctor_Who_polity_delta_gen$prop_high[Doctor_Who_polity_delta_gen$polity_dummy=="0"]
                          Doctor_Who_polity_delta_gen$prop_adjusted[Doctor_Who_polity_delta_gen$polity_dummy=="1"]<-(Doctor_Who_polity_delta_gen$prop_high[Doctor_Who_polity_delta_gen$polity_dummy=="1"])^2
                          Doctor_Who_polity_delta_gen$prop_adjusted_gm<-Doctor_Who_polity_delta_gen$prop_adjusted
                          
                          
                          
                          ########Possibly what I have been looking for#############correlation=corAR1()
                          ###note the huge fucking difference that is adding a adjustment to treat democ and dict diff
                          ##can be ^2 or ^3 for democ... also, gm done after and can be included if desired
                          summary(lm(AVEfive_yr_inc_change~prop_high*polity_change, data = Doctor_Who_polity_delta))
                          summary(lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_adjusted_gm*polity_change_gm,
                                      random = ~1 + Year_gm + polity_change_gm |Country, data=Doctor_Who_polity_delta,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          
                          summary(lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_adjusted_gm*polity_change_gm,
                                      random = ~1 + Year_gm  |Country, data=Doctor_Who_polity_delta,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          ##sens test...does not seem to hurt in any way
                          summary(lme(AVEfive_yr_inc_change ~ 1 + Year_gm + GDP_cap + UE + prop_adjusted_gm*polity_change_gm,
                                      random = ~1 + Year_gm + GDP_cap + UE  + polity_change_gm |Country, data=Doctor_Who_polity_delta_sensitivity,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          
                          
                          ####some plots of polity change events
                          Doctor_Who_polity_delta_sensitivity11<-Doctor_Who[Doctor_Who$polity_change!=0,]
                          Doctor_Who_polity_delta_sensitivity11<-na.omit(Doctor_Who_polity_delta_sensitivity11[,c("Country", "ISO.x", "Year", "Income_Pro", "polity_change", "dummy_direction_dict", "polity_dummy", "polity_change_dum",
                                                                                                                  "weightings_polity_change", "Work_Index", "high_prop_low", "prop_high")])
                          ####
                          ####do diff colour if dem/dict
                          plot(Income_Pro~prop_high, data = Doctor_Who_polity_delta_sensitivity11, col = as.factor(polity_dummy))
                          legend(x="topright", legend = levels(as.factor(Doctor_Who_polity_delta_sensitivity11$polity_dummy)), col=c("red","blue"), pch=1)
                          abline(lm(Income_Pro[polity_dummy=="0"]~prop_high[polity_dummy=="0"], data = Doctor_Who_polity_delta_sensitivity11), col = "red")
                          abline(lm(Income_Pro[polity_dummy=="1"]~prop_high[polity_dummy=="1"], data = Doctor_Who_polity_delta_sensitivity11), col = "black")
                          
                          plot(Income_Pro~prop_high, data = Doctor_Who_polity_delta_sensitivity11, col = as.factor(polity_dummy))
                          legend(x="topright", legend = levels(as.factor(Doctor_Who_polity_delta_sensitivity11$dummy_direction_dict)), col=c("red","blue"), pch=1)
                          abline(lm(Income_Pro[dummy_direction_dict=="0" & prop_high< 0.5]~prop_high[dummy_direction_dict=="0" & prop_high< 0.5], data = Doctor_Who_polity_delta_sensitivity11), col = "red")
                          abline(lm(Income_Pro[dummy_direction_dict=="0"& prop_high > 0.5]~prop_high[dummy_direction_dict=="0" & prop_high> 0.5], data = Doctor_Who_polity_delta_sensitivity11), col = "red")
                          abline(lm(Income_Pro[dummy_direction_dict=="1"]~prop_high[dummy_direction_dict=="1"], data = Doctor_Who_polity_delta_sensitivity11), col = "black")
                          text(Doctor_Who_polity_delta_sensitivity11$prop_high,Doctor_Who_polity_delta_sensitivity11$Income_Pro,Doctor_Who_polity_delta_sensitivity11$ISO.x, cex = .6)
                          
                          
                          plot(Income_Pro[polity_change<0]~prop_high[polity_change<0], data = Doctor_Who_polity_delta_sensitivity11)
                          plot(Income_Pro[polity_change_dum<0]~prop_high[polity_change_dum<0], data = Doctor_Who_polity_delta_sensitivity11)
                          
                          plot(AVEfive_yr_inc_change~prop_high, data = Doctor_Who_polity_delta_sensitivity)
                          
                          plot(AVEfive_yr_inc_change[polity_change<0]~prop_high[polity_change<0], data = Doctor_Who_polity_delta_sensitivity)
                          plot(AVEfive_yr_inc_change[polity_change_dum<0]~prop_high[polity_change_dum<0], data = Doctor_Who_polity_delta_sensitivity)
                          text(Doctor_Who_polity_delta_sensitivity$prop_high,Doctor_Who_polity_delta_sensitivity$AVEfive_yr_inc_change,Doctor_Who_polity_delta_sensitivity$Country, cex = .6)
                          
                          plot(AVEfive_yr_inc_change[polity_change>0]~prop_high[polity_change>0], data = Doctor_Who_polity_delta_sensitivity)
                          text(Doctor_Who_polity_delta_sensitivity$prop_high,Doctor_Who_polity_delta_sensitivity$AVEfive_yr_inc_change,Doctor_Who_polity_delta_sensitivity$Country, cex = .6)
                          
                          plot(AVEfive_yr_inc_change[polity_change_dum>0]~prop_high[polity_change_dum>0], data = Doctor_Who_polity_delta_sensitivity)
                          text(Doctor_Who_polity_delta_sensitivity$prop_high,Doctor_Who_polity_delta_sensitivity$AVEfive_yr_inc_change,Doctor_Who_polity_delta_sensitivity$Country, cex = .6)
                          
                          ###woah....did this overfit somehow????
                          ##can include UE at fixed
                          Doctor_Who_polity_delta_sensitivity$GDP_cap_gm<-Doctor_Who_polity_delta_sensitivity$GDP_cap-mean(Doctor_Who_polity_delta_sensitivity$GDP_cap_gm, na.rm = TRUE)
                          summary(lme(AVEfive_yr_inc_change ~ 1 + Year_gm + GDP_cap + prop_adjusted_gm*polity_change_gm,
                                      random = ~1 + Year_gm +  prop_adjusted_gm*polity_change_gm |Country, data=Doctor_Who_polity_delta_sensitivity,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          summary(lme(AVEfive_yr_inc_change ~ 1 + Year_gm + GDP_cap + UE + prop_adjusted_gm*polity_change_gm,
                                      random = ~1 + Year_gm + GDP_cap + UE + polity_change_gm |Country, data=Doctor_Who_polity_delta_sensitivity,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          ###cont alts
                          lme_model_taxes<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_adjusted_gm*polity_change_gm,
                                               random = ~1 + Year_gm + polity_change_gm |Country, data=Doctor_Who_polity_delta,
                                               na.action=na.omit, method="ML", control=list(opt="optim"),
                                               correlation=corAR1())
                          summary(lme_model_taxes)
                          null_lme_model_taxes<-lme(AVEfive_yr_inc_change ~ 1,
                                                    random = ~1 |Country, data=Doctor_Who_polity_delta,
                                                    na.action=na.omit, method="ML", control=list(opt="optim"),
                                                    correlation=corAR1())
                          
                          anova(null_lme_model_taxes, lme_model_taxes)
                          ##This is also a plausible model
                          lme_model_taxes1<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + I(prop_high_gm^3)*polity_change_gm,
                                                random = ~1 + Year_gm +  + polity_change_gm |Country, data=Doctor_Who_polity_delta,
                                                na.action=na.omit, method="ML", control=list(opt="optim"),
                                                correlation=corAR1())
                          summary(lme_model_taxes1)
                          
                          ###note this case done for extra analysis...really not so bad
                          lme_model_taxes1_resid<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_high_gm*polity_change_gm,
                                                      random = ~1 +Year_gm + polity_change_gm|Country, data=Doctor_Who_polity_delta,
                                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                                      correlation=corAR1())
                          summary(lme_model_taxes1_resid)
                          plot(lme_model_taxes1_resid)
                          qqnorm(lme_model_taxes1, ~ranef(., level=1))
                          library(DAMisc)
                          mod2<-lm(AVEfive_yr_inc_change~prop_adjusted_gm*cater_polity_WE , Doctor_Who)
                          trellis.par.set(
                            superpose.line = list(col=palette()[-1]),
                            superpose.polygon = list(col=palette()[-1]))
                          intQualQuant(mod2, c("prop_adjusted_gm", "cater_polity_WE"), type="slopes", plot=T, rug=T)
                          mod3<-lm(AVEfive_yr_inc_change~poly(prop_high,2)*polity_change , Doctor_Who_polity_delta)
                          summary(mod3)
                          DAintfun(mod3, c("poly(prop_high,2)","polity_change"),
                                   theta=-45, phi=20)
                          
                          
                          ###vs???
                          plot(lme_model_taxes1)
                          #
                          #c(0, .1,.2,.3,.4,.5,.6,.7,.8,.9,1)
                          lme_model_taxes_frame <- data.frame("prop_adjusted_gm*polity_change_gm" = c(1,.2,.4,.9,1.6,2.5,3.6,4.9,6.4,8.1,10),
                                                              "prop_adjusted_gm" = c(0, .1,.2,.3,.4,.5,.6,.7,.8,.9,1),
                                                              "polity_change_gm" = -c(0, 1,2,3,4,5,6,7,8,9,10),
                                                              "Year_gm" = 0)
                          lme_model_taxes_frame$pred_inc<-predict(lme_model_taxes, lme_model_taxes_frame, level = 0)
                          plot(lme_model_taxes_frame$pred_inc~lme_model_taxes_frame$prop_adjusted_gm)
                          
                          ###understanding the transformation I make
                          plot(Doctor_Who_polity_delta1i$prop_adjusted~Doctor_Who_polity_delta1i$prop_high, xlim = c(0,1), ylim = c(0,1))
                          
                          
                          #note...can slip into .05 without "correlation=corAR1()"... I don't think it should be
                          #but maybe due to the proximity of some years.... also...I(prop_high^3) or ^2
                          ###########################################################
                          ##any way to build piecewise a linear relation for dictatorships and squared for demo???
                          ###########################################################
                          
                          ####in fashion of RDD...works with bars at .3 and .7... but, note...need use pretty specific set up
                          ##does seem to hold though...as bars changed
                          
                          ###even stronger now that focused!!!
                          Doctor_Who_polity_delta1i<-Doctor_Who_polity_delta[Doctor_Who_polity_delta$prop_high>.35&Doctor_Who_polity_delta$prop_high<.65,]
                          lme_model_taxes_rdd<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_adjusted_gm*polity_change_gm,
                                                   random = ~1 + Year_gm +  + polity_change_gm |Country, data=Doctor_Who_polity_delta1i,
                                                   na.action=na.omit, method="ML", control=list(opt="optim"),
                                                   correlation=corAR1())
                          summary(lme_model_taxes_rdd)
                          
                          
                          
                          #####can also now look at demo and dict seperately
                          #as RDD
                          Doctor_Who_polity_delta1<-Doctor_Who_polity_delta1i[Doctor_Who_polity_delta1i$dummy_direction_dict=="0",]
                          #As full data
                          Doctor_Who_polity_delta11<-Doctor_Who_polity_delta[Doctor_Who_polity_delta$dummy_direction_dict=="0",]
                          length(Doctor_Who_polity_delta1$Country)
                          #okay, 85 democracies...now can swap delta1 and delta11 for rdd or not
                          #note...with and without autocorr
                          lme_model_taxes_demo<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_adjusted_gm*polity_change_gm,
                                                    random = ~1 + Year_gm +  + polity_change_gm |Country, data=Doctor_Who_polity_delta11,
                                                    na.action=na.omit, method="ML", control=list(opt="optim"),
                                                    correlation=corAR1())
                          lme_model_taxes_demo<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_high*polity_change_gm,
                                                    random = ~1 + Year_gm +  + polity_change_gm |Country, data=Doctor_Who_polity_delta1,
                                                    na.action=na.omit, method="ML", control=list(opt="optim"),
                                                    correlation=corAR1())
                          
                          summary(lme_model_taxes_demo)
                          
                          ##Let's model for the half above and half below'..this uses full data on democ
                          #this is not satisfying...not consistent with my theory of two positive slopes on each
                          #side of fifty... but also not signifigant...not many cases at play
                          Doctor_Who_polity_delta111<-Doctor_Who_polity_delta11[Doctor_Who_polity_delta11$prop_high<.5,]
                          lme_model_taxes_lowerdemo<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_high*polity_change_gm,
                                                         random = ~1 + Year_gm +  + polity_change_gm |Country, data=Doctor_Who_polity_delta111,
                                                         na.action=na.omit, method="ML", control=list(opt="optim"),
                                                         correlation=corAR1())
                          summary(lme_model_taxes_lowerdemo)
                          
                          Doctor_Who_polity_delta111i<-Doctor_Who_polity_delta11[Doctor_Who_polity_delta11$prop_high>.5,]
                          lme_model_taxes_updemo<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_high*polity_change_gm,
                                                      random = ~1 + Year_gm +  + polity_change_gm |Country, data=Doctor_Who_polity_delta111i,
                                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                                      correlation=corAR1())
                          summary(lme_model_taxes_updemo)
                          
                          
                          ##Let's model for the towards dictatorship moves...no sig...too few cases?
                          Doctor_Who_polity_delta1111<-Doctor_Who_polity_delta[Doctor_Who_polity_delta$dummy_direction_dict=="1",]
                          plot(Doctor_Who_polity_delta1111$AVEfive_yr_inc_change~Doctor_Who_polity_delta1111$prop_high)
                          lme_model_taxes_dict<-lme(AVEfive_yr_inc_change ~ 1 + Year_gm + prop_high_gm,
                                                    random = ~1 + Year_gm |Country, data=Doctor_Who_polity_delta1111,
                                                    na.action=na.omit, method="ML", control=list(opt="optim"),
                                                    correlation=corAR1())
                          summary(lme_model_taxes_dict)
                          ###note on using change not actual rate/usage... pretty much, strip out country level idiosyncracies
                          #just focus on change without incorporation of all the other things that clearly matter
                          
                          
                          
                          
                          summary(lme(Income_Pro ~ 1 + Year_gm + prop_high*polity_change,
                                      random = ~1 + Year_gm |Country, data=Doctor_Who_polity_delta1,
                                      na.action=na.omit, method="ML", control=list(opt="optim"),
                                      correlation=corAR1()))
                          
                          
                          
                          summary(lme(AVEfive_yr_inc_change~prop_high*polity_change, data = Doctor_Who_polity_delta1))
                          
                          summary(lm(AVEfive_yr_inc_change~prop_high*polity_change, data = Doctor_Who_polity_delta1))
                          
                          
                          
                          length(Doctor_Who_polity_delta$Country)
                          Doctor_Who_polity_delta
                          Doctor_Who_polity_delta
                          Doctor_Who_pre_RDDII<-Doctor_Who_pre_RDD[Doctor_Who_pre_RDD$polity_change!=0,]
                          Doctor_Who_pre_RDDII
                          
                          length(Doctor_Who_polity_delta$Country)
                          
                          test_lme_1<-lme(Income_Pro ~ 1 + Year + dummy_direction_demo*median_mean + dummy_direction_dict*prop_high,
                                          random = ~1 + Year + dummy_direction_demo + dummy_direction_dict |Country, data=Doctor_Who,
                                          na.action=na.omit, method="ML", control=list(opt="optim"),
                                          correlation=corAR1())
                          summary(test_lme_1)
                          
                          test_lme_1<-lme(Income_Pro ~ 1 + Year + cater_polity_WE*exp(prop_high),
                                          random = ~1 + Year |Country, data=Doctor_Who,
                                          na.action=na.omit, method="ML", control=list(opt="optim"))
                          summary(test_lme_1)
                          
                          test_lm<-lm(Income_Pro ~ dummy_direction_demo*prop_high + dummy_direction_dict*prop_high, data = Doctor_Who)
                          
                          test_lm<-lm(six_yr_inc_change ~ cater_polity_WE*prop_high, data = Doctor_Who)
                          
                          test_lm<-lm(six_yr_inc_change ~ cater_polity_WE*sqrt(prop_high), data = Doctor_Who)
                          
                          
                          summary(test_lm)
                          
                          
                          
                          #Doctor_Who[Doctor_Who$Country=="Germany" & Doctor_Who$Year>1990,]
                          
                          ##with added stress tests
                          Doctor_Who_polity_delta_tests<-Doctor_Who[Doctor_Who$polity_change!=0,]
                          Doctor_Who_polity_delta_tests<-na.omit(Doctor_Who_polity_delta_tests[,c("Country","Year", "Income_Pro", "AVEfive_yr_inc_change", "polity_change", "dummy_direction_dict", "polity_dummy", "cater_polity_WE",
                                                                                                  "weightings_polity_change", "Work_Index", "high_prop_low", "prop_high", "Growth", "UE", "GDP_cap", "GINI")])
                          Doctor_Who_polity_delta_tests$Year_gm<-Doctor_Who_polity_delta_tests$Year-mean(Doctor_Who_polity_delta_tests$Year)
                          Doctor_Who_polity_delta_tests$prop_high_gm<-Doctor_Who_polity_delta_tests$prop_high-mean(Doctor_Who_polity_delta_tests$prop_high)
                          Doctor_Who_polity_delta_tests$polity_change_gm<-Doctor_Who_polity_delta_tests$polity_change-mean(Doctor_Who_polity_delta_tests$polity_change)
                          Doctor_Who_polity_delta_tests$prop_adjusted<-0
                          Doctor_Who_polity_delta_tests$prop_adjusted[Doctor_Who_polity_delta_tests$polity_dummy=="0"]<-Doctor_Who_polity_delta_tests$prop_high[Doctor_Who_polity_delta_tests$polity_dummy=="0"]
                          Doctor_Who_polity_delta_tests$prop_adjusted[Doctor_Who_polity_delta_tests$polity_dummy=="1"]<-(Doctor_Who_polity_delta_tests$prop_high[Doctor_Who_polity_delta_tests$polity_dummy=="1"])^2
                          Doctor_Who_polity_delta_tests$prop_adjusted_gm<-Doctor_Who_polity_delta_tests$prop_adjusted-mean(Doctor_Who_polity_delta_tests$prop_adjusted)
                          
                          lme_model_taxes_demo_stress_test<-lme(AVEfive_yr_inc_change ~ 1 + Growth + UE + GDP_cap + GINI + Year_gm + prop_adjusted_gm*polity_change_gm,
                                                                random = ~1 + Year_gm + Growth + UE + GDP_cap + GINI + polity_change_gm |Country, data=Doctor_Who_polity_delta_tests,
                                                                na.action=na.omit, method="ML", control=list(opt="optim"),
                                                                correlation=corAR1())
                          summary(lme_model_taxes_demo_stress_test)
                          
                          lme_model_taxes_demo_stress_test<-lme(Income_Pro ~ 1 + Year_gm + prop_adjusted_gm*polity_change_gm,
                                                                random = ~1 + Year_gm   |Country, data=Doctor_Who_polity_delta_tests,
                                                                na.action=na.omit, method="ML", control=list(opt="optim"),
                                                                correlation=corAR1())
                          summary(lme_model_taxes_demo_stress_test)
                          
                          
                          
                          ################################
                          ################################
                          ######### The USSR #############
                          ################################
                          ################################
                          
                          ####Potential redresses...must be 1990s data?... would kick out weird shit...phacking?
                          
                          USSR_RDD<-
                            Doctor_Who[Doctor_Who$Country=="Albania"| Doctor_Who$Country=="Azerbaijan"|
                                         Doctor_Who$Country=="Armenia"| Doctor_Who$Country=="Bosnia and Herzegovina"|
                                         Doctor_Who$Country=="Bulgaria"| Doctor_Who$Country=="Belarus"|
                                         Doctor_Who$Country=="Croatia"|
                                         Doctor_Who$Country=="Czech Republic"|
                                         Doctor_Who$Country=="Estonia"| Doctor_Who$Country=="Georgia"| Doctor_Who$Country=="Hungary"|
                                         Doctor_Who$Country=="Kyrgyz Republic"|
                                         Doctor_Who$Country=="Latvia"| Doctor_Who$Country=="Lithuania"|
                                         Doctor_Who$Country=="Moldova"| Doctor_Who$Country=="Montenegro"|
                                         Doctor_Who$Country=="Romania"|
                                         Doctor_Who$Country=="Russian Federation"|
                                         Doctor_Who$Country=="Serbia"| Doctor_Who$Country=="Slovak Republic"|
                                         Doctor_Who$Country=="Slovenia"| Doctor_Who$Country=="Ukraine"|
                                         Doctor_Who$Country=="Macedonia, FYR",]
                          GG_USSR_Inc<-na.omit(USSR_RDD[,c("Country","Year", "Income_Pro",
                                                           "Work_Index", "high_prop_low", "prop_high")])
                          
                          summary(GG_USSR_Inc$prop_high)
                          ###GG_USSR_Inc <-aggregate(USSR_RDD$Income_Pro, by=list(USSR_RDD$Country, USSR_RDD$Year), FUN=mean, na.rm=TRUE)
                          GG_USSR_Inc$WE_Grouping<-0
                          GG_USSR_Inc$WE_Grouping[GG_USSR_Inc$prop_high<0.35] <- "1"
                          GG_USSR_Inc$WE_Grouping[GG_USSR_Inc$prop_high>=0.35 & GG_USSR_Inc$prop_high<0.5] <- "2"
                          GG_USSR_Inc$WE_Grouping[GG_USSR_Inc$prop_high>=0.5 & GG_USSR_Inc$prop_high<1] <- "3"
                          
                          GG_USSR_Inc$WE_Grouping[GG_USSR_Inc$prop_high>=0.5 & GG_USSR_Inc$prop_high<0.65] <- "4"
                          GG_USSR_Inc$WE_Grouping[GG_USSR_Inc$prop_high>=0.65] <- "5"
                          summary(as.factor(GG_USSR_Inc$WE_Grouping))
                          #####syck plot
                          GG_USSR_Inc_cater <-aggregate(GG_USSR_Inc$Income_Pro, by=list(GG_USSR_Inc$WE_Grouping, GG_USSR_Inc$Year), FUN=mean, na.rm=TRUE)
                          GG_USSR_Inc_cater<-subset(GG_USSR_Inc_cater, Group.2>1992)
                          GG_USSR_Inc_cater_plot<-qplot(Group.2, x, data=GG_USSR_Inc_cater, geom = c("point", "smooth"), colour=Group.1)+geom_line()+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
                          GG_USSR_Inc_cater_plot
                          
                          
                          
                          GG_USSR_Inc<-subset(GG_USSR_Inc, Group.2>1985)
                          
                          ##First not totally Shitty Graph Attempt
                          head(GG_USSR_Inc)
                          library(ggplot2)
                          GG_USSR_Inc_plot<-qplot(Year, Income_Pro, data=GG_USSR_Inc, colour=Country)+geom_line()+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Country")+facet_wrap(~WE_Grouping)
                          GG_USSR_Inc_plot
                          
                          
                          
                          GG_USSR_Inc_high<-subset(GG_USSR_Inc, prop_high>0.5 & prop_high<0.8)
                          GG_USSR_Inc_high_plot<-qplot(Year, Income_Pro, data=GG_USSR_Inc_high, colour=Country)+geom_line()+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Country")
                          GG_USSR_Inc_high_plot
                          GG_USSR_Inc_low<-subset(GG_USSR_Inc, prop_high>0.4 & prop_high<0.5)
                          GG_USSR_Inc_low_plot<-qplot(Year, Income_Pro, data=GG_USSR_Inc_low, colour=Country)+geom_line()+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Country")
                          GG_USSR_Inc_low_plot
                          
                          
                          GG_USSR_Inc_rdd<-subset(GG_USSR_Inc, prop_high>0.4 & prop_high<0.6)
                          GG_USSR_Inc_rdd
                          
                          GG_USSR_Inc_rdd_plot<-qplot(Year, Income_Pro, data=GG_USSR_Inc_rdd, colour=Country)+geom_line()+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Country")
                          GG_USSR_Inc_rdd_plot
                          
                          GG_USSR_Inc
                          GG_USSR_Inc_lme<-lme(Income_Pro ~ 1 + Year*prop_high,
                                               random = ~1 + Year |Country, data=GG_USSR_Inc_rdd,
                                               na.action=na.omit, method="ML", control=list(opt="optim"),
                                               correlation=corAR1())
                          summary(GG_USSR_Inc_lme)
                          
                          GG_USSR_Inc_lme<-lme(Income_Pro ~ 1 + Year + poly(prop_high,2),
                                               random = ~1 + Year |Country, data=GG_USSR_Inc,
                                               na.action=na.omit, method="ML", control=list(opt="optim"),
                                               correlation=corAR1())
                          summary(GG_USSR_Inc_lme)
                          
                          ##################
                          ### Income Tax###
                          #################
                          USSR_RDD_sub
                          USSR_RDD_sub<-na.omit(USSR_RDD[,c("Country","Year", "Income_Pro", "Growth", "UE", "GDP_cap", "GINI",
                                                            "Work_Index", "high_prop_low", "prop_high")])
                          USSR_RDD_sub$Work_Index_SQ<-USSR_RDD_sub$Work_Index^2
                          USSR_RDD_sub$prop_high_sq<-USSR_RDD_sub$prop_high^2
                          
                          USSR_RDD_sub$dummy_WE<-as.numeric(USSR_RDD_sub$Work_Index>3.5)
                          unique(USSR_RDD_sub$Country)
                          
                          unique(USSR_RDD_sub$Country)
                          describe(USSR_RDD_sub$Work_Index)
                          hist(USSR_RDD_sub$Work_Index, freq = TRUE)
                          describe(USSR_RDD_sub$prop_high)
                          hist(USSR_RDD_sub$prop_high, freq = TRUE)
                          
                          summary(USSR_RDD_sub$Work_Index_SQ)
                          plot(tapply(USSR_RDD_sub$Income_Pro, USSR_RDD_sub$Country, mean) ~ tapply(USSR_RDD_sub$Work_Index_SQ, USSR_RDD_sub$Country, mean))
                          lm_USSR<-lm(tapply(USSR_RDD_sub$Income_Pro, USSR_RDD_sub$Country, mean) ~ tapply(USSR_RDD_sub$Work_Index, USSR_RDD_sub$Country, mean))
                          
                          summary(lm_USSR)
                          abline(lm_USSR)
                          
                          #####
                          ####
                          ###
                          plot(tapply(USSR_RDD_sub$Income_Pro, USSR_RDD_sub$Country, mean) ~ tapply(USSR_RDD_sub$prop_high, USSR_RDD_sub$Country, mean), main = "income taxes and proportion of high work ethic", ylab = "income tax", xlab = "prop high")
                          lm_USSR_II<-lm(tapply(USSR_RDD_sub$Income_Pro, USSR_RDD_sub$Country, mean) ~ tapply(USSR_RDD_sub$prop_high, USSR_RDD_sub$Country, mean))
                          lm_USSR_II<-lm(tapply(USSR_RDD_sub$Income_Pro, USSR_RDD_sub$Country, mean) ~ tapply(USSR_RDD_sub$prop_high_sq, USSR_RDD_sub$Country, mean))
                          
                          lm_USSR_III<-lm(tapply(USSR_RDD_sub$Income_Pro, USSR_RDD_sub$Country, mean) ~ poly(tapply(USSR_RDD_sub$prop_high, USSR_RDD_sub$Country, mean), 2))
                          abline(lm_USSR_II)
                          summary(lm_USSR_II)
                          stargazer(lm_USSR_II)
                          ###works...prob have to justify the 3.75
                          aveUSSR_post<-tapply(USSR_RDD_sub$Work_Index, USSR_RDD_sub$Country, mean)
                          aveUSSR_post
                          
                          summary(USSR_RDD_sub$Work_Index)
                          #mean/median
                          t.test(tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$Work_Index>3.6],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$Work_Index>3.6], median),
                                 tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$Work_Index<3.6],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$Work_Index<3.6], median))
                          
                          t.test(tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$Work_Index_SQ>12.5],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$Work_Index_SQ>12.5], mean),
                                 tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$Work_Index_SQ<12.5],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$Work_Index_SQ<12.5], mean))
                          
                          
                          t.test(tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$Work_Index>3.75],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$Work_Index>3.75], mean),
                                 tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$Work_Index<3.75],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$Work_Index<3.75], mean))
                          
                          t.test(tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high>.5],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high>.5], median),
                                 tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high<.5],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high<.5], median))
                          
                          USSR_RDD_sub$prop_high_sq
                          
                          
                          USSR_RDD$Taxes.on.income..profits..and.capital.gains.
                          
                          USSR_RDD$Indiv_all<-as.numeric(USSR_RDD$Taxes.on.income..profits..and.capital.gains.)
                          USSR_RDD$Sales<-as.numeric(USSR_RDD$Taxes.on.goods.and.services..of.which.Taxes.on.Sales)
                          USSR_RDD$Inc_Sales<-USSR_RDD$Income_Pro/USSR_RDD$Sales
                          USSR_RDD_sub_ii<-na.omit(USSR_RDD[,c("Country","Year", "Indiv_all", "Growth", "UE", "GDP_cap", "GINI",
                                                               "Work_Index", "high_prop_low", "prop_high")])
                          t.test(tapply(USSR_RDD_sub_ii$Indiv_all[USSR_RDD_sub_ii$prop_high>.6],
                                        USSR_RDD_sub_ii$Country[USSR_RDD_sub_ii$prop_high>.6], mean),
                                 tapply(USSR_RDD_sub_ii$Indiv_all[USSR_RDD_sub_ii$prop_high<.6],
                                        USSR_RDD_sub_ii$Country[USSR_RDD_sub_ii$prop_high<.6], mean))
                          USSR_RDD_sub_ii
                          lm_USSR_II<-lm(tapply(USSR_RDD_sub_ii$Inc_Sales, USSR_RDD_sub_ii$Country, mean) ~ tapply(USSR_RDD_sub_ii$Inc_Sales, USSR_RDD_sub_ii$Country, mean))
                          summary(lm_USSR_II)
                          USSR_lme_is<-lme(Inc_Sales ~ 1 + year + high_prop_low,
                                           random = ~1 +year  |Country, data=USSR_RDD_sub_ii,
                                           na.action=na.omit, method="ML", control=list(opt="optim"),
                                           correlation=corAR1())
                          summary(USSR_lme_is)
                          plot(tapply(USSR_RDD_sub_ii$Inc_Sales, USSR_RDD_sub_ii$Country, mean) ~ tapply(USSR_RDD_sub_ii$prop_high, USSR_RDD_sub_ii$Country, mean), main = "income taxes and proportion of high work ethic", ylab = "income tax", xlab = "prop high")
                          
                          ####
                          ####
                          ###
                          
                          t.test(tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high_sq>.15],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high_sq>.15], median),
                                 tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high_sq<.15],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high_sq<.15], median))
                          
                          ###
                          ###
                          ###
                          
                          
                          
                          t.test(tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high>.5],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high>.5], median),
                                 tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high<.5],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high<.5], median))
                          
                          USSR_lme_null<-lme(Income_Pro ~ 1,
                                             random = ~1|Country, data=USSR_RDD_sub,
                                             na.action=na.omit, method="ML", control=list(opt="optim"),
                                             correlation=corAR1())
                          
                          #######
                          USSR_lme<-lme(Income_Pro ~ 1 + Growth + UE + GINI + log(GDP_cap)+ poly(prop_high,2),
                                        random = ~1  + Growth + UE+ GINI + log(GDP_cap)|Country, data=USSR_RDD_sub,
                                        na.action=na.omit, method="ML", control=list(opt="optim"),
                                        correlation=corAR1())
                          summary(USSR_lme)
                          ####
                          ###
                          ###I think the poly nomial really captures around the 50% mark while allowing the tails to trail off
                          ##more flatly/with less commitment than the I(^2) which forces the tails downward (my theory
                          ##is agnostic about the tails I think)
                          summary(USSR_lme)
                          USSR_lme<-lme(Income_Pro ~ 1+ poly(prop_high, 2),
                                        random = ~1  |Country, data=USSR_RDD_sub,
                                        na.action=na.omit, method="ML", control=list(opt="optim"),
                                        correlation=corAR1())
                          USSR_lme1<-lme(Income_Pro ~ 1+ I(prop_high^2),
                                         random = ~1  |Country, data=USSR_RDD_sub,
                                         na.action=na.omit, method="ML", control=list(opt="optim"),
                                         correlation=corAR1())
                          summary(USSR_lme1)
                          ###
                          ##
                          ##
                          USSR_lme<-lme(Income_Pro ~ 1 + dummy_WE,
                                        random = ~1 |Country, data=USSR_RDD_sub,
                                        na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          summary(USSR_lme)
                          anova(USSR_lme_null, USSR_lme)
                          
                          
                          USSR_lme_gm<-lme(Income_Pro~ 1+ Year_gm + agri_gm + log_GDP_gm + log_GDP_cap_gm +
                                             polity_dummy * Work_Index_gm, random = ~1 + Year_gm +
                                             log_GDP_cap_gm + agri_gm + polity_dummy |Country,
                                           data=tax_econ_polity_informal_WVS,
                                           na.action=na.omit, method="ML",
                                           control=list(opt="optim"), correlation=corAR1())
                          
                          
                          
                          ###still to do
                          #a t-test with restricted sample..."proper" RDD about .5
                          USSR_RDD_sub$prop_high_dem_quant<-cut(USSR_RDD_sub$prop_high, c(0, 0.2, 0.5, 0.8, 1), right=FALSE, ordered_result = TRUE, labels = c("first", "second", "third", "fourth"))
                          
                          aggregate(USSR_RDD_sub$Income_Pro, by = list(USSR_RDD_sub$prop_high_dem_quant), FUN = mean)
                          aggregate(USSR_RDD_sub$Income_Pro, by = list(USSR_RDD_sub$prop_high_dem_quant), FUN = median)
                          
                          aggregate(USSR_RDD_sub$Income_Pro, by = list(USSR_RDD_sub$prop_high_dem_quant), FUN = count)
                          
                          t.test(tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high_dem_quant=="second"],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high_dem_quant=="second"], median),
                                 tapply(USSR_RDD_sub$Income_Pro[USSR_RDD_sub$prop_high_dem_quant=="third"],
                                        USSR_RDD_sub$Country[USSR_RDD_sub$prop_high_dem_quant=="third"], median))
                          
                          ###general anova across all four categories
                          summary(aov(Income_Pro~prop_high_dem_quant, data = USSR_RDD_sub))
                          hold<-aov(Income_Pro[USSR_RDD_sub$prop_high_dem_quant=="second"|USSR_RDD_sub$prop_high_dem_quant=="third"]~prop_high_dem_quant[USSR_RDD_sub$prop_high_dem_quant=="second"|USSR_RDD_sub$prop_high_dem_quant=="third"], data = USSR_RDD_sub)
                          summary(hold)
                          
                          USSR_categor_null<-lme(Income_Pro ~ 1,
                                                 random = ~1 |Country, data=USSR_RDD_sub,
                                                 na.action=na.omit, method="ML", control=list(opt="optim"),
                                                 correlation=corAR1())
                          #####could use to predict
                          USSR_categor<-lme(Income_Pro ~ 1 + prop_high_dem_quant,
                                            random = ~1 |Country, data=USSR_RDD_sub,
                                            na.action=na.omit, method="ML", control=list(opt="optim"),
                                            correlation=corAR1())
                          summary(USSR_categor)
                          anova(USSR_categor_null, USSR_categor)
                          
                          #a test that looks at diff in income tax use
                          USSR_RDD_sub_diff<-na.omit(USSR_RDD[,c("Country","Year", "Income_Pro", "Growth", "UE", "GDP_cap", "GINI",
                                                                 "Work_Index", "two_yr_inc_change", "high_prop_low", "prop_high")])
                          
                          USSR_RDD_sub_diff
                          t.test(tapply(USSR_RDD_sub_diff$two_yr_inc_change[USSR_RDD_sub_diff$Work_Index>3.75],
                                        USSR_RDD_sub_diff$Country[USSR_RDD_sub_diff$Work_Index>3.75], mean),
                                 tapply(USSR_RDD_sub_diff$two_yr_inc_change[USSR_RDD_sub_diff$Work_Index<3.75],
                                        USSR_RDD_sub_diff$Country[USSR_RDD_sub_diff$Work_Index<3.75], mean))
                          t.test(tapply(USSR_RDD_sub_diff$two_yr_inc_change[USSR_RDD_sub_diff$Work_Index>3.5],
                                        USSR_RDD_sub_diff$Country[USSR_RDD_sub_diff$Work_Index>3.5], median),
                                 tapply(USSR_RDD_sub_diff$two_yr_inc_change[USSR_RDD_sub_diff$Work_Index<3.5],
                                        USSR_RDD_sub_diff$Country[USSR_RDD_sub_diff$Work_Index<3.5], median))
                          
                          #a test that incorporates non-lin relation that I anticipate
                          
                          
                          #attempt to account for backslides
                          #
                          #
                          USSR_RDD_sub_polity<-na.omit(USSR_RDD[,c("Country","Year", "Income_Pro", "polity_dummy", "polity_change",
                                                                   "dummy_direction_dict", "Work_Index", "high_prop_low", "prop_high")])
                          USSR_lme_bs_null<-lme(Income_Pro ~ 1,
                                                random = ~1 |Country, data=USSR_RDD_sub_polity,
                                                na.action=na.omit, method="ML", control=list(opt="optim"),
                                                correlation=corAR1())
                          USSR_lme_bs_init<-lme(Income_Pro ~ 1+ poly(prop_high, 2),
                                                random = ~1 |Country, data=USSR_RDD_sub_polity,
                                                na.action=na.omit, method="ML", control=list(opt="optim"),
                                                correlation=corAR1())
                          USSR_lme_bs<-lme(Income_Pro ~ 1+ poly(prop_high, 2)*polity_change,
                                           random = ~1 + dummy_direction_dict |Country, data=USSR_RDD_sub_polity,
                                           na.action=na.omit, method="ML", control=list(opt="optim"),
                                           correlation=corAR1())
                          summary(USSR_lme_bs_init)
                          summary(USSR_lme_bs)
                          
                          anova(USSR_lme_bs_null, USSR_lme_bs)
                          
                          USSR_lme_bs_frame <- data.frame("poly(prop_high, 2)*polity_change" = 0,
                                                          "prop_high" = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
                                                          "polity_change" = 0)
                          USSR_lme_bs_frame$predinc<-predict(USSR_lme_bs, USSR_lme_bs_frame, level = 0)
                          plot(USSR_lme_bs_frame$predinc~USSR_lme_bs_frame$prop_high, main = "predicted income tax at level of prop_high", xlab = "prop_high", ylab = "predicted income taxes")
                          
                          ####################################
                          ### Now with indiv profits too #####
                          ####################################
                          
                          USSR_RDD_Indic_sub<-na.omit(USSR_RDD[,c("Country","Year", "Indiv_inc_prof", "Growth", "UE", "GDP_cap", "GINI",
                                                                  "Work_Index", "high_prop_low", "prop_high")])
                          
                          plot(tapply(USSR_RDD_Indic_sub$Indiv_inc_prof, USSR_RDD_Indic_sub$Country, mean) ~ tapply(USSR_RDD_Indic_sub$Work_Index, USSR_RDD_Indic_sub$Country, mean))
                          lm_USSR_ind<-lm(tapply(USSR_RDD_Indic_sub$Indiv_inc_prof, USSR_RDD_Indic_sub$Country, mean) ~ tapply(USSR_RDD_Indic_sub$Work_Index, USSR_RDD_Indic_sub$Country, mean))
                          summary(lm_USSR_ind)
                          abline(lm_USSR_ind)
                          plot(tapply(USSR_RDD_Indic_sub$Indiv_inc_prof, USSR_RDD_Indic_sub$Country, mean) ~ tapply(USSR_RDD_Indic_sub$prop_high, USSR_RDD_Indic_sub$Country, mean))
                          lm_USSRind_II<-lm(tapply(USSR_RDD_Indic_sub$Indiv_inc_prof, USSR_RDD_Indic_sub$Country, mean) ~ tapply(USSR_RDD_Indic_sub$prop_high, USSR_RDD_Indic_sub$Country, mean))
                          summary(lm_USSRind_II)
                          abline(lm_USSRind_II)
                          
                          USSR__ind_lme<-lme(Indiv_inc_prof ~ 1 + Growth + UE + log(GDP_cap) + GINI + poly(prop_high,2),
                                             random = ~1  + Growth + UE+ log(GDP_cap) + GINI|Country, data=USSR_RDD_Indic_sub,
                                             na.action=na.omit, method="ML", control=list(opt="optim"),
                                             correlation=corAR1())
                          
                          USSR__ind_lme<-lme(Indiv_inc_prof ~ 1 + Growth + UE + poly(prop_high,2),
                                             random = ~1  + Growth + UE|Country, data=USSR_RDD_Indic_sub,
                                             na.action=na.omit, method="ML", control=list(opt="optim"),
                                             correlation=corAR1())
                          
                          summary(USSR__ind_lme)
                          
                          t.test(tapply(USSR_RDD_Indic_sub$Indiv_inc_prof[USSR_RDD_Indic_sub$prop_high>.5],
                                        USSR_RDD_Indic_sub$Country[USSR_RDD_Indic_sub$prop_high>.5], mean),
                                 tapply(USSR_RDD_Indic_sub$Indiv_inc_prof[USSR_RDD_Indic_sub$prop_high<.5],
                                        USSR_RDD_Indic_sub$Country[USSR_RDD_Indic_sub$prop_high<.5], mean))
                          
                          #######################
                          #######################
                          ####### RDD ###########
                          #######################
                          #######################
                          unique(Doctor_Who$polity2[Doctor_Who$Country=="Algeria"])
                          WVS_Work_Leisure_Scores
                          
                          Doctor_Who$cater_polity<-as.factor(cut(Doctor_Who$polity_change, c(-20, -0.1, 0.1, 20), right=FALSE, ordered_result = TRUE, labels = c("dic_slid", "even", "dem_slid")))
                          levels(Doctor_Who$cater_polity)
                          Doctor_Who$cater_polity <- relevel(Doctor_Who$cater_polity, ref = "even")
                          
                          doc_who_without_lme<-lme(AVEfive_yr_inc_change ~ 1 +  dummy_direction_dict*high_prop_low + dummy_direction_demo*poly(high_prop_low,2), random = ~ 1  + dummy_direction_demo + dummy_direction_dict|Country,
                                                   data=Doctor_Who, na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          doc_who_without_lme<-lme(AVEfive_yr_inc_change ~ 1 +  polity_change*high_prop_low, random = ~ 1  + polity_change|Country,
                                                   data=Doctor_Who, na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          summary(doc_who_without_lme)
                          Doctor_Who$dummy_direction_demo<-as.numeric(Doctor_Who$polity_change>0)
                          Doctor_Who$dummy_direction_dict<-as.numeric(Doctor_Who$polity_change<0)
                          Doctor_Who$become_democ<-as.numeric(Doctor_Who$polity_change_dum>0)
                          Doctor_Who$become_dict<-as.numeric(Doctor_Who$polity_change_dum<0)
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          doc_who_without_lme<-lme(three_yr_inc_change ~ 1 + Year + log(GDP_cap) + UE + prop_high*polity_change*polity_dummy, random = ~ 1 + Year + log(GDP_cap) + UE |Country,
                                                   data=Doctor_Who_RDD,
                                                   na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          
                          
                          doc_who_lme<-lme(three_yr_inc_change ~ 1 + Year + log(GDP_cap) + UE + prop_high*polity_change*polity_dummy, random = ~ 1 + Year + log(GDP_cap) + UE |Country,
                                           data=Doctor_Who_RDD,
                                           na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          
                          summary(doc_who_lme)
                          ####another method....RESTOR TO .3 and .6
                          Doctor_Who_pre_RDD<-Doctor_Who[Doctor_Who$prop_high>.4&Doctor_Who$prop_high<.6,]
                          Doctor_Who_pre_RDDII<-Doctor_Who_pre_RDD[Doctor_Who_pre_RDD$polity_change!=0,]
                          Doctor_Who_pre_RDDIII<-na.omit(Doctor_Who_pre_RDDII[ ,c("Country", "Year", "Income_Pro", "six_yr_inc_change", "dummy_direction_demo", "polity_change",
                                                                                  "Work_Index", "GDP_cap", "UE", "Growth", "high_prop_low", "prop_high")])
                          Doctor_Who_pre_RDDIII<-na.omit(Doctor_Who_pre_RDDII[ ,c("Country", "Year", "Income_Pro", "dummy_direction_demo", "polity_change",
                                                                                  "Work_Index", "high_prop_low", "prop_high")])
                          plot(Doctor_Who_pre_RDDIII$six_yr_inc_change~Doctor_Who_pre_RDDIII$polity_change*Doctor_Who_pre_RDDIII$prop_high)
                          text(Doctor_Who_pre_RDDIII$prop_high, Doctor_Who_pre_RDDIII$six_yr_inc_change, Doctor_Who_pre_RDDIII$Country)
                          ###not sig... but really cool and telling...at .35 and .65...then near sig at .3 and .6
                          Doctor_Who_pre_RDDI_lme<-lme(six_yr_inc_change ~ 1 + Year + dummy_direction_demo*high_prop_low, random = ~ 1  + Year + dummy_direction_demo|Country,
                                                       data=Doctor_Who_pre_RDDIII, na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          
                          Doctor_Who_pre_RDDI_lme<-lme(six_yr_inc_change ~ 1 +  dummy_direction_demo*poly(prop_high, 2)+  Year, random = ~ 1 + dummy_direction_demo+  Year|Country,
                                                       data=Doctor_Who_pre_RDDIII, na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          
                          Doctor_Who_pre_RDDI_lme<-lme(six_yr_inc_change ~ 1 +  Year + polity_change*poly(prop_high, 2), random = ~ 1  +polity_change+ Year |Country,
                                                       data=Doctor_Who_pre_RDDIII, na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          
                          Doctor_Who_pre_RDDI_lme<-lme(six_yr_inc_change ~ 1 +  Year + polity_change*prop_high, random = ~ 1  +polity_change+ Year |Country,
                                                       data=Doctor_Who_pre_RDDIII, na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          t.test(Doctor_Who_pre_RDDIII$six_yr_inc_change[Doctor_Who_pre_RDDIII$high_prop_low>3], Doctor_Who_pre_RDDIII$six_yr_inc_change[Doctor_Who_pre_RDDIII$high_prop_low<3])
                          t.test(Doctor_Who_pre_RDDIII$Income_Pro[Doctor_Who_pre_RDDIII$Work_Index>3.5 & Doctor_Who_pre_RDDIII$dummy_direction_demo=="1"], Doctor_Who_pre_RDDIII$Income_Pro[Doctor_Who_pre_RDDIII$Work_Index<3.5 & Doctor_Who_pre_RDDIII$dummy_direction_demo=="1"])
                          t.test(Doctor_Who_pre_RDDIII$six_yr_inc_change[Doctor_Who_pre_RDDIII$high_prop_low>4 & Doctor_Who_pre_RDDIII$dummy_direction_demo=="0"], Doctor_Who_pre_RDDIII$six_yr_inc_change[Doctor_Who_pre_RDDIII$high_prop_low<4& Doctor_Who_pre_RDDIII$dummy_direction_demo=="0"])
                          
                          summary(Doctor_Who_pre_RDDIII$high_prop_low[Doctor_Who_pre_RDDIII$dummy_direction_demo=="1"])
                          
                          
                          summary(Doctor_Who_pre_RDDI_lme)
                          
                          Doctor_Who_pre_RDDI_lm<-lm(six_yr_inc_change~polity_change*poly(prop_high, 3), data = Doctor_Who_pre_RDDIII)
                          Doctor_Who_pre_RDDI_lm<-lm(six_yr_inc_change~polity_change*I(prop_high^1/3), data = Doctor_Who_pre_RDDIII)
                          
                          Doctor_Who_pre_RDDI_lm<-lm(Income_Pro~polity_change*poly(prop_high,3), data = Doctor_Who_pre_RDDIII)
                          summary(Doctor_Who_pre_RDDI_lm)
                          
                          Doctor_Who_pre_RDDIII
                          
                          
                          ###okay, diff method for same idea
                          
                          Doctor_Who_RDD<-Doctor_Who[Doctor_Who$polity_change!=0,]
                          Doctor_Who_RDD_i<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "three_yr_inc_change", "polity_change_dum",
                                                                       "Work_Index", "GDP_cap", "UE", "Growth", "high_prop_low", "prop_high")])
                          Doctor_Who_RDD_i<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "six_yr_inc_change", "polity_change_dum",
                                                                       "Work_Index", "GDP_cap", "UE", "Growth", "high_prop_low", "prop_high")])
                          Doctor_Who_RDD_ii<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "polity_change", "dummy_direction_demo",
                                                                        "Work_Index", "high_prop_low", "prop_high", "three_yr_inc_change")])
                          
                          ###GG_USSR_Inc <-aggregate(USSR_RDD$Income_Pro, by=list(USSR_RDD$Country, USSR_RDD$Year), FUN=mean, na.rm=TRUE)
                          Doctor_Who_gg<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "polity_change", "dummy_direction_demo", "six_yr_inc_change",
                                                                    "Work_Index", "high_prop_low", "prop_high")])
                          Doctor_Who_RDD$AVEfive_yr_inc_change
                          Doctor_Who_gg$WE_Grouping<-0
                          Doctor_Who_gg$WE_Grouping[Doctor_Who_gg$prop_high<0.35] <- "1"
                          Doctor_Who_gg$WE_Grouping[Doctor_Who_gg$prop_high>=0.35 & Doctor_Who_gg$prop_high<0.5] <- "2"
                          Doctor_Who_gg$WE_Grouping[Doctor_Who_gg$prop_high>=0.5 & Doctor_Who_gg$prop_high<1] <- "3"
                          summary(as.factor(Doctor_Who_gg$dummy_direction_demo))
                          
                          #####syck plot
                          ##Doctor_Who_gg_plot <-aggregate(GG_USSR_Inc$Income_Pro, by=list(GG_USSR_Inc$WE_Grouping, GG_USSR_Inc$Year), FUN=mean, na.rm=TRUE)
                          ##GG_USSR_Inc_cater<-subset(GG_USSR_Inc_cater, Group.2>1992)
                          #really interesting with three year change
                          Doctor_Who_gg_plot<-qplot(prop_high, six_yr_inc_change, data=Doctor_Who_gg, geom = c("smooth"), colour=as.factor(dummy_direction_demo))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
                          Doctor_Who_gg_plot
                          Doctor_Who_gg_plot<-qplot(prop_high, Income_Pro, data=Doctor_Who_gg, geom = c("smooth"), colour=as.factor(dummy_direction_demo))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
                          
                          
                          
                          AVEfive_yr_inc_change
                          
                          Doctor_Who_RDD$AVEfive_yr_inc_changeIS
                          Doctor_Who_gg_dis<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "become_democ", "eleven_yr_inc_change",
                                                                        "Work_Index", "high_prop_low", "prop_high")])
                          Doctor_Who_gg_dis
                          #####three_yr_inc_change...AVEfive_yr_inc_change also interesting...two years
                          #really cool with per_diff_after
                          ##bat man
                          Doctor_Who_gg_dis_plot<-qplot(prop_high, per_diff_after, data=Doctor_Who_gg_dis, geom = c("smooth"), colour=as.factor(become_democ))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
                          Doctor_Who_gg_dis_plot
                          Doctor_Who_RDD$per_diff_after
                          Doctor_Who_gg_dis<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "per_diff_after", "become_democ",
                                                                        "Work_Index", "high_prop_low", "prop_high")])
                          
                          Doctor_Who_RDD$AVEfive_yr_inc_change
                          Doctor_Who_RDD$dummy_direction_demo
                          Doctor_Who_RDD<-Doctor_Who[Doctor_Who$polity_change!=0,]
                          Doctor_Who_RDD$per_diff_after
                          Doctor_Who_gg_dis<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "countzero", "dummy_direction_demo", "six_yr_inc_change", "ten_yr_inc_change", "two_yr_inc_change", "AVEfive_yr_inc_change",
                                                                        "polity_change", "Work_Index", "high_prop_low", "prop_high")])
                          Doctor_Who_gg_dis
                          Doctor_Who_gg_dis<-Doctor_Who_gg_dis[Doctor_Who_gg_dis$Year>1980,]
                          Doctor_Who_gg_dis<-Doctor_Who_gg_dis[Doctor_Who_gg_dis$polity_change<(-3)|Doctor_Who_gg_dis$polity_change>3,]
                          
                          Doctor_Who_gg_dis$WE_Grouping<-0
                          Doctor_Who_gg_dis$WE_Grouping[Doctor_Who_gg_dis$prop_high<0.35] <- "1"
                          Doctor_Who_gg_dis$WE_Grouping[Doctor_Who_gg_dis$prop_high>=0.35 & Doctor_Who_gg_dis$prop_high<0.5] <- "2"
                          Doctor_Who_gg_dis$WE_Grouping[Doctor_Who_gg_dis$prop_high>=0.5 & Doctor_Who_gg_dis$prop_high<1] <- "3"
                          
                          
                          
                          
                          Doctor_Who_ggcase<-Doctor_Who[Doctor_Who$Year>1980]
                          Doctor_Who_gg_dis_plo_yrs<-qplot(prop_high, ten_yr_inc_change, data=Doctor_Who_gg_dis, geom = c("smooth"), colour=as.factor(dummy_direction_demo))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
                          Doctor_Who_gg_dis_plo_yrs
                          
                          Doctor_Who_RDD1<-Doctor_Who[Doctor_Who$polity_change_dum!=0,]
                          Doctor_Who_RDD1_1<-na.omit(Doctor_Who_RDD1[ ,c("Country", "Year", "Income_Pro", "polity_change_dum", "six_yr_inc_change",
                                                                         "polity_change", "Work_Index", "prop_high")])
                          Doctor_Who_RDD1_1
                          
                          six_yr_inc_change_plot<-qplot(prop_high, six_yr_inc_change, data=Doctor_Who_RDD1_1, geom = c("smooth"), colour=as.factor(polity_change_dum))+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
                          six_yr_inc_change_plot
                          
                          
                          
                          "six_yr_inc_change", "AVEfive_yr_inc_change",
                          plot(Doctor_Who_RDD_ii$Income_Pro~Doctor_Who_RDD_ii$prop_high)
                          length(unique(Doctor_Who_RDD$Country))
                          
                          length(Doctor_Who_RDD_ii$Country)
                          
                          
                          
                          
                          
                          
                          
                          #### with and without poly
                          #
                          #
                          #
                          summary(lm(Doctor_Who_RDD_ii$Income_Pro~poly(Doctor_Who_RDD_ii$prop_high,3)*Doctor_Who_RDD_ii$dummy_direction_demo))
                          case_works_trial<-(lm(Income_Pro~high_prop_low*dummy_direction_demo, data = Doctor_Who_RDD_ii))
                          summary(case_works_trial)
                          case_works_frame <- data.frame("dummy_direction_demo*high_prop_low" = 0,
                                                         "dummy_direction_demo" = 1,
                                                         "high_prop_low" = c(0, 5,10,15,20,25,30,35,40,45))
                          case_works_frame$okay<-predict(case_works_trial, case_works_frame)
                          plot(case_works_frame$okay~case_works_frame$high_prop_low)
                          
                          #
                          #
                          #
                          ####
                          Doctor_Who_RDD_iii<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "polity_change", "dummy_direction_demo", "dummy_direction_dict", "Growth", "UE", "GDP_cap",
                                                                         "Work_Index", "high_prop_low", "prop_high", "three_yr_inc_change")])
                          summary(lm(Income_Pro~sqrt(high_prop_low)*dummy_direction_demo, data = Doctor_Who_RDD_iii))
                          summary(lm(Doctor_Who_RDD_ii$Income_Pro~poly(Doctor_Who_RDD_ii$prop_high, 3)*Doctor_Who_RDD_ii$dummy_direction_demo))
                          
                          case_works<-(lm(Income_Pro~dummy_direction_demo*high_prop_low+GDP_cap+Growth, data = Doctor_Who_RDD_iii))
                          summary(case_works)
                          summary(Doctor_Who_RDD_ii$high_prop_low)
                          
                          Doctor_Who_RDD_iii
                          test_tri<-lm(Income_Pro~poly(prop_high,3)*dummy_direction_demo, data = Doctor_Who_RDD_iii)
                          summary(test_tri)
                          case_works_tri_frame <- data.frame("poly(prop_high,3)*dummy_direction_demo" = .5,
                                                             "prop_high" = c(0, .1,.2,.3,.4,.5,.6,.7,.8,.9,1),
                                                             "dummy_direction_demo" = 1)
                          case_works_tri_frame$okay<-predict(test_tri, case_works_tri_frame)
                          plot(case_works_tri_frame$okay~case_works_tri_frame$prop_high)
                          
                          plot(Income_Pro~prop_high, data=Doctor_Who_RDD_iii)
                          
                          test_tri<-lm(Income_Pro~poly(prop_high,3)*dummy_direction_demo, data = Doctor_Who_RDD_iii)
                          
                          
                          #
                          #
                          #
                          USSR_RDD_sub$prop_high_dem_quant<-
                            
                            Doctor_Who_RDD_iii$catergor_polity<-cut(Doctor_Who_RDD_iii$polity_change, c(-20, -0.1, 0.1, 20), right=FALSE, ordered_result = TRUE, labels = c("dic_slid", "even", "dem_slid"))
                          boxplot(three_yr_inc_change~catergor_polity, data=Doctor_Who_RDD_iii)
                          case_works<-(lm(Income_Pro~dummy_direction_demo*high_prop_low+GDP_cap+Growth, data = Doctor_Who_RDD_iii))
                          
                          case_works<-(lm(Income_Pro~dummy_direction_demo*poly(prop_high, 3), data = Doctor_Who_RDD_iii))
                          case_works<-(lm(Income_Pro~dummy_direction_demo*poly(prop_high, 2)+GDP_cap+Growth, data = Doctor_Who_RDD_iii))
                          summary(case_works)
                          
                          
                          ####
                          case_works<-(lm(Income_Pro~dummy_direction_demo*prop_high+GDP_cap+Growth, data = Doctor_Who_RDD_iii))
                          summary(case_works)
                          ####
                          
                          
                          2.27*2000
                          
                          Doctor_Who_RDD_iiii<-na.omit(Doctor_Who_RDD[ ,c("Country", "Year", "Income_Pro", "polity_change", "high_prop_low", "prop_high", "three_yr_inc_change")])
                          t.test(Doctor_Who_RDD_iiii$Income_Pro[Doctor_Who_RDD_iiii$prop_high>median(Doctor_Who_RDD_iiii$prop_high)], Doctor_Who_RDD_iiii$Income_Pro[Doctor_Who_RDD_iiii$prop_high<median(Doctor_Who_RDD_iiii$prop_high)])
                          Doctor_Who_RDD_iiii$prop_high_dem_quant<-cut(Doctor_Who_RDD_iiii$prop_high, c(0, 0.35, 0.5, 0.65, 1), right=FALSE, ordered_result = TRUE, labels = c("first", "second", "third", "fourth"))
                          
                          boxplot(Income_Pro[dummy_direction_demo=="1"]~prop_high_dem_quant[dummy_direction_demo=="1"], data = Doctor_Who_RDD_iiii)
                          boxplot(Income_Pro[polity_change>0]~prop_high_dem_quant[polity_change>0], data = Doctor_Who_RDD_iiii)
                          boxplot(Income_Pro[polity_change<0]~prop_high_dem_quant[polity_change<0], data = Doctor_Who_RDD_iiii)
                          
                          
                          ###"Growth", "UE", "GDP_cap", "GINI",
                          tax_econ_polity_informal_WVS$GDP_cap_gm<-tax_econ_polity_informal_WVS$GDP_cap-mean(tax_econ_polity_informal_WVS$GDP_cap, na.rm = TRUE)
                          tax_econ_polity_informal_WVS$agri_gm<-tax_econ_polity_informal_WVS$agri-mean(tax_econ_polity_informal_WVS$agri, na.rm = TRUE)
                          tax_econ_polity_informal_WVS$GDP_gm<-tax_econ_polity_informal_WVS$GDP-mean(tax_econ_polity_informal_WVS$GDP, na.rm = TRUE)
                          tax_econ_polity_informal_WVS$Work_Index_gm<-tax_econ_polity_informal_WVS$Work_Index-mean(tax_econ_polity_informal_WVS$Work_Index, na.rm = TRUE)
                          tax_econ_polity_informal_WVS$Year_gm<-tax_econ_polity_informal_WVS$Year-mean(tax_econ_polity_informal_WVS$Year, na.rm = TRUE)
                          tax_econ_polity_informal_WVS$log_GDP_gm<-log(tax_econ_polity_informal_WVS$GDP)-mean(log(tax_econ_polity_informal_WVS$GDP), na.rm = TRUE)
                          tax_econ_polity_informal_WVS$log_GDP_cap_gm<-log(tax_econ_polity_informal_WVS$GDP_cap)-mean(log(tax_econ_polity_informal_WVS$GDP_cap), na.rm = TRUE)
                          
                          doc_who_lm<-lm(six_yr_inc_change~poly(prop_high, 2)*polity_change_dum, data = Doctor_Who_RDD_i)
                          doc_who_lm<-lm(six_yr_inc_change~prop_high*polity_change_dum, data = Doctor_Who_RDD_i)
                          
                          doc_who_lm<-lm(six_yr_inc_change~prop_high*polity_change_dum+log(GDP_cap)+UE+Growth, data = Doctor_Who_RDD_i)
                          doc_who_II_lm<-lm(six_yr_inc_change~Work_Index*polity_change, data = Doctor_Who_RDD_ii)
                          
                          summary(doc_who_lm)
                          summary(doc_who_II_lm)
                          
                          doc_who_lme<-lme(three_yr_inc_change ~ 1 + Year + prop_high + polity_dummy+prop_high*polity_change, random = ~ 1 + Year |Country,
                                           data=Doctor_Who_RDD,
                                           na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          summary(doc_who_lme)
                          
                          doc_who_lme<-lme(three_yr_inc_change ~ 1 + Year + log(GDP_cap) + UE + prop_high*polity_change*polity_dummy, random = ~ 1 + Year + log(GDP_cap) + UE |Country,
                                           data=Doctor_Who_RDD,
                                           na.action=na.omit, method="ML", control=list(opt="optim"), correlation=corAR1())
                          summary(doc_who_lme)
                          
                          