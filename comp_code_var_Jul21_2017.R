###############P2#####################
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


Doctor_Who$FDI<-as.numeric(gsub(",", "", Doctor_Who$Foreign.direct.investment..net.inflows....of.GDP...BX.KLT.DINV.WD.GD.ZS.))
Doctor_Who$Resource_Revenues<-as.numeric(sub("%", "", Doctor_Who$Total.natural.resources.rents....of.GDP...NY.GDP.TOTL.RT.ZS.))

##dummy for pres
Doctor_Who$dummy_pres<-as.numeric(Doctor_Who$system=="Presidential")
##dummy for parl
Doctor_Who$dummy_parl<-as.numeric(Doctor_Who$system=="Parliamentary")
##dummy for pr
Doctor_Who$pr[Doctor_Who$pr=="-999"]<-"NA"
Doctor_Who$pr<-as.numeric(as.character(Doctor_Who$pr))
##dummy for plurality (1=yes)
Doctor_Who$pluralty[Doctor_Who$pluralty=="-999"]<-"NA"
Doctor_Who$pluralty<-as.numeric(as.character(Doctor_Who$pluralty))
#trade % gdp
Doctor_Who$Trade1<-as.numeric(sub("%", "", Doctor_Who$Trade))

Doctor_Who$Social_Security<-as.numeric(sub("%", "", Doctor_Who$Social.contributions))



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
Doctor_Who$AVE_inc_n2years<-rowMeans(Doctor_Who[,c("lead_n1_inc_ratio",
                                                   "lead_n2_inc_ratio"
                                                   )],
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
Doctor_Who$AVE_inc_2years<-rowMeans(Doctor_Who[,c("lead_1_inc_ratio",
                                                  "lead_2_inc_ratio")],
                                    na.rm = TRUE)

Doctor_Who$Diff_5_bef_aft<-Doctor_Who$AVE_inc_5years-Doctor_Who$AVE_inc_n5years
Doctor_Who$Diff_2_bef_aft<-Doctor_Who$AVE_inc_2years-Doctor_Who$AVE_inc_n2years

head(na.omit(Doctor_Who[,c("Country", "Year", "Income_Pro", "lead_2_inc_ratio")]))
