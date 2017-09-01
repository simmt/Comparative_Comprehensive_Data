##P3#####comp_code_lm_Jul21_2017

ggplot(Income_Pro ~ prop_high)
Plot_Doc_Who<-na.omit(Doctor_Who[,c("ISO.x", "Year", "prop_high", "Income_Pro", "polity_dummy")]) 
ggplot(na.omit(Plot_Doc_Who[Plot_Doc_Who$Year=="2002",]), 
       aes(x=prop_high, y=Income_Pro, color=as.factor(polity_dummy))) +
  geom_point(shape=1) + scale_colour_hue(l=50) + theme(legend.position="right") + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=T)    # Don't add shaded confidence region

ggplot(Plot_Doc_Who[Plot_Doc_Who$Year=="2004",], aes(prop_high, Income_Pro, linetype = as.factor(polity_dummy))) + 
  geom_point() + 
  geom_text(aes(label=ISO.x)) + 
  geom_smooth(method = lm, fullrange = TRUE, alpha = .1) + 
  theme_bw() + 
  labs(x = "Proportion Valuing Work", y = "Income tax % Government Revenue") + 
  scale_linetype_discrete(name="Regime Type", breaks=c("0", "1"), labels = c("Autocracy", "Democracy"))
?geom_text   
summary(lm(Income_Pro ~ log(GDP_cap), data = Doctor_Who[Doctor_Who$Year=="2004",]))

summary(lm(Income_Pro ~ prop_high*polity_dummy, data = Doctor_Who[Doctor_Who$Year=="2004",]))
summary(lm(Income_Pro ~ log(GDP_cap)+prop_high*polity_dummy, data = Doctor_Who[Doctor_Who$Year=="2004",]))
summary(lm(Income_Pro ~ log(GDP_cap)+prop_high*polity_dummy+Var*polity_dummy, data = Doctor_Who[Doctor_Who$Year=="2004",]))

####table 1...need figure out which cases dropped due to polity
we_inctax_1<-lm(Income_Pro ~ prop_high, data = Doctor_Who[Doctor_Who$Year=="2004",])
we_inctax_2<-lm(Income_Pro ~ prop_high+log(GDP_cap), data = Doctor_Who[Doctor_Who$Year=="2004",])
we_inctax_3<-lm(Income_Pro ~ prop_high+log(GDP_cap)+polity_dummy, data = Doctor_Who[Doctor_Who$Year=="2004",])
we_inctax_4<-lm(Income_Pro ~ prop_high*polity_dummy+log(GDP_cap), data = Doctor_Who[Doctor_Who$Year=="2004",])

stargazer(we_inctax_1, we_inctax_2, we_inctax_3, we_inctax_4)

#table 2 - the batshit regressions
we_inctax_5<-lm(Income_Pro ~ prop_high*polity_dummy+log(GDP_cap)+Trade1+FDI+Resource_Revenues, data = Doctor_Who[Doctor_Who$Year=="2004",])
we_inctax_6<-lm(Income_Pro ~ prop_high*polity_dummy+log(GDP_cap)+Trade1+FDI+Resource_Revenues
                +dummy_pres, data = Doctor_Who[Doctor_Who$Year=="2004",])
we_inctax_7<-lm(Income_Pro ~ prop_high*polity_dummy+log(GDP_cap)+Trade1+FDI+Resource_Revenues
                +pr, data = Doctor_Who[Doctor_Who$Year=="2004",])
stargazer(we_inctax_5, we_inctax_6, we_inctax_7)
summary(we_inctax_7)



####dictatorship tax flip

dem_rdd<-subset(Doctor_Who, polity_change < -4)
DW_dem_high<-na.omit(dem_rdd[, c("Country", "Year", "Diff_2_bef_aft", "prop_high", "threes")])
dem_rdd<-na.omit(dem_rdd[,c("ISO.x", "Year","prop_high", "Diff_2_bef_aft", "Var", "threes", "Work_Index")])
plot(Diff_2_bef_aft ~prop_high, data = dem_rdd)
ggplot(dem_rdd, aes(prop_high, Diff_2_bef_aft)) + 
  geom_point() + 
  geom_text(aes(label=ISO.x)) + 
  geom_smooth(method = lm, fullrange = TRUE, alpha = .1) + 
  theme_bw() + 
  labs(x = "Proportion Valuing Work", y = "2 Year Change in Income tax % Government Revenue")
summary(lm(Diff_2_bef_aft ~ prop_high, data = dem_rdd))
dic_flip2<-lm(Diff_2_bef_aft ~ prop_high, data = dem_rdd)

####five year diff
dem_rdd<-subset(Doctor_Who, polity_change < -4)
dem_rdd<-na.omit(dem_rdd[,c("prop_high", "Diff_5_bef_aft", "Var", "threes", "Work_Index")])
plot(Diff_5_bef_aft ~prop_high, data = dem_rdd)
summary(lm(Diff_5_bef_aft ~ prop_high, data = dem_rdd))
dic_flip5<-lm(Diff_5_bef_aft ~ prop_high, data = dem_rdd)
ggplot(dem_rdd, aes(prop_high, Diff_5_bef_aft)) + 
  geom_point() + 
  geom_text(aes(label=ISO.x)) + 
  geom_smooth(method = lm, fullrange = TRUE, alpha = .1) + 
  theme_bw() + 
  labs(x = "Proportion Valuing Work", y = "5 Year Change in Income tax % Government Revenue")

stargazer(dic_flip2, dic_flip5)

####now democracy

dem_rdd<-subset(Doctor_Who, polity_change > 4)
DW_dem_high<-na.omit(dem_rdd[, c("Country", "Year", "Income_Pro", "Diff_2_bef_aft", "prop_high", "threes", "polity2")])
dem_rdd<-na.omit(dem_rdd[,c("Country", "Year","prop_high",  "Diff_2_bef_aft", "Var",  "threes", "prop_high_dummy")])
dem_rdd
prop_rdd<-RDestimate(Diff_2_bef_aft~prop_high, data=dem_rdd, cutpoint = .49)
summary(prop_rdd)
plot(prop_rdd)
Doctor_Who$polity_change_dum
dem_rdd<-subset(Doctor_Who, polity_change > 4)
DW_dem_high<-na.omit(dem_rdd[, c("Country", "Year", "Income_Pro", "Diff_2_bef_aft", "prop_high", "threes", "polity2")])
dem_rdd<-na.omit(dem_rdd[,c("Country", "Year","prop_high", 
                            "Diff_5_bef_aft", "Var",  "threes", "prop_high_dummy")])
dem_rdd
prop_rdd<-RDestimate(Diff_5_bef_aft~prop_high, data=dem_rdd, cutpoint = .49)
summary(prop_rdd)
plot(prop_rdd, ylab = "5 Year Change in Income tax % Government Revenue")
summary(lm(Diff_5_bef_aft~prop_high, data = dem_rdd))
plot(Diff_5_bef_aft~prop_high, data = dem_rdd)

####for total flip from dict to dem
dem_rdd<-subset(Doctor_Who, is.na(Doctor_Who$polity_change_dum) == TRUE)
dem_rdd
dem_rdd<-subset(Doctor_Who, polity_change_dum == "1")
DW_dem_high<-na.omit(dem_rdd[, c("Country", "Year", "Income_Pro", "Diff_2_bef_aft", "prop_high", "threes", "polity2")])
dem_rdd<-na.omit(dem_rdd[,c("Country", "Year","prop_high", 
                            "Diff_2_bef_aft", "Var",  "threes", "prop_high_dummy")])

prop_rdd<-RDestimate(Diff_2_bef_aft~prop_high, data=dem_rdd, cutpoint = .48)
summary(prop_rdd)
plot(prop_rdd, ylab = "5 Year Change in Income tax % Government Revenue")
summary(lm(Diff_2_bef_aft~prop_high, data = dem_rdd))
plot(Diff_2_bef_aft~prop_high, data = dem_rdd)


rdd_trial<-rdrobust(dem_rdd$Diff_2_bef_aft, dem_rdd$prop_high, c = 0.49)
summary(rdd_trial)
rdplot(dem_rdd$Diff_2_bef_aft, dem_rdd$prop_high, c = 0.49)

###need way to eextract polity dummy = 1 at t, but !1 at t-1...can be NA or anything

###vars in play three_yr_inc_change ; six_yr_inc_change;
#AVEfive_yr_inc_change ; lead_2_inc_ratio ; lead_5_inc_ratio;
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
               Doctor_Who$Country=="Romania"|Doctor_Who$Country=="Poland"|
               Doctor_Who$Country=="Russian Federation"|
               Doctor_Who$Country=="Serbia"| Doctor_Who$Country=="Slovak Republic"|
               Doctor_Who$Country=="Slovenia"| Doctor_Who$Country=="Ukraine"|
               Doctor_Who$Country=="Macedonia, FYR",]



USSR_RDD<-
  Doctor_Who[
               Doctor_Who$Country=="Bulgaria"|Doctor_Who$Country=="Hungary"|
               Doctor_Who$Country=="Czech Republic"| Doctor_Who$Country=="Georgia"| 
               Doctor_Who$Country=="Lithuania"|Doctor_Who$Country=="Czech Republic"|
               Doctor_Who$Country=="Moldova"| Doctor_Who$Country=="Slovak Republic"|
               Doctor_Who$Country=="Slovenia"| Doctor_Who$Country=="Ukraine"|
               Doctor_Who$Country=="Macedonia, FYR",]

USSR_RDD$group_med<-NA
USSR_RDD$group_med[USSR_RDD$prop_high<0.45] <- "0"
USSR_RDD$group_med[USSR_RDD$prop_high>=0.45] <- "1"



GG_USSR_Inc<-na.omit(USSR_RDD[,c("Country", "ISO.x", "Year", "Dis_Count", "Income_Pro", 
                                 "Work_Index", "high_prop_low", "prop_high", "prop_low",
                                 "group_high", "group_med")])
GG_USSR_Inc<-na.omit(USSR_RDD[,c("Country","Year", "Income_Pro", "polity_dummy", "GDP_cap",
                                 "Work_Index", "high_prop_low", "prop_high", "prop_low",
                                 "group_high")])
GG_USSR_Inc<-na.omit(USSR_RDD[,c("Country","Year", "Income_Pro", "prop_high", "polity_dummy")])
GG_USSR_Inc1[GG_USSR_Inc1$Country=="Croatia",]
###demonstrate tendency across years
ggplot(GG_USSR_Inc[GG_USSR_Inc$prop_high<=.42,] %>% mutate(group = paste(Country,format(prop_high, digits = 2), sep="-")), aes(Year, Income_Pro))+geom_point(aes(colour=as.factor(polity_dummy)))+
  facet_wrap(~group) + geom_smooth(method='lm',formula=y~x, show.legend = TRUE)
ggplot(GG_USSR_Inc[GG_USSR_Inc$prop_high>.42,] %>% mutate(group = paste(Country,format(prop_high, digits = 2), sep="-")), aes(Year, Income_Pro))+geom_point(aes(colour=as.factor(polity_dummy)))+
  facet_wrap(~group) + geom_smooth(method='lm',formula=y~x, show.legend = TRUE)

dem_rdd<-na.omit(USSR_RDD[,c("Country", "Year","prop_high", 
                            "Income_Pro",
                          #  "Var",  "threes", 
                            "prop_high_dummy",
                            "polity_dummy"
                  #        , "GDP_cap"
                          )])

summary(felm(Income_Pro~as.factor(prop_high>.49)*Year|Country, data = dem_rdd[dem_rdd$prop_high<.8 & dem_rdd$prop_high>.2,]))
felm_USSR_dem<-felm(Income_Pro~as.factor(prop_high>.49)*Year|Country, data = dem_rdd[dem_rdd$prop_high<.8 & dem_rdd$prop_high>.2,])
felm_USSR_dem1<-felm(Income_Pro~prop_high*Year|Country, data = dem_rdd[dem_rdd$prop_high<.8 & dem_rdd$prop_high>.2,])
stargazer(felm_USSR_dem, felm_USSR_dem1)

felm_USSR_dem<-felm(Income_Pro~as.factor(prop_high>.49)*Year|Country, data = dem_rdd)
felm_USSR_dem1<-felm(Income_Pro~prop_high*Year|Country, data = dem_rdd)
stargazer(felm_USSR_dem, felm_USSR_dem1)

summary(felm(Income_Pro~prop_high*Year
            # +log(GDP_cap)
             |Country, data = dem_rdd))
summary(felm(Income_Pro~as.factor(prop_high>.49)*Year
           #   +log(GDP_cap)
             |Country, data = dem_rdd))


dem_rdd<-na.omit(USSR_RDD[,c("Country", "Year","prop_high", 
                             "Diff_5_bef_aft", "Var",  "threes", "prop_high_dummy",
                             "polity_dummy")])
dem_rdd[dem_rdd$prop_high_dummy=="0",]
prop_rdd<-RDestimate(Diff_2_bef_aft~prop_high, data=dem_rdd, cutpoint = .48)
summary(prop_rdd)
plot(prop_rdd, ylab = "5 Year Change in Income tax % Government Revenue")
summary(lm(Diff_2_bef_aft~prop_high, data = dem_rdd))
plot(Diff_2_bef_aft~prop_high, data = dem_rdd)


ggplot(GG_USSR_Inc[GG_USSR_Inc$Year=="1993" | 
                     GG_USSR_Inc$Year=="1997" | 
                     GG_USSR_Inc$Year== "2001",], 
       aes(x=prop_high, y=Income_Pro, color=as.factor(Year))) +
  geom_point(shape=1) + scale_colour_hue(l=50) + theme(legend.position="right") + # Use a slightly darker palette than normal
  geom_smooth(span = 1.5,   # Add linear regression lines
              se=F)    # Don't add shaded confidence region
ggplot(GG_USSR_Inc[GG_USSR_Inc$Year=="1993"|GG_USSR_Inc$Year=="1998"|GG_USSR_Inc$Year=="2003",], 
       aes(x=prop_high, y=Income_Pro, color=as.factor(Year))) +
  geom_point(shape=1) + scale_colour_hue(l=50) + theme(legend.position="right") + # Use a slightly darker palette than normal
  geom_smooth(span = 1.5,   # Add linear regression lines
              se=F)+    # Don't add shaded confidence region
labs(x = "Proportion Valuing Work", y = "Income tax % Government Revenue", fill = "Year")  +
  guides(fill=guide_legend(title="New Legend Title"))


ussr_relation<-lm(Income_Pro~prop_high, data = GG_USSR_Inc[GG_USSR_Inc$Year=="1994",])
ussr_relation<-lm(Income_Pro~log(GDP_cap)+prop_high, data = GG_USSR_Inc[GG_USSR_Inc$Year=="1994",])
ussr_relation<-lm(Income_Pro~prop_high, data = GG_USSR_Inc[GG_USSR_Inc$Year=="1998",])
ussr_relation<-lm(Income_Pro~log(GDP_cap)+prop_high, data = GG_USSR_Inc[GG_USSR_Inc$Year=="1998",])
summary(ussr_relation)

summary(lm(After_Inc~prop_high+Before_Inc, data = USSR_WVS))
summary(lm(After_Inc~prop_high+Before_Inc, data = USSR_WVS1))

summary(felm(Income_Pro ~ prop_high*Year|Country, data = GG_USSR_Inc[GG_USSR_Inc$polity_dummy=="1",]))
summary(felm(Income_Pro ~ as.factor(prop_high>.49)*Year|Country, data = GG_USSR_Inc[GG_USSR_Inc$polity_dummy=="1",]))

DW_dem_high<-na.omit(USSR_RDD[, c("Country", "Year", "Income_Pro", "prop_high")])
DW_dem_high<-na.omit(USSR_RDD[, c("Country", "Year", "Income_Pro", "AVEfive_yr_inc_change", "prop_high", "threes", "polity2")])
dem_rdd<-na.omit(USSR_RDD[,c("Country", "Year", "prop_high",  "Income_Pro", "Var",  "threes", "prop_high_dummy", "polity_dummy")])
dem_rdd_1993<-dem_rdd[dem_rdd$Year=="1996",]
dem_rdd_1993
dem_rdd
nrow(dem_rdd)
prop_rdd<-RDestimate(Income_Pro~prop_high, data=dem_rdd_1993, cutpoint = .5)
summary(prop_rdd)
plot(prop_rdd)

plot(Income_Pro ~ prop_high, data = dem_rdd_1993)
summary(lm(Income_Pro ~ prop_high, data = dem_rdd_1993))
summary(lm(Diff_2_bef_aft ~ prop_high*as.factor(prop_high>.49), data = dem_rdd))
summary(felm(Income_Pro ~ as.factor(prop_high>.5)*Year|Country, data = dem_rdd[dem_rdd$polity_dummy=="1",]))


USSR_RDD$group_high<-NA
USSR_RDD$group_high[USSR_RDD$prop_high<0.38] <- "1"
USSR_RDD$group_high[USSR_RDD$prop_high>=0.38 & USSR_RDD$prop_high<0.45] <- "2"
USSR_RDD$group_high[USSR_RDD$prop_high>=0.45 & USSR_RDD$prop_high<0.51] <- "3"
USSR_RDD$group_high[USSR_RDD$prop_high>=0.51 & USSR_RDD$prop_high<1] <- "4"

USSR_RDD$group_med<-NA
USSR_RDD$group_med[USSR_RDD$prop_high<0.45] <- "0"
USSR_RDD$group_med[USSR_RDD$prop_high>=0.45] <- "1"

USSR_RDD$Dis_Count<-USSR_RDD$Year-1994
USSR_RDD$ISO<-USSR_RDD$ISO.x
GG_USSR_Inc<-na.omit(USSR_RDD[,c("Country","Year", "Dis_Count", "Income_Pro", "polity2",
                                 "Work_Index", "high_prop_low", "prop_high", "prop_low",
                                 "group_high", "group_med")])
GG_USSR_Inc<-na.omit(USSR_RDD[,c("Country","Year", "Income_Pro", 
                                 "Work_Index", "high_prop_low", "prop_high"
                                 )])

GG_USSR_Inc<-GG_USSR_Inc[GG_USSR_Inc$,]
USSR_elapse_tool<-GG_USSR_Inc[GG_USSR_Inc$Year>1988 & GG_USSR_Inc$Year<1996,]
USSR_elapse_tool_after<-GG_USSR_Inc[GG_USSR_Inc$Year>1995 & GG_USSR_Inc$Year<2000,]
library(plyr)
USSR_agg<-aggregate(USSR_elapse_tool$Income_Pro, 
                    by = list(USSR_elapse_tool$Country),
                    FUN=mean, na.rm=TRUE)
USSR_agg <- rename(USSR_agg, c(Group.1 = "Country", x="Before_Inc"))
USSR_agg_after<-aggregate(USSR_elapse_tool_after$Income_Pro,
                          by = list(USSR_elapse_tool_after$Country),
                          FUN=mean, na.rm=TRUE)
USSR_agg_after <- rename(USSR_agg_after, c(Group.1 = "Country", x="After_Inc"))

USSR_merge<-merge(USSR_agg, USSR_agg_after, by=c("Country"), all = TRUE)

USSR_WVS<-merge(USSR_merge, WVS_Work_Leisure_Scores, by=c("Country"), all.y = FALSE)
USSR_WVS$Diff<-((USSR_WVS$After_Inc-USSR_WVS$Before_Inc)/USSR_WVS$Before_Inc)*100
USSR_WVS
summary(lm(After_Inc~Before_Inc+prop_high, data = USSR_WVS))
par(mfrow=c(1,1))
plot(Before_Inc~prop_high, data = USSR_WVS)
text(abs_losses, percent_losses, labels=namebank, cex= 0.7)

plot(After_Inc~prop_high, data = USSR_WVS)
text(abs_losses, percent_losses, labels=namebank, cex= 0.7)

plot(Diff~prop_high, xlab = "Proportion Valuing Work", ylab = "Change in Income Tax Usage", data = USSR_WVS)
text(USSR_WVS$prop_high, USSR_WVS$Diff, labels=USSR_WVS$Country, cex= 0.7)

USSR_WVS$split<-as.factor(as.numeric(USSR_WVS$prop_high>.49))
USSR_WVS$prop_diff_med<-NA
USSR_WVS$prop_diff_med[USSR_WVS$prop_high<.5]<-USSR_WVS$prop_high[USSR_WVS$prop_high<.5]
USSR_WVS$prop_diff_med[USSR_WVS$prop_high>.5]<-USSR_WVS$prop_high[USSR_WVS$prop_high>.5]-.5

summary(lm(Diff~prop_high*as.numeric(prop_high>.48), data = USSR_WVS))
summary(lm(Diff~prop_high, data = USSR_WVS))

ussr_test<-lm(Diff~prop_high, data = USSR_WVS)
ussr_test1<-lm(Diff~as.numeric(prop_high>.48), data = USSR_WVS)
library(stargazer)
stargazer(ussr_test, ussr_test1)

par(mfrow=c(2,2))

summary(lm(Diff~as.factor(prop_high>.49), data = USSR_WVS))
summary(lm(Diff~prop_high, data = USSR_WVS))

plot(Diff~prop_high, data = USSR_WVS)
USSR_WVSa<-USSR_WVS[USSR_WVS$prop_high>.35 & USSR_WVS$prop_high<.65,]
prop_rdd<-RDestimate(Diff~prop_high, data=USSR_WVS, cutpoint = .48)
summary(prop_rdd)
plot(prop_rdd, ylab = "5 Year Change in Income tax % Government Revenue")

USSR_WVS[USSR_WVS$Country=="Georgia",]


USSR_RDD$WE_Grouping<-NA
USSR_RDD$WE_Grouping[USSR_RDD$prop_high<0.35] <- "1"
USSR_RDD$WE_Grouping[USSR_RDD$prop_high>=0.35 & USSR_RDD$prop_high<0.5] <- "2"
USSR_RDD$WE_Grouping[USSR_RDD$prop_high>=.5 & USSR_RDD$prop_high<1] <- "3"

GG_USSR_Inc_cater <-aggregate(USSR_RDD$Income_Pro, by=list(USSR_RDD$WE_Grouping, USSR_RDD$Year), FUN=mean, na.rm=TRUE)
GG_USSR_Inc_cater<-subset(GG_USSR_Inc_cater, Group.2>1990 & Group.2<2004)
GG_USSR_Inc_cater_plot<-qplot(Group.2, x, data=GG_USSR_Inc_cater, geom = c("point", "smooth"), colour=Group.1)+geom_line()+ggtitle("Income Taxes Percent Budget")+labs(x="Year",y="Income Taxes")+scale_color_discrete(name = "Work Ethic Grouping")
GG_USSR_Inc_cater_plot

summary(felm(Income_Pro~log(GDP_cap)|ISO.x+Year, data = Doctor_Who))
