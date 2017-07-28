####comp code P4, incl. RDD trials###
#####dictatorships for <-4, good at 2, 5
###vars in play three_yr_inc_change ; six_yr_inc_change;
#AVEfive_yr_inc_change ; lead_2_inc_ratio ; lead_5_inc_ratio;
dem_rdd<-subset(Doctor_Who, polity_change < -4)
DW_dem_high<-na.omit(dem_rdd[, c("Country", "Year", "Diff_2_bef_aft", "prop_high", "threes")])
dem_rdd<-na.omit(dem_rdd[,c("prop_high", "Diff_2_bef_aft", "Var", "threes", "Work_Index")])
plot(Diff_2_bef_aft ~prop_high, data = dem_rdd)
summary(lm(Diff_2_bef_aft ~ prop_high, data = dem_rdd))
summary(felm(Diff_2_bef_aft~Year*prop_high|Country, data = DW_dem_high))



DW_dem_high
?RDestimate
####
####
?RDestimate
dem_rdd<-subset(Doctor_Who, polity_change > 4)
DW_dem_high<-na.omit(dem_rdd[, c("Country", "Year", "Income_Pro", "Diff_2_bef_aft", "prop_high", "threes", "polity2")])
dem_rdd<-na.omit(dem_rdd[,c("prop_high",  "Diff_2_bef_aft", "Var",  "threes", "prop_high_dummy")])
dem_rdd
DW_dem_high
prop_rdd<-RDestimate(Diff_2_bef_aft~prop_high, data=dem_rdd, cutpoint = .49)
summary(prop_rdd)
plot(prop_rdd)

plot(Diff_2_bef_aft ~ prop_high, data = dem_rdd)
summary(lm(Diff_2_bef_aft ~ prop_high+log(GDP_cap), data = dem_rdd))
summary(lm(Diff_2_bef_aft ~ prop_high*as.factor(prop_high>.49), data = dem_rdd))

#rprop_rdd <- RDdata(y = dem_rdd$six_yr_inc_change, x = dem_rdd$prop_high, cutpoint = 0.48)
rdd_trial<-rdrobust(dem_rdd$six_yr_inc_change, dem_rdd$prop_high, c = 0.49)
summary(rdd_trial)
rdplot(dem_rdd$six_yr_inc_change, dem_rdd$prop_high, c = 0.49)

texreg(rdd_trial)

?rdplot
??RDdata
rdrobust(DW_dems$per_diff_after_6yr, DW_dems$prop_high, c = 0.48)



dict_rdd<-subset(Doctor_Who, polity_change<0)

prop_rdd<-RDestimate(per_diff_after_6yr~prop_high, data=dem_rdd, cutpoint = .49)


######
######

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

####ggplotting