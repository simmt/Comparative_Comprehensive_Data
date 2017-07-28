######DATA PREPERATION######
library(foreign)
library(reshape2)
######Tax data
international_tax_data<-read.csv("~/Desktop/Network Data Construction/ICTDGRD_June2016_CentralGeneralMerged/Merged-Table 1.csv")
names(international_tax_data)[names(international_tax_data)=="Calendar.year..nearest."] <- "Year"
#Likeness & Covariates#
######GDP per cap, Population
international_econ_data<-read.csv("~/Desktop/Network Data Construction/Data_Extract_From_World_Development_Indicators (5) 2/country_stats.csv")
international_econ_data$GDP_cap<-as.numeric(international_econ_data$GDP.per.capita..constant.2010.US....NY.GDP.PCAP.KD.)
international_econ_data$Pop<-as.numeric(international_econ_data$Population..total..SP.POP.TOTL.)
international_econ_data$GINI<-as.numeric(international_econ_data$GINI.index..World.Bank.estimate...SI.POV.GINI.)
international_econ_data$ISO<-international_econ_data$Country.Code
international_econ_data$Year<-international_econ_data$Time

######Polity
##add polity score
polity<-read.csv("~/Desktop/Network Data Construction/Polity/p4v2014.csv")
library(plyr)
polity<-rename(polity, c(year="Year", country="Country"))
polity$ISO<-as.factor(countrycode(polity$scode, "p4_scode",  "iso3c"))

#polity$Country<-revalue(polity$Country, c("Bosnia"="Bosnia and Herzegovina", "Kyrgyzstan"="Kyrgyz Republic", "Korea South"="Korea, Rep.",
#                                          "Iran"="Iran, Islamic Rep.", "Russia"="Russian Federation", "Macedonia"="Macedonia, FYR", "Egypt"="Egypt, Arab Rep.",
#                                          "Venezuela"="Venezuela, RB"))
#end likeness
#####WTO Member, GATT
###also, now get in some world trade org data
WTO_mem<-read.csv("~/Desktop/Network Data Construction/wto_mem.csv",stringsAsFactors=F)
WTO_mem1<-read.csv("~/Desktop/Network Data Construction/added_WTO.csv",stringsAsFactors=F)
WTO_mem<-rbind(WTO_mem, WTO_mem1)
library(stringr)
WTO_mem$Year<-as.numeric(str_sub(WTO_mem$WTO.Accession.Date, -4, -1))
WTO_mem$Year[is.na(WTO_mem$Year)]<-9999
names(WTO_mem)[names(WTO_mem)=="Country.Name"] <- "importer1a"
library(countrycode)
WTO_mem$importer1<-as.factor(countrycode(WTO_mem$importer1a, "country.name",  "iso3c"))

#now establish data in form needed
######DVs..... Tax data
international_tax_data$Social_Security<-as.numeric(sub("%", "", international_tax_data$Social.contributions))
#international_tax_data$Corp<-as.numeric(sub("%", "", international_tax_data$Corp))
international_tax_data$Corporate<-as.numeric(sub("%", "", international_tax_data$Corporations.and.other.enterprises))
international_tax_data$Inco<-as.numeric(sub("%", "", international_tax_data$Income))
international_tax_data$Sales<-as.numeric(sub("%", "", international_tax_data$Taxes.on.goods.and.services..of.which.Taxes.on.Sales))
international_tax_data$Reves<-as.numeric(sub("%", "", international_tax_data$TotRev))



###now, industrial composition
indus_comp<-read.csv("~/Desktop/Network Data Construction/Competitive_Manu/high_tech_comp.csv", header = TRUE)
indus_comp<-indus_comp[indus_comp$Indicator=="Competitive Industrial Performance Score",]
indus_comp$ISO<-indus_comp$Country.ISO3
indus_comp$X1989<-indus_comp$X1990

religion<-read.dta("/Users/Kevin/Downloads/world_religion.dta")


religion_1955<-religion[religion$year==2010,]
religion_1955<-religion_1955[,c("ISO3", "chprtpct")]
names(religion_1955)[names(religion_1955)=="ISO3"] <- "ISO"

total_taxes<-merge(total_taxes, religion_1955, by = c("ISO"))


total_taxes$loggdp<-log(total_taxes$GDP_cap)
total_taxes_comp <- total_taxes[
                                #total_taxes$ISO=="GBR"|total_taxes$ISO=="IRL"|
                                total_taxes$ISO=="ISL"|total_taxes$ISO=="DEU"|
                                  total_taxes$ISO=="FRA"|total_taxes$ISO=="PRT"|
                                  total_taxes$ISO=="ESP" ,]
total_taxes_comp <- total_taxes[
  total_taxes$ISO=="SWE"|total_taxes$ISO=="FIN"|total_taxes$ISO=="MEX"|
 # total_taxes$ISO=="EST"|total_taxes$ISO=="LVA"|
    total_taxes$ISO=="CAN"|total_taxes$ISO=="USA" ,]

total_taxes_comp

total_taxes$region<-countrycode(total_taxes$ISO, "iso3c", "continent")
total_taxes_euro<-total_taxes[total_taxes$region=="Europe",]
total_taxes_euro<-na.omit(total_taxes_euro)
#switch
total_taxes_euro_2000
total_taxes_euro_2000<-total_taxes[total_taxes$Year==2010,]
#total_taxes_euro_2000<-total_taxes_euro[total_taxes_euro$Year==2010,]
total_taxes_euro_2000$relig<-0
total_taxes_euro_2000
#UK, Iceland, Netherland, Germany, Swi, Den, 
#Estonia, LAtvia, Fin, Swe, Nor
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="GBR"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="ISL"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="NLD"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="DEU"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="CHE"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="DNK"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="EST"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="LVA"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="FIN"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="SWE"]<-1
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="NOR"]<-1

total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="IRl"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="FRA"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="ESP"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="PRT"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="LUX"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="BEL"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="ITA"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="SVN"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="HRV"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="AUT"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="HUN"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="CZE"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="SVK"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="POL"]<-2
total_taxes_euro_2000$relig[total_taxes_euro_2000$ISO=="LTU"]<-2


total_taxes_euro_2000
total_taxes_euro_2000$ratio_work_all<-((
  (total_taxes_euro_2000$Inco)
          +
         total_taxes_euro_2000$Social_Security)
          /(total_taxes_euro_2000$Sales
              +total_taxes_euro_2000$Inco
            +total_taxes_euro_2000$Social_Security))

total_taxes_euro_2000
total_taxes_euro_2000$ratio_inc_sal<-total_taxes_euro_2000$Inco-total_taxes_euro_2000$Sales
total_taxes_euro_2000$ratio_incss_sal<-(total_taxes_euro_2000$Inco+total_taxes_euro_2000$Social_Security)-total_taxes_euro_2000$Sales
boxplot(total_taxes_euro_2000$Social_Security~total_taxes_euro_2000$relig)
boxplot(total_taxes_euro_2000$Inco~total_taxes_euro_2000$relig)
boxplot(total_taxes_euro_2000$Sales~total_taxes_euro_2000$relig)
boxplot(total_taxes_euro_2000$ratio_work_all~total_taxes_euro_2000$relig)
summary(lm(Social_Security~as.factor(relig)+loggdp, data = total_taxes_euro_2000))
summary(lm(Inco~as.factor(relig)+loggdp, data = total_taxes_euro_2000))
summary(lm(Sales~as.factor(relig)+loggdp, data = total_taxes_euro_2000))
summary(lm(ratio_work_all~as.factor(relig)+loggdp, data = total_taxes_euro_2000))

summary(lm(ratio_work_all~as.factor(relig)+loggdp, data = total_taxes_euro_2000))
plot(ratio_work_all~chprtpct, data = total_taxes_euro_2000)
plot(Inco~chprtpct, data = total_taxes_euro_2000)
plot(Sales~chprtpct, data = total_taxes_euro_2000)
plot(Social_Security~chprtpct, data = total_taxes_euro_2000)

summary(lm(ratio_work_all~chprtpct+loggdp, data = total_taxes_euro_2000))
summary(lm(Inco~chprtpct+loggdp, data = total_taxes_euro_2000))
summary(lm(Sales~chprtpct+loggdp, data = total_taxes_euro_2000))





international_tax_data$Revenues<-international_tax_data$Reves

total_taxes<-international_tax_data[,c("ISO", 
                                       "Year",
                                       "Social_Security",
                                       "Corporate", 
                                       "Inco",
                                       "Sales",
                                       "Revenues")]
total_taxes<-merge(total_taxes, gdp_data_tnam, by = c("ISO", "Year"))
religion<-international_economic_data<-read.dta("/Users/Kevin/Downloads/world_religion.dta")
religion<-religion[,c("ISO3", "year", "chprtpct")]
names(religion)[names(religion)=="ISO3"] <- "ISO"
names(religion)[names(religion)=="year"] <- "Year"
total_taxes<-merge(total_taxes, religion, by = c("ISO", "Year"))
total_taxes$loggdp<-log(total_taxes$GDP_cap)
total_taxes$ratio_work_all<-((
  (total_taxes$Inco)
  +
    total_taxes$Social_Security)
  /(total_taxes$Sales
    +total_taxes$Inco
    +total_taxes$Social_Security))

library(DataCombine)

lag_tax_ratio <- slide(total_taxes, Var = "ratio_work_all", 
                       TimeVar = "Year", GroupVar = "ISO",
                       NewVar = "lagged_ratio",
                       slideBy = -3)
lag_tax_ratio <- slide(total_taxes, Var = "ratio_work_all", 
                       TimeVar = "Year", GroupVar = "ISO",
                       NewVar = "lead_ratio",
                       slideBy = 1)

total_taxes<-lag_tax_ratio
lag_tax_ratio1 <- slide(total_taxes, Var = "Revenues", 
                       TimeVar = "Year", GroupVar = "ISO",
                       NewVar = "lagged_Revenues",
                       slideBy = -3)
total_taxes<-lag_tax_ratio1
plot(ratio_work_all ~ chprtpct, data = total_taxes[total_taxes$ISO=="DEU",])
plot(ratio_work_all ~ chprtpct, data = total_taxes[total_taxes$ISO=="SWE",])
plot(ratio_work_all ~ chprtpct, data = total_taxes[total_taxes$ISO=="CZE",])
plot(ratio_work_all ~ chprtpct, data = total_taxes[total_taxes$ISO=="CAN",])
plot(ratio_work_all ~ chprtpct, data = total_taxes[total_taxes$ISO=="USA",])

library(car)
dev.off()
scatterplot(ratio_work_all~chprtpct|ISO, boxplots=FALSE, 
            smooth=TRUE, reg.line=FALSE, 
            data=total_taxes)
scatterplot(ratio_work_all~chprtpct|ISO, 
            boxplots=FALSE, xlab="x1", 
            ylab="yhat",smooth=FALSE, data=total_taxes)
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")
library(ggplot2)
dev.off()
ggplot(total_taxes, aes(x=chprtpct, y=Inco, color=ISO)) +
 geom_point(shape=1) + scale_colour_hue(l=50) + theme(legend.position="none") + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region


total_taxes$chprtpct
library(lfe)
summary(felm(ratio_work_all~GDP_cap+chprtpct+Year|ISO, data = total_taxes))
summary(felm(ratio_work_all~GDP_cap+chprtpct+lagged_ratio|ISO, data = total_taxes))

total_taxes1<-na.omit(total_taxes[,c("ISO", "Year", "chprtpct", "Inco")])
total_taxes1[total_taxes1$chprtpct>.35 & total_taxes$chprtpct<.65,]
total_taxes1

plot(Inco~chprtpct , data = total_taxes1[total_taxes1$Year=="2000",])
summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+Year|ISO, data = total_taxes[total_taxes$chprtpct>.35 & total_taxes$chprtpct<.65,]))
summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+Year|ISO, data = total_taxes[total_taxes$chprtpct>.3 & total_taxes$chprtpct<.7,]))
summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+Year|ISO, data = total_taxes[total_taxes$chprtpct>.2 & total_taxes$chprtpct<.8,]))
summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+Year|ISO, data = total_taxes[total_taxes$chprtpct>.1 & total_taxes$chprtpct<.9,]))
summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+Year|ISO, data = total_taxes[total_taxes$chprtpct<.3 | total_taxes$chprtpct>.7,]))
summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+Year|ISO, data = total_taxes[total_taxes$chprtpct<.3 ,]))
summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+Year|ISO, data = total_taxes[ total_taxes$chprtpct>.7,]))

total_taxes$dist_med<-NA
total_taxes$dist_med[total_taxes$chprtpct<.5] <- 0.5-total_taxes$chprtpct[total_taxes$chprtpct<.5]
total_taxes$dist_med[total_taxes$chprtpct>.5] <- total_taxes$chprtpct[total_taxes$chprtpct<.5]-0.5
summary(felm(ratio_work_all~GDP_cap+chprtpct*dist_med+Year+lagged_ratio|ISO, data = total_taxes))


prot_vary<-felm(ratio_work_all~GDP_cap+chprtpct+lagged_ratio+Year|ISO, data = total_taxes[total_taxes$chprtpct>.3 & total_taxes$chprtpct<.7,])
prot_vary1<-felm(ratio_work_all~GDP_cap+chprtpct+lagged_ratio+Year|ISO, data = total_taxes[total_taxes$chprtpct>.2 & total_taxes$chprtpct<.8,])
prot_vary2<-felm(ratio_work_all~GDP_cap+chprtpct+lagged_ratio+Year|ISO, data = total_taxes[total_taxes$chprtpct>.1 & total_taxes$chprtpct<.9,])
stargazer(prot_vary, prot_vary1, prot_vary2)

summary(felm(ratio_work_all~GDP_cap+chprtpct*as.factor(chprtpct>.5)+lagged_ratio|ISO, data = total_taxes))


prot_tax1<-felm(ratio_work_all~GDP_cap+chprtpct+Year|ISO, data = total_taxes)
prot_tax2<-felm(ratio_work_all~GDP_cap+chprtpct+lagged_ratio|ISO, data = total_taxes)
stargazer(prot_tax1,prot_tax2 )

total_taxes$levels_prot<-NA
total_taxes$levels_prot[total_taxes$chprtpct<.3]<-"1"
total_taxes$levels_prot[total_taxes$chprtpct>=.3 &total_taxes$chprtpct<.5]<-"2"
total_taxes$levels_prot[total_taxes$chprtpct>=.5 &total_taxes$chprtpct<.7]<-"3"
total_taxes$levels_prot[total_taxes$chprtpct>=.7 &total_taxes$chprtpct<1]<-"4"
prot_tax1<-felm(ratio_work_all~GDP_cap+chprtpct*levels_prot+Year|ISO, data = total_taxes)
summary(prot_tax1)

lead_ratio

##3with lag
summary(felm(ratio_work_all~GDP_cap+chprtpct+Year|ISO, data = lag_tax_ratio))
summary(felm(ratio_work_all~GDP_cap+chprtpct+lagged_ratio|ISO, data = lag_tax_ratio))

summary(felm(lead_ratio~GDP_cap+chprtpct+ratio_work_all|ISO, data = lag_tax_ratio[lag_tax_ratio$chprtpct>.3&lag_tax_ratio$chprtpct<.7,]))



total_taxes_sub
#can toy with this between .25 and .35... lose discontinuity effect vs. cases
total_taxes_sub<-total_taxes[total_taxes$chprtpct>.25 & total_taxes$chprtpct <.75,]
summary(felm(Social_Security~loggdp+chprtpct+Year|ISO, data = total_taxes_sub))
summary(felm(Inco~loggdp+chprtpct+Year|ISO, data = total_taxes_sub))
summary(felm(Sales~loggdp+chprtpct+Year|ISO, data = total_taxes_sub))
##log gdp or not?
summary(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub))
summary(felm(ratio_work_all~loggdp+chprtpct+lagged_ratio|ISO, data = total_taxes_sub))
summary(felm(ratio_work_all~loggdp+chprtpct+lagged_ratio+Year|ISO, data = total_taxes_sub))
summary(felm(ratio_work_all~loggdp+chprtpct+lagged_ratio|ISO+Year, data = total_taxes_sub))


summary(felm(Social_Security~loggdp+chprtpct+Year+Revenues|ISO, data = total_taxes_sub))
summary(felm(Inco~loggdp+chprtpct+Year+Revenues|ISO, data = total_taxes_sub))
summary(felm(Sales~loggdp+chprtpct+Year+Revenues|ISO, data = total_taxes_sub))
summary(felm(ratio_work_all~loggdp+chprtpct+Year+Revenues|ISO, data = total_taxes_sub))
summary(felm(ratio_work_all~loggdp+chprtpct+
               Year+Revenues+lagged_ratio|ISO, data = total_taxes_sub))
summary(felm(ratio_work_all~loggdp+chprtpct+
               Revenues+lagged_ratio|Year+ISO, data = total_taxes_sub))
summary(felm(ratio_work_all~loggdp+chprtpct+
               Revenues+lagged_ratio|ISO, data = total_taxes_sub))
###check, does protestantism drive higher taxes in general
summary(felm(Revenues~loggdp+chprtpct+Year|ISO, data = total_taxes))
summary(felm(Inco~loggdp+chprtpct+Year|ISO, data = total_taxes))

summary(felm(Revenues~loggdp+chprtpct+Year+lagged_Revenues|ISO, data = total_taxes))


cases<-felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub)
cases$X
total_taxes_sub
na.omit(total_taxes_sub[,c("ISO", "Year", "ratio_work_all",
                          "loggdp", "Revenues",
                          "chprtpct")])

total_taxes_sub[total_taxes_sub$ISO=="USA",]
plot(ratio_work_all~chprtpct, data = total_taxes_sub)


#.2/.8 seems good on lagged... if unlagged  .25..75 sorta, .3/.7...both intersting with .3333/.6666
total_taxes_sub1<-total_taxes[ total_taxes$chprtpct >=0 & total_taxes$chprtpct<=.3,]
total_taxes_sub2<-total_taxes[total_taxes$chprtpct>.3333 & total_taxes$chprtpct <=.6666,]
#total_taxes_sub3<-total_taxes[total_taxes$chprtpct>.5 & total_taxes$chprtpct <.75,]
total_taxes_sub4<-total_taxes[total_taxes$chprtpct>.6666 & total_taxes$chprtpct <= 1,]

summary(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub1))
summary(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub2))
#summary(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub3))
summary(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub4))

summary(felm(ratio_work_all~loggdp+chprtpct+Year+lagged_ratio|ISO, data = total_taxes_sub1))
summary(felm(ratio_work_all~loggdp+chprtpct+Year+lagged_ratio|ISO, data = total_taxes_sub2))
#summary(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub3))
summary(felm(ratio_work_all~loggdp+chprtpct+Year+lagged_ratio|ISO, data = total_taxes_sub4))

#.3333/.6666 works very well for sales tax...can crank to .1/.9
summary(felm(Sales~loggdp+chprtpct+Year|ISO, data = total_taxes_sub1))
summary(felm(Sales~loggdp+chprtpct+Year|ISO, data = total_taxes_sub2))
#summary(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_sub3))
summary(felm(Sales~loggdp+chprtpct+Year|ISO, data = total_taxes_sub4))



total_taxes_half_l<-total_taxes[ total_taxes$chprtpct >=0 & total_taxes$chprtpct<=.2,]
total_taxes_half_m<-total_taxes[ total_taxes$chprtpct >=.2 & total_taxes$chprtpct<=.6,]
total_taxes_half_u<-total_taxes[total_taxes$chprtpct>.6 & total_taxes$chprtpct <=1,]
library(lfe)
low_prot<-(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_half_l))
mid_prot<-(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_half_m))
high_prot<-(felm(ratio_work_all~loggdp+chprtpct+Year|ISO, data = total_taxes_half_u))
stargazer(low_prot, mid_prot, high_prot)

summary(felm(ratio_work_all~loggdp+chprtpct|ISO+Year, data = total_taxes_half_l))
summary(felm(ratio_work_all~loggdp+chprtpct|ISO+Year, data = total_taxes_half_m))
summary(felm(ratio_work_all~loggdp+chprtpct|ISO+Year, data = total_taxes_half_u))
??felm

