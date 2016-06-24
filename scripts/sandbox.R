library(ggplot2)
library(dplyr)

###### Working with Sarah's OSBS dbh/height data

osbs.dbh <- read.csv("supplemental_data/sarah_osbs_data.csv")

# look at data
head(osbs.dbh)

# 
ggplot(osbs.dbh, aes(x = ht_m, y = dbh_cm, color = sp))+
     geom_point()+
     geom_smooth(method = lm, se = FALSE)

# subset to pinus palustrus
pine <- subset(osbs.dbh, group == "e")
oak <- subset(osbs.dbh, group == "d")


ggplot(pine, aes(x = ht_m, y = dbh_cm, color = sp))+
     geom_point()+
     geom_smooth(method = lm)
ggplot(oak, aes(x = ht_m, y = dbh_cm, color = sp))+
     geom_point()+
     geom_smooth(method = lm)

lm.pine <- lm(dbh_cm ~ ht_m, data = pine)
lm.oak <- lm(dbh_cm ~ ht_m, data = oak)


coef(lm.pine)
coef(lm.oak)


##### testing function

osbs.dbh$calc.dbh <- calc_dbh(osbs.dbh$ht_m, osbs.dbh$group)


dbh.error <- lm(calc.dbh ~ dbh_cm, data = osbs.dbh)
     
ggplot(osbs.dbh, aes(x = dbh_cm, y = calc.dbh))+
     geom_point()+
     geom_smooth(method = lm)


osbs.dbh$calc.dbh <- calc_dbh(osbs.dbh$ht_m, osbs.dbh$group)
osbs.dbh$biomass.t <- calc_dbh(osbs.dbh$calc.dbh, osbs.dbh$group)
osbs.dbh$foliar.mass <- calc_foliar_biomass(osbs.dbh$biomass.t, osbs.dbh$group)

