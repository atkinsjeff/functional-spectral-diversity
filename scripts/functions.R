###########################
 

###### function to calculate dbh from height
calc_dbh <- function(height, group) {
     # gives calculated dbh in cm
     # this seperates if the group is an evergreen tree
     if(group == "c") {
          dbh <- 1.05 * height + 22.58
          #f.mass <- exp(-2.9584 + (4.4766/dbh))
     } else {
          dbh <- 1.96 * height + 10.60
          #f.mass <- exp(-4.0813 + (5.8816/dbh))
     }
}


##### function to calculate biomass
calc_biomass <- function(dbh, group) {
     # Gives biomass in kg
     # this seperates if the group is an evergreen tree
     if(group == "c") {
          biomass.t <- exp(-2.5356 + (2.4349*(log(dbh))))
     } else {
          biomass.t <- exp(-2.0127 + (2.4342*(log(dbh)))) 
     }
}

# calc foliar biomass
calc_foliar_biomass <- function(biomass, group) {
     # this gives foliar mass in kg
     # this seperates if the group is an evergreen tree
     if(group == "c"){
          biomass.f <- biomass * 0.16
     } else {
          biomass.f <- biomass * 0.08         
     }
}

