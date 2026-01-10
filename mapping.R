##CA Mapsss
library('ggplot2')
library(maps)
library(mapdata)

data.set <- read.csv("california_hospitalreports2016-2020.csv",header = TRUE,sep = ",")
pop <- read.csv("ca_pop_2018.csv",header = TRUE,sep=",")

year <- data.set[,1]
county <- data.set[,2]
hospital_name <- data.set[,3]
OSHPDID <- data.set[,4]
hospital_system <- data.set[,5]
type_report <- data.set[,6]
performance <- data.set[,7]
adverse_events <- data.set[,8]
total_cases <- data.set[,9]
longitude <- data.set[,12]
latitude <- as.numeric(as.character(data.set[,13]))


T1 <- length(year)
num_hospitals <- length(unique(hospital_name))

#matrix with county, hospital_system, hospital_name, longitude, and latitude
bare2 <- matrix(0,nrow=num_hospitals,ncol=5)
bare2[,1] <- unique(hospital_name)

for(l in 1:T1){
  for(m in 1:num_hospitals)
    if((hospital_name[l] %in% bare2[m,1])){
      bare2[m, ] <- c(hospital_name[l],county[l],hospital_system[l],longitude[l],latitude[l])
    }
}


##using bare2 to add each hospital to the map

#creating data frame of longitude and latitude from bare2
df.numerical_loc <- data.frame(lg=c(as.numeric(bare2[,4])),la=c(as.numeric(bare2[,5])))


#creating california map with counties

county <- map_data("county")
CA_county <- subset(county, region == "california")

CA_w_counties <- ggplot(data=CA_county) + 
  geom_polygon(aes(x = long, y= lat, group=group), fill = "lightblue", color = "blue") + 
  coord_fixed(1.3)

hospital_locations <-CA_w_counties + geom_point(data=df.numerical_loc,aes(x=lg,y=la),
                                                color="black",fill="orange",pch=21, size=1, alpha=I(0.5))

