library(ggplot2)
library(maps)
library(mapdata)
library(RColorBrewer)
library(stringr)
library(dplyr)
library(gridExtra)
library(ggforce)

#### STATE MAPSSSSSSSS

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

#matrix with year, county, hospital_system, hospital_name, performance, 
#total_cases, and adverse
bare <- matrix(0,nrow=T1,ncol=7)

#populating subsetted data sets
for (i in 1:T1){
  bare[i,] <- c(year[i],county[i],hospital_system[i],hospital_name[i],performance[i],
                total_cases[i],adverse_events[i])
}


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
df.numerical_loc <- na.omit(df.numerical_loc)

county <- map_data("county")
CA_county <- subset(county, region == "california")

CA_w_counties <- ggplot(data=CA_county) + geom_polygon(aes(x = long, y= lat, group=group), 
                                                       fill = "lightblue", color = "blue") + coord_fixed(1.3)


hospital_locations <- CA_w_counties + geom_point(data=df.numerical_loc,aes(x=lg,y=la),
                                                color="black",fill="black",pch=21, size=1, alpha=I(0.5))


copy_CA_county_1 <- CA_county[CA_county$subregion != "alpine", ]
copy_CA_county_1 <- copy_CA_county_1[copy_CA_county_1$subregion != "sierra",]
copy_CA_county_1 <- copy_CA_county_1[copy_CA_county_1$subregion != "sutter",]
# copy_CA_county_1 <- copy_CA_county_1[copy_CA_county_1$subregion != "glenn",]
# copy_CA_county_1 <- copy_CA_county_1[copy_CA_county_1$subregion != "lassen",]
# copy_CA_county_1 <- copy_CA_county_1[copy_CA_county_1$subregion != "mono",]
# copy_CA_county_1 <- copy_CA_county_1[copy_CA_county_1$subregion != "modoc",]


################################

##creating out array that maps county to the condition to the total cases, adverse cases, and prop of adverse/total

conditions <- unique(bare[,5])
county.names <- unique(bare[,2])
time <- unique(bare[,1])

## lets take out the conditions that do not have significant information 

conditions<- conditions[-6] #Carotid Endarterectomy
conditions <- conditions[-9] #PCI
conditions <- conditions[-(11:18)] #AAA Repair Endo-Unrupture - Isolated CABG Operative Mortality


## lets take out the counties that do not have significant information 

# county.names <- county.names[-10] #Glenn 
# county.names <- county.names[-16] #Lassen
# county.names <- county.names[-(22:23)] #Mono and Modoc

## Also take these counties out of the population data set

# pop <- pop[-10,]
# pop <- pop[-16,]
# pop <- pop[-(22:23),]


K <- length(conditions)
C <- length(county.names)
T <- length(time)

################################

out <- array(0,dim=c(C,K,T,3),dimnames = list(county.names,conditions,time))

for(t in 1:T)
{
  for(c in 1:C)
  {
    for(k in 1:K)
    {
      these <- which((bare[,5]==conditions[k])&(bare[,2]==county.names[c])
                     &(bare[,1]==time[t]))
      out[c,k,t,1] <- sum(strtoi(bare[these,6]),na.rm=TRUE)/pop[c,2]
      out[c,k,t,2] <- sum(strtoi(bare[these,7]),na.rm=TRUE)/pop[c,2]
      out[c,k,t,3] <- (out[c,k,t,2]/out[c,k,t,1])
    }
    
  }
}

pdf("hospital_locations_CA.pdf",height=12,width=12)
par(mfrow=c(1,1))

print(hospital_locations + ggtitle("Map of Hospital Locations"))

dev.off()
################################

res <- NULL

for(k in 1:k){
  for(t in 1:T){
    t3 <- matrix(0,nrow=C,ncol=4)
    
    t_list <- rep(list(t3),T)

    t3[,1] <- unique(copy_CA_county_1[,6])
    t3[,2] <- time[t]
    t3[,3] <- conditions[k]
    t3[,4] <- (out[,k,t,3])
    
    t_list[[t]] <- t3

    res <- rbind(res,t_list[[t]])
  }
}


res <- as.data.frame(res)
names(res) <- c("subregion","year","condition","proportion")

ilock <- inner_join(copy_CA_county_1,res, by = "subregion",relationship = "many-to-many")


pdf("testing23.pdf",height=11,width=8.5)

  
dens <- ggplot(ilock,aes(long,lat)) +
geom_polygon(aes(group=group,fill=as.numeric(proportion))) +
  scale_x_continuous(limits = c(-125,-114)) +
  scale_y_continuous(limits = c(31,43)) + coord_fixed(1.3)


#facet_wrap(~year+condition,nrow=10,ncol=5)
dens2 <- dens + scale_fill_gradientn(colours =
                                           rev(rainbow(8)),breaks=c(0,0.025,0.05,0.1,0.3,0.7,1.0))

dens3 <- dens2 + geom_point(data=df.numerical_loc,aes(x=lg,y=la),
                                color="black",fill="black",pch=21, size=1, alpha=I(0.5))

#print(dens3)
for(i in 1:T){
  print(dens3 +facet_wrap_paginate(~condition+year,ncol = 5, nrow=2, page = i))
}
  
dev.off()









