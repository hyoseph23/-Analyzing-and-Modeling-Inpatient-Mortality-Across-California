## import of library

library("ggplot2")
library(maps)
library(mapdata)

##read in data set


data.set <- read.csv("california_hospitalreports2016-2020.csv",header = TRUE,sep = ",")
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
latitude <- data.set[,13]


##matrix with just year,county, hospital_system[i], hospital_name, adverse_events, and total_cases
T1 <- length(year)
num_hospitals <- length(unique(hospital_name))
bare <- matrix(0,nrow=T1,ncol=7)
bare2 <- matrix(0,nrow=num_hospitals,ncol=5)
bare2[1,] <- c(county[1],hospital_system[1],hospital_name[1],longitude[1],latitude[1])



for (i in 1:T1){
  bare[i,] <- c(year[i],county[i],hospital_system[i],hospital_name[i],performace[i],
                total_cases[i],adverse_events[i])
}

for (m in 2:num_hospitals){
  if(bare2[m-1,3] != hospital_name[m]){
    bare2[m,] <- c(county[m],hospital_system[m],hospital_name[m],longitude[m],latitude[m])
  }
}

# # bare.2016 <- bare[1:3990,]
# # bare.2017 <- bare[3991:7993,]
# # bare.2018 <- bare[7994:11995,]
# # bare.2019 <- bare[11995:16645,]
# # bare.2020 <- bare[16646:21277,]
# 

# conditions <- unique(bare[,5])
# county.names <- unique(bare[,2])
# time <- unique(bare[,1])
# 
# K <- length(conditions)
# C <- length(county.names)
# T <- length(time)
# 
# out <- array(0,dim=c(C,K,T,3),dimnames = list(county.names,conditions,time))
# 
# for(t in 1:T)
#   {
#   for(c in 1:C)
#   {
#     for(k in 1:K)
#     {
#       these <- which((bare[,5]==conditions[k])&(bare[,2]==county.names[c])
#                      &(bare[,1]==time[t]))
#       out[c,k,t,1] <- sum(strtoi(bare[these,6]),na.rm=TRUE)
#       out[c,k,t,2] <- sum(strtoi(bare[these,7]),na.rm=TRUE)
#       out[c,k,t,3] <- (out[c,k,t,2]/out[c,k,t,1])
#     }
# 
#   }
# }
# 
# return(out)
# 
# 
# #prop
# for(l in 1:T){
#   for(j in 1:C){
#     p <- plot(out[j, ,l,3],type="h",main =
#                 paste("Proportion of adverse events in",county.names[j],"county",time[l]),
#               xlab="Procedure index",ylim=c(0,0.2))
# }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
