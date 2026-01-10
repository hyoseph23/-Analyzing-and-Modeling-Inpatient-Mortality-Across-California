##copy copy copy copy 
## import of libraries

library(ggplot2)
library(maps)
library(mapdata)
library(knitr)
library("spate")

##color matrix to use in plotting with for loops 
cl <- matrix(0,nrow=55,ncol=1)

cl[1:10] <- c("red")
cl[11:20] <- c("darkorange")
cl[21:30] <- c("blue")
cl[31:40] <- c("green")
cl[41:50] <- c("deeppink")
cl[51:55] <- c("purple")

##read in data set
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


#matrix with county, hospital_system, hospital_name, longitude, and latitude
bare2 <- matrix(0,nrow=num_hospitals,ncol=5)
bare2[,1] <- unique(hospital_name)


#populating subsetted data sets
for (i in 1:T1){
  bare[i,] <- c(year[i],county[i],hospital_system[i],hospital_name[i],performance[i],
                total_cases[i],adverse_events[i])
}


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

CA_w_counties <- ggplot(data=CA_county) + geom_polygon(aes(x = long, y= lat, group=group), fill = "lightblue", color = "blue") + coord_fixed(1.3)

hospital_locations <-CA_w_counties + geom_point(data=df.numerical_loc,aes(x=lg,y=la),
                                                color="black",fill="orange",pch=21, size=1, alpha=I(0.5))


##creating out array that maps county to the condition to the total cases, adverse cases, and prop of adverse/total

conditions <- unique(bare[,5])
county.names <- unique(bare[,2])
time <- unique(bare[,1])

K <- length(conditions)
C <- length(county.names)
T <- length(time)

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

##out2 array maps conditions to all the county 5 year prop of adverse  
out2 <- array(0,dim=c(K,C,5))


for(t in 1:T)
{
  for(k in 1:K)
  {
    #plot(1,type="n",main = paste(conditions[k]),xlab="",ylab="",xlim=c(2016,2020),ylim=c(0,1))
    
    for(c in 1:C)
    {
      out2[k,c,t] <- out[c,k,t,3]
      ##na is produced when county does 0 procedures and has 0 adverse
      if(is.na(out2[k,c,t])){
        out2[k,c,t] <- 0.00
      }
    }
  }
}


# maps the county to the hospitals associated with it and their information 
out3 <- array(0,dim=c(C))

for(c in 1:C){
  this <- which(bare2[,2] == county.names[c])
  
  hospitals <- matrix(0,nrow=length(this),ncol=2)
  hospitals[,1] <- c(county.names[c])
  hospitals[,2] <- c(bare2[this,1])
  print(hospitals)
}

# #prop of adverse plots
# 
# for(p in 1:T){
#   pdf(paste('proportion of adverse',time[p],'.pdf',sep=""),height=10,width=10)
#   
#   par(mfrow=c(4,4),mar=rep(2.5,4))
#   
#   for(j in 1:C){
#     plot(out[j, ,p,3],type="h",main =
#            paste(county.names[j]),
#          xlab="",ylab="",ylim=c(0,1))
#     box()
#   }
#   dev.off()
# }
# 
# 
# pdf('Conditions_over_5.pdf')
# 
# par(mfrow=c(1,1))
# # title <- "Legend for Procedure Plots"
# # legend <- plot(0,0,type="n",legend(x="center",legend = c("Alameda-Glenn","Humboldt-Marin","Mariposa-Placer",
# #                                        "Plumas-San Mateo","Santa Barbara-Trinity","Tulare-Yuba"),
# #                  fill=c("red","darkorange","blue","green","deeppink","purple")))
# # 
# # 
# # title_page <- paste("\\documentclass{article}, 
# # \\usepackage{graphix}, 
# # \\begin{document}, 
# # \\title{", title, "},
# # \\author{", legend,"}
# # \\maketitle,
# # \\clearpage")
# 
# for(k in 1:K){
#   plot(0,0,type="n",main = paste(conditions[k]),xlab="",ylab="",xlim=c(2016,2020),ylim=c(0,1))
#   
#   op <- par(cex=0.5)
#   legend("topright",legend = c("Alameda-Glenn","Humboldt-Marin",
#                                "Mariposa-Placer","Plumas-San Mateo",
#                                "Santa Barbara-Trinity","Tulare-Yuba"),
#          fill=c("red","darkorange","blue","green","deeppink","purple"))
#   
#   
#   for(c in 1:C){
#     
#     info <- matrix(0,nrow=5,ncol=2)
#     info[,1] <- c(2016,2017,2018,2019,2020)
#     info[,2] <- out2[k,c,]
#     lines(info[,1],info[,2],col=cl[c],pch=20,type="b")
#   }
# }
# 
# dev.off()










