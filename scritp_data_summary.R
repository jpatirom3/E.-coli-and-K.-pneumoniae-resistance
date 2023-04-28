#### Summary plots for E. coli and P. neumoniae data


# Libraries ---------------------------------------------------------------


library(ggplot2)              #needs to be done each r session
library(tidyverse)            #needs to be done each r session



# Load data for E.coli---------------------------------------------------------------

setwd("C:/Users/Eq_PEP_IEP_SPN_Tco/Dropbox/my_research/Paper JWK/Scripts and data Felipe")
decoli=read.table("Data_Ecoli.txt", h=T)
View(decoli)


# Load world data ---------------------------------------------------------
mapdata <- map_data("world") ##ggplot2, load lat, lon for every country in the world
#rename column "region" to "Country" to match with decoli dataframe
mapdata = rename(mapdata, Country = region)
#replace Czech Repblic to Czech_Republic to match with decoli names
mapdata$Country[mapdata$Country=="Czech Republic"]<-"Czech_Republic"
View(mapdata)


# Merge both previous dataframes ------------------------------------------
decolimap = left_join(mapdata,decoli,by = "Country")
View(decolimap)


# #Filter out countries with no data --------------------------------------
decolimap1 = decolimap %>% filter(!is.na(decolimap$R_multi))
View(decoli)


# #same filter function but without the infix operator %>% ------------------
decolimap1 = filter(decolimap,!is.na(decolimap$R_multi))
View(decolimap1)


# Plot the map ------------------------------------------------------------

map1=ggplot(decolimap1, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill = R_multi),color="black")
map1


# another plot of the map -------------------------------------------------
map2_ecoli = map1 + scale_fill_gradient(name = "R_multi",low="yellow",high="red",na.value = "blue")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle("a")
map2_ecoli







# Load data for K. neumoniae---------------------------------------------------------------

setwd("C:/Users/Eq_PEP_IEP_SPN_Tco/Dropbox/my_research/Paper JWK/Scripts and data Felipe")
dkleb=read.table("Data_Klebsiella.txt" , h=T)
View(dkleb)


# Load world data ---------------------------------------------------------
mapdata <- map_data("world") ##ggplot2, load lat, lon for every country in the world
#rename column "region" to "Country" to match with decoli dataframe
mapdata = rename(mapdata, Country = region)
#replace Czech Repblic to Czech_Republic to match with decoli names
mapdata$Country[mapdata$Country=="Czech Republic"]<-"Czech_Republic"
View(mapdata)



# Merge both previous dataframes ------------------------------------------
dklebmap = left_join(mapdata,dkleb,by = "Country")
View(dklebmap)


# #Filter out countries with no data --------------------------------------
dklebmap1 = dklebmap %>% filter(!is.na(dklebmap$R_multi))
View(dklebmap1)


# #same filter function but without the infix operator %>% ------------------
dklebmap1 = filter(dklebmap,!is.na(dklebmap$R_multi))
View(dklebmap1)



# Plot the map ------------------------------------------------------------

map1_kleb=ggplot(dklebmap1, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill = R_multi),color="black")
map1_kleb


# another plot of the map -------------------------------------------------
map2_kleb = map1_kleb + scale_fill_gradient(name = "R_multi",low="yellow",high="red",na.value = "blue")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  ggtitle("b")
map2_kleb


# final graph of both plots
library(gridExtra)
grid.arrange(map2_ecoli,map2_kleb,ncol=2)

######### spatial concordance between resistance to antibiotics in two bacteria

tabla_ecoli=data.frame(aggregate(decolimap1$R_multi,by=list(decolimap1$Country),mean))
tabla_kleb=data.frame(aggregate(dklebmap1$R_multi,by=list(dklebmap1$Country),mean))

tabla_ecoli = tabla_ecoli[order(tabla_ecoli$x),]
tabla_kleb = tabla_kleb[order(tabla_kleb$x),]


cor(tabla_ecoli$x,tabla_kleb$x)
#correlación de 0.65 entre países


barplot(tabla_ecoli$x,names.arg = tabla_ecoli$Group.1,las=2)
barplot(tabla_kleb$x,names.arg = tabla_kleb$Group.1,las=2)


###########################################################################
# Time series graph by region ---------------------------------------------
###########################################################################

#eyr = ecoly_year_region
eyr=aggregate(decolimap1$R_multi,by=list(decolimap1$Region,decolimap1$Year),mean)
names(eyr)=c("region","year","rmulti")

#kyr = kleb_year_region
kyr=aggregate(dklebmap1$R_multi,by=list(dklebmap1$Region,dklebmap1$Year),mean)
names(kyr)=c("region","year","rmulti")

#####
par(mar=c(2.5,4,0,0.2))
a=layout(matrix(c(1,1,2,3),2,2,byrow = TRUE), c(1,1), c(1,5.5))
layout.show(a)
#
par(mar=c(0,0,0,0))
plot(1, type="n", xlim=c(0,5), ylim=c(0,5),xaxt="none",yaxt="none",xlab="",ylab="",bty="n" )
legend(x="center",horiz=T, legend=c("Eastern","Southern","Central","Western","Northern"),
       col=c("red","orange","green4","blue","lightblue"),pch=19,cex=1.5)
#
par(mar=c(2.5,5,0,0.2))
plot(eyr$rmulti~eyr$year,col=as.factor(eyr$region),pch=19,cex=1.5,xlab="Year",ylab="R_multi",
     cex.lab=1.5,cex.axis=1.5)
abline(v=seq(2008,2018,1),lwd=0.1,col="grey",lty=3)
points(eyr$rmulti~eyr$year,col=c("green4","red","lightblue","orange","blue"),pch=19,cex=1.5)
lines(lowess(eyr$rmulti[eyr$region=="Eastern"]~eyr$year[eyr$region=="Eastern"]),col="red")
lines(lowess(eyr$rmulti[eyr$region=="Southern"]~eyr$year[eyr$region=="Southern"]),col="orange")
lines(lowess(eyr$rmulti[eyr$region=="Central"]~eyr$year[eyr$region=="Central"]),col="green4")
lines(lowess(eyr$rmulti[eyr$region=="Western"]~eyr$year[eyr$region=="Western"]),col="blue")
lines(lowess(eyr$rmulti[eyr$region=="Northern"]~eyr$year[eyr$region=="Northern"]),col="lightblue")



#
par(mar=c(2.5,4,0,0.5))
plot(kyr$rmulti~kyr$year,col=as.factor(kyr$region),pch=19,cex=1.5,xlab="Year",ylab="",
     cex.axis=1.5)
abline(v=seq(2008,2018,1),lwd=0.1,col="grey",lty=3)
points(kyr$rmulti~kyr$year,col=c("green4","red","lightblue","orange","blue"),pch=19,cex=1.5)
lines(lowess(kyr$rmulti[kyr$region=="Eastern"]~kyr$year[kyr$region=="Eastern"]),col="red")
lines(lowess(kyr$rmulti[kyr$region=="Southern"]~kyr$year[kyr$region=="Southern"]),col="orange")
lines(lowess(kyr$rmulti[kyr$region=="Central"]~kyr$year[kyr$region=="Central"]),col="green4")
lines(lowess(kyr$rmulti[kyr$region=="Western"]~kyr$year[kyr$region=="Western"]),col="blue")
lines(lowess(kyr$rmulti[kyr$region=="Northern"]~kyr$year[kyr$region=="Northern"]),col="lightblue")


#####
