library(raster)
library(MASS)
Year_2015<-'C:\\Users\\user\\Downloads\\MAR\\LC_MAR_Matrix_2015_2020_Raster_Reclassifed - LC_Morocco_300m_2015_Reclassified.tif'

Year_2020<-'C:\\Users\\user\\Downloads\\MAR\\LC_MAR_Matrix_2015_2020_Raster_Reclassifed - LC_Morocco_300m_2020_Reclassified.tif'

Year_2015<-raster(Year_2015)
Year_2020<-raster(Year_2020)
landcover<-data.frame(Year_2015=values(Year_2015),Year_2020=values(Year_2020))
landcover[landcover==0]<-NA
landcover[landcover==255]<-NA
na.omit(landcover)
landcover_change<-table(landcover)
#This command converts the values into a matrix table
land_coverchange_Matrix<-(round
                          (addmargins(landcover_change)*300*300*1/1000000,digits = 1))

#This command converts the pixel data from 300m*300m to square kms
#- each pixel presents 300 m in length by 300m in width
write.table(land_coverchange_Matrix,file="landtransitionmatrix.csv",append=TRUE,sep=",",col.names=NA,row.names=TRUE,quote=FALSE)

#Percentage Change Table 

Opening_Year<-cbind(rowSums(round(table(landcover)*300*300*1/1000000,digits= 1)))
Closing_Year<-cbind(colSums(round(table(landcover)*300*300*1/1000000,digits= 1)))
#The command cbind is a sum function
Opening_Year_percentage<-cbind(round((Opening_Year/sum(Opening_Year)*100),digits = 1))
Closing_Year_percentage<-cbind(round((Closing_Year/sum(Closing_Year)*100),digits = 1))
Difference<-cbind(c(Closing_Year_percentage)-c(Opening_Year_percentage))
percentageDifference<-(Difference/Opening_Year_percentage)*100

#between 1995 and 2015
FinalTable<-cbind(c(Opening_Year),c(Closing_Year),c(Difference),c(Opening_Year_percentage),c(Closing_Year_percentage),c(percentageDifference))
colnames(FinalTable)<-c("Year 2015","Year 2020","Difference","Year 2015 % of Total","Year 2020 % of Total","% Difference")


write.table(FinalTable ,file="percentage_land coverchange.csv",append=TRUE,sep=",",col.names=NA,row.names=TRUE,quote=FALSE)


