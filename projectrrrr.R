#assignment

#package installing
install.packages("tidyverse")
library(tidyverse)

install.packages("caret")
library(caret)

install.packages("RCurl")
library(RCurl)

install.packages("skimr")
library(skimr)


#importing .csv into global enviroment
data <- read.csv("stockdata.csv")

#data preprocessing
str(data)
head(data)
view(data)

#dimenstion of dataset
dim(data)

#list the datatype of each attribute
sapply(data,class)

#check wheather there is null value
sum(is.na(data))

#sumarize
summary(data)

#displaying detailed summary statistics
skim(data)

#group data by index
data %>%
  dplyr::group_by(Index)%>%
  skim()

#create validation set
validation <- createDataPartition(data$Index,p=0.50,list=FALSE)
validation50 <-data[-validation,]

data<-data[validation,]
view(data)

#visulizing the dataset

#input and output
x<-data[,3:8]
view(x)
y<-data[,9]
view(y)

#plots
boxplot(x[,4],main=names(data)[4],ylab="high")
boxplot(x[,3],main=names(data)[3],ylab="open")
boxplot(x[,5],main=names(data)[5],ylab="low")
boxplot(x[,6],main=names(data)[6],ylab="close")
boxplot(x[,7],main=names(data)[7],ylab="adj.close")
boxplot(x[,9],main=names(data)[9],ylab="USD")




#for all attribute
par (mfrow=c(3,8))
for(i in 3:8)
{
  boxplot(x[,i],main=names(data)[i])
}

library(caret)
plot(y)

#scatter plot
library(caret)
featurePlot(x=x,y=y)

install.packages("ellipse")
library(ellipse)
library(caret)
featurePlot(x=x,y=y,plot ="ellipse")


#box plots
library(caret)
featurePlot(x=x,y=y,plot="box")


#density plot
scales<-list(x=list(relation="free"),y=list(relation="free"))

scales<-list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y,plot = "density",scales=scales)

#barplot
counts<-table(data$Open)
barplot(counts)
barplot(counts,horiz=TRUE)
barplot(counts,
        main="Barplot",xlab="open",legend=rownames(counts),col=c("red","yellow","green"))


#boxplot
bwplot(~Open,data=data,main="boxplots")



#High
bwplot(~Open|Index,data=data,main="boxplots")
bwplot(~High|Index,data=data,main="boxplots")
bwplot(~Low|Index,data=data,main="boxplots")
bwplot(~Close|Index,data=data,main="boxplots")
bwplot(~Adj.Close|Index,data=data,main="boxplots")
bwplot(~CloseUSD|Index,data=data,main="boxplots")




#hisogram
install.packages("lattice")
library(lattice)
histogram(~Open,data = data,main="histogram")
histogram(~Close,data = data,main="histogram")
histogram(~High,data = data,main="histogram")
histogram(~Low,data = data,main="histogram")
histogram(~Adj.Close,data = data,main="histogram")
histogram(~CloseUSD|Date,data = data,main="histogram")
histogram(Open~CloseUSD|Index,data = data,main="histogram")
histogram(High~CloseUSD|Index,data = data,main="histogram")
histogram(Low~CloseUSD|Index,data = data,main="histogram")
histogram(Close~CloseUSD|Index,data = data,main="histogram")
histogram(Adj.Close~CloseUSD|Index,data = data,main="histogram")


#scatter
xyplot(Open~CloseUSD,data=data,main="Scatterplot")
xyplot(High~CloseUSD,data=data,main="Scatterplot")
xyplot(Low~CloseUSD,data=data,main="Scatterplot")
xyplot(Close~CloseUSD,data=data,main="Scatterplot")
xyplot(Adj.Close~CloseUSD,data=data,main="Scatterplot")
xyplot(Volume~CloseUSD,data=data,main="Scatterplot")
xyplot(Volume~CloseUSD|Index,data=data,main="Scatterplot")
xyplot(Open~CloseUSD|Index,data=data,main="Scatterplot")
xyplot(High~CloseUSD|Index,data=data,main="Scatterplot")
xyplot(Low~CloseUSD|Index,data=data,main="Scatterplot")
xyplot(Close~CloseUSD|Index,data=data,main="Scatterplot")
xyplot(Adj.Close~CloseUSD|Index,data=data,main="Scatterplot")



#barchart
barchart(~Open,data=data,main="Barchart")
barchart(~High,data=data,main="Barchart")
barchart(~Low,data=data,main="Barchart")
barchart(~Close,data=data,main="Barchart")
barchart(~Adj.Close,data=data,main="Barchart")
barchart(Open~CloseUSD,data=data,main="Barchart")


#densityplot
densityplot(~Open,data=data)
densityplot(~High,data=data)
densityplot(~Close,data=data)
densityplot(~Low,data=data)
densityplot(~Adj.Close,data=data)
densityplot(Open~CloseUSD,data=data)
densityplot(High~CloseUSD,data=data)
densityplot(Low~CloseUSD,data=data)
densityplot(~CloseUSD|Index,data=data)


#piechart
install.packages("plotrix")
library(plotrix)
slices<-c("Open")
lbls<-c("Index")
pie(slices,labels=lbls,main="Piecharts")



#ggplot
library(ggplot2)
library(grid)
library(gridExtra)
(plot1<-ggplot(data=data,mapping=aes(x=Index,y=Open))+geom_boxplot())
(plot2<-ggplot(data=data,mapping=aes(x=Index,y=High))+geom_boxplot())
(plot3<-ggplot(data=data,mapping=aes(x=Index,y=Low))+geom_boxplot())
(plot4<-ggplot(data=data,mapping=aes(x=Index,y=Close))+geom_boxplot())
(plot5<-ggplot(data=data,mapping=aes(x=Index,y=CloseUSD))+geom_boxplot())
ggplot(data=data,mapping=aes(x=Index,y=Open))+geom_boxplot(notch = TRUE,outlier.colour = "red",outlier.size = 3)
ggplot(data=data,mapping=aes(x=Index,y=Close))+geom_boxplot(notch = TRUE,outlier.colour = "red",outlier.size = 3)
ggplot(data=data,mapping=aes(x=Index,y=High))+geom_boxplot(notch = TRUE,outlier.colour = "red",outlier.size = 3)
ggplot(data=data,mapping=aes(x=Index,y=Low))+geom_boxplot(notch = TRUE,outlier.colour = "red",outlier.size = 3)
ggplot(data=data,mapping=aes(x=Index,y=Open))+geom_boxplot(notch = TRUE,outlier.colour = "red",outlier.size = 3)+coord_flip()
(plot7<-ggplot(data=data,mapping=aes(x=Index,y=CloseUSD))+geom_boxplot(notch = TRUE,outlier.colour = "red",outlier.size = 3))

#histogram
ggplot(data=data,mapping=aes(Open))+geom_histogram(binwidth = 50,colour="blue")
ggplot(data=data,mapping=aes(High))+geom_histogram(binwidth = 50,colour="blue")
ggplot(data=data,mapping=aes(Low))+geom_histogram(binwidth = 50,colour="blue")
ggplot(data=data,mapping=aes(Close))+geom_histogram(binwidth = 50,colour="blue")
ggplot(data=data,mapping=aes(Adj.Close))+geom_histogram(binwidth = 30,colour="blue")
(plot8<-ggplot(data=data,mapping=aes(CloseUSD))+geom_histogram(binwidth = 50,colour="lightblue"))


#combining
plots<-list(plot1,plot2,plot3,plot4,plot5)
layout<-rbind(c(1,2),c(3,4),c(5,5))
grid.arrange(grobs=plots,layout_matrix=layout)

plots<-list(plot7,plot8)
layout<-rbind(c(7,8))
grid.arrange(grobs=plots,layout_matrix=layout)

#modify
ggplot(data=data,mapping=aes(x=CloseUSD,y=Open,colour=cut))+geom_point()+facet_grid(~Index)










