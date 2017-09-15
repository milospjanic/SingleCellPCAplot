#!/bin/bash

echo "#!/usr/bin/Rscript
library(reshape2)
first=$1 #pass the first argument to a variable so that it can be used in grep with double quotes, in order to use spaced argument 
second=$2

x<-read.delim(\"$first\", header=T, row.names=1)

x1<-x
#x1<-x[c(-1,-2),c(-186,-187,-188)] #if formating is needed

numc <- sapply(x1, is.factor)
x1[numc]<- lapply(x1[numc], function(x) as.numeric(as.character(x)))

pca= prcomp( x1 , center=T, scale=T)

#plot simple pca

pdf("pca_nocol.pdf")
plot(pca$rotation[,1],pca$rotation[,2], xlab = \"PC1\", ylab = \"PC2\")
text(pca$rotation[,1],pca$rotation[,2], row.names(pca$rotation), cex=0.2, pos=4)
dev.off()

#select row with the gene of interest (second parameter), melt, assign rownames
x2<- melt(x1[\"$second\",])
row.names(x2)<-x2$variable
x2$variable<-NULL

#cbind with pca matrix, pca$rotation
x3<-cbind(pca$rotation,x2)

#assign basic color
x3$color=\"grey\"

#assign 3 color scheme
x3$color[x3$value<range(x3$value)[2]/10 & x3$value>0] ="lavenderblush1"
x3$color[x3$value<(range(x3$value)[2]/10)*2 & x3$value>range(x3$value)[2]/10] ="lightpink"
x3$color[x3$value<(range(x3$value)[2]/10)*3 & x3$value>(range(x3$value)[2]/10)*2] ="lightpink1"

pdf("pca.pdf")
plot(pca$rotation[,1],pca$rotation[,2], col=x3$color, xlab = "PC1", ylab = "PC2", pch=19)
text(pca$rotation[,1],pca$rotation[,2], row.names(pca$rotation), cex=0.2, pos=4)
dev.off()

x3$color="grey96"
x3$color[x3$value<range(x3$value)[2]/10 & x3$value>0] ="lavenderblush1"
x3$color[x3$value<(range(x3$value)[2]/10)*2 & x3$value>range(x3$value)[2]/10] ="lightpink"
x3$color[x3$value<(range(x3$value)[2]/10)*3 & x3$value>(range(x3$value)[2]/10)*2] ="lightpink1"
x3$color[x3$value<(range(x3$value)[2]/10)*4 & x3$value>(range(x3$value)[2]/10)*3] ="lightpink2"
x3$color[x3$value<(range(x3$value)[2]/10)*5 & x3$value>(range(x3$value)[2]/10)*4] ="lightpink3"
x3$color[x3$value<(range(x3$value)[2]/10)*6 & x3$value>(range(x3$value)[2]/10)*5] ="orangered3"
x3$color[x3$value<(range(x3$value)[2]/10)*7 & x3$value>(range(x3$value)[2]/10)*6] ="red3"
x3$color[x3$value<(range(x3$value)[2]/10)*8 & x3$value>(range(x3$value)[2]/10)*7] ="red2"
x3$color[x3$value<(range(x3$value)[2]/10)*9 & x3$value>(range(x3$value)[2]/10)*8] ="red1"
x3$color[x3$value<(range(x3$value)[2]/10)*10 & x3$value>(range(x3$value)[2]/10)*9] ="red"

data<- read.table('sample_data.txtt', header=TRUE, row.name=1)
# Create new column filled with default colour
data$Colour="black"
# Set new column values to appropriate colours
data$Colour[data$col_name2>=3]="red"
data$Colour[data$col_name2<=1]="blue"
# Plot all points at once, using newly generated colours
plot(data$col_name1,data$col_name2, ylim=c(0,5), col=data$Colour, ylim=c(0,10))


" > script.r

#run R script

chmod 775 script.r
./script.r
