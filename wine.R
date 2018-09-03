rm(list=ls())
setwd("~/wine")
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(randomForest)
library(svglite)


wine_red<- read.csv("winequality-red.csv",sep=";")
wine_white<- read.csv("winequality-white.csv",sep=";")

wine_red$color<-factor("red")
wine_white$color<-factor("white")

wine_red$color1<- 1
wine_white$color1<- 0

wine<-rbind(wine_red,wine_white)

#wine$quality<-as.factor(wine$quality)

wine1<-gather(wine,attributes,value,fixed.acidity:alcohol)
head(wine1)


########## correlation ######################


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

wine_cor <- cor(wine[,1:12])   %>% melt()

wine_cor <- wine_cor[wine_cor$value!=1,]
wine_cor <- wine_cor[!duplicated(wine_cor$value),]
#wine_cor <- wine_cor[order(abs(wine_cor$value),decreasing=T),]

wine_cor_plot<-ggplot(data = wine_cor, aes(x=Var1, y=Var2, fill=value))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, space = "Lab", 
                       name="Pearson\nCorrelation")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1)) + 
  geom_tile()+xlab(NULL)+ylab(NULL)

ggsave(file="wine_cor.png", plot=wine_cor_plot, width=4, height=4)


####### What factors determine the quality of wine ########


####### Which color wine has more density? white or red? ######
ggplot(wine)+aes(x=density,color=color)+geom_histogram(binwidth = .0001) + 
  scale_x_continuous(limits = c(.987,  1.005)) + geom_density()+
  theme_classic() + ggtitle("Distribution of densities of red and white wine")
ggplot(wine)+aes(y=density,x=color,fill=color)+geom_boxplot() + 
  scale_y_continuous(limits = c(.987,  1.005)) +
  theme_classic() + ggtitle("Density comparison of red and white wine")

"The density of red wine is more than white "

####### Which color wine scored more in quality ##########

quality <- as.data.frame.matrix(table(wine$quality,wine$color))
quality$red <- quality$red/sum(quality$red)
quality$white <- quality$white/sum(quality$white)
quality$quality <- rownames(quality)

q1<-ggplot(quality)+aes(x=quality,y=red,fill="red")+geom_bar(stat = "identity")+ guides(fill=FALSE)
q2<-ggplot(quality)+aes(x=quality,y=white,fill=2)+geom_bar(stat = "identity")+ guides(fill=FALSE)
grid.arrange(q1,q2,ncol=2)

ggplot(wine)+aes(x=quality,fill=color)+facet_wrap(vars(color))+geom_bar()

"White wine scored more 6,7,8 score and red scored more 5 score"

####### Does percent of alcohol affect wine quality ##################


ggplot(wine)+aes(y=alcohol,x=as.factor(quality))+
  geom_boxplot()+xlab("Quality")+ylab("Alcohol")+
  ggtitle("Percentage alcohol content of wine by quality")

"Wine with more alcohol content got good quality scores 7, 8, 9"


####### Does increase or decrease in alcohol percentage affect density ##########

ggplot(wine[wine$quality<9,])+aes(x=density,y=alcohol,col=color)+
  facet_wrap(vars(quality))+
  scale_x_continuous(limits = c(.98,  1.01))+
  geom_point(alpha = 1/5)+geom_smooth(method = "lm")


cor(wine$density,wine$alcohol)
lm(density~alcohol,data=wine)


ggplot(wine)+aes(x=density,y=fixed.acidity,col=color)+
  facet_wrap(vars(color))+
  scale_x_continuous(limits = c(.98,  1.01))+
  geom_point(alpha = 1/5)+geom_smooth(method = "lm")


"For red wine increase in density increases fixed acidity"

##### Why is this trend different for white wine ########


######### what is contributing to the color of  wine


####### 

p1<-ggplot(wine)+aes(y=fixed.acidity,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p2<-ggplot(wine)+aes(y=volatile.acidity,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p8<-ggplot(wine)+aes(y=pH,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p9<-ggplot(wine)+aes(y=sulphates,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p3<-ggplot(wine)+aes(y=citric.acid,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p4<-ggplot(wine)+aes(y=residual.sugar,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p5<-ggplot(wine)+aes(y=chlorides,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p6<-ggplot(wine)+aes(y=free.sulfur.dioxide,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p7<-ggplot(wine)+aes(y=density,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)
p10<-ggplot(wine)+aes(y=alcohol,x=color,fill=color)+geom_boxplot()+ guides(fill=FALSE)


p11<-grid.arrange(p1,p2,p8,p9,p3,p4,p5,p6,p7,p10, nrow=2)



ggsave(file="wine.png", plot=p11, width=10, height=3)


ggplot(wine)+aes(x=as.numeric(quality),fill=color)+geom_density() + 
  facet_grid(cols = vars(color))+
  ggtitle("Quality of red and white wine")



ggplot(wine)+aes(x=density,color=color)+geom_density()+ 
  scale_x_continuous(limits = c(.987,  1.005)) +
  facet_grid(vars(quality))+
  ggtitle("Distribution for various qualities of wine")

ggplot(wine)+aes(y=density,fill=color,x=color)+geom_boxplot()+facet_grid(cols=vars(quality))+
  scale_y_continuous(limits = c(.98,  1.015))+
  ggtitle("Distribution for various qualities of wine")











