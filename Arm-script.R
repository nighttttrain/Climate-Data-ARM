library(ggplot2)
library(dplyr)
library(webr)
library(readxl)

arm_data1 <- read_excel("/Users/rain/Documents/career/otvi/Arm/Arm Carbon Data.xlsx", sheet="Sheet2-r")
arm_data1 <- arm_data1[-9,]

arm_data2 <- read_excel("/Users/rain/Documents/career/otvi/Arm/Arm Carbon Data.xlsx", sheet="Sheet3-r")
arm_data2 <- arm_data2[-9,]

# Overview

ggplot(arm_data1, aes(x=Emissions_Category, y=FYE23)) +
  geom_bar(stat='identity', position='dodge')

# Task 1

# local based
PieDonut(arm_data2, aes(Scope, Emissions_Category, count=FYE23), 
         selected=1,
         labelposition=0.5,
         ratioByGroup=FALSE,
         use.labels = FALSE,
         pieLabelSize = 4,
         donutLabelSize = 3.7,
         titlesize = 4,
         donutAlpha = 1,
         r0=0.4,r1=0.8,r2=1.2,
         maxx = 2.5,
         showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0),
)

# market based
arm_data1_pie <- arm_data1[-2,]
arm_data1_pie <- arm_data1_pie[,c(1,3,4)]
PieDonut(arm_data1_pie, aes(Scope, Emissions_Category, count=FYE23), 
         labelposition=0.5,
         ratioByGroup=FALSE,
         use.labels = FALSE,
         pieLabelSize = 4,
         donutLabelSize = 3.7,
         titlesize = 4,
         donutAlpha = 1,
         r0=0.4,r1=0.8,r2=1.2,
         maxx = 2.5,
         showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0),
)


PieDonut(acs,aes(Dx,smoking), 
         selected=c(1),explodeDonut=TRUE)

# Task 2
library(RColorBrewer)
display.brewer.all()

task2data <- read_excel("/Users/rain/Documents/career/otvi/Arm/Arm Carbon Data.xlsx", sheet="task2-r")

theme <- theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line.x = NULL,
               axis.line.y = NULL,
               axis.text.x = element_text(size = 14),
               axis.text.y = element_text(size = 14),
)

library(scales)
options(scipen = 999)

ggplot(aes(x=FYE, y=Emission, fill=Emissions_Category),data=task2data) +
geom_bar(stat='identity',
         width=0.2
         )+
  theme+
  coord_flip()+
  scale_fill_brewer(palette = "Set3")+
  geom_col(
    width = 0.6,
    position = position_stack(reverse = TRUE))+
  scale_y_continuous(labels = scales::comma)

  
ggplot(aes(x=FYE, y=Emission, fill=Emissions_Category),data=task2data) +
  geom_bar(stat='identity',
           width=0.2
  )+
  theme+
  coord_flip()+
  scale_fill_brewer(palette = 1)+
  geom_col(
    width = 0.6,
    position = position_stack(reverse = TRUE))+
  scale_y_continuous(labels = scales::comma)




