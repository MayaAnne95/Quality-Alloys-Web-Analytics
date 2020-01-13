#Case study "Web Analytics at Quality Alloys, Inc."
#Team 7: Maya Anne | Melissa Camilo | Peter Huesmann  | Deepak Kumar | Kapilesh Kothvale
#Reading required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lattice)


#Creating names for column headers
cnames1 <-c("Week","Visits","Unique Visits","Pageviews","Pages/Visit","Avg_Time","Bounce Rate","Perc_New_Visits")
#Creating dataframe for the second excel sheet containing only the required values, without the description in the beginning
QA_df_weekly_visits <- read_excel("Web Analytics Case Student Spreadsheet.xls",2,col_names= cnames1)[6:71,1:8]

#Creating names for column headers
cnames2 <-c("Week","Revenue","Profit","Lbs. Sold","Inquiries","1")
#Creating dataframe for the third excel sheet containing only the required values, without the description in the beginning
QA_df_financials <- read_excel("Web Analytics Case Student Spreadsheet.xls",3,col_names= cnames2)[6:71,1:5]

#Merging the weekly visits dataframe and financials dataframe to ease analytics
QA_df_Weekly_merged <- merge(QA_df_weekly_visits,QA_df_financials,by="Week",sort = FALSE)

#Creating names for column headers
cnames3 <-c("Week","Lbs. Sold")
#Creating dataframe for the fourth excel sheet containing only the required values, without the description in the beginning
QA_df_amountsold <- read_excel("Web Analytics Case Student Spreadsheet.xls",4,col_names= cnames3)[6:295,1:2]
#Converting the Week column from character to numeric in order to change it to Date
QA_df_amountsold$Week <- as.numeric(QA_df_amountsold$Week)
#Converting the Week column to Date
QA_df_amountsold$Week <- as.Date(QA_df_amountsold$Week,origin = "1899-12-30")

#Creating names for column headers
cnames4 <-c("Day","Visits")
#Creating dataframe for the fifth excel sheet containing only the required values, without the description in the beginning
QA_df_daily_visits <- read_excel("Web Analytics Case Student Spreadsheet.xls",5,col_names= cnames4)[6:467,1:2]

#The Demographics sheet will be split into six different dataframes, because of the way the excel is structured

#Creating names for column headers
cnames5 <- c(" ","All_Traffic_Source", "Visits")
#Create dataframe for "All Traffic Sources"
qa_demographics1 <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet = 6, col_names=cnames5)[8:11,2:3]

#Creating names for column headers
cnames6 <- c(" ","Top_Ten_Referring_Sites", "Visits")
#Create dataframe for "Top Ten Referring Sites"
qa_demographics2 <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet = 6, col_names=cnames6)[15:24,2:3]

#Creating names for column headers
cnames7 <- c(" ","Top_Ten_Search_Engine_Sources_of_Visits", "Visits")
#Create dataframe for "Top Ten Search Engine Sources of Visits"
qa_demographics3 <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet = 6, col_names=cnames7)[28:37,2:3]

#Creating names for column headers
cnames8 <- c(" ","Top_Ten_Geographic_Sources_by_Sub_Continent_Region", "Visits")
#Create dataframe for "Top Ten Geographic Sources by Sub Continent Region"
qa_demographics4 <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet = 6, col_names=cnames8)[41:50,2:3]

#Creating names for column headers
cnames9 <- c(" ","Top_Ten_Browser_Used", "Visits")
#Create dataframe for "Top Ten Browsers Used"
qa_demographics5 <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet = 6, col_names=cnames9)[55:64,2:3]

#Creating names for column headers
cnames10 <- c(" ","Top_Ten_Operating_System_Used", "Visits")
#Create dataframe for "Top Ten Operating Systems Used"
qa_demographics6 <- read_excel("Web Analytics Case Student Spreadsheet.xls", sheet = 6, col_names=cnames10)[69:78,2:3]

for (i in 1: nrow(QA_df_Weekly_merged)){
  QA_df_Weekly_merged$Index[i] <-i
}
for (i in 1:14){
  QA_df_Weekly_merged$Index[i]<- "Initial Period"
}
for (i in 15:35){
  QA_df_Weekly_merged$Index[i]<- "Pre-Promotion"
}
for (i in 36:52){
  QA_df_Weekly_merged$Index[i]<- "Promotion"
}
for (i in 53:66){
  QA_df_Weekly_merged$Index[i]<- "Post-Promotion"
}

QA_df_Weekly_merged$Visits<-as.numeric(QA_df_Weekly_merged$Visits)
QA_df_Weekly_merged$`Unique Visits`<-as.numeric(QA_df_Weekly_merged$`Unique Visits`)
QA_df_Weekly_merged$Pageviews<-as.numeric(QA_df_Weekly_merged$Pageviews)
QA_df_Weekly_merged$`Pages/Visit`<-as.numeric(QA_df_Weekly_merged$`Pages/Visit`)
QA_df_Weekly_merged$`Avg_Time`<-as.numeric(QA_df_Weekly_merged$`Avg_Time`)
QA_df_Weekly_merged$`Bounce Rate`<-as.numeric(QA_df_Weekly_merged$`Bounce Rate`)
QA_df_Weekly_merged$Revenue<-as.numeric(QA_df_Weekly_merged$Revenue)
QA_df_Weekly_merged$Profit<-as.numeric(QA_df_Weekly_merged$Profit)
QA_df_Weekly_merged$`Lbs. Sold`<-as.numeric(QA_df_Weekly_merged$`Lbs. Sold`)
QA_df_Weekly_merged$Inquiries<-as.numeric(QA_df_Weekly_merged$Inquiries)
QA_df_Weekly_merged$Perc_New_Visits<-as.numeric(QA_df_Weekly_merged$Perc_New_Visits)
QA_df_Weekly_merged$Perc_New_Visits<-round(QA_df_Weekly_merged$Perc_New_Visits, digits = 5)

qa_demographics1$Visits<-as.numeric(qa_demographics1$Visits)
qa_demographics2$Visits<-as.numeric(qa_demographics2$Visits)
qa_demographics3$Visits<-as.numeric(qa_demographics3$Visits)
qa_demographics4$Visits<-as.numeric(qa_demographics4$Visits)
qa_demographics5$Visits<-as.numeric(qa_demographics5$Visits)
qa_demographics6$Visits<-as.numeric(qa_demographics6$Visits)


#Two ways to create a mean profit graph for the 4 time periods
QA_df_Weekly_merged_mean <- QA_df_Weekly_merged[2:13] %>%
  group_by(Index) %>%
  summarise_all(funs(mean))

QA_df_Weekly_merged_mean$Index <- factor(QA_df_Weekly_merged_mean$Index, levels =c("Initial Period","Pre-Promotion","Promotion","Post-Promotion"))



#Group daily visit by weekday

for (i in 1: nrow(QA_df_daily_visits)){
  QA_df_daily_visits$weekday[i]<-"Weekday"
}

for (i in 1: nrow(QA_df_daily_visits)){
  if(grepl("Sunday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$weekday[i]<-"Weekend"}
  if(grepl("Saturday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$weekday[i]<-"Weekend"}
  
}

# daily visits by day

for (i in 1: nrow(QA_df_daily_visits)){
  QA_df_daily_visits$daytype[i]<-"Weekday"
}

for (i in 1: nrow(QA_df_daily_visits)){
  if(grepl("Sunday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$daytype[i]<-"Sunday"}
  if(grepl("Saturday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$daytype[i]<-"Saturday"}
  if(grepl("Monday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$daytype[i]<-"Monday"}
  if(grepl("Tuesday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$daytype[i]<-"Tuesday"}
  if(grepl("Thursday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$daytype[i]<-"Thursday"}
  if(grepl("Friday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$daytype[i]<-"Friday"}
  if(grepl("Wednesday",QA_df_daily_visits$Day[i])){QA_df_daily_visits$daytype[i]<-"Wednesday"}
}

df3$Index <-factor(df3$Index, levels =c("Initial Period","Pre-Promotion","Promotion","Post-Promotion"))

#Heatmap

Heatmap_weekly <-QA_df_Weekly_merged[2:12]
Correlation_weekly <- round(cor(Heatmap_weekly),2)

get_upper_triangle <- function(Correlation_weekly){
  Correlation_weekly[lower.tri(Correlation_weekly)]<- NA
  return(Correlation_weekly)
}

upper_triangle <- get_upper_triangle(Correlation_weekly)
melted_Correlation_weekly <- melt(upper_triangle, na.rm = TRUE)
# Heatmap
ggheatmap<- ggplot(data = melted_Correlation_weekly, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   face="bold", color="black", size=10, hjust = 1,),
        axis.text.y = element_text(vjust = 1,hjust = 1,face="bold", color="black", size=10, angle=0),)+
  coord_fixed()

ggheatmap +ggtitle("Heatmap: Weekly Visits and Financials")+ 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(color="black", size=15, face="bold.italic"),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.title = element_text(color="black", size=12, face="bold"),
    legend.text = element_text(color="black", size=10, face="bold",angle = 0),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#Barplot for Revenue and profit over visits

  df3 <- QA_df_Weekly_merged_mean[,c(1,2,9,10)]
df3
for (i in 1: nrow(df3)){ 
  df3$Profit_per_visit[i]<-df3$Profit[i] / df3$Visits[i]
  df3$Revenue_per_visit[i]<-df3$Revenue[i] / df3$Visits[i]
}

df4 <- melt(df3[,c(1,5,6)], id.vars='Index')
head(df4)

ggplot(df4, aes(x=Index, y=value, fill=variable)) +ggtitle("Mean Profit and Revenue per Visit")+
  geom_bar(stat='identity', position='dodge')+
  theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
        plot.title = element_text(color="black", size=15, face="bold.italic"),
        legend.title = element_text(color="black", size=12, face="bold"),
        legend.text = element_text(color="black", size=10, face="bold",angle = 0),
        panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                        linetype="solid", color="black"),
        panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                        colour = "lightgrey"),
        legend.position = c(0.8, 0.8)) +
  scale_y_continuous(name="Profits and Revenue in $", limits=c(0, 1000))+
  labs(fill="Legend")

#Scatterplot for Revenue over visits

scatter5<-ggplot(QA_df_Weekly_merged, aes(y = Revenue, x = Visits, color= Index,shape=Index))+geom_smooth(method = lm,se = FALSE)
scatter5 +geom_point(size=2.5)+
  ggtitle("Revenue by number of Visits") + labs(x="Visits", y="Revenue in $" ) +
  theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
        plot.title = element_text(color="black", size=15, face="bold.italic"),
        legend.title = element_text(color="black", size=12, face="bold"),
        legend.text = element_text(color="black", size=10, face="bold",angle = 0),
        panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                        linetype="solid", color="black"),
        panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                        colour = "lightgrey"),
        legend.position = c(0.85, 0.8),
        legend.key = element_rect(colour = "transparent", fill = "transparent"))+
  stat_ellipse()



#Visit by weekday                      
new1 <- ggplot(QA_df_daily_visits , aes(x=weekday, y=as.numeric(Visits), group=1)) 
  new1 + stat_summary(fun.y = mean, geom = "bar", fill = "lightblue", colour = "Black")  + labs(x = "Weekday", y = "Average Visits per day")+ ggtitle("Average daily visits split by Weekday/Weekend")+ scale_y_continuous(breaks=seq(0,200,20))+
    theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
          axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
          plot.title = element_text(color="black", size=15, face="bold.italic"),
          legend.title = element_text(color="black", size=12, face="bold"),
          legend.text = element_text(color="black", size=10, face="bold",angle = 0),
          panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                          linetype="solid", color="black"),
          panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                          colour = "black"), 
          panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                          colour = "black"),
          legend.position = c(0.85, 0.8),
          legend.key = element_rect(colour = "transparent", fill = "transparent"))
    

 

  
  #Scatterplot average time on site and revenue
  scatter<-ggplot(QA_df_Weekly_merged, aes(y = Revenue, x = Avg_Time, color= Index, shape=Index))
  scatter +geom_point(size=2)+ggtitle("Relationship between time on site and revenue") +labs(x="Time spent on site in seconds", y="Revenue in $" )+geom_smooth(method = lm,se = FALSE)+
  theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
        plot.title = element_text(color="black", size=15, face="bold.italic"),
        legend.title = element_text(color="black", size=12, face="bold"),
        legend.text = element_text(color="black", size=10, face="bold",angle = 0),
        panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                        linetype="solid", color="black"),
        panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                        colour = "lightgrey"),
        legend.position = c(0.9, 0.15),
        legend.key = element_rect(colour = "transparent", fill = "transparent"))+ 
    stat_ellipse()
  
  
  
  
  #Top Ten Referring sites
  dem2 <- ggplot(data = qa_demographics2,aes(Top_Ten_Referring_Sites, Visits))+
    geom_bar(stat = "identity",color='black',fill='lightgreen')+
    ggtitle("Top Ten Referring Sites")+
    scale_x_discrete(limits= qa_demographics2$Top_Ten_Referring_Sites)
  dem2+theme(axis.text.x = element_text(face="bold", color="black", size=10, angle = 50, vjust = 1, hjust = 1),
             axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
             axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
             plot.title = element_text(color="black", size=15, face="bold.italic"),
             legend.title = element_text(color="black", size=12, face="bold"),
             legend.text = element_text(color="black", size=10, face="bold",angle = 0),
             panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                             linetype="solid", color="black"),
             panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"), 
             panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"),
             legend.position = c(0.9, 0.8),
             legend.key = element_rect(colour = "transparent", fill = "transparent"),
             axis.title.x = element_blank())
  
  #Top Ten Search Engines
  dem3 <-ggplot(data = qa_demographics3,aes(Top_Ten_Search_Engine_Sources_of_Visits,Visits))+
    geom_bar(stat = "identity",fill='lightblue')+ggtitle("Top Ten Search Engines")+
     scale_x_discrete(limits= qa_demographics3$Top_Ten_Search_Engine_Sources_of_Visits)
  dem3+theme(axis.text.x = element_text(face="bold", color="black", size=10, angle = 0, vjust = 1, hjust = 1),
             axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
             axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
             plot.title = element_text(color="black", size=15, face="bold.italic"),
             legend.title = element_text(color="black", size=12, face="bold"),
             legend.text = element_text(color="black", size=10, face="bold",angle = 0),
             panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                             linetype="solid", color="black"),
             panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"), 
             panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"),
             legend.position = c(0.9, 0.8),
             legend.key = element_rect(colour = "transparent", fill = "transparent"),
             axis.title.x = element_blank())
  
  #Top Ten Geographic sources
  dem4 <- ggplot(data = qa_demographics4,aes(Top_Ten_Geographic_Sources_by_Sub_Continent_Region,Visits))+
    geom_bar(stat = "identity",color='lightslateblue',fill='lightslateblue')+ggtitle("Top Ten Geographic Sources")+
    scale_x_discrete(limits= qa_demographics4$Top_Ten_Geographic_Sources_by_Sub_Continent_Region)
  dem4+theme(axis.text.x = element_text(face="bold", color="black", size=10, angle = 25, vjust = 1, hjust = 1),
             axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
             axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
             plot.title = element_text(color="black", size=15, face="bold.italic"),
             legend.title = element_text(color="black", size=12, face="bold"),
             legend.text = element_text(color="black", size=10, face="bold",angle = 0),
             panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                             linetype="solid", color="black"),
             panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"), 
             panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"),
             legend.position = c(0.9, 0.8),
             legend.key = element_rect(colour = "transparent", fill = "transparent"),
             axis.title.x = element_blank())
  
  #Visits per week
  
  VisitsBar<- ggplot(data = QA_df_Weekly_merged,aes(x=factor(Week, levels=unique(Week)),y=Visits,fill=Index))+
    geom_bar(stat = "identity",)+ggtitle("Visits per Week")
  VisitsBar+theme(axis.text.x = element_text(face="bold", color="black", size=10, angle = 45, vjust = 1, hjust = 1),
                  axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
                  axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
                  plot.title = element_text(color="black", size=15, face="bold.italic"),
                  legend.title = element_text(color="black", size=12, face="bold"),
                  legend.text = element_text(color="black", size=10, face="bold",angle = 0),
                  panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                                  linetype="solid", color="black"),
                  panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                                  colour = "lightgrey"), 
                  panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                                  colour = "lightgrey"),
                  legend.position = c(0.9, 0.8),
                  legend.key = element_rect(colour = "transparent", fill = "transparent"),
                  axis.title.x = element_blank()) + 
    scale_x_discrete(breaks = QA_df_Weekly_merged$Week[c(T,F,F,F)])
   
  
  #Top Ten Search Engines
 
  qa_demographics1$All_Traffic_Source <- factor(qa_demographics1$All_Traffic_Source, levels =c("Referring Sites","Search Engines","Direct Traffic","Other"))
  
  dem1 <-ggplot(data = qa_demographics1,aes(qa_demographics1$All_Traffic_Source,Visits))+
    geom_bar(stat = "identity",fill='lightblue')+ggtitle("Traffic Sources")+
    scale_x_discrete(limits= qa_demographics1$qa_demographics1$All_Traffic_Source)
  dem1+theme(axis.text.x = element_text(face="bold", color="black", size=10, angle = 0, vjust = 1, hjust = 1),
             axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
             axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
             plot.title = element_text(color="black", size=15, face="bold.italic"),
             legend.title = element_text(color="black", size=12, face="bold"),
             legend.text = element_text(color="black", size=10, face="bold",angle = 0),
             panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                             linetype="solid", color="black"),
             panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"), 
             panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                             colour = "lightgrey"),
             legend.position = c(0.9, 0.8),
             legend.key = element_rect(colour = "transparent", fill = "transparent"),
             axis.title.x = element_blank())
  
  
  
  #Scatter plot Visits & profits
  scatter2<-ggplot(QA_df_Weekly_merged, aes(y = Profit, x = Visits, color= Index, shape=Index))
  scatter2 +geom_point(size=2)+ggtitle("Influence of number of Visits on Profit") +labs(x="Visits", y="Profit in $" )+geom_smooth(method = lm,se = FALSE)+
    theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
          axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
          plot.title = element_text(color="black", size=15, face="bold.italic"),
          legend.title = element_text(color="black", size=12, face="bold"),
          legend.text = element_text(color="black", size=10, face="bold",angle = 0),
          panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                          linetype="solid", color="black"),
          panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                          colour = "lightgrey"), 
          panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                          colour = "lightgrey"),
          legend.position = c(0.9, 0.8),
          legend.key = element_rect(colour = "transparent", fill = "transparent")) +
    stat_ellipse() + scale_y_continuous(labels = scales::comma)
  
  #scatter revenue over new visits
  scatter3<-ggplot(QA_df_Weekly_merged, aes(y = Revenue, x = Perc_New_Visits, color= Index, shape=Index))
  scatter3 +geom_point(size=2)+ggtitle("Influence of new Visits on Revenue") +labs(y="Revenue in $", x="Percentage of New Visits" )+
    theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
          axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
          plot.title = element_text(color="black", size=15, face="bold.italic"),
          legend.title = element_text(color="black", size=12, face="bold"),
          legend.text = element_text(color="black", size=10, face="bold",angle = 0),
          panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                          linetype="solid", color="black"),
          panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                          colour = "lightgrey"), 
          panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                          colour = "lightgrey"),
          legend.position = c(0.9, 0.85),
          legend.key = element_rect(colour = "transparent", fill = "transparent")) +
    stat_ellipse()
    
    #Separate regressions of Inquries on Visits for each period
  scatter9<-ggplot(QA_df_Weekly_merged, aes(y = Inquiries, x = Visits, color= Index, shape=Index))
  scatter9 +geom_point(size=2)+ggtitle("Influence of Number of Visits on the amount of Inquiries generated") +labs(y="Inquiries", x="Visits" )+geom_smooth(method = lm,se = FALSE)+
    theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
          axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold",angle = 90),
          plot.title = element_text(color="black", size=15, face="bold.italic"),
          legend.title = element_text(color="black", size=12, face="bold"),
          legend.text = element_text(color="black", size=10, face="bold",angle = 0),
          panel.background = element_rect(fill= "white", colour="white", size=0.5, 
                                          linetype="solid", color="black"),
          panel.grid.major = element_line(size = 0.01, linetype = 'solid',
                                          colour = "lightgrey"), 
          panel.grid.minor = element_line(size = 0.01, linetype = 'solid',
                                          colour = "lightgrey"),
          legend.position = c(0.85, 0.85),
          legend.key = element_rect(colour = "transparent", fill = "transparent")) +
    stat_ellipse()
  
  
  

  
  