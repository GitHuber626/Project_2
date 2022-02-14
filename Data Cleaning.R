#########################################
#Data cleaning on the Supermarket data set

#To read the dataset needed
S=read.csv('Supermarket.csv',header=FALSE)

#To separate S into related columns
S = separate(S, V1, c("ID", "Year_Birth","Education",
                                          "Marital_Status","Income","Kidhome"
                                          ,"Teenhome","Dt_Customer","Recency"
                                          ,"MntWines","MntFruits","MntMeatProducts"
                                          ,"MntFishProducts","MntSweetProducts",
                                          "MntGoldProds","NumDealsPurchases",
                                          "NumWebPurchases","NumCatalogPurchases",
                                          "NumStorePurchases","NumWebVisitsMonth",
                                          "AcceptedCmp3","AcceptedCmp4","AcceptedCmp5"
                                          ,"AcceptedCmp1","AcceptedCmp2","Complain",
                                          "Z_CostContact","Z_Revenue","Response"), sep = "[\t]")
S=S[-1,]
#Get rid of usefulness columns
S=subset(S, select = -c(Z_CostContact,Z_Revenue))
#Get rid of rows that contain NaN/empty cells
sum( is.na(S) )
sum(S=='')
which( rowSums( S=='' ) != 0 )
S=S[-c(11,28,44,49,59,72,91,92,93,129,134,313,320,1380,
                           1383,1384,1387,2060,2062,2079,2080,2082,2085,2229),]
rownames(S)<-1:2216
str(S)
#Convert data type
S[,'ID']=as.numeric(S[,'ID'])
S[,'Income']=as.numeric(S[,'Income'])
S[,'Year_Birth']=as.numeric(S[,'Year_Birth'])
S[,'Recency']=as.numeric(S[,'Recency'])
S[,'MntWines']=as.numeric(S[,'MntWines'])
S[,'MntFruits']=as.numeric(S[,'MntFruits'])
S[,'MntMeatProducts']=as.numeric(S[,'MntMeatProducts'])
S[,'MntFishProducts']=as.numeric(S[,'MntFishProducts'])
S[,'MntSweetProducts']=as.numeric(S[,'MntSweetProducts'])
S[,'MntGoldProds']=as.numeric(S[,'MntGoldProds'])
S[,'NumDealsPurchases']=as.numeric(S[,'NumDealsPurchases'])
S[,'NumWebPurchases']=as.numeric(S[,'NumWebPurchases'])
S[,'NumCatalogPurchases']=as.numeric(S[,'NumCatalogPurchases'])
S[,'NumStorePurchases']=as.numeric(S[,'NumStorePurchases'])
S[,'NumWebVisitsMonth']=as.numeric(S[,'NumWebVisitsMonth'])
S[,'Kidhome']=as.numeric(S[,'Kidhome'])
S[,'Teenhome']=as.numeric(S[,'Teenhome'])
S[,'AcceptedCmp3']=as.factor(S[,'AcceptedCmp3'])
S[,'AcceptedCmp4']=as.factor(S[,'AcceptedCmp4'])
S[,'AcceptedCmp5']=as.factor(S[,'AcceptedCmp5'])
S[,'AcceptedCmp1']=as.factor(S[,'AcceptedCmp1'])
S[,'AcceptedCmp2']=as.factor(S[,'AcceptedCmp2'])
S[,'Complain']=as.factor(S[,'Complain'])
S[,'Response']=as.factor(S[,'Response'])
str(S)
S[,'Dt_Customer']=as.Date(S[,'Dt_Customer'],format="%d-%m-%Y")

#########################################

#########################################
#Data cleaning on the Airplane data set

#To read the dataset needed
A=readr::read_csv('Airplane.csv')

#Get rid of usefulness columns
A=subset(A, select = -1)
str(A)
#Get rid of rows that contain NaN/empty cells
sum( is.na(A) )
sum(A=='')
A=na.omit(A)
##Convert data type
unique(A$`Class`)
`Inflight wifi service`=as.factor(A$`Inflight wifi service`)
A$`Inflight wifi service`=`Inflight wifi service`
`Departure/Arrival time convenient`=as.factor(A$`Departure/Arrival time convenient`)
A$`Departure/Arrival time convenient`=`Departure/Arrival time convenient`
`Ease of Online booking`=as.factor(A$`Ease of Online booking`)
A$`Ease of Online booking`=`Ease of Online booking`
`Gate location`=as.factor(A$`Gate location`)
A$`Gate location`=`Gate location`
`Food and drink`=as.factor(A$`Food and drink`)
A$`Food and drink`=`Food and drink`
`Online boarding`=as.factor(A$`Online boarding`)
A$`Online boarding`=`Online boarding`
`Seat comfort`=as.factor(A$`Seat comfort`)
A$`Seat comfort`=`Seat comfort`
`Inflight entertainment`=as.factor(A$`Inflight entertainment`)
A$`Inflight entertainment`=`Inflight entertainment`
`On-board service`=as.factor(A$`On-board service`)
A$`On-board service`=`On-board service`
`Leg room service`=as.factor(A$`Leg room service`)
A$`Leg room service`=`Leg room service`
`Baggage handling`=as.factor(A$`Baggage handling`)
A$`Baggage handling`=`Baggage handling`
`Checkin service`=as.factor(A$`Checkin service`)
A$`Checkin service`=`Checkin service`
`Inflight service`=as.factor(A$`Inflight service`)
A$`Inflight service`=`Inflight service`
`Cleanliness`=as.factor(A$`Cleanliness`)
A$`Cleanliness`=`Cleanliness`
str(A)

#########################################

#########################################
#Data cleaning on the Starbucks data set

#To read the dataset needed
SB=readr::read_csv('Starbucks.csv')
SB=na.omit(SB)

#Get rid of usefulness columns
SB=subset(SB, select = -1)

#Change column names
colnames(SB)=c('Gender','Age','Status','Annual Income','Visit Frenquency'
                      ,'Visit Method','Time Spent','Distance','Member','Purchase'
                      ,'Money Spent','Quality Rating','Price Range Rating',
                      'Sales and Promotion Importance','Ambience Rating','Wifi 
                      Quality Rating','Service Rating','Business or Friends 
                      Meeting Likelyhood','Promotion Means','Continuity')

#Convert Column Types and Edit Column Values
SB$Age=(case_when(
  SB$Age=='Below 20'~'<20',
  SB$Age=='From 20 to 29'~'20-29',
  SB$Age=='From 30 to 39'~'30-39',
  SB$Age=='40 and above'~'>=40',
))

SB$`Annual Income`=(case_when(
  SB$`Annual Income`=='Less than RM25,000'~'<25000',
  SB$`Annual Income`=='RM25,000 - RM50,000'~'25000-50000',
  SB$`Annual Income`=='RM50,000 - RM100,000'~'50000-100000',
  SB$`Annual Income`=='RM100,000 - RM150,000'~'100000-150000',
  SB$`Annual Income`=='More than RM150,000'~'>150000',
))

SB$`Money Spent`=(case_when(
  SB$`Money Spent`=='Zero'~'0',
  SB$`Money Spent`=='Less than RM20'~'<4.77',
  SB$`Money Spent`=='Around RM20 - RM40'~'4.77-9.55',
  SB$`Money Spent`=='More than RM40'~'>9.55',
))

SB$Member=as.factor(SB$Member)

SB=SB%>%mutate_at(.vars=c(12:18,20), .funs = as.factor)
str(SB)
#########################################

#########################################
#Data Analysis and Plot Drawing

#Draw Correlation Plot(Plot#1)
corrplot(cor(S[,-c(1,3,4,8,21:32)]), method = 'square', order = 'FPC', type = 'lower', diag = FALSE)

#Draw bar plots to show relations between categorical independent variables
#and dependent variables
unique(S$Marital_Status)
S=S[!(S$Marital_Status%in%"YOLO"),]
S=S[!(S$Marital_Status%in%"Absurd"),]
S$Marital_Status=(case_when(
  S$Marital_Status=='Alone'~'Single',
  TRUE~as.character(S$Marital_Status)
))
S$Education=(case_when(
  S$Education=='Graduation'~'Bachelor',
  S$Education=='Basic'~'Primary',
  S$Education=='2n Cycle'~'2nd Cycle',
  TRUE~as.character(S$Education)
))

#Extract year that people became customer data from the table
S=S%>%mutate(Dt_Customer_Year=format(S$Dt_Customer,format="%Y"))
S$Dt_Customer_Year=as.numeric(S$Dt_Customer_Year)
str(S)

#Use pivot longer to transform the table
S_PL=pivot_longer(S,col=10:15,names_to = 'MntProducts',values_to = 'Dollars',
                  values_drop_na = FALSE)
#Draw bar plot to show impact of variables on purchase spending
S_PL%>%group_by(Education,MntProducts)%>%summarise(mean_prod=mean(Dollars))%>%ggplot(aes(x=MntProducts,Education))+geom_bar(aes(y=mean_prod,fill=Education),stat='identity',position='dodge')+labs(x='Product',y='Dollar',title='Education Impact on Purchase')

S_PL%>%group_by(Marital_Status,MntProducts)%>%summarise(mean_prod=mean(Dollars))%>%
  ggplot(aes(x=MntProducts,Marital_Status))+geom_bar(aes(y=mean_prod,fill=Marital_Status),stat='identity',position
                                                ='dodge')+labs(x='Product',y='Dollar',title='Marital Status Impact on Purchase')



S_PL%>%group_by(Dt_Customer_Year,MntProducts)%>%summarise(mean_prod=mean(Dollars))%>%
  ggplot(aes(x=MntProducts,Dt_Customer_Year))+geom_bar(aes(y=mean_prod,fill=as.factor(Dt_Customer_Year)),stat='identity',position
                                                     ='dodge')+labs(x='Product',y='Dollar',title='Customer Enrollment Date Impact on Purchase')


#Use pivot longer to transform the table
S_PL2=pivot_longer(S,col=16:20,names_to = 'NumPhurchases',values_to = 'Number',
                  values_drop_na = FALSE)

#Draw bar plot to show impact of variables on purchase frequency
S_PL2%>%group_by(Education,NumPhurchases)%>%summarise(mean_freq=mean(Number))%>%
  ggplot(aes(x=NumPhurchases,Education))+geom_bar(aes(y=mean_freq,fill=Education),stat='identity',position
                                                ='dodge')+labs(
                                                  x='Purchase Means',y=
                                                    'Avg Frequency',title=
                                                    'Education Impact on Purchase Frequency'
                                                )

S_PL2%>%group_by(Marital_Status,NumPhurchases)%>%summarise(mean_freq=mean(Number))%>%
  ggplot(aes(x=NumPhurchases,Marital_Status))+geom_bar(aes(y=mean_freq,fill=Marital_Status),stat='identity',position
                                                     ='dodge')+labs(x='Purchase Means',y='Avg Frequency',title='Marital Status Impact on Purchase Frequency')


S_PL2%>%group_by(Dt_Customer_Year,NumPhurchases)%>%summarise(mean_freq=mean(Number))%>%
  ggplot(aes(x=NumPhurchases,Dt_Customer_Year))+geom_bar(aes(y=mean_freq,fill=as.factor(Dt_Customer_Year)),stat='identity',position
                                                       ='dodge')+labs(x='Purchases Means',y='Avg Frequency',title='Customer Enrollment Date Impact on Purchase Frequency')



#To better study the possible factors that may have an impact on the purchase products and purchase frequency,
#we add two other factors into consideration: CPI increase% from 2012 to 2014 and Average Wage increase%
#from 2012 to 2014
CPI_Increase=c(2.1,1.5,1.6) #source:https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-
Average_Wage_Increase=c(3.12,1.28,3.55) #source:https://www.ssa.gov/oact/cola/awidevelop.html
Date=as.integer(c(2012,2013,2014))
CPI_AWI=data.frame(
   Dt_Customer_Year=Date,
   CPI_Increase=CPI_Increase,
   AWI_Increase=Average_Wage_Increase
)
S_PL3=join(S_PL2,CPI_AWI,by='Dt_Customer_Year',type='inner')

S_PL3%>%group_by(Dt_Customer_Year,NumPhurchases)%>%ggplot(aes(x=Dt_Customer_Year))+geom_line(aes(y=CPI_Increase,colour='CPI%'),size=2)+geom_line(aes(y=AWI_Increase,colour='AWI%'),size=2)+scale_color_manual("",breaks=c('CPI%','AWI%'),values=c('red','blue'))+scale_x_continuous(breaks=c(2012,2013,2014))
  
#Analysis on Logistic Regression over the three tables
#Focus on the impact of age and income on customer satisfactory level

S$Age1=as.numeric(2022-S$Year_Birth)
S$Age=case_when(
  (S$Age1-10)<20~'<20',
  (S$Age1-10)<=29&(S$Age1-5)>=20~'20-29',
  (S$Age1-10)<=39&(S$Age1-5)>=30~'30-39',
  (S$Age1-10)>=40~'>=40',
  )
SB$Age=as.factor(SB$Age)
SB$Age=factor(SB$Age,levels=c("<20","20-29","30-39",">=40"))
S$Age=as.factor(S$Age)
S$Age=factor(S$Age,levels=c("<20","20-29","30-39",">=40"))

A$Age1=A$Age
A$Age1=as.numeric(A$Age1)

A$Age=case_when(
  A$Age1<20~'<20',
  A$Age1<=29&A$Age1>=20~'20-29',
  A$Age1<=39&A$Age1>=30~'30-39',
  A$Age1>=40~'>=40',
)
A$Age=as.factor(A$Age)
A$Age=factor(A$Age,levels=c("<20","20-29","30-39",">=40"))

S$Annual_Income=case_when(
  S$Income<50000~'Low',
  S$Income<=85000&S$Income>=50000~'Middle',
  S$Income>85000~'High',
)

SB$`Annual_Income`=case_when(
  SB$`Annual Income`=="<25000"~'Low',
  SB$`Annual Income`=="25000-50000"~'Low',
  SB$`Annual Income`=="50000-100000"~'Middle',
  SB$`Annual Income`=="100000-150000"~'High',
  SB$`Annual Income`==">150000"~'High'
)

A$Annual_Income=case_when(
  A$`Class`=="Eco"~'Low',
  A$`Class`=="Eco Plus"~'Middle',
  A$`Class`=="Business"~'High'
)

S$Annual_Income=as.factor(S$Annual_Income)
S$Annual_Income=factor(S$Annual_Income,levels=c("Low","Middle","High"))
SB$Annual_Income=as.factor(SB$Annual_Income)
SB$Annual_Income=factor(SB$Annual_Income,levels=c("Low","Middle","High"))
A$Annual_Income=as.factor(A$Annual_Income)
A$Annual_Income=factor(A$Annual_Income,levels=c("Low","Middle","High"))

S$Satisfaction=case_when(
  S$Complain==0~'Satisfied',
  S$Complain==1~'NotSatisfied'
)

SB=SB%>%mutate_at(.vars=c(12,13,15,16,17), .funs = as.integer)
SB$Avg_Rating=rowMeans(SB[,c(12,13,15,16,17)],na.rm=TRUE)

SB$Satisfaction=case_when(
  SB$Avg_Rating>2.5~'Satisfied',
  SB$Avg_Rating<=2.5~'NotSatisfied'
)

A$Satisfaction=case_when(
  A$satisfaction=='satisfied'~'Satisfied',
  A$satisfaction=='neutral or dissatisfied'~'NotSatisfied'
)

S$Satisfaction=as.factor(S$Satisfaction)
S$Satisfaction=factor(S$Satisfaction,levels=c("Satisfied","NotSatisfied"))
SB$Satisfaction=as.factor(SB$Satisfaction)
SB$Satisfaction=factor(SB$Satisfaction,levels=c("Satisfied","NotSatisfied"))
A$Satisfaction=as.factor(A$Satisfaction)
A$Satisfaction=factor(A$Satisfaction,levels=c("Satisfied","NotSatisfied"))

S_Log=S[,c(30,31,32)]
SB_Log=SB[,c(2,21,23)]
A_Log=A[,c(4,26,27)]
str(A_Log)
S_Log$Satisfaction=as.factor(S_Log$Satisfaction)
S_Log$Satisfaction=factor(S_Log$Satisfaction,levels=c("Satisfied","NotSatisfied"))
SB_Log$Age=as.factor(SB_Log$Age)
SB_Log$Age=factor(SB_Log$Age,levels=c("<20","20-29","30-39",">=40"))

#Take a look at the independent variables and their numbers in comparison to the dependent variable
#to see if they are testable
xtabs(~Satisfaction+Age,data=S_Log)
xtabs(~Satisfaction+Annual_Income,data=S_Log)

#Logistic Regression
Sa<- glm(Satisfaction~.,data=S_Log,family='binomial')
summary(glm(Satisfaction~Age1,data=S,family = 'binomial'))
summary(glm(Satisfaction~Income,data=S,family = 'binomial'))
summary(Sa)
plot(Sa)

scatter.smooth(Sa$fit,
               residuals(Sa, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Satisfaction Data")
abline(h = 0, lty = 2)

influencePlot(Sa)

#Try dropping the Age
Sa_Income<-glm(Satisfaction~Annual_Income,data=S_Log,family='binomial')

reduced.deviance = Sa_Income$deviance #Comparing the deviance of the reduced
reduced.df = Sa_Income$df.residual    #model (the one without Age) to...

full.deviance = Sa$deviance #...the deviance of the full model (the
full.df = Sa$df.residual    #one with Age).

pchisq(reduced.deviance - full.deviance,
       reduced.df - full.df,
       lower.tail = FALSE)
pchisq(full.deviance, full.df, lower.tail = FALSE)
1 - full.deviance/Sa$null.deviance

#Try predicting the other two tables
Sa_A=A_Log[,c(1,2)]
Sa_SB=SB_Log[,c(1,2)]
predict(Sa,Sa_A,type='response')

#Multiple Linear Regression and numeric relationship plots
S_PL%>%group_by(MntProducts,Year_Birth)%>%select(c(2,5,6,7,23,24))%>%ggplot()+geom_point(
  aes(x=Year_Birth,y=Dollars,colour=MntProducts))+ggtitle('Amount Spent on Different Products')+scale_x_continuous(limits=c(1945,1996))
  
S_PL%>%group_by(MntProducts,Income)%>%select(c(2,5,6,7,23,24))%>%ggplot()+geom_point(
  aes(x=Income,y=Dollars,colour=MntProducts))+ggtitle('Amount Spent on Different Products')+scale_x_continuous(limits=c(2000,120000)) 
  
S_PL2%>%group_by(NumPhurchases,Year_Birth)%>%select(c(2,5,6,7,24,25))%>%summarise(meanfreq=mean(Number))%>%ggplot()+geom_line(
  aes(x=Year_Birth,y=meanfreq,colour=NumPhurchases))+ggtitle('Purchase Frequency on Different Products')+scale_x_continuous(limits=c(1945,1996))

S_PL2%>%group_by(NumPhurchases,Income)%>%select(c(2,5,6,7,24,25))%>%summarise(meanfreq=mean(Number))%>%ggplot(aes(x=Income))+geom_density(aes(y=meanfreq,colour=NumPhurchases),stat='identity')+ggtitle('Purchase Frequency on Different Products')+scale_x_continuous(limits=c(2000,120000))







