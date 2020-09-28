#Project Statement
#A nationwide survey of hospital costs conducted by the US Agency for 
#Healthcare consists of hospital records of inpatient samples. 
#The given data is restricted to the city of Wisconsin and relates to 
#patients in the age group 0-17 years. The agency wants to analyze the data to 
#research on healthcare costs and their utilization.

#5 variables we have
# AGE = continuous variable
# FEMALE = discrete, categorical variable
# LOS = continuous variable
# RACE = discrete, continuous variable
# TOTCHRG = total discharge cost
# Aprdg = categorical value of severity of illness


#1. To record the patient statistics, the agency wants to find the 
#age category of people who frequent the hospital and has the maximum expenditure.

#Find frequencies based on AGE groups
hospitaldata<-read.csv(file.choose(),header = T)
names(hospitaldata) #verify the columns
str(hospitaldata)
head(hospitaldata) #check top few rows to get the feel of the data
levels(as.factor(hospitaldata$AGE))
table(as.factor(hospitaldata$AGE),hospitaldata$LOS) #check the totals for each age as category against the LOS
#above command gives out long and wide output to, but gives and idea that age=0 has the maximum LOS entries between 0-4 days of stay
#lets try summary of AGE variable
summary(as.factor(hospitaldata$AGE)) #this gives us the number of records grouped by AGE
#as a confirmation, AGE=0 has 307 entries - clear that AGE=0 are most frequent visitors to the Hospital

#now to find the Maximum cost by AGE, we need the SUM of Costsgrouped by AGE
#we need aggregation
aggregate(hospitaldata$TOTCHG,list(hospitaldata$AGE),FUN=sum)
#or, to have proper headers names in output
cost_aggregate<-aggregate(hospitaldata$TOTCHG~hospitaldata$AGE,FUN = sum)
#looking at the output, AGE=0 seems to be the maximum cost
#using the function
max(cost_aggregate) #678118

#for the management, visualisation is much useful, lets creare charts
#here we need AGE vs Visits LOS
hist(hospitaldata$AGE,breaks = nlevels(as.factor(hospitaldata$AGE)),xlab = "Age",ylab = "Total Records",col = "green",freq = T,density = 100,border = 4)
table(hospitaldata$LOS,hospitaldata$AGE)
#so the histogram chart, we see AGE 0 has maximum records
barplot(table(hospitaldata$LOS,hospitaldata$AGE),xlab = "Age",ylab = "Total rows, Days Stayed stacked",col = "green",density = 100,border = 4)
#barplot with AGE as x, Records and LOS stacked shows the same explanation, ex: LOS=2 has 170 such records for AGE=0



#2. In order of severity of the diagnosis and treatments and to find out 
#the expensive treatments, the agency wants to find the 
#diagnosis-related group that has maximum hospitalization and expenditure.
head(hospitaldata)
#this is smiliar problem as 1, here we need the cost based on diagnosis group
table(as.factor(hospitaldata$APRDRG),hospitaldata$TOTCHG)
which.max(summary(as.factor(hospitaldata$APRDRG))) #roughly, code 640 occurs 267 times in the sample
cost_diagnosis_agg<-aggregate(TOTCHG~APRDRG,FUN=sum,data=hospitaldata)
cost_diagnosis_agg
which.max(cost_diagnosis_agg$TOTCHG) #to get the index of highest TOTCHRG value
cost_diagnosis_agg[which.max(cost_diagnosis_agg$TOTCHG),] #fetch the row from aggregate using above index
#as we see, code 640 has the maximum charges
#lets draw a plot
hist(hospitaldata$APRDRG,breaks = nlevels(as.factor(hospitaldata$APRDRG)),xlab = "Diagnosis Code",ylab = "Sum of expenses",col = "green",freq = T,density = 100,border = 4)
#very clear from this graph

#also barplot for exact value of diag codes
#barplot(table(hospitaldata$TOTCHG,as.factor(hospitaldata$APRDRG)),xlab = "Diagnosis code",ylab = "Total Expenses",col = "green",density = 100,border = 4)
nlevels(as.factor(hospitaldata$APRDRG))
#barplot won't be practical, because we have 63 different codes of diagnosis. RStudio eats up lot of ram while drawing this
#So with Histogram and aggregate methods we are able to conclude the answer

#3. To make sure that there is no malpractice, 
#the agency needs to analyze if the race of the patient 
#is related to the hospitalization costs.

#we have a categorical variable- RACE
# and continuous variable - Cost
#what we have learnt - ANOVA is used to find relation between categorical + continuous variables
#Data cleaning/wrangling
#lets find if we have any NA values
#Here, we will be performing the Hypothesis test
#H0 - RACE has NO EFFECT on Costs
#H1 - RACE impacts Total hospital costs
#In ANOVA -  we have F-statistic
# Significance level aplha is set to 0.05
summary(hospitaldata$RACE)
#or
anyNA(hospitaldata$RACE) #we have records where RACE is NA
anyNA(hospitaldata$TOTCHG)#no NA here
#do we replace the NA for RACE? Maybe not, we can't find 
#any value for missing RACE since its a categorical data
#we only can ignore those or remove such records - btw its only 1, so won't make much difference
hospitaldata_cleaned<-na.omit(hospitaldata)
hospitaldata_cleaned
anyNA(hospitaldata_cleaned$RACE) # does not have that NA row
#lets apply anova test on these two variables
model_race_vost_aov<-aov(hospitaldata_cleaned$TOTCHG~hospitaldata_cleaned$RACE) 
#here, the dependent variable comes before the independent variable
model_race_vost_aov
#this test returns sum of squares for race, residuals
#using the summary of the model, we can get to know the F-statistic value
summary(model_race_vost_aov)
#F-statistic is 0.164, and P-value (probability) is 0.686 > 0.05 (significance level)
#hence we CANNOT REJECT the NULL Hypothesis
#means RACE HAS NO IMPACT ON COSTS

#4. To properly utilize the costs, the agency has to 
#analyze the severity of the hospital costs by age and gender 
#for the proper allocation of resources.
#Now, AGE and GENDER both are categorical variables
#COst is continuous
#We need to find the effect of AGE and GENDER on Costs 
#or relation between AGE+GENDER and Cost
#Hospital wants to understand, whether The Cost Increases/decreases with AGE or GENDER
#so we need to find the linear relation among these
#Use Linear RRegression model
linear_model<-lm(hospitaldata$TOTCHG~hospitaldata$AGE+hospitaldata$FEMALE)
linear_model
unique(hospitaldata$FEMALE)
str(hospitaldata)
#FEMALE is Integer type, convert that to Factor
hospitaldata$FEMALE<-as.factor(hospitaldata$FEMALE)
linear_fit<-lm(hospitaldata$TOTCHG~hospitaldata$AGE+hospitaldata$FEMALE)
linear_fit
summary(linear_fit)
#We see, p-values for AGE is much lower than significance level, 
#but p-value for Gender is much closer to 0.05
#means - AGE has bigger impact on costs, although GENDER also is significant for the model
#coefficeint of AGE is higher, means cost is positively increasing with age
summary(hospitaldata$FEMALE)
#We have almost similar count for FEMALE and MALE
#from the coefficient values, FEMALE=1 has very negative value for t value 
#that means the costs for females decreases or lesser than those for males.

#lets plot this
install.packages('tidyverse')
library(ggplot2)
ggplot(hospitaldata,aes(y=TOTCHG,x=AGE,color=factor(FEMALE)))+geom_point()+stat_smooth(method="lm",se=FALSE)
#The plot clearly defines our regression model, female have lesser costs than males,
#and More the age, more the costs



#5. Since the length of stay is the crucial factor for inpatients, 
#the agency wants to find if the length of stay can be 
#predicted from age, gender, and race.

#this is simlar problem as 3, here the predictor variables are AGE,GENDER,RACE
#Predicted variable is Length of stay
#lets use linear regression to find the relation
hospitaldata$RACE<-as.factor(hospitaldata$RACE)
hospitaldata<-na.omit(hospitaldata)
fit_los<-lm(data = hospitaldata,formula = LOS~AGE+FEMALE+RACE)
fit_los
summary(fit_los)
#None of the predictor variables show significance to the model, 
#All the p-values are high, thus, Accepting the Null Hypothesis
# concluding that Length of stay cannot be predicted by AGE, RACE, or GENDER


#6. To perform a complete analysis, the agency wants to 
#find the variable that mainly affects hospital costs.
#same as 5, with more independent variables to predict the cost
fit_all_los<-lm(data = hospitaldata,formula = TOTCHG~AGE+FEMALE+RACE+APRDRG+LOS)
fit_all_los
summary(fit_all_los)
#AGE, LOS, APRDRG affect the costs for hospitals, none other does
#here, LOS is the continuous variable, we can compare this with costs as
#with each increment in days, the Cost increases by 742.97 units
