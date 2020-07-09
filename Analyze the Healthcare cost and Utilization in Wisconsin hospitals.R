#1. To record the patient statistics, the agency wants to find the age category of people who frequent the hospital
#   and has the maximum expenditure.

hospital_cost <- read.csv("HospitalCosts.csv") 

View(hospital_cost)
table(hospital_cost$AGE);
max(summary(as.factor(hospital_cost$AGE)))
#for visual plotting of the above result
hist(hospital_cost$AGE,xlab = "Age", ylab = 'No. of Visits',col = "green",main = "Age wise Frequency of Paitents")


#for expendiature analysis 

age <- aggregate(TOTCHG ~ AGE, data = hospital_cost, sum)
age
max(age)

#-----------------------------------------------------------------------------------------------------------------------


#In order of severity of the diagnosis and treatments and to find out the
#expensive treatments, the agency wants to find the diagnosis related group
#that has maximum hospitalization and expenditure.


t <- table(hospital_cost$APRDRG)
d <- as.data.frame(t)
names(d)[1] = 'Diagnosis Group'
d
which.max(table(hospital_cost$APRDRG))
res <- aggregate(TOTCHG ~ APRDRG, data = hospital_cost, sum)
res
which.max(res$TOTCHG)
res[which.max(res$TOTCHG),]







#-----------------------------------------------------------------------------------------------------------------------


#To make sure that there is no malpractice, the agency needs to analyze if
#the race of the patient is related to the hospitalization costs.

hosp_cost <- na.omit(hospital_cost)
hosp_cost$RACE <- as.factor(hosp_cost$RACE)
table(hospital_cost$RACE) 
fr1 <- aov(TOTCHG ~ RACE,data=hosp_cost)
fr1
summary(fr1)


#----------------------------------------------------------------------------------------------------------------------


#To properly utilize the costs, the agency has to analyze the severity of the
#hospital costs by age and gender for proper allocation of resources

hospital_cost$FEMALE<-as.factor(hospital_cost$FEMALE)
table(hospital_cost$FEMALE)
b <- lm(TOTCHG ~ AGE+FEMALE,data=hosp_cost)
summary(b)

#---------------------------------------------------------------------------------------------------------------------


#Since the length of stay is the crucial factor for inpatients, the agency wants
#to find if the length of stay can be predicted from age, gender, and race.

hospital_cost$RACE<-as.factor(hospital_cost$RACE) 
table(hosp_cost$LOS)
chek <- aov(LOS ~ AGE+FEMALE+RACE,data=hosp_cost)
summary(chek)
cat <- lm(LOS ~ AGE+FEMALE+RACE,data=hosp_cost)
summary(cat)






















#---------------------------------------------------------------------------------------------------------------------



#To perform a complete analysis, the agency wants to find the variable that
#mainly affects the hospital costs.

cost <- lm(TOTCHG ~ .,data=hosp_cost)
summary(cost)







#---------------------------------------------------------------------------------------------------------------------