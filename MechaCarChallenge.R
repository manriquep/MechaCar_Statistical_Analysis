
# Deliverable 1

#add dplyr library
library(dplyr) 
#read the file
MechaCar_data_frame <- read.csv(file = 'MechaCar_mpg.csv') 
#create linear regression to pass all six variables
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_data_frame) 
#determine the p-value and r-squared for the linear regression
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_data_frame)) 

# Deliverable 2

#read the file
table <- read.csv(file = 'Suspension_Coil.csv') 
#get a total summary
total_summary <- table %>% data.frame()%>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),) 
#create a lot summary
lot_summary <- table %>%group_by(Manufacturing_Lot)%>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 

#Deliverable 3

#t-test for PSI across all lots
t.test(table$PSI,mu=1500) 
#t-test for lot 1
t.test(subset(table, Manufacturing_Lot=="Lot1")$PSI,mu=1500) 
#t-test for lot 2
t.test(subset(table, Manufacturing_Lot=="Lot2")$PSI,mu=1500) 
#t-test for lot 3
t.test(subset(table, Manufacturing_Lot=="Lot3")$PSI,mu=1500) 
