
# **MechaCar_Statistical_Analysis**

**Background**

The client is asking for a statistical analysis of MechaCar due to its production troubles which is blocking the manufacturing team's progress.

**Purpose**

The purpose of this analysis is to predict the mpg of MechaCar prototypes, collect summary statistics on the pounds per square inch(PSI) of the suspension coils, and determine if the manufacturing lots are statistically different from the mean population.

**Linear Regression to Predict MPG**

Running a multiple linear regression model with all of our variables in the dataset we get an r-squared value of 0.715 and a p-value (5.35e-11). The p-value well below our alpha of 0.05 (assumed 95% confidence level) allows us to reject the null hypothesis that the slope of the linear model is zero (m=0). We can interpret the statistical significance of each of the variables in the summary table by comparing their probabilities (Pr(\&gt;|t|) in the output) to alpha = 0.05. Probabilities less than alpha are statistically significant, meaning that they have a significant relationship with the dependent variable (mpg) and are statistically unlikely to be random variation. In our dataset, the variables that are providing non-random variation are: vehicle length, ground clearance, and intercept. The fact that the intercept is statistically significant tells us that there are other factors contributing to the variation of our model that may or may not have been included. The r-squared value of 0.715 suggests that our linear model fits roughly 71% of the data. On the surface, this seems like a good predictor of mpg for the MechaCar prototypes. However, it would be wise to study the residuals versus fits and residual normality plots to confirm the assumptions of the analysis.

**Summary Statistics on Suspension Coils**

When we run summary statistics for the dataset as a whole, we get the following output:


 Mean = 1499.531 
 Median = 1499.747 
 Standard Deviation = 8.731242 
 Variance = 76.23459 



  At first glance, this suggests the current manufacturing data meets the design specification that the variance of the suspension coils must not exceed 100 psi. However, if we run code to group by Manufacturing Lot the data tells another story. From the grouped data, Manufacturing Lot 3 would be rejected and root cause would need to be investigated and counter measured since the variance is well above our limit of 100 psi. 



Lot 1             Lot 2         Lot 3 
Mean =                1500.018          1499.571      1499.004 
Median =              1500.206          1498.873      1497.981 
Standard Deviation =     1.071             3.183      14.833 
Variance =               1.146            10.132      220.011 

**T-Tests on Suspension Coils**

We use the one-sample t-test to assert if there is a statistical difference between the means of the sample dataset and hypothesized, potential population dataset by examining the following hypotheses: 



 
Ho : There is no statistical difference between the observed sample mean and its presumed population mean. 
Ha : There is a statistical difference between the observed sample mean and its presumed population mean.



 
 When running the t-Test with our script, we get a p-value of 0.5117, well above an alpha of 0.05 (assuming a 95% confidence level). Thus, we fail to reject the null hypothesis and we can conclude that the sample mean of 1499.531 and the desired population mean of 1500 are statistically similar. 

**Further Study**

 A couple of the most important aspects of auto ownership are quality and reliability. Consumers are very conscience of these factors across automotive brands. As consumers have good experiences with vehicles, they tend to stick with that manufacturer for future purchases. The initial quality upon purchase tells the consumer a lot about the brand. The JD Power Initial Quality Study (IQS) is a great metric to benchmark. Manufacturers are ranked by Problems per 100 Vehicles (PPH) in the first 90 days of ownership. We would need to compile all defects for the prototype MechaCars and calculate the PPH. This could be compared to the mean of the data from the latest JD Power IQS. Is our PPH statistically similar to the mean of the JD Power IQS? Where would the MechaCar rank in the study at this prototype level? Since we will not have 3 year data for our prototypes, it would be difficult to look at reliability but perhaps initial quality translates to 3 year reliability. 


 
The hypotheses we will test are: 


Ho : There is no statistical difference between the PPH observed for the MechaCar prototypes sample and the mean of the JD Power IQS data. 
Ha : There is a statistical difference between the PPH observed for the MechaCar prototypes sample and the mean of the JD Power IQS data. 



 
We could use a two-sample t-Test to compare our PPH data to the mean of all manufacturers in the IQS. This comparison would still be sample to sample since we do not have a true sense of the entire population of data. Thus, the two-sample t-Test is used rather than the one-sample t-Test. 