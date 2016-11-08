
### MULTIPLE REGRESSION IN CLASS PRACTICE 

library(tidyverse)
my.data <- read.csv ("regLectureData.csv")

glimpse(my.data)

library(apaTables)
apa.cor.table(my.data)

##check to make sure normal distribution (not curved, etc)
psych::pairs.panels(as.data.frame(my.data))

### Do regression
my.regression <- lm(VidScore ~ iq + age, data=my.data)
#get b weights
my.regression
summary(my.regression)

apa.reg.table(my.regression)

#   Predictor        b        b_95%_CI  beta    beta_95%_CI sr2  sr2_95%_CI      r             Fit
#(Intercept) 102.23** [87.76, 116.71]                                                            
#iq   0.33**    [0.24, 0.42]  0.46   [0.34, 0.58] .20  [.11, .30]  .50**                
  #age  -0.37**  [-0.59, -0.15] -0.21 [-0.33, -0.09] .04 [-.01, .09] -.30**                
  #R2 = .291**    95% CI[.19,.38] = PREDCITING .30, BUT COULD BE AS LOW/HIGH AS 
# - together, age/iq predcict .29% of variance in VG scores - .04 is uniquely predicted by age, .20
# uniquely predicted by iq 
# what would R2 be if you removed age? It would be .25 (.29 - .04) - the missing .05*between .29 and .24) is the OVERLAP 
# if you just use one predictor (e.g. iq - it would change slighty because variance of .05 would be added - this is the overlap)


#what's the best guess of someone's video game score based on age and IQ
x_axis_range <- data.frame(age = c(43), iq=c(130))
x_axis_range
CI_data <- predict(my.regression, newdata = x_axis_range, interval = "confidence", level =.095)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data

PI_data <- predict(my.regression, newdata = x_axis_range, interval = "prediction", level =.095)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
PI_data


###hierarchical regression
head(attitude)
reg1 <- lm(rating ~ complaints + privileges, data=attitude)
reg2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
#run anova
anova(reg1,reg2)
apa.reg.table(reg1,reg2)
# Delta R2 = .03 - not significant, adding learning didnt do anything
#95% CI[-.04, .10]
