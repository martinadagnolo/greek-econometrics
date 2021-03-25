#Country: Greece
#Group: Martina D'Agnolo, Anja Stevovic, Dimitris Selekos

#loading libraries
library(readr)
library(dplyr)
library(ggplot2)

#plotting the phillips curve
phillips <- ggplot(data = final, aes(x=Unemployment_rate, y=Inflation_rate)) + geom_point() + geom_smooth()
phillips

#plotting unemployment bar graph
graph <- ggplot(data = table, aes(x=country, y= unemployment)) + geom_bar(stat="identity", fill="seagreen4")
graph + coord_flip()
graph

#plotting unemployment over the years
unemployment_over_the_years <- ggplot(data = final, aes(x= Annual, y=Unemployment_rate)) + geom_point()
unemployment_over_the_years

#plotting inflation over the years
inflation_over_the_years <- ggplot(data = final, aes(x= Annual, y=Inflation_rate)) + geom_point()
inflation_over_the_years

# calculating OLS estimators manually
b_hat <- cov(final$Inflation_rate,final$Unemployment_rate)/var(final$Inflation_rate) 

# now that we have our estimated slope parameter we can compute the intercept. 
a_hat <- mean(final$Unemployment_rate) - b_hat*mean(final$Inflation_rate)

attach(final)
b_hat <- cov(Inflation_rate, Unemployment_rate)/var(Unemployment_rate)
a_hat <- mean(Inflation_rate) - b_hat*mean(Unemployment_rate)
detach(final)

# Fitting the linear model
(inflation_reg <- lm(Inflation_rate ~ Unemployment_rate, data=final)) 

# We find the fitted and residual values
y_hat <- fitted(lm(Inflation_rate ~ Unemployment_rate, data=final))
u_hat <- resid(inflation_reg)

cov(final$Unemployment_rate,u_hat)
(u_bar <- mean(final$Inflation_rate- a_hat - b_hat*mean(final$Unemployment_rate)))

# checking goodness of fit
TSS <- (length(final$Inflation_rate)-1)*var(final$Inflation_rate)
ESS <- (length(final$Inflation_rate)-1)*var(y_hat)
RSS <- (length(final$Inflation_rate)-1)*var(u_hat)

(R2 <- ESS / TSS)
1- RSS/TSS               
cor(final$Unemployment_rate,y_hat)^2  


# Computing standard errors of the OLS coefficients manually
N <- length(final$Inflation_rate)
sigma_hat <- sd(u_hat)*sqrt((N-1)/(N-2)) 
# We need a dof correction for having sigma hat unbiased
ssx <- (N-1)*var(final$Unemployment_rate)

# Standard error of beta_0
(se0 <- sigma_hat * sqrt(mean(final$Unemployment_rate^2)) / sqrt(ssx))
# Standard error of beta_1
(se1 <- sigma_hat / sqrt(ssx))

summary(lm(Inflation_rate ~ Unemployment_rate, data=final))

#Interpreting coefficients

# level-level
summary(lm(Inflation_rate ~ Unemployment_rate, data=final))
summary(inflation_reg) #equivalent way given that we already stored everything

# log-log
final$loginflation <- log(final$Inflation_rate)
final$logunemployment <- log(final$Unemployment_rate)
summary(lm(loginflation ~ logunemployment, data=final))
# plotting the log
logplot <- ggplot(data = final, aes(x=logunemployment, y=loginflation)) + geom_point() + geom_smooth()
logplot


#REPEATING SAME PROCESS WITH TAX

my_plot <- ggplot(data = Tax, aes(x=TaxR, y=inflation_rate))+geom_point()+ geom_smooth()
my_plot
Tax_Revenue_as_a_of_GDP <- ggplot(data = Tax, aes(x= Year, y=TaxR)) + geom_point()
Tax_Revenue_as_a_percentage_of_GDP
inflation_over_the_years <- ggplot(data = Tax, aes(x= Year, y=inflation_rate)) + geom_point()
inflation_over_the_years

# manual calculation for the OLS Estimators
b_hat <- cov(Tax$inflation_rate,Tax$TaxR)/var(Tax$inflation_rate) 

a_hat <- mean(Tax$TaxR) - b_hat*mean(Tax$inflation_rate)

attach(Tax)  
b_hat <- cov(inflation_rate, TaxR)/var(TaxR)
a_hat <- mean(inflation_rate) - b_hat*mean(TaxR)
detach(Tax)

# Command to do linear regressions
(inflation_reg <- lm(inflation_rate ~ TaxR, data=Tax)) 

y_hat <- fitted(inflation_reg)
u_hat <- resid(inflation_reg)


cov(Tax$TaxR,u_hat)
(u_bar <- mean(Tax$inflation_rate- a_hat - b_hat*mean(Tax$TaxR)))

# testing goodness of fit
TSS <- (length(Tax$inflation_rate)-1)*var(Tax$inflation_rate)
ESS <- (length(Tax$inflation_rate)-1)*var(y_hat)
RSS <- (length(Tax$inflation_rate)-1)*var(u_hat)

(R2 <- ESS / TSS)
1- RSS/TSS               
cor(Tax$TaxR,y_hat)^2   

# standard errors of OLS coefficients
N <- length(Tax$inflation_rate)
sigma_hat <- sd(u_hat)*sqrt((N-1)/(N-2)) 
# dof correction for having sigma hat unbiased
ssx <- (N-1)*var(Tax$TaxR)

# standard error of beta_0
(se0 <- sigma_hat * sqrt(mean(Tax$TaxR^2)) / sqrt(ssx))
# standard error of beta_1
(se1 <- sigma_hat / sqrt(ssx))

summary(lm(inflation_rate ~ TaxR, data=Tax))

#Interpreting coefficients

# level-level
summary(lm(inflation_rate ~ TaxR, data=Tax))
summary(inflation_reg) #equivalent way given that we already stored everything

# log-log
Tax$loginflation <- log(Tax$inflation_rate)
Tax$tax <- log(Tax$TaxR)
summary(lm(loginflation ~ logunemployment, data=Tax))


#F-TEST

df_melt <- reshape2::melt(df, id.var = 'Tax')
df_melt

my_plot <- ggplot(data = Tax, aes(x=TaxR, Unemployment, y=inflation_rate))+geom_point()+ geom_smooth()
my_plot

# F test for joint signigicance directly reported on the summary(lm(...)), 
#test whether all parameters jointly have no impact on Y.
(summlb1<- summary(lm(inflation_rate ~ TaxR + Unemployment, data=Tax)))

# manual calculation

#Unrestricted (U) model
reg.U <- lm(inflation_rate ~ TaxR + Unemployment, data=Tax )
(R2_U <- summary(reg.U)$r.squared)  # R2 of Unrestricted model

# Restricted (R) model
reg.R <- lm(inflation_rate ~Unemployment, data=Tax)
(R2_R <- summary(reg.R)$r.squared)  # R2 of Restricted model

# F statistic
df <- length(Tax$inflation_rate) - 2
(Fstat <- ( (R2_U - R2_R) / 3 ) / ( (1-R2_U) / df ) )

# 1% critical value from the F(r,N-K) distribution

cv_f <- qf(1-0.1, 3 , df)

# reject H0 if Fstat > cv_f
(Fstat > cv_f)

# or, look at p-value (and reject H0 if p < 0.01)
(p <- 1 - pf(Fstat, 3, df))

# installing "car"
install.packages("car")
library(car)

myH0 <- c("Unemployment=0","TaxR=0") # stating our H0 in a string vector
linearHypothesis(reg.U, myH0)   # running F test in a single command



