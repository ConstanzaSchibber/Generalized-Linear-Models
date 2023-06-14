First Differences and Monte Carlo Simulations
================
Constanza F. Schibber
2020-10-1

**Agenda**

- [Data](Data:%20Voting%20Intentions%20in%20the%201988%20Chilean%20Plebiscite)
- [Simulation](#%20Using%20the%20%60sim%60%20function%20in%20the%20%60arm%60%20library)
- [First Differences \# 1](#%20First%20Differences:%20Sex%20&%20SQ)
- [First Differences \# 2](#%20First%20Differences:%20Sex%20&%20Income)

# Data: Voting Intentions in the 1988 Chilean Plebiscite

The Chile data frame has 2700 rows and 8 columns. The data are from a
national survey conducted in April and May of 1988 by FLACSO/Chile.
There are some missing data.

The survey wanted to capture Chileans attitudes and whether they would
vote in favor/against the authoritarian regime during the Chilean
plebiscite/referendum. In this referendum, all citizens would vote on
whether Chile would continue as a military regime or it would transition
to open and free elections towards democracy.

``` r
library(car)
```

    ## Loading required package: carData

``` r
#help(Chile) # for more information on the dataset
summary(Chile)
```

    ##  region     population     sex           age        education  
    ##  C :600   Min.   :  3750   F:1379   Min.   :18.00   P   :1107  
    ##  M :100   1st Qu.: 25000   M:1321   1st Qu.:26.00   PS  : 462  
    ##  N :322   Median :175000            Median :36.00   S   :1120  
    ##  S :718   Mean   :152222            Mean   :38.55   NA's:  11  
    ##  SA:960   3rd Qu.:250000            3rd Qu.:49.00              
    ##           Max.   :250000            Max.   :70.00              
    ##                                     NA's   :1                  
    ##      income         statusquo          vote    
    ##  Min.   :  2500   Min.   :-1.80301   A   :187  
    ##  1st Qu.:  7500   1st Qu.:-1.00223   N   :889  
    ##  Median : 15000   Median :-0.04558   U   :588  
    ##  Mean   : 33876   Mean   : 0.00000   Y   :868  
    ##  3rd Qu.: 35000   3rd Qu.: 0.96857   NA's:168  
    ##  Max.   :200000   Max.   : 2.04859             
    ##  NA's   :98       NA's   :17

``` r
# Recode yes/no 
# We recode "undecided" and "abstain" as NA
Chile$yes <- with(Chile, ifelse(vote == "Y", 1, ifelse(vote=="N", 0, NA))) 
table(Chile$vote)
```

    ## 
    ##   A   N   U   Y 
    ## 187 889 588 868

``` r
table(Chile$yes)
```

    ## 
    ##   0   1 
    ## 889 868

Then omit observations with missing data - Issue is that if you don’t
omit missing the predicted values will have a different length than the
data and it can cause problems. *We will cover in a few weeks how to
handle missing values using multiple imputation.* Do not omit missing
data in your own work!

``` r
Chile<-na.omit(Chile)
```

# Model, Logit

``` r
Chile.out <- glm(yes ~ statusquo+age+income+sex, family=binomial(link=logit), data=Chile) 
summary(Chile.out)
```

    ## 
    ## Call:
    ## glm(formula = yes ~ statusquo + age + income + sex, family = binomial(link = logit), 
    ##     data = Chile)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.3063  -0.2775  -0.1503   0.1960   2.9069  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  4.356e-01  3.151e-01   1.383  0.16679    
    ## statusquo    3.206e+00  1.470e-01  21.815  < 2e-16 ***
    ## age          9.308e-03  6.908e-03   1.347  0.17785    
    ## income      -6.200e-06  2.414e-06  -2.568  0.01023 *  
    ## sexM        -6.186e-01  2.005e-01  -3.085  0.00204 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2360.29  on 1702  degrees of freedom
    ## Residual deviance:  717.47  on 1698  degrees of freedom
    ## AIC: 727.47
    ## 
    ## Number of Fisher Scoring iterations: 6

# Using the `sim` function in the `arm` library

We simulate predictions for sex = Female and sex = Male. Remember that
we are setting other covariates at their mean, but in a few weeks we
will cover the observed value approach to produce better predictions.

``` r
library(arm)
```

    ## Loading required package: MASS

    ## Loading required package: Matrix

    ## Loading required package: lme4

    ## 
    ## arm (Version 1.13-1, built: 2022-8-25)

    ## Working directory is /Users/connie/Generalized-Linear-Models/Labs/03.week

    ## 
    ## Attaching package: 'arm'

    ## The following object is masked from 'package:car':
    ## 
    ##     logit

``` r
library(faraway)
```

    ## 
    ## Attaching package: 'faraway'

    ## The following objects are masked from 'package:arm':
    ## 
    ##     fround, logit, pfround

    ## The following objects are masked from 'package:car':
    ## 
    ##     logit, vif

``` r
# new data for prediction
sex.F<-cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)), mean(na.omit(Chile$income)), 0 ) #0 for female
sex.M<-cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)), mean(na.omit(Chile$income)), 1 ) # 1 for male

# sim

sim.model<-coef(sim(Chile.out, 1000))

# use sim

predict.F<- ilogit(sex.F %*% t(sim.model))
predict.M<- ilogit(sex.M %*% t(sim.model))
```

We can now calculate first differences: P(vote\|M) - P(vote\|F)

``` r
# first difference P(vote|M)-P(vote|F)

first.difference<-predict.M-predict.F

FD.M.F<-quantile(first.difference, 0.5)

CI.FD.M.F<-quantile(first.difference, probs=c(0.025, 0.975)) #For a 95% CI (CHECK THAT FOR SOME FIGURES I DID 90% CI. -- A 90% CI IS using quantile 0.05 and 0.95

(c(CI.FD.M.F[1], FD.M.F, CI.FD.M.F[2]))
```

    ##        2.5%         50%       97.5% 
    ## -0.24198190 -0.15264426 -0.06040539

# First Differences: Sex & SQ

Let’s add another dimension: we can calculate predictions varying two
covariates – status quo and sex.

Below I am using the same figure and calculations like in the predicted
probability figure. We have to specify “1” because we have a matrix in
which the rows refer to each value of sq and the columns refer to
simulated coefficient. Basically, we have a prediction for each
coefficient for each sq. We need to do the calculations for each value
of sq. We have to slice a distribution of predictions to get the median
and the bounds for the confidence intervals.

``` r
# Here I will show an example when status quo varies

sq<-seq(min(Chile$statusquo),max(Chile$statusquo), length.out=100 )

sex.F.sq<-cbind(1, sq, mean(na.omit(Chile$age)), mean(na.omit(Chile$income)), 0 ) #0 for female
sex.M.sq<-cbind(1, sq, mean(na.omit(Chile$age)), mean(na.omit(Chile$income)), 1 ) # 1 for male

# sim

sim.model<-coef(sim(Chile.out, 1000))

# use sim

predict.F.sq<- ilogit(sex.F.sq %*% t(sim.model))
predict.M.sq<- ilogit(sex.M.sq %*% t(sim.model))

# first difference P(vote|F)-P(vote|M)

first.difference<-predict.F.sq-predict.M.sq


par(mfrow=c(1,2))
plot(NULL, xlab="Support for the Status Quo", ylab="P(Vote in favor of the military regime)",main = "Predicted Probabilities", ylim=c(-0.01,1), xlim=c(min(na.omit(Chile$statusquo)),max(na.omit(Chile$statusquo))))
lines(sq, apply(predict.F.sq, 1, quantile, 0.5), lwd = 3)
lines(sq, apply(predict.F.sq, 1, quantile, 0.025), lty = 2)
lines(sq, apply(predict.F.sq, 1, quantile, 0.97), lty = 2)
lines(sq, apply(predict.M.sq, 1, quantile, 0.5), lwd = 3, col='blue')
lines(sq, apply(predict.M.sq, 1, quantile, 0.025), lty = 2, col='blue')
lines(sq, apply(predict.M.sq, 1, quantile, 0.97), lty = 2, col='blue')
legend("bottomright", legend=c("Female", "Male"), col=c("black", "blue"), lty=c(1,1), cex=.5)

plot(NULL, xlab="Support for the Status Quo", ylab="P(Vote|Female)-P(Vote|Male)",main="First Differences", ylim=c(-0.01,1), xlim=c(min(na.omit(Chile$statusquo)),max(na.omit(Chile$statusquo))))
lines(sq, apply(first.difference, 1, quantile, 0.5), lwd = 3)
lines(sq, apply(first.difference, 1, quantile, 0.025), lty = 2)
lines(sq, apply(first.difference, 1, quantile, 0.97), lty = 2)
abline(h=0, lty=3)
```

![](FirstDifferencesMCSimulation_files/figure-gfm/first%20differences%201-1.png)<!-- -->

Important for interpretation:

- Overlapping CI for predictions DOES NOT mean there is no difference
  between the predictions. This is a very common error. You have to
  calculate first difference and the SE for the first difference! By
  doing simulation the calculation of the CI for the difference is
  trivial.

- We see on the first difference plot that there is a difference between
  the probability of voting in favor of the Pinochet regime for men and
  women and it is different from zero for values of status quo from -1
  to 1. Because the first difference is positive, it means women were
  more likely than men to support the Pinochet regime.

# First Differences: Sex & Income

Now, we calculate predictions for another two varying covariates –
income and sex.

``` r
# Here I will show an example when status quo varies

income<-seq(min(Chile$income),max(Chile$income), length.out=100 )

sex.F.income<-cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)), income, 0 ) #0 for female
sex.M.income<-cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)), income, 1 ) # 1 for male

# sim

sim.model<-coef(sim(Chile.out, 1000))

# use sim

predict.F.income<- ilogit(sex.F.income %*% t(sim.model))
predict.M.income<- ilogit(sex.M.income %*% t(sim.model))

# first difference P(vote|F)-P(vote|M)

first.difference<-predict.F.income-predict.M.income


par(mfrow=c(1,2))
plot(NULL, xlab="Income", ylab="P(Vote in favor of the military regime)",main = "Predicted Probabilities", ylim=c(-0.01,1), xlim=c(min(na.omit(Chile$income)),max(na.omit(Chile$income))))
lines(income, apply(predict.F.income, 1, quantile, 0.5), lwd = 3)
lines(income, apply(predict.F.income, 1, quantile, 0.025), lty = 2)
lines(income, apply(predict.F.income, 1, quantile, 0.97), lty = 2)
lines(income, apply(predict.M.income, 1, quantile, 0.5), lwd = 3, col='blue')
lines(income, apply(predict.M.income, 1, quantile, 0.025), lty = 2, col='blue')
lines(income, apply(predict.M.income, 1, quantile, 0.97), lty = 2, col='blue')
legend("bottomright", legend=c("Female", "Male"), col=c("black", "blue"), lty=c(1,1), cex=.5)

plot(NULL, xlab="Income", ylab="P(Vote|Female)-P(Vote|Male)",main="First Differences", ylim=c(-0.01,1), xlim=c(min(na.omit(Chile$income)),max(na.omit(Chile$income))))
lines(income, apply(first.difference, 1, quantile, 0.5), lwd = 3)
lines(income, apply(first.difference, 1, quantile, 0.025), lty = 2)
lines(income, apply(first.difference, 1, quantile, 0.97), lty = 2)
abline(h=0, lty=3)
```

![](FirstDifferencesMCSimulation_files/figure-gfm/first%20differences%202-1.png)<!-- -->

Discussion:

- Do we observe a difference between the vote choice of women and men?
- Do we observe a difference between the voice choice of women and men
  as income increases? How would you calculate a second difference?
- Do we observe a difference between the vote choice of women of lower
  income and women of high income? How would you calculate the first
  difference?
- Do we observe a difference between the vote choice of women of lower
  income and men of lower income? How would you calculate the first
  difference?
