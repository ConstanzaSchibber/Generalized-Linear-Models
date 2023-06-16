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
# help(Chile) # for more information on the dataset
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer

``` r
stargazer(Chile, type = "text")
```

    ## 
    ## =======================================================
    ## Statistic    N      Mean      St. Dev.    Min     Max  
    ## -------------------------------------------------------
    ## population 2,700 152,222.200 102,198.000 3,750  250,000
    ## age        2,699   38.549      14.756      18     70   
    ## income     2,602 33,875.860  39,502.870  2,500  200,000
    ## statusquo  2,683   -0.000       1.000    -1.803  2.049 
    ## -------------------------------------------------------

``` r
# Recode yes/no We recode 'undecided' and 'abstain' as NA
Chile$yes <- with(Chile, ifelse(vote == "Y", 1, ifelse(vote ==
    "N", 0, NA)))
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
Chile <- na.omit(Chile)
```

# Model, Logit

``` r
Chile.out <- glm(yes ~ statusquo + age + income + sex, family = binomial(link = logit),
    data = Chile)

stargazer(Chile.out, type = "text")
```

    ## 
    ## =============================================
    ##                       Dependent variable:    
    ##                   ---------------------------
    ##                               yes            
    ## ---------------------------------------------
    ## statusquo                  3.206***          
    ##                             (0.147)          
    ##                                              
    ## age                          0.009           
    ##                             (0.007)          
    ##                                              
    ## income                    -0.00001**         
    ##                            (0.00000)         
    ##                                              
    ## sexM                       -0.619***         
    ##                             (0.201)          
    ##                                              
    ## Constant                     0.436           
    ##                             (0.315)          
    ##                                              
    ## ---------------------------------------------
    ## Observations                 1,703           
    ## Log Likelihood             -358.736          
    ## Akaike Inf. Crit.           727.473          
    ## =============================================
    ## Note:             *p<0.1; **p<0.05; ***p<0.01

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
sex.F <- cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)),
    mean(na.omit(Chile$income)), 0)  #0 for female
sex.M <- cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)),
    mean(na.omit(Chile$income)), 1)  # 1 for male

# sim

sim.model <- coef(sim(Chile.out, 1000))

# use sim

predict.F <- ilogit(sex.F %*% t(sim.model))
predict.M <- ilogit(sex.M %*% t(sim.model))
```

We can now calculate first differences: P(vote\|M) - P(vote\|F)

``` r
# first difference P(vote|M)-P(vote|F)

first.difference <- predict.M - predict.F

FD.M.F <- quantile(first.difference, 0.5)

# For a 95% CI (CHECK THAT FOR SOME FIGURES I DID 90% CI
# w/quantile 0.05 and 0.95
CI.FD.M.F <- quantile(first.difference, probs = c(0.025, 0.975))

(c(CI.FD.M.F[1], FD.M.F, CI.FD.M.F[2]))
```

    ##        2.5%         50%       97.5% 
    ## -0.24266928 -0.15607783 -0.06435134

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

sq <- seq(min(Chile$statusquo), max(Chile$statusquo), length.out = 100)

sex.F.sq <- cbind(1, sq, mean(na.omit(Chile$age)), mean(na.omit(Chile$income)),
    0)  #0 for female
sex.M.sq <- cbind(1, sq, mean(na.omit(Chile$age)), mean(na.omit(Chile$income)),
    1)  # 1 for male

# sim

sim.model <- coef(sim(Chile.out, 1000))

# use sim

predict.F.sq <- ilogit(sex.F.sq %*% t(sim.model))
predict.M.sq <- ilogit(sex.M.sq %*% t(sim.model))

# first difference P(vote|F)-P(vote|M)

first.difference <- predict.F.sq - predict.M.sq


par(mfrow = c(1, 2))
plot(NULL, xlab = "Support for the Status Quo", ylab = "P(Vote in favor of the military regime)",
    main = "Predicted Probabilities", ylim = c(-0.01, 1), xlim = c(min(na.omit(Chile$statusquo)),
        max(na.omit(Chile$statusquo))))
lines(sq, apply(predict.F.sq, 1, quantile, 0.5), lwd = 3)
lines(sq, apply(predict.F.sq, 1, quantile, 0.025), lty = 2)
lines(sq, apply(predict.F.sq, 1, quantile, 0.97), lty = 2)
lines(sq, apply(predict.M.sq, 1, quantile, 0.5), lwd = 3, col = "blue")
lines(sq, apply(predict.M.sq, 1, quantile, 0.025), lty = 2, col = "blue")
lines(sq, apply(predict.M.sq, 1, quantile, 0.97), lty = 2, col = "blue")
legend("bottomright", legend = c("Female", "Male"), col = c("black",
    "blue"), lty = c(1, 1), cex = 0.5)

plot(NULL, xlab = "Support for the Status Quo", ylab = "P(Vote|Female)-P(Vote|Male)",
    main = "First Differences", ylim = c(-0.01, 1), xlim = c(min(na.omit(Chile$statusquo)),
        max(na.omit(Chile$statusquo))))
lines(sq, apply(first.difference, 1, quantile, 0.5), lwd = 3)
lines(sq, apply(first.difference, 1, quantile, 0.025), lty = 2)
lines(sq, apply(first.difference, 1, quantile, 0.97), lty = 2)
abline(h = 0, lty = 3)
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

income <- seq(min(Chile$income), max(Chile$income), length.out = 100)

sex.F.income <- cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)),
    income, 0)  #0 for female
sex.M.income <- cbind(1, mean(na.omit(Chile$statusquo)), mean(na.omit(Chile$age)),
    income, 1)  # 1 for male

# sim

sim.model <- coef(sim(Chile.out, 1000))

# use sim

predict.F.income <- ilogit(sex.F.income %*% t(sim.model))
predict.M.income <- ilogit(sex.M.income %*% t(sim.model))

# first difference P(vote|F)-P(vote|M)

first.difference <- predict.F.income - predict.M.income


par(mfrow = c(1, 2))
plot(NULL, xlab = "Income", ylab = "P(Vote in favor of the military regime)",
    main = "Predicted Probabilities", ylim = c(-0.01, 1), xlim = c(min(na.omit(Chile$income)),
        max(na.omit(Chile$income))))
lines(income, apply(predict.F.income, 1, quantile, 0.5), lwd = 3)
lines(income, apply(predict.F.income, 1, quantile, 0.025), lty = 2)
lines(income, apply(predict.F.income, 1, quantile, 0.97), lty = 2)
lines(income, apply(predict.M.income, 1, quantile, 0.5), lwd = 3,
    col = "blue")
lines(income, apply(predict.M.income, 1, quantile, 0.025), lty = 2,
    col = "blue")
lines(income, apply(predict.M.income, 1, quantile, 0.97), lty = 2,
    col = "blue")
legend("bottomright", legend = c("Female", "Male"), col = c("black",
    "blue"), lty = c(1, 1), cex = 0.5)

plot(NULL, xlab = "Income", ylab = "P(Vote|Female)-P(Vote|Male)",
    main = "First Differences", ylim = c(-0.01, 1), xlim = c(min(na.omit(Chile$income)),
        max(na.omit(Chile$income))))
lines(income, apply(first.difference, 1, quantile, 0.5), lwd = 3)
lines(income, apply(first.difference, 1, quantile, 0.025), lty = 2)
lines(income, apply(first.difference, 1, quantile, 0.97), lty = 2)
abline(h = 0, lty = 3)
```

![](FirstDifferencesMCSimulation_files/figure-gfm/first%20differences%202-1.png)<!-- -->

**Discussion:**

- Do we observe a difference between the vote choice of women and men?
- Do we observe a difference between the voice choice of women and men
  as income increases? How would you calculate a second difference?
- Do we observe a difference between the vote choice of women of lower
  income and women of high income? How would you calculate the first
  difference?
- Do we observe a difference between the vote choice of women of lower
  income and men of lower income? How would you calculate the first
  difference?
