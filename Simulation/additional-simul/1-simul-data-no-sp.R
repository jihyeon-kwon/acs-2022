################
#Simulate Data #
################


##################################
# Simulate Explanatory Variables #
##################################

##### Goals --------------------------------------------------------------------

# We simulate 100 data sets of 4 explanatory variables 

# Simulation Assumptions :
# 1. For two demographic variables (var1, var2), we assume that they vary a little over time 
#    and there is a spatial correlation
# 2. For two socioeconomic variables (var3, var4), we assume that they fluctuate over time 
#    and there is a spatial correlation.


# Simulated (true) X : year 1-18 (18 years)
# We actually use data from 5-18 but created 18 years for convenience.
# Observed X : year 5-18 (14 years)
# Observed Y : year 9-18 (10 years)

##### set-up -------------------------------------------------------------------

set.seed(4122022)

library(mapplots)
library(ngspatial)
library(MASS)
library(tidyverse)
library(zoo)
library(here)
select <- dplyr::select

##### Data grid and time -------------------------------------------------------

# set up
n <- 100 # 100 locations
T <- 14 # 10 years (observation period for explanatory variable)
x0 <- seq(0, 1, length.out = sqrt(n))
y0 <- seq(0, 1, length.out = sqrt(n))
data.locations <- expand.grid(x0, y0)

##### Data frame ---------------------------------------------------------------

# data frame for simulated data
SimulatedData <- tibble(
        "location" = rep(1:n, T+4), # 100 areas with (T+4) times 
        "x" = rep(data.locations[, 1], T+4),
        "y" = rep(data.locations[, 2], T+4),
        "time" = kronecker(1:(T+4), rep(1,n))
)

head(SimulatedData)

# adjacency matrix for these 100 locations 
A <- adjacency.matrix(sqrt(n), sqrt(n))

##### Simulate explanatory variables -------------------------------------------

# two demographic variables 
# simulate demographic variable with some spatial correlation

# generate the data
SimulatedData %<>%
        rowwise() %>%
        mutate(var1 = rnorm(n(), -0.3 + .5*x + .1*y, .15), 
               var2 = rnorm(n(), 0.5 -.5*x^2 +.5*x - .1*y, .5)) 

# two socioeconomic variables :

# generate the data
SimulatedData %<>%
        rowwise() %>%
        mutate(var3 = rnorm(n(), -.2 * x^2 * y + .3 * (time - 13)^2/50, .15),
               var4 = rnorm(n(), .5 * x * (1-y) - .05 * (time < 8) * (time - 8) + .08 * time, .2))


##### Generate true 1-year and 5-year ------------------------------------------
# generate 1 and 5-year estimates that we will use in fitting the model

# locations with both 1 year and 5 year observed data
l <- 40
Ind1and5 <- sort(sample(1:n, l) ) # select 40 locations 

# create an indicator with yearlyACS == 1 means the location has both data
SimulatedData <- SimulatedData %>%
        mutate(YearlyACS = case_when(location %in% Ind1and5 ~ 1,
                                     TRUE ~ 0))
head(SimulatedData)
sum(SimulatedData$YearlyACS)/(T+4)

# true 1 year data
SimulatedData <- SimulatedData %>%
        arrange(location, time) %>%
        mutate(var1.1yr = ifelse(YearlyACS == 1, var1, NA),
               var2.1yr = ifelse(YearlyACS == 1, var2, NA),
               var3.1yr = ifelse(YearlyACS == 1, var3, NA),
               var4.1yr = ifelse(YearlyACS == 1, var4, NA))

# true 5 year data :
# rearrange data so that it would be easier to compute the rolling means
SimulatedData$var1.5yr <- rollmean(SimulatedData$var1, k = 5, align = "right", fill = NA)
SimulatedData$var2.5yr <- rollmean(SimulatedData$var2, k = 5, align = "right", fill = NA)
SimulatedData$var3.5yr <- rollmean(SimulatedData$var3, k = 5, align = "right", fill = NA)
SimulatedData$var4.5yr <- rollmean(SimulatedData$var4, k = 5, align = "right", fill = NA)

# fill with NA when time < 5
SimulatedData <- SimulatedData %>%
        mutate(var1.5yr = ifelse(time < 5, NA, var1.5yr),
               var2.5yr = ifelse(time < 5, NA, var2.5yr),
               var3.5yr = ifelse(time < 5, NA, var3.5yr),
               var4.5yr = ifelse(time < 5, NA, var4.5yr))



##### Create standard error ----------------------------------------------------

SimulatedData <- SimulatedData %>%
        mutate(var1.1se = .01*YearlyACS + .05*(1-YearlyACS),
               var2.1se = .06*YearlyACS + .09*(1-YearlyACS),
               var3.1se = .04*YearlyACS + .09*(1-YearlyACS),
               var4.1se = .05*YearlyACS + .1*(1-YearlyACS))

SimulatedData <- SimulatedData %>%
        mutate(var1.5se = .01/5*YearlyACS + .05/5*(1-YearlyACS),
               var2.5se = .06/5*YearlyACS + .09/5*(1-YearlyACS),
               var3.5se = .04/5*YearlyACS + .09/5*(1-YearlyACS),
               var4.5se = .05/5*YearlyACS + .1/5*(1-YearlyACS))


##### Generate observed 1-year and 5-year --------------------------------------
# add noise to the observed data (hat)

SimulatedData <- SimulatedData %>%
        mutate(var1hat.5yr = ifelse(var1.5yr == "NA", "NA", 
                                    rnorm(n(), var1.5yr, var1.5se)),
               var2hat.5yr = ifelse(var2.5yr == "NA", "NA", 
                                    rnorm(n(), var2.5yr, var2.5se)),
               var3hat.5yr = ifelse(var3.5yr == "NA", "NA", 
                                    rnorm(n(), var3.5yr, var3.5se)),
               var4hat.5yr = ifelse(var4.5yr == "NA", "NA", 
                                    rnorm(n(), var4.5yr, var4.5se)),
               var1hat.1yr = ifelse(var1.1yr == "NA", "NA", 
                                    rnorm(n(), var1.1yr, var1.1se)),
               var2hat.1yr = ifelse(var2.1yr == "NA", "NA", 
                                    rnorm(n(), var2.1yr, var2.1se)),
               var3hat.1yr = ifelse(var3.1yr == "NA", "NA", 
                                    rnorm(n(), var3.1yr, var3.1se)),
               var4hat.1yr = ifelse(var4.1yr == "NA", "NA", 
                                    rnorm(n(), var4.1yr, var4.1se)))


##### Spatial Autocorrelation --------------------------------------------------
# THIS DATA SETS DON't HAVE SPACE-TIME COMPONENTS
# simulate the f_it random effect using a proper CAR model 
# with say, 0.9 spatial autocorrelation
# D <- diag(rowSums(A))
# tausq.f <- .05
# rho.f <- .8
# phi.f <- .9
# f <- rep(0,n*T)
# f0 <- rep(0,n*T)
# 
# f0[1:n] <- mvrnorm(1, rep(0,n), tausq.f * solve(D-rho.f * A))
# f[1:n] <- f0[1:n] -mean(f0[1:n])
# 
# for (t in 2:T){
#         f0[((t-1)*n+1):(t*n)] <- mvrnorm(1,phi.f*f[((t-2)*n+1):((t-1)*n)],tausq.f*solve(D-rho.f*A))
#         f[((t-1)*n+1):(t*n)] <- f0[((t-1)*n+1):(t*n)] - mean(f0[((t-1)*n+1):(t*n)])
# }
# 
# SimulatedData$f.RE[which(SimulatedData$time>4)] <- f # note the first few time periods are missing

######################################
# Simulate State-level One year Data #
######################################

##### Observed state-level one year data ---------------------------------------

# one year mean = weighted average of true one year (weight = 1/5yr_se)

SimulatedData_state <- SimulatedData %>%
        group_by(time) %>%
        summarize(var1.1yr = weighted.mean(var1, 1/var1.1se),  #1/var1.1se
                  var2.1yr = weighted.mean(var2, 1/var2.1se),
                  var3.1yr = weighted.mean(var3, 1/var3.1se),
                  var4.1yr = weighted.mean(var4, 1/var4.1se))

# SimulatedData_state <- SimulatedData %>%
#         group_by(time) %>%
#         summarize(var1.1yr = mean(var1),  #1/var1.1se
#                   var2.1yr = mean(var2),
#                   var3.1yr = mean(var3),
#                   var4.1yr = mean(var4))


head(SimulatedData_state)

# standard error
YearlyACS <- c(rep(1, each = 40), rep(0, each = 60))
w1 <- .1*YearlyACS + .05*(1-YearlyACS)
w2 <- .07*YearlyACS + .09*(1-YearlyACS)
w3 <- .08*YearlyACS + .09*(1-YearlyACS)
w4 <- .1*YearlyACS + .1*(1-YearlyACS)



SimulatedData_state <- SimulatedData_state %>%
        mutate(var1.1se = 100 / sum(1/w1)^2 * 1.5,
               var2.1se = 100 / sum(1/w2)^2 * 1.5,
               var3.1se = 100 / sum(1/w3)^2 * 1.5,
               var4.1se = 100 / sum(1/w4)^2 * 1.5)

SimulatedData_state <- SimulatedData_state %>%
        mutate(var1hat.1yr = rnorm(n(), var1.1yr, var1.1se),
               var2hat.1yr = rnorm(n(), var2.1yr, var2.1se),
               var3hat.1yr = rnorm(n(), var3.1yr, var3.1se),
               var4hat.1yr = rnorm(n(), var4.1yr, var4.1se))


##### Generate M = 100 different sets of observed Y ----------------------------

# generate M = 100 different sets of "observed" data y based on the *actual* X 

M <- 100 ### total number of simulated data sets
beta <- c(1, 2, -3, 1, 0.5) # same sign for var 3 and var 4

# 100 data sets for uninterrupted time series :
# create an empty matrix to store M = 100 results
y.sim <- matrix(NA,n*(T+4),M) 

# add column for location & time & true data & 1-year and 5-year observed data
y.sim <- cbind(SimulatedData, y.sim)

c <- dim(SimulatedData)[2]

# simulate the outcome variable using the *true* data
for (m in 1:M){
        y.sim[, m+c] <- rnorm(n*(T+4), 
                              beta[1] + beta[2] * y.sim$var1 + beta[3] * y.sim$var2 
                              + beta[4] * y.sim$var3 + beta[5] * y.sim$var4,
                              #+ y.sim$f.RE,
                              sd = 0.5) # increase from 0.1 to 0.5 to because we removed f
}
#######################
# Save Simulated Data #
#######################

# Save simulated data so that we can use existing script
# First, make sure to arrange by GEOID and Year

y.sim <- y.sim %>%
        arrange(time, location)

N <- n
N1 <- length(Ind1and5)
oyInd <- Ind1and5
num <- colSums(A)
adj <- NULL

for (j in 1:n){
        adj <-c (adj, which(A[j,] == 1))
}

adj<-as.vector(adj)
num<-as.vector(num)

# X1
X1 <- y.sim %>%
        arrange(time, location) %>%
        filter(YearlyACS == 1) %>%
        filter(time > 4) %>%
        dplyr::select(var1hat.1yr, var2hat.1yr, var3hat.1yr, var4hat.1yr)

dim(X1)/l

# X1_se
se1 <- y.sim %>%
        arrange(time, location) %>%
        filter(YearlyACS == 1) %>%
        filter(time > 4) %>%
        dplyr::select(var1.1se, var2.1se, var3.1se, var4.1se)

dim(se1)/l

# X5
X5 <- y.sim %>%
        arrange(time, location) %>%
        filter(time > 4) %>%
        dplyr::select(var1hat.5yr, var2hat.5yr, var3hat.5yr, var4hat.5yr) 

dim(X5)

# X5_se
se5 <- y.sim %>%
        arrange(time, location) %>%
        filter(time > 4) %>%
        dplyr::select(var1.5se, var2.5se, var3.5se, var4.5se)

dim(se5)

# NC1
NC1 <- SimulatedData_state %>%
        arrange(time) %>%
        filter(time > 4) %>%
        dplyr::select(var1hat.1yr, var2hat.1yr, var3hat.1yr, var4hat.1yr)

# NC1_se
NCse1 <- SimulatedData_state %>%
        arrange(time) %>%
        filter(time > 4) %>%
        dplyr::select(var1.1se, var2.1se, var3.1se, var4.1se)

dim(NCse1)

y <- y.sim %>%
        arrange(time, location) %>%
        filter(time > 8)

y <- y[, 34:133]
dim(y)
names(y)

# True Value
X <- y.sim[, c("time", "location", "var1", "var2", "var3", "var4")] |>
        arrange(time, location) |>
        filter(time > 4)

# Some data for plotting later
colnames <- c("location", "time",
              "var1", "var2", "var3", "var4",
              "var1hat.1yr", "var1hat.5yr", "var2hat.1yr", "var2hat.5yr",
              "var3hat.1yr", "var3hat.5yr", "var4hat.1yr", "var4hat.5yr")

temp <- SimulatedData_state
names(temp) <- str_c(names(temp), "_state")
temp <- temp %>%
        rename(time = time_state)

plot_data <- y.sim[, colnames] %>%
        left_join(temp[, c("time", "var3.1yr_state", "var4.1yr_state",
                           "var3hat.1yr_state", "var4hat.1yr_state")], by = "time") %>%
        arrange(location, time)

##### save data ----------------------------------------------------------------

save(N, N1, oyInd, adj, num,
     se5, se1, NCse1,
     y, X5, X1, NC1, X,
     plot_data,
     file = here("Simulation/additional-simul/simul-data-no-sp.Rda"))

