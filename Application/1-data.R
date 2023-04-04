###################################################
# 1 - Data Download and Wrangling for Application #
###################################################


# set-up -----------------------------------------------------------------------

library(tidyverse) # data wrangling
library(tidycensus) # download ACS data using API
library(readxl) # read excel file
library(here) # working directory management
library(spdep) # spatial structure
# census_api_key("yourkey",
#                overwrite = TRUE,
#                install = TRUE) # enter your api key to install (do it only once)
readRenviron("~/.Renviron") # reload R env
Sys.getenv("CENSUS_API_KEY") # check if the API key is ready to use

# ACS data info ----------------------------------------------------------------

# * Variable names are found on census developers API page.
# + https://api.census.gov/data/2014/acs/acs5/profile/variables.html
# 
# * Variable names are the same for both 1-year and 5-year estimates.
# 
# * Variable names (2010 - 2018) 
# + Percentage
# 1. % of families below the poverty level (2010-18: DP03_0119PE)
# 1. % of female population (2010-18: DP05_0003PE)
# 1. % of the black population (2010-16: DP05_0033PE, 2017-18: DP05_0038PE)
# 1. % of married couples (2010-16: DP02_0004PE)
# 1. % of the population 25 years and over who are college-educated (2010-18: DP02_0067PE)
# 1. % of unemployed population of population 16 years and over (2010-18: DP03_0005PE)
# 
# 
# + Estimate
# 1. Median age (2010-16: DP05_0017E, 2017-18: DP05_0018E)
# 1. Travel time to work (minutes) (2010-18: DP03_0025E)
# 
# 
# + Factor
# 1. RUCC 2013
# 
# 
# * 2013 Rural-urban Continuum Codes
# 
# | Code | Description |
# |------|------------------------------------------------------------|
# | 1 | Counties in metro areas of 1 million population or more |
# | 2 | Counties in metro areas of 250,000 to 1 million population |
# | 3 | Counties in metro areas of fewer than 250,000 population |
# | 4 | Urban population of 20,000 or more, adjacent to a metro area |
# | 5 | Urban population of 20,000 or more, not adjacent to a metro area |
# | 6 | Urban population of 2,500 to 19,999, adjacent to a metro area |
# | 7 | Urban population of 2,500 to 19,999, not adjacent to a metro area |
# | 8 | Completely rural or less than 2,500 urban population, adjacent to a metro area |
# | 9 | Completely rural or less than 2,500 urban population, not adjacent to a metro area |


# ACS data download (County level) ---------------------------------------------

## variable codes
## year 2010-16
vars1 <- c("DP03_0119P", "DP05_0003P", "DP05_0033P", "DP02_0004P", "DP02_0067P", "DP03_0005P")
pct1 <- c("DP03_0119PE", "DP05_0003PE", "DP05_0033PE", "DP02_0004PE", "DP02_0067PE", "DP03_0005PE")
moe1 <- c("DP03_0119PM", "DP05_0003PM", "DP05_0033PM", "DP02_0004PM", "DP02_0067PM", "DP03_0005PM")

## year 2017-18
vars2 <- c("DP03_0119P", "DP05_0003P", "DP05_0038P", "DP02_0004P", "DP02_0067P", "DP03_0005P")
pct2 <- c("DP03_0119PE", "DP05_0003PE", "DP05_0038PE", "DP02_0004PE", "DP02_0067PE", "DP03_0005PE")
moe2 <- c("DP03_0119PM", "DP05_0003PM", "DP05_0038PM", "DP02_0004PM", "DP02_0067PM", "DP03_0005PM")

## create a variable name vector
names1 <- c("GEOID", "Year", "Name", 
            "Poverty_5yr", "Female_5yr", "Black_5yr", "Married_5yr", "CollegeEd_5yr", "Unemployed_5yr")

names2 <- c("GEOID", "Year", "Name", 
            "Poverty_1yr", "Female_1yr", "Black_1yr", "Married_1yr", "CollegeEd_1yr", "Unemployed_1yr")

## proportion data
## download 5-year ACS

## create an empty data set
acs_county_pct5 <- tibble() # save proportion data
acs_county_moe5 <- tibble() # save corresponding MOE

## save data
for (yr in 2010:2018) {
        
        if (yr %in% c(2017, 2018)) {
                vars <- vars2
                pct <- pct2
                moe <- moe2
        } else {
                vars <- vars1
                pct <- pct1
                moe <- moe1
        }
        
        ## extract data from ACS API
        acs <- get_acs(geography = "county",
                       variables = vars,
                       state = "NC",
                       output = "wide",
                       year = yr)
        
        ## store percentage estimates
        pct_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% pct)])
        names(pct_temp) <- names1
        
        ## store margin of errors
        moe_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% moe)])
        names(moe_temp) <- names1
        
        
        acs_county_pct5 <- rbind(acs_county_pct5, pct_temp)
        acs_county_moe5 <- rbind(acs_county_moe5, moe_temp)
        
}

## download 1-year ACS
## create an empty data set
acs_county_pct1 <- tibble() 
acs_county_moe1 <- tibble()

## save data
for (yr in 2010:2018) {
        
        if (yr %in% c(2017, 2018)) {
                vars <- vars2
                pct <- pct2
                moe <- moe2
        } else {
                vars <- vars1
                pct <- pct1
                moe <- moe1
        }
        
        ## extract data from ACS API
        acs <- get_acs(geography = "county",
                       variables = vars,
                       state = "NC",
                       output = "wide",
                       survey = "acs1",
                       year = yr)
        
        ## store percentage estimates
        pct_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% pct)])
        names(pct_temp) <- names2
        
        ## store margin of errors
        moe_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% moe)])
        names(moe_temp) <- names2
        
        
        acs_county_pct1 <- rbind(acs_county_pct1, pct_temp)
        acs_county_moe1 <- rbind(acs_county_moe1, moe_temp)
        
}

## non proportion data
## year 2010-16
vars3 <- c("DP05_0017", "DP03_0025")
est3 <- c("DP05_0017E", "DP03_0025E")
moe3 <- c("DP05_0017M", "DP03_0025M")

## year 2017-18
vars4 <- c("DP05_0018", "DP03_0025")
est4 <- c("DP05_0018E", "DP03_0025E")
moe4 <- c("DP05_0018M", "DP03_0025M")

## create a variable name vector
names3 <- c("GEOID", "Year", "Name", 
            "MedianAge_5yr", "Commute_5yr")

names4 <- c("GEOID", "Year", "Name", 
            "MedianAge_1yr", "Commute_1yr")

## download 5-year ACS
## create an empty data set
acs_county_est5 <- tibble()
acs_county_est_moe5 <- tibble()

## save data
for (yr in 2010:2018) {
        
        if (yr %in% c(2017, 2018)) {
                vars <- vars4
                est <- est4
                moe <- moe4
        } else {
                vars <- vars3
                est <- est3
                moe <- moe3
        }
        
        ## extract data from ACS API
        acs <- get_acs(geography = "county",
                       variables = vars,
                       state = "NC",
                       output = "wide",
                       year = yr)
        
        ## store estimates
        est_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% est)])
        names(est_temp) <- names3
        
        ## store margin of errors
        moe_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% moe)])
        names(moe_temp) <- names3
        
        
        acs_county_est5 <- rbind(acs_county_est5, est_temp)
        acs_county_est_moe5 <- rbind(acs_county_est_moe5, moe_temp)
        
}

## download 1-year ACS
## create an empty data set
acs_county_est1 <- tibble()
acs_county_est_moe1 <- tibble()

## save data
for (yr in 2010:2018) {
        
        if (yr %in% c(2017, 2018)) {
                vars <- vars4
                est <- est4
                moe <- moe4
        } else {
                vars <- vars3
                est <- est3
                moe <- moe3
        }
        
        ## extract data from ACS API
        acs <- get_acs(geography = "county",
                       variables = vars,
                       state = "NC",
                       output = "wide",
                       survey = "acs1",
                       year = yr)
        
        ## store percentage estimates
        est_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% est)])
        names(est_temp) <- names4
        
        ## store margin of errors
        moe_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% moe)])
        names(moe_temp) <- names4
        
        
        acs_county_est1 <- rbind(acs_county_est1, est_temp)
        acs_county_est_moe1 <- rbind(acs_county_est_moe1, moe_temp)
        
}

## combine data

acs_county5 <- full_join(acs_county_pct5, acs_county_est5,
                         by = c("GEOID", "Year", "Name"))
acs_county1 <- full_join(acs_county_pct1, acs_county_est1,
                         by = c("GEOID", "Year", "Name"))
acs_county5_moe <- full_join(acs_county_moe5, acs_county_est_moe5,
                             by = c("GEOID", "Year", "Name"))
acs_county1_moe <- full_join(acs_county_moe1, acs_county_est_moe1,
                             by = c("GEOID", "Year", "Name"))

# ACS data download (State level) ---------------------------------------------

## proportion data
## create an empty data set
acs_state_pct1 <- tibble()
acs_state_moe1 <- tibble()

## save data
for (yr in 2010:2018) {
        
        if (yr %in% c(2017, 2018)) {
                vars <- vars2
                pct <- pct2
                moe <- moe2
        } else {
                vars <- vars1
                pct <- pct1
                moe <- moe1
        }
        
        ## extract data from ACS API
        acs <- get_acs(geography = "state",
                       variables = vars,
                       output = "wide",
                       survey = "acs1",
                       year = yr)
        
        ## store percentage estimates
        pct_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% pct)])
        names(pct_temp) <- names2
        
        ## store margin of errors
        moe_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% moe)])
        names(moe_temp) <- names2
        
        
        acs_state_pct1 <- rbind(acs_state_pct1, pct_temp)
        acs_state_moe1 <- rbind(acs_state_moe1, moe_temp)
        
}

acs_state_pct1 <- acs_state_pct1 %>% 
        filter(GEOID == 37)
acs_state_moe1 <- acs_state_moe1 %>%
        filter(GEOID == 37)

## non-proportion data
## create an empty data set
acs_state_est1 <- tibble()
acs_state_est_moe1 <- tibble()

## save data
for (yr in 2010:2018) {
        
        if (yr %in% c(2017, 2018)) {
                vars <- vars4
                est <- est4
                moe <- moe4
        } else {
                vars <- vars3
                est <- est3
                moe <- moe3
        }
        
        ## extract data from ACS API
        acs <- get_acs(geography = "state",
                       variables = vars,
                       output = "wide",
                       survey = "acs1",
                       year = yr)
        
        ## store percentage estimates
        est_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% est)])
        names(est_temp) <- names4
        
        ## store margin of errors
        moe_temp <- cbind(acs$GEOID,
                          rep(yr, nrow(acs)),
                          acs$NAME,
                          acs[, which(names(acs) %in% moe)])
        names(moe_temp) <- names4
        
        
        acs_state_est1 <- rbind(acs_state_est1, est_temp)
        acs_state_est_moe1 <- rbind(acs_state_est_moe1, moe_temp)
        
}

acs_state_est1 <- acs_state_est1 %>% 
        filter(GEOID == 37)
acs_state_est_moe1 <- acs_state_est_moe1 %>%
        filter(GEOID == 37)

## combine data

acs_state1 <- full_join(acs_state_pct1, acs_state_est1,
                        by = c("GEOID", "Year", "Name"))
acs_state1_moe <- full_join(acs_state_moe1, acs_state_est_moe1,
                            by = c("GEOID", "Year", "Name"))
# RUCC data --------------------------------------------------------------------

#  2013 Rural-urban Continuum Codes
# 
# | Code | Description |
# |------|------------------------------------------------------------|
# | 1 | Counties in metro areas of 1 million population or more |
# | 2 | Counties in metro areas of 250,000 to 1 million population |
# | 3 | Counties in metro areas of fewer than 250,000 population |
# | 4 | Urban population of 20,000 or more, adjacent to a metro area |
# | 5 | Urban population of 20,000 or more, not adjacent to a metro area |
# | 6 | Urban population of 2,500 to 19,999, adjacent to a metro area |
# | 7 | Urban population of 2,500 to 19,999, not adjacent to a metro area |
# | 8 | Completely rural or less than 2,500 urban population, adjacent to a metro area |
# | 9 | Completely rural or less than 2,500 urban population, not adjacent to a metro area |
#         

metro_extract <- read_excel(here("Application/ruralurbancodes2013.xls"),
                            sheet = "Rural-urban Continuum Code 2013")

rucc <- metro_extract[, c("FIPS", "RUCC_2013")] %>% rename(GEOID = FIPS)

# Combine ACS + RUCC (Covariates) ----------------------------------------------

## Combine Tables
county_5yr <- left_join(acs_county5, rucc,
                        by = "GEOID")
county_1yr <- left_join(acs_county1, rucc,
                        by = "GEOID")
county_both <- left_join(county_5yr, county_1yr,
                         by = c("GEOID", "Year", "Name")) %>%
        mutate(RUCC_2013 = RUCC_2013.x) %>%
        select(-c(RUCC_2013.x, RUCC_2013.y))

# FMD data ---------------------------------------------------------------------

# -   Frequent Mental Distress from County Health Rankings
# -   Percentage of adults reporting 14 or more days of poor mental health per month (age-adjusted)
# 
# 1.  2016 Rankings used data from 2014 for this measure
# 2.  2017 Rankings used data from 2015
# 3.  2018 Rankings used data from 2016
# 4.  2019 Rankings used data from 2016
# 5.  2020 Rankings used data from 2017
# 6.  2021 Rankings used data from 2018

path <- paste0(here("Application"))
setwd(path)

filename <- "County Health Rankings North Carolina Data"
varname <- c("...1", "...2", "...3", "Frequent mental distress")

## create a function to extract Frequent mental distress

mental_extract <- function(rankingyr, datayr, format) {
        temp <- read_excel(paste0(rankingyr, " ", filename, ".", format),
                           sheet = "Additional Measure Data")
        temp <- temp[, varname]
        data <- temp[-1, ]
        colnames(data) <- temp[1, ]
        data <- cbind(data, Year = datayr)
}


## check if rankings 18 == rankings 19 (both used data from 2016)

data16 <- mental_extract(2018, 2016, "xls")
data162 <- mental_extract(2019, 2016, "xls")
data16 == data162

## extract years 2014-2018
data14 <- mental_extract(2016, 2014, "xls")
data15 <- mental_extract(2017, 2015, "xls")
data16 <- mental_extract(2018, 2016, "xls")
data17 <- mental_extract(2020, 2017, "xlsx")
data18 <- mental_extract(2021, 2018, "xlsx")

## combine data
mental_temp <- rbind(data14, data15, data16, data17, data18)

## delete rows with county = NA or empty rows
mental <- mental_temp[which(mental_temp$County != "NA"), ]

## coerce characters to Numeric
mental[, "% Frequent Mental Distress"] <- as.numeric(mental[, "% Frequent Mental Distress"])

## relocate "Year" and change column names 
fmd <- mental %>% 
        mutate(fmd = mental[, 4],
               GEOID = FIPS) %>%
        select(GEOID, Year, fmd)

# Adjacency Matrix for NC ------------------------------------------------------

nc <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
st_crs(nc) <- "+proj=longlat +datum=NAD27"

nc <- arrange(nc, FIPSNO) # reorder them so that we can match with our data later

row.names(nc) <- as.character(nc$FIPSNO)
W.nb <- poly2nb(nc, row.names = rownames(nc$NAME))
nbInfo <- nb2WB(W.nb)

# Transform data (logit/log) and get corresponding SEs from MOE ---------------- 

# create a logit function

logit <- function(x, na.rm = T) {
        log(x) - log((1-x))
}
# create a estimations dataset
est <- left_join(county_both, fmd, by = c("GEOID", "Year")) %>%
        arrange(Year, GEOID) %>%
        select(-c(fmd, RUCC_2013, Name))

est_nc <- acs_state1 %>% 
        arrange(Year, GEOID) %>%
        select(-c(Name))

table(est[1:100, 1] == row.names(nc)) # make sure that the order of dataset and adjacency matrix match

# create a s.e dataset
# note that moe / 1.645 = s.e (90% two-sided MOE)

moe <- left_join(acs_county5_moe, acs_county1_moe,
                 by = c("GEOID", "Year", "Name")) %>%
        arrange(Year, GEOID) %>%
        select(-Name)

moe_nc <- acs_state1_moe %>%
        arrange(Year, GEOID) %>%
        select(-c(Name))

table(est[, 1:2] == moe[, 1:2]) # same order
table(est_nc[, 1:2] == moe_nc[, 1:2]) # same order

## create a function that will convert moe to se

moe_to_se <- function(x, na.rm = TRUE) {
        x / qnorm(0.95) # upper tail 5% & both tail 10%
}

se <- moe %>%
        mutate_at(c(3:18), moe_to_se, na.rm = TRUE)

se_nc <- moe_nc %>%
        mutate_at(c(3:10), moe_to_se, na.rm = TRUE)

head(se)
head(se_nc)

# check the order of est/se with fmd
table(est[1:100, 1] == fmd[1:100, 1]) # GEOID order same


# Data is in % format
# Create a function that divides number by 100
prop <- function(x, na.rm = T) {
        x/100
}

# index
N <- length(unique(est$GEOID)) # 100 counties

# information needed for CAR models
adj <- as.vector(nbInfo$adj)
num <- as.vector(nbInfo$num)

# constants
oneyear <- est$oneyear
X5.demo <- est[, c(4, 5, 7, 9)] # 5-Year Estimates year 2010 - 2018
X5.socio <- est[, c(3, 6, 8, 10)] 
se5.demo <- se[, c(4, 5, 7, 9)] # 5-Year s.e year 2010 - 2018
se5.socio <- se[, c(3, 6, 8, 10)]
X5 <- cbind(X5.demo, X5.socio) %>%
        mutate_at(c(1:3, 5:7), prop, na.rm = T) #4-th and 8-th estimator aren't %
se5 <- cbind(se5.demo, se5.socio) %>%
        mutate_at(c(1:3, 5:7), prop, na.rm = T)


# Index for counties with 1-year estimates over the entire observation period
oyList <- est %>% na.omit() %>% count(GEOID) %>% filter(n == 9) %>% select(GEOID)
oyList <- as.double(oyList[,1])

N1 <- length(oyList)

est1 <- est %>% filter(GEOID %in% oyList)
se1 <- se %>% filter(GEOID %in% oyList)

# 1-Year Estimates year 2010 - 2018
X1.demo <- est1 %>% select(c("Female_1yr", "Black_1yr", "CollegeEd_1yr", "MedianAge_1yr")) 
X1.socio <- est1 %>% select(c("Poverty_1yr", "Married_1yr", "Unemployed_1yr", "Commute_1yr"))
se1.demo <- se1 %>% select(c("Female_1yr", "Black_1yr", "CollegeEd_1yr", "MedianAge_1yr"))
se1.socio <- se1 %>% select(c("Poverty_1yr", "Married_1yr", "Unemployed_1yr", "Commute_1yr"))
X1 <- cbind(X1.demo, X1.socio) %>%
        mutate_at(c(1:3, 5:7), prop, na.rm = T) #4-th and 8-th estimator aren't %
se1 <- cbind(se1.demo, se1.socio) %>%
        mutate_at(c(1:3, 5:7), prop, na.rm = T)

oyInd <- which(est$GEOID[1:N] %in% est1$GEOID[1:N1])

# recategorize RUCC (9 levels -> 3 levels)

X.rucc <- county_both %>% 
        mutate(metro = case_when(RUCC_2013 %in% c(1, 2, 3) ~ 1,
                                 TRUE ~ 0),
               urban = case_when(RUCC_2013 %in% c(4, 5, 6, 7) ~ 1,
                                 TRUE ~ 0)) %>% # base = rural
        select(c(metro, urban)) # Year 2015 - 2018

# data
y <- as.numeric(fmd$fmd / 100) 
y.t <- logit(y)

# check order match
table(fmd[1:100, 1] == est[1:100, 1])
table(se[1:100, 1] == est[1:100, 1])
table(county_both[1:100, 1] == est[1:100, 1])

# 1-Year estimates for NC 

NC1.demo <- est_nc %>% select(c("Female_1yr", "Black_1yr", "CollegeEd_1yr", "MedianAge_1yr")) 
NC1.socio <- est_nc %>% select(c("Poverty_1yr", "Married_1yr", "Unemployed_1yr", "Commute_1yr"))
NC1.se.demo <- se_nc %>% select(c("Female_1yr", "Black_1yr", "CollegeEd_1yr", "MedianAge_1yr"))
NC1.se.socio <- se_nc %>% select(c("Poverty_1yr", "Married_1yr", "Unemployed_1yr", "Commute_1yr"))

NC1 <- cbind(NC1.demo, NC1.socio) %>%
        mutate_at(c(1:3, 5:7), prop, na.rm = T) #4-th and 8-th estimator aren't %
NC1.se <- cbind(NC1.se.demo, NC1.se.socio) %>%
        mutate_at(c(1:3, 5:7), prop, na.rm = T)

## Transform the data (Using Delta Method)

## Logit and Log


X5.t <- X5 %>%
        mutate_at(c(4, 8), log) %>% # log transformation
        mutate_at(c(1:3, 5:7), logit)

X1.t <- X1 %>%
        mutate_at(c(4, 8), log) %>% # log transformation
        mutate_at(c(1:3, 5:7), logit)

NC1.t <- NC1 %>%
        mutate_at(c(4, 8), log) %>% # log transformation
        mutate_at(c(1:3, 5:7), logit)

se5.t <- cbind(1 / (X5[, 1:3] * (1 - X5[, 1:3]))^2 * se5[, 1:3]^2,
               1 / (X5[, 4])^2 * se5[, 4]^2,
               1 / (X5[, 5:7] * (1 - X5[, 5:7]))^2 * se5[, 5:7]^2,
               1 / (X5[, 8])^2 * se5[, 8]^2) %>% # Delta Method for Variance
        mutate_at(1:8, sqrt) # get s.e
colnames(se5.t) <- colnames(X5.t)

se1.t <- cbind(1 / (X1[, 1:3] * (1 - X1[, 1:3]))^2 * se1[, 1:3]^2,
               1 / (X1[, 4])^2 * se1[, 4]^2,
               1 / (X1[, 5:7] * (1 - X1[, 5:7]))^2 * se1[, 5:7]^2,
               1 / (X1[, 8])^2 * se1[, 8]^2) %>%
        mutate_at(1:8, sqrt) 
colnames(se1.t) <- colnames(X1.t)

NC1.se.t <- cbind(1 / (NC1[, 1:3] * (1 - NC1[, 1:3]))^2 * NC1.se[, 1:3]^2,
                  1 / (NC1[, 4]) * NC1.se[, 4]^2,
                  1 / (NC1[, 5:7] * (1 - NC1[, 5:7]))^2 * NC1.se[, 5:7]^2,
                  1 / (NC1[, 8])^2 * NC1.se[, 8]^2) %>%
        mutate_at(1:8, sqrt) 
colnames(NC1.se.t) <- colnames(NC1.t)


## Save Data -------------------------------------------------------------------

# Save data so that we can run the program on the cluster

save(N, N1, oyInd, adj, num, y, y.t, X.rucc,
     X5.demo, X5.socio, X5, se5, X5.t, se5.t,
     X1.demo, X1.socio, X1, se1, X1.t, se1.t,
     NC1.demo, NC1.socio, NC1, NC1.se, NC1.t, NC1.se.t,
     file = here("Application/Model1data.Rda"))

