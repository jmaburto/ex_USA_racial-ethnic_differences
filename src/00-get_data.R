# Get data in functional form

# Init ------------------------------------------------------------
library(here); library(glue)
library(tidyverse); library(yaml)
library(patchwork); library(data.table)
library(gt)
library(haven)

# Constants -------------------------------------------------------

wd <- here()
cnst <- list()
config <- read_yaml(glue('{wd}/cfg/config.yaml'))

cnst <- within(cnst, {
  regions_for_analysis = config$regions_for_all_cause_analysis
  path_out = glue('{wd}/out')
  path_tmp = glue('{wd}/tmp')
  # number of Poisson life-table replicates
  n_sim = 500
  min_age = 15
  var_names = c('age','year','sex','reth','total_deaths','cvd','respiratory',
                'infectious','despair','cancer',
                'accidents','covid','rest','pop')
})

dat <- list()
fig <- list()
tab <- list()

# Read data --------------------------------------------------------

##Pop	population count in race/ethnic, sex, year group	
#Mort	total number of deaths  in race/ethnic, sex, year group	
#Reth	race/ethnic group variable, should have labels (1 "NHW" 2 "NHB" 3 "LatinX" 4 "AI/AN" 5 "Other")	
#Male	1=male, 0=female	


#some checks prior

dat$analytic <- data.table(read_dta(glue('{wd}/dat/analytic_le.dta')))

dat$analytic <- dat$analytic[,c(1:12,20:21)]

names(dat$analytic) <- cnst$var_names

#check death rates, mort >  pop, consistency with totals, etc.

dat$analytic[total_deaths > pop]

#move to long format, easier to calculate proportions

dat$deaths_input_100 <- melt.data.table(data = dat$analytic,
                                          id.vars = c('age','year','sex','reth','total_deaths','pop'),
                                          variable.name = 'cause',value.name = 'deaths_cause')

# subset to 1 "NHW" 2 "NHB" 3 "LatinX" 

dat$deaths_input_100 <- dat$deaths_input_100[reth %in% 1:3]

#check for NA's

#view(check_NA)
dat$deaths_input_100[is.na(dat$deaths_input_100$deaths)]
dat$deaths_input_100[is.na(dat$deaths_input_100$total_deaths)]
dat$deaths_input_100[is.na(dat$deaths_input_100$pop)]

#check (cause-specific) = total
sum(dat$deaths_input_100[,sum(deaths_cause) - total_deaths[1], by = .(age,year,sex,reth)]$V1)

#save dataset
saveRDS(dat$deaths_input_100,glue('{cnst$path_out}/deaths_input_100.rds'))


