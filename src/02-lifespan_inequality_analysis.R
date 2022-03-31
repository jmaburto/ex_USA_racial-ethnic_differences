# General life-table analysis


##Pop	population count in race/ethnic, sex, year group	
#Mort	total number of deaths  in race/ethnic, sex, year group	
#Reth	race/ethnic group variable, should have labels (1 "NHW" 2 "NHB" 3 "LatinX" 4 "AI/AN" 5 "Other")	
#Male	1=male, 0=female	


# Init ------------------------------------------------------------
set.seed(1987)

library(data.table)
library(here); library(glue)
library(tidyverse); library(yaml)
library(patchwork)
library(gt)
library(DemoDecomp)
library(doParallel)

# Constants -------------------------------------------------------

wd <- here()
cnst <- list()
cnst <- within(cnst, {
  groups_for_analysis = c('nhw','nhb','latinx')
  path_out = glue('{wd}/out')
  path_tmp = glue('{wd}/tmp')
  # number of Poisson life-table replicates
  n_sim = 500
  years_ind = 2010:2020
})

dat <- list()
fig <- list()
tab <- list()

# Function --------------------------------------------------------


#function to return standad deviation 
sd_fun <-
  function (x,nx = c(rep(1,100),Inf),Dx,Ex, age = 0){
    mx <- {{Dx}}/{{Ex}}
    px <- exp(-mx*{{nx}})
    qx <- 1-px
    lx <- head(cumprod(c(1, px)), -1)
    dx <- c(-diff(lx), tail(lx, 1))
    Lx <- ifelse(mx==0, lx*nx, dx/mx)
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx/lx
    sdx <-  sqrt(sum(dx*(x+.5-ex[age +1])^2))
    
    return(sdx)
  }

# Data ------------------------------------------------------------

dat$deaths_input_100 <- readRDS(glue('{cnst$path_out}/deaths_input_100.rds'))

# input data for life-table calculation

dat$lt_input_100 <- dat$deaths_input_100[, .(total_deaths = sum(deaths_cause)), by = .(year,sex,age,reth,pop)]

# Calculate annual life tables ------------------------------------

# sd by group, sex, and year
# open age_group 100+

dat$sd_results <- dat$lt_input_100[, .(sd = sd_fun(x = age,Dx = total_deaths,Ex = pop)), by = .(year,sex,reth)]

# Export ----------------------------------------------------------

# save the sd data
saveRDS(dat$sd_results, file = glue('{wd}/out/sd_results.rds'))

# Decomposition over time by causes of death ------------------------------
sd_from_mx_fun <-
  function (mx,x,nx = c(rep(1,100),Inf),age = 0){
    px <- exp(-mx*{{nx}})
    qx <- 1-px
    lx <- head(cumprod(c(1, px)), -1)
    dx <- c(-diff(lx), tail(lx, 1))
    Lx <- ifelse(mx==0, lx*nx, dx/mx)
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx/lx
    sdx <-  sqrt(sum(dx*(x+.5-ex[age +1])^2))
    
    return(sdx)
  }


sd.cod.fun <-
  function(mx.cod, x, nx,cond_age = 0){
    dim(mx.cod) <- c(length(x),length(mx.cod)/length(x))
    mx          <- rowSums(mx.cod)
    sd_from_mx_fun(mx,x,nx,cond_age)
  }

# get cause specific mx
dat$deaths_input_100[, mx_cause := deaths_cause/pop]

# check causes
unique(dat$deaths_input_100$cause)

#DT <- dat$deaths_input_100[sex %in% 0 & reth %in% 1]
#y <- 2011
#mx.cod <- mx1

Decomp_fun_time_sd <-
  function(DT = .SD, cond_age = 0){
    x          <- sort(unique(DT$age))
    years      <- sort(unique(DT$year))
    nx         <- c(rep(1,100),Inf)
    
    
    decomp <- mclapply(years[-1],function(y,x =x, nx = nx , m = m, N = N, cond_age = cond_age){
      
        mx2  <- c(m[year == y]$mx_cause)
        mx1  <- c(m[year == y-1]$mx_cause)
      
      
      hor  <- horiuchi(func = sd.cod.fun,pars1 = mx1,pars2 = mx2,N = N,x =x, nx = nx, cond_age)
      
      dim(hor) <- c(length(x), length(hor)/length(x))
      
      colnames(hor) <- unique(m$cause)
      
      rownames(hor) <- x
      
      hor <- data.table(melt(hor))
      
      names(hor) <- c('x','cause','contribution')
      hor$year.final <- as.numeric(y)
      hor
    }, m = DT, x = x, nx = nx, N = 50, mc.cores = 1, cond_age)
    
    decomp.results <- data.table(do.call(rbind,decomp))
    
    decomp.results
    
  }


dat$decomposition_sd_results_time <- dat$deaths_input_100[, 
                                                          Decomp_fun_time_sd(DT = .SD,cond_age = 0), 
                                                       by = .(reth,sex)]


saveRDS(dat$decomposition_sd_results_time, file = glue('{wd}/out/decomposition_time_sd.rds'))

# Decomposition by racial/ethnic groups -----------------------------------
#Reth	race/ethnic group variable, should have labels (1 "NHW" 2 "NHB" 3 "LatinX" 4 "AI/AN" 5 "Other")	

#DT <- dat$deaths_input_100[sex %in% 0 & year %in% 2010]

Decomp_fun_reth_sd <-
  function(DT = .SD, cond_age = 0){
    x          <- sort(unique(DT$age))
    nx         <- c(rep(1,100),Inf)
    
    
    decomp <- mclapply(1,function(y,x =x, nx = nx , m = m, N = N, cond_age = cond_age){
      
      mx1  <- c(m[reth == 1]$mx_cause)
      mx2  <- c(m[reth == 2]$mx_cause)
      mx3  <- c(m[reth == 3]$mx_cause)
      
      
      # Decomposition whites v blacks
      hor_1  <- horiuchi(func = sd.cod.fun,pars1 = mx2,pars2 = mx1,N = N,x =x, nx = nx, cond_age)
      
      dim(hor_1) <- c(length(x), length(hor_1)/length(x))
      
      colnames(hor_1) <- unique(m$cause)
      
      rownames(hor_1) <- x
      
      hor_1 <- data.table(melt(hor_1))
      
      names(hor_1) <- c('x','cause','contribution')
      hor_1$decomp_group <- 'white_black'
      
      # Decomposition hispanic v white
      hor_2  <- horiuchi(func = sd.cod.fun,pars1 = mx1,pars2 = mx3,N = N,x =x, nx = nx, cond_age)
      
      dim(hor_2) <- c(length(x), length(hor_2)/length(x))
      
      colnames(hor_2) <- unique(m$cause)
      
      rownames(hor_2) <- x
      
      hor_2 <- data.table(melt(hor_2))
      
      names(hor_2) <- c('x','cause','contribution')
      hor_2$decomp_group <- 'latino_white'
      
      
      # Decomposition hispanic v black
      hor_3  <- horiuchi(func = sd.cod.fun,pars1 = mx2,pars2 = mx3,N = N,x =x, nx = nx, cond_age)
      
      dim(hor_3) <- c(length(x), length(hor_3)/length(x))
      
      colnames(hor_3) <- unique(m$cause)
      
      rownames(hor_3) <- x
      
      hor_3 <- data.table(melt(hor_3))
      
      names(hor_3) <- c('x','cause','contribution')
      hor_3$decomp_group <- 'latino_black'
      
      hor <- rbind(hor_1, hor_2, hor_3)
      
      hor
      
      
      
    }, m = DT, x = x, nx = nx, N = 50, mc.cores = 1, cond_age = 0)
    
    decomp.results <- data.table(do.call(rbind,decomp))
    
    decomp.results
    
  }


dat$decomposition__sd_results_reth <- dat$deaths_input_100[, 
                                                           Decomp_fun_reth_sd(DT = .SD,cond_age = 0), 
                                                       by = .(year,sex)]


saveRDS(dat$decomposition__sd_results_reth, file = glue('{wd}/out/decomposition_between_reth_sd.rds'))



