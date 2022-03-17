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
  lyl_age = 95
})

dat <- list()
fig <- list()
tab <- list()

# Function --------------------------------------------------------

# simple piecewise-exponential life-table
CalculateLifeTable <-
  function (df, x, nx = c(rep(1,100),Inf), Dx, Ex, lyl_age = cnst$lyl_age) {

    require(dplyr)

    df %>%
      transmute(
        x = {{x}},
        nx = {{nx}},
        mx = {{Dx}}/{{Ex}},
        px = exp(-mx*{{nx}}),
        qx = 1-px,
        lx = head(cumprod(c(1, px)), -1),
        dx = c(-diff(lx), tail(lx, 1)),
        Lx = ifelse(mx==0, lx*nx, dx/mx),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx,
        sd0 =  c(sqrt(sum(dx*(x+.5-ex[1])^2)), rep(0,length(x)-1)),
        lyl_x = nx*(1-lx)+(nx-0.5)*dx,
        lyl= c(sum(lyl_x[1:96]), lyl_x[-length(lyl_x)])
      )

  }

#function to return life expectancy 
life_expectancy_fun <-
  function (x,nx = c(rep(1,100),Inf),Dx,Ex, age = 0){
    mx <- {{Dx}}/{{Ex}}
    px <- exp(-mx*{{nx}})
    qx <- 1-px
    lx <- head(cumprod(c(1, px)), -1)
    dx <- c(-diff(lx), tail(lx, 1))
    Lx <- ifelse(mx==0, lx*nx, dx/mx)
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx/lx
    
    return(ex[age])
  }



# Data ------------------------------------------------------------

dat$deaths_input_100 <- readRDS(glue('{cnst$path_out}/deaths_input_100.rds'))

# input data for life-table calculation

dat$lt_input_100 <- dat$deaths_input_100[, .(total_deaths = sum(deaths_cause)), by = .(year,sex,age,reth,pop)]

# Calculate annual life tables ------------------------------------

# life-tables by group, sex, and year
# open age_group 100+
dat$lt_100 <-
  dat$lt_input_100 %>%
  arrange(reth, sex, year, age) %>%
  group_by(reth, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(df =.x,x = age,Dx = total_deaths,Ex = pop)
  }) %>%
  ungroup()


# Create Poisson life-table replicates ----------------------------

# create life table replicates by group, sex, and year
# based on repeatedly sampling death counts from a Poisson
dat$lt_100_sim <-
  dat$lt_input_100%>%
  expand_grid(id_sim = 1:cnst$n_sim) %>%
  group_by(reth, sex, year, age) %>%
  mutate(death_total_sim = rpois(cnst$n_sim, total_deaths)) %>%
  arrange(age,reth, sex, year) %>%
  group_by(id_sim, reth, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(df =.x,x = age,Dx = death_total_sim,Ex = pop)
  }) %>%
  ungroup()

# Assemble table with ex statistics -------------------------------

# central estimates of life-expectancy, annual life-expectancy difference,
# and average annual life-expectancy difference 2015 to 2020

# 95% uncertainty intervals around the central estimates
dat$lt_ex_ci <-
  dat$lt_100_sim %>%
  filter(year %in% cnst$years_ind) %>%
  select(id_sim, reth, sex, year, x, mx, ex, sd0,lyl) %>%
  arrange(id_sim, reth, sex, x, year) %>%
  group_by(reth, sex, x, year) %>%
  summarise(
    ex_q025 = quantile(ex, 0.025, na.rm = TRUE),
    ex_q975 = quantile(ex, 0.975, na.rm = TRUE),
    sd0_q025 = quantile(sd0, 0.025, na.rm = TRUE),
    sd0_q975 = quantile(sd0, 0.975, na.rm = TRUE),
    lyl_q025 = quantile(lyl, 0.025, na.rm = TRUE),
    lyl_q975 = quantile(lyl, 0.975, na.rm = TRUE),
  )


# assemble all the ex statistics in a single table
# for further computation
dat$lt_ex_long <-
  left_join(
    dat$lt_100,
    dat$lt_ex_ci
  )



# Export ----------------------------------------------------------

# save the regrouped life table input data
saveRDS(dat$lt_ex_long, file = glue('{wd}/out/lt_CI.rds'))


saveRDS(dat$lt_100_sim, file = glue('{wd}/out/lt_sims.rds'))

# Decomposition over time by causes of death ------------------------------
life_expectancy_from_mx_fun <-
  function (mx,x,nx = c(rep(1,100),Inf),age = 0){
    px <- exp(-mx*{{nx}})
    qx <- 1-px
    lx <- head(cumprod(c(1, px)), -1)
    dx <- c(-diff(lx), tail(lx, 1))
    Lx <- ifelse(mx==0, lx*nx, dx/mx)
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx/lx
    
    return(ex[age+1])
  }


life.expectancy.cod.fun <-
  function(mx.cod, x, nx = c(rep(1,100),Inf),cond_age = 0){
    dim(mx.cod) <- c(length(x),length(mx.cod)/length(x))
    mx          <- rowSums(mx.cod)
    life_expectancy_from_mx_fun(mx,x,nx,cond_age)
  }

# get cause specific mx
dat$deaths_input_100[, mx_cause := deaths_cause/pop]
dat$deaths_input_100[, mx_total := total_deaths/pop]

#double check that life expectancy function is working 
dat$lt_ex_long <- data.table(dat$lt_ex_long)
dat$check_1 <- dat$lt_ex_long[x == 0]
dat$e0_from_new_fun <- dat$deaths_input_100[ cause == 'cvd', .(e0 = life_expectancy_from_mx_fun(mx = mx_total,x = age )), by = .(year,sex,reth)]
dat$check_1 <- merge(dat$check_1,dat$e0_from_new_fun,by = c('year','sex','reth'))
dat$check_1[,diff_1 := ex - e0]
dat$e0_from_new_fun_2 <- dat$deaths_input_100[, .(e0_2 = life.expectancy.cod.fun (mx.cod = mx_cause,x = 0:100)), by = .(year,sex,reth)]
dat$check_1 <- merge(dat$check_1,dat$e0_from_new_fun_2,by = c('year','sex','reth'))
dat$check_1[,diff_2 := ex - e0_2]
# no difference


# check causes
unique(dat$deaths_input_100$cause)

#DT <- dat$deaths_input_100[sex %in% 0 & reth %in% 1]
#y <- 2011
#mx.cod <- mx1

Decomp_fun_time <-
  function(DT = .SD, cond_age = 0){
    x          <- sort(unique(DT$age))
    years      <- sort(unique(DT$year))
    nx         <- c(rep(1,100),Inf)
    
    
    decomp <- mclapply(years[-1],function(y,x =x, nx = nx , m = m, N = N, cond_age = cond_age){
      
        mx2  <- c(m[year == y]$mx_cause)
        mx1  <- c(m[year == y-1]$mx_cause)
      
      
      hor  <- horiuchi(func = life.expectancy.cod.fun,pars1 = mx1,pars2 = mx2,N = N,x =x, nx = nx, cond_age)
      
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


dat$decomposition_results_time <- dat$deaths_input_100[, 
                                                       Decomp_fun_time(DT = .SD,cond_age = 0), 
                                                       by = .(reth,sex)]


#Do some checks
dat$check_1 <- dat$check_1[order(reth,sex,year),]

dat$check_1_dif <- dat$check_1[,.(year.final = year[-1], diff(e0_2)), by = .(sex,reth)]

dat$check_2 <- dat$decomposition_results_time[, sum(contribution), by = .(year.final,reth,sex)]

dat$check_2 <- merge(dat$check_2, dat$check_1_dif)

dat$check_2[,dif_check := V1-V2]


saveRDS(dat$decomposition_results_time, file = glue('{wd}/out/decomposition_time_reth.rds'))


# Decomposition by racial/ethnic groups -----------------------------------
#Reth	race/ethnic group variable, should have labels (1 "NHW" 2 "NHB" 3 "LatinX" 4 "AI/AN" 5 "Other")	

#DT <- dat$deaths_input_100[sex %in% 0 & year %in% 2010]

Decomp_fun_reth <-
  function(DT = .SD, cond_age = 0){
    x          <- sort(unique(DT$age))
    nx         <- c(rep(1,100),Inf)
    
    
    decomp <- mclapply(1,function(y,x =x, nx = nx , m = m, N = N, cond_age = cond_age){
      
      mx1  <- c(m[reth == 1]$mx_cause)
      mx2  <- c(m[reth == 2]$mx_cause)
      mx3  <- c(m[reth == 3]$mx_cause)
      
      
      # Decomposition whites v blacks
      hor_1  <- horiuchi(func = life.expectancy.cod.fun,pars1 = mx2,pars2 = mx1,N = N,x =x, nx = nx, cond_age)
      
      dim(hor_1) <- c(length(x), length(hor_1)/length(x))
      
      colnames(hor_1) <- unique(m$cause)
      
      rownames(hor_1) <- x
      
      hor_1 <- data.table(melt(hor_1))
      
      names(hor_1) <- c('x','cause','contribution')
      hor_1$decomp_group <- 'white_black'
      
      # Decomposition hispanic v white
      hor_2  <- horiuchi(func = life.expectancy.cod.fun,pars1 = mx1,pars2 = mx3,N = N,x =x, nx = nx, cond_age)
      
      dim(hor_2) <- c(length(x), length(hor_2)/length(x))
      
      colnames(hor_2) <- unique(m$cause)
      
      rownames(hor_2) <- x
      
      hor_2 <- data.table(melt(hor_2))
      
      names(hor_2) <- c('x','cause','contribution')
      hor_2$decomp_group <- 'latino_white'
      
      hor <- rbind(hor_1, hor_2)
      
      hor
      
      
      
    }, m = DT, x = x, nx = nx, N = 50, mc.cores = 1, cond_age)
    
    decomp.results <- data.table(do.call(rbind,decomp))
    
    decomp.results
    
  }


dat$decomposition_results_reth <- dat$deaths_input_100[, 
                                                       Decomp_fun_reth(DT = .SD,cond_age = 0), 
                                                       by = .(year,sex)]


saveRDS(dat$decomposition_results_reth, file = glue('{wd}/out/decomposition_between_reth.rds'))



