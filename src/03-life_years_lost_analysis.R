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
library(reshape2)

# Constants -------------------------------------------------------

wd <- here()
cnst <- list()
cnst <- within(cnst, {
  groups_for_analysis = c('nhw','nhb','latinx')
  path_out = glue('{wd}/out')
  path_tmp = glue('{wd}/tmp')
  # number of Poisson life-table replicates
  n_sim = 500
  years_ind = c(2010,2019,2020)
  lyl_age = 95
})

dat <- list()
fig <- list()
tab <- list()

# Function --------------------------------------------------------

CalculateLifeTable <-
  function (df, x, nx = c(rep(1,100),Inf), Dx, Ex) {
    
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
      )
    
  }


#function to get LYL
LYL_fun <-function(x,nx = c(rep(1,100),Inf),mx,DT){
  COD_prop <- DT[,c('cvd','respiratory','infectious','despair','cancer','accidents','covid','rest')]
  px <- exp(-mx*{{nx}})
  qx <- 1-px
  lx <- head(cumprod(c(1, px)), -1)
  dx <- c(-diff(lx), tail(lx, 1))
  Lx <- ifelse(mx==0, lx*nx, dx/mx)
  Rxi <-  COD_prop
  G3  <- apply(COD_prop,2,cumsum)
  FD  <- cumsum(rowSums(COD_prop))
  fxi <- G3/FD
  ## we use the life table functions to separate the person years and 
  ## person lost
  LYLi<-nx*(1-lx)*fxi+(nx-0.5)*dx*Rxi
  return(LYLi)
}

# DT <- dat$lyl_input_100[year == 2010 & sex == 0 & reth == 1,]
# x <- DT$age
# mx <- DT$mx
# COD_prop <- DT[,c('cvd','respiratory','infectious','despair','cancer','accidents','covid','rest')]
# nx <- c(rep(1,100),Inf)
# plot(dx)

Dxi_fun<-function(x,nx = c(rep(1,100),Inf),mx,DT){
  COD_prop <- DT[,c('cvd','respiratory','infectious','despair','cancer','accidents','covid','rest')]
  px <- exp(-mx*{{nx}})
  qx <- 1-px
  lx <- head(cumprod(c(1, px)), -1)
  dx <- c(-diff(lx), tail(lx, 1))
  Rxi <-  COD_prop
  Fdxi<-Rxi*dx
  dxi <- data.table(apply(Fdxi,2,cumsum))
  return(dxi)
}

# Data ------------------------------------------------------------

dat$deaths_input_100 <- readRDS(glue('{cnst$path_out}/deaths_input_100.rds'))

# input data for life-table calculation

dat$deaths_input_100[,deaths_prop:= deaths_cause/total_deaths]

dat$deaths_input_100[,mx:= total_deaths/pop]

# Calculate annual life tables ------------------------------------
dat$lyl_input_100 <- dcast.data.table(data = dat$deaths_input_100[,c(1:4,7,9,10)],age+year+sex+reth+mx ~ cause,value.var = 'deaths_prop')

#rowSums(dat$lyl_input_100[,6:13])

dat$lyl_results <- dat$lyl_input_100[,  LYL_fun(x = age,mx = mx, DT = .SD ), 
                                          by = .(year,sex,reth)]

dat$lyl_results[,age := 0:100, by = .(year,sex,reth)]


dat$dxi_results <- dat$lyl_input_100[,  Dxi_fun(x = age,mx = mx, DT = .SD ), 
                                     by = .(year,sex,reth)]

dat$dxi_results[,age := 0:100, by = .(year,sex,reth)]

saveRDS(dat$dxi_results, file = glue('{wd}/out/dxi_results.rds'))
saveRDS(dat$lyl_results, file = glue('{wd}/out/lyl_results.rds'))




