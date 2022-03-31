# General life-table analysis


##Pop	population count in race/ethnic, sex, year group	
#Mort	total number of deaths  in race/ethnic, sex, year group	
#Reth	race/ethnic group variable, should have labels (1 "NHW" 2 "NHB" 3 "LatinX" 4 "AI/AN" 5 "Other")	
#Male	1=male, 0=female	


# Init ------------------------------------------------------------
library(data.table)
library(here); library(glue)
library(tidyverse); library(yaml)
library(patchwork)
library(ggplot2)
library(haven)

# Constants -------------------------------------------------------

wd <- here()
cnst <- list()
cnst <- within(cnst, {
  groups_for_analysis = c('nhw','nhb','latinx')
  path_out = glue('{wd}/out')
  path_tmp = glue('{wd}/tmp')
  labels_sex = c('Female','Male')
  labels_reth =  c('White','Black','Hispanic')
  labels_reth_dif =  c('Hispanic-White','White-Black')
  labels_cause = c('CVD','Respiratory', 'Infectious', 'Despair', 'Cancer', 'Accidents', 'COVID-19', 'Rest')
  ordered_cause = c('Cancer', 'CVD', 'Despair', 'Accidents', 'COVID-19', 'Rest', 'Infectious', 'Respiratory')
  causes_to_show = c('Cancer', 'CVD', 'Despair', 'COVID-19', 'Rest')
  colors_fig_cod_1 =c(
    '#000033',
    '#0000B6',
    '#2E00FF',
    '#9408F7',
    '#FA4AB5',
    '#FF8B74',
    '#FFCD32',
    '#FFFF60')
  time_cut = c(2010, 2019, 2020)
  age_cut = c(seq(0,100,10))
  years_between = c(2015,2019,2020)
  colors_fig_cod_2 = c( '#00525D', 
                        #'#A8B6BA', 
                        '#A70316')
})

dat <- list()
fig <- list()
tab <- list()

# Data ------------------------------------------------------------

dat$lt_CI <- data.table(readRDS(glue('{cnst$path_out}/lt_CI.rds')))

dat$decomp_e0_time <-  data.table(readRDS(glue('{cnst$path_out}/decomposition_time_reth.rds')))

dat$decomp_e0_between <-  data.table(readRDS(glue('{cnst$path_out}/decomposition_between_reth.rds')))


# Reshape data with labels ------------------------------------------------

dat$decomp_e0_time[, sex:= factor(x = sex,labels = cnst$labels_sex)]
dat$decomp_e0_between[, sex:= factor(x = sex,labels = cnst$labels_sex)]
dat$lt_CI[, sex:= factor(x = sex,labels = cnst$labels_sex)]

dat$decomp_e0_time[, cause:= factor(x = cause,labels = cnst$labels_cause)]
dat$decomp_e0_between[, cause:= factor(x = cause,labels = cnst$labels_cause)]

dat$decomp_e0_time[, reth:= as_factor(x = reth)]
dat$decomp_e0_time[, reth:= factor(x = reth,labels = cnst$labels_reth)]
dat$lt_CI[, reth:= factor(x = reth,labels = cnst$labels_reth)]

dat$decomp_e0_between[, decomp_group:= factor(x = decomp_group,labels = cnst$labels_reth_dif)]

dat$decomp_e0_time[, cause:= factor(cause,levels = cnst$ordered_cause)]


# Figure of decomposition of life expectancy over time --------------------

# regroup data as aprropriate
dat$decomp_e0_time[,period := cut(dat$decomp_e0_time$year.final , 
                                      breaks=c(cnst$time_cut[-length(cnst$time_cut)],Inf),
                                      labels= paste0(cnst$time_cut[-length(cnst$time_cut)],'-',cnst$time_cut[-1]))]

#View(dat$decomp_e0_time)

dat$decomp_e0_time[,age_group := cut(dat$decomp_e0_time$x +1, 
                                  breaks=c(cnst$age_cut,Inf),
                                  labels= c(paste0(cnst$age_cut[-length(cnst$age_cut)],'-',(cnst$age_cut[-1]-1)),'100+'))]

#aggregate over period
dat$fig_decomp_e0_time <- dat$decomp_e0_time[, .(contribution =  sum(contribution)), by = .(reth,sex,period,cause,age_group)]



#Figure 1

### both sexes

library(ggh4x)
library(scales)

fig$fig_decomp_time_e0_male_female <- ggplot(dat$fig_decomp_e0_time[ cause %in% cnst$causes_to_show], 
                                             aes(x = age_group, y = contribution, fill = period)) + 
  ggtitle(paste0('Contribution of causes of death to changes in life expectancy from 2010 to 2020'))+
  geom_bar(stat = "identity",position = "stack",show.legend = T)+
  facet_nested(sex + reth ~ cause, scales = "free_x", space = "free_x",)+
  scale_fill_manual('',values = cnst$colors_fig_cod_2)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  labs(x = NULL, y = "Losses|Gains in life expectancy at birth (years)",size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_time_e0_male_female

ggsave("{wd}/out/main_figs/fig-1.pdf" %>% glue, fig$fig$fig_decomp_time_e0_male_female, width = 14, height = 13, device = cairo_pdf)



### figure for appendix

fig$fig_decomp_time_e0_male_female_appendix <- ggplot(dat$fig_decomp_e0_time, 
                                             aes(x = age_group, y = contribution, fill = period)) + 
  ggtitle(paste0('Contribution of causes of death to changes in life expectancy from 2010 to 2020'))+
  geom_bar(stat = "identity",position = "stack",show.legend = T)+
  facet_nested(sex + reth ~ cause, scales = "free_x", space = "free_x",)+
  scale_fill_manual('',values = cnst$colors_fig_cod_2)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  labs(x = NULL, y = "Losses|Gains in life expectancy at birth (years)",size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_time_e0_male_female_appendix

ggsave("{wd}/out/main_figs/fig-1_appendix.pdf" %>% glue, fig$fig_decomp_time_e0_male_female_appendix, width = 24, height = 13, device = cairo_pdf)




#Fig by total causes of death for appendix
#aggregate over period
dat$fig_decomp_e0_time_cause <- dat$decomp_e0_time[, .(contribution =  sum(contribution)), by = .(reth,sex,period,cause)]
dat$fig_decomp_e0_time_cause[, cause:= factor(cause,levels = rev(c('COVID-19','Rest','Despair','CVD','Accidents','Infectious','Respiratory','Cancer')))]

#Figure total by cause

### both sexes
fig$fig_decomp_cause_male_female <- ggplot(dat$fig_decomp_e0_time_cause, 
                                             aes(x = cause, y = contribution, fill = period)) + 
  #ggtitle(paste0(cnst$labels_sex[2],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = T)+
  facet_grid(sex ~ reth, scales = "free_x", space = "free_x")+
  scale_fill_manual( ' ',values = cnst$colors_fig_cod_2)+
  labs(x = NULL, y = "Losses|Gains in life expectancy at birth (years)",size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_cause_male_female

ggsave("{wd}/out/main_figs/fig-2_appendix.pdf" %>% glue, fig$fig_decomp_cause_male_female, width = 8, height = 8, device = cairo_pdf)



dat$table_decomp_cause_e0 <- dat$fig_decomp_e0_time_cause

dat$table_decomp_cause_e0[,contribution:= round(contribution,2)]

dat$table_decomp_cause_e0 <- dcast(dat$table_decomp_cause_e0, reth + sex + period ~ cause)

write.csv(x = dat$table_decomp_cause_e0, file = glue('{cnst$path_out}/main_tables/Cause_contrib_e0.csv'))


### Quantify contributions below and above age 60 of COVID-19 mortality 

dat$covid <- dat$decomp_e0_time[cause %in% 'COVID-19' & period %in% '2019-2020']

dat$covid[ ,new.age := ifelse(x < 61, 1, 2)]

dat$covid <- dat$covid[ , .(contribution = sum(contribution)), by = .(reth,sex,new.age)]

dat$covid[ , proportion := round(contribution/sum(contribution)*100,1), by = .(reth,sex)]



