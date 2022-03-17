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

dat$sd <- data.table(readRDS(glue('{cnst$path_out}/sd_results.rds')))

dat$decomp_sd_time <-  data.table(readRDS(glue('{cnst$path_out}/decomposition_time_sd.rds')))

dat$decomp_sd_between <-  data.table(readRDS(glue('{cnst$path_out}/decomposition_between_reth_sd.rds')))


# Reshape data with labels ------------------------------------------------

dat$decomp_sd_time[, sex:= factor(x = sex,labels = cnst$labels_sex)]
dat$decomp_sd_between[, sex:= factor(x = sex,labels = cnst$labels_sex)]
dat$sd[, sex:= factor(x = sex,labels = cnst$labels_sex)]

dat$decomp_sd_time[, cause:= factor(x = cause,labels = cnst$labels_cause)]
dat$decomp_sd_between[, cause:= factor(x = cause,labels = cnst$labels_cause)]

dat$decomp_sd_time[, reth:= as_factor(x = reth)]
dat$decomp_sd_time[, reth:= factor(x = reth,labels = cnst$labels_reth)]
dat$sd[, reth:= factor(x = reth,labels = cnst$labels_reth)]

dat$decomp_sd_between[, decomp_group:= factor(x = decomp_group,labels = cnst$labels_reth_dif)]

dat$decomp_sd_time[, cause:= factor(cause,levels = cnst$ordered_cause)]


# Figure of decomposition of life expectancy over time --------------------

# regroup data as aprropriate
dat$decomp_sd_time[,period := cut(dat$decomp_sd_time$year.final , 
                                      breaks=c(cnst$time_cut[-length(cnst$time_cut)],Inf),
                                      labels= paste0(cnst$time_cut[-length(cnst$time_cut)],'-',cnst$time_cut[-1]))]

#View(dat$decomp_e0_time)

dat$decomp_sd_time[,age_group := cut(dat$decomp_sd_time$x +1, 
                                  breaks=c(cnst$age_cut,Inf),
                                  labels= c(paste0(cnst$age_cut[-length(cnst$age_cut)],'-',(cnst$age_cut[-1]-1)),'100+'))]

#aggregate over period
dat$decomp_sd_time <- dat$decomp_sd_time[, .(contribution =  sum(contribution)), by = .(reth,sex,period,cause,age_group)]



#Figure sd
library(ggh4x)
library(scales)

### both sexes
fig$fig_decomp_time_sd_male_female <- ggplot(dat$decomp_sd_time[ cause %in% cnst$causes_to_show], 
                                             aes(x = age_group, y = contribution, fill = period)) + 
  ggtitle(paste0('A) Contribution of causes of death to changes in lifespan inequality from 2010 to 2020'))+
  geom_bar(stat = "identity",position = "stack",show.legend = F)+
  facet_nested(sex + reth ~ cause, scales = "free_x", space = "free_x",)+
  scale_fill_manual('',values = cnst$colors_fig_cod_2)+
  scale_y_continuous(labels = label_number(accuracy = 0.01))+
  labs(x = NULL, y = "Losses|Gains in life expectancy at birth (years)",size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_time_sd_male_female




#Fig by total causes of death 
#aggregate over period
dat$fig_decomp_sd_time_cause <- dat$decomp_sd_time[, .(contribution =  sum(contribution)), by = .(reth,sex,period,cause)]
dat$fig_decomp_sd_time_cause[, cause:= factor(cause,levels = rev(c('COVID-19','Rest','Despair','CVD','Accidents','Infectious','Respiratory','Cancer')))]

#Figure total by cause

### figure for appendix


fig$fig_decomp_cause_male_female_sd <- ggplot(dat$fig_decomp_sd_time_cause, 
                                             aes(x = cause, y = contribution, fill = period)) + 
  ggtitle(paste0('Total contribution by cause of death'))+
  geom_bar(stat = "identity",position = "stack",show.legend = T)+
  facet_grid(sex ~ reth, scales = "free_x", space = "free_x")+
  scale_fill_manual( ' ',values = cnst$colors_fig_cod_2)+
  labs(x = NULL, y = "Losses|Gains in life expectancy at birth (years)",size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_cause_male_female_sd


fig$fig_s3_appendix <-  fig$fig_decomp_time_sd_male_female / fig$fig_decomp_cause_male_female_sd+
  plot_layout(heights = c(3, 1))


ggsave("{wd}/out/main_figs/fig-3_appendix.pdf" %>% glue, fig$fig_s3_appendix, width = 14, height = 15, device = cairo_pdf)




