
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
  labels_reth_dif =  c('Hispanic-White','White-Black','Hispanic-Black')
  labels_cause = c('CVD','Respiratory', 'Infectious', 'Despair', 'Cancer', 'Accidents', 'COVID-19', 'Rest')
  ordered_cause = c('Cancer', 'CVD', 'Despair', 'Accidents', 'COVID-19', 'Rest', 'Infectious', 'Respiratory')
  causes_to_show = c('Cancer', 'CVD', 'Despair', 'COVID-19', 'Rest')
  colors_fig_cod_1 =c("#999999", "#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
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
dat$decomp_e0_between <-  data.table(readRDS(glue('{cnst$path_out}/decomposition_between_reth.rds')))

dat$decomp_sd_between <-  data.table(readRDS(glue('{cnst$path_out}/decomposition_between_reth_sd.rds')))

# Reshape data with labels ------------------------------------------------

dat$decomp_e0_between[, sex:= factor(x = sex,labels = cnst$labels_sex)]
dat$decomp_sd_between[, sex:= factor(x = sex,labels = cnst$labels_sex)]

dat$decomp_e0_between[, cause:= factor(x = cause,labels = cnst$labels_cause)]
dat$decomp_sd_between[, cause:= factor(x = cause,labels = cnst$labels_cause)]

dat$decomp_e0_between[, decomp_group:= factor(x = decomp_group,labels = cnst$labels_reth_dif)]
dat$decomp_sd_between[, decomp_group:= factor(x = decomp_group,labels = cnst$labels_reth_dif)]



# Figure of decomposition of life expectancy racial/ethnic groups --------------------

# regroup data as aprropriate

dat$decomp_e0_between[,age_group := cut(dat$decomp_e0_between$x +1, 
                                        breaks=c(cnst$age_cut,Inf),
                                        labels= c(paste0(cnst$age_cut[-length(cnst$age_cut)],'-',(cnst$age_cut[-1]-1)),'100+'))]

#aggregate over age
dat$fig_decomp_e0_between <- dat$decomp_e0_between[year %in% cnst$years_between , .(contribution =  sum(contribution)), by = .(decomp_group,year,sex,cause,age_group)]

dat$fig_gaps <- dat$fig_decomp_e0_between[, .(gap = round(sum(contribution),1)), by = .(decomp_group,year,sex)]

dat$fig_gaps[, gap_text:= paste0('Gap = ',gap)]


#### Figures for females
fig$fig_decomp_e0_between_females  <- ggplot(dat$fig_decomp_e0_between[sex == cnst$labels_sex[1] & decomp_group %in% cnst$labels_reth_dif[1:2]]) + 
  ggtitle(paste0(cnst$labels_sex[1],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = F,aes(x = age_group, y = contribution, fill = cause))+
  geom_text(data = dat$fig_gaps[sex == cnst$labels_sex[1] & decomp_group %in% cnst$labels_reth_dif[1:2]], aes(x = 2, y = -.9, label = gap_text),hjust = 0)+
  facet_grid(year ~ decomp_group )+
  ylim(c(-1,1.6))+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_e0_between_females


#### Figures for males
fig$fig_decomp_e0_between_males  <- ggplot(dat$fig_decomp_e0_between[sex == cnst$labels_sex[2] & decomp_group %in% cnst$labels_reth_dif[1:2]]) + 
  ggtitle(paste0(cnst$labels_sex[2],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = T,aes(x = age_group, y = contribution, fill = cause))+
  geom_text(data = dat$fig_gaps[sex == cnst$labels_sex[2] & decomp_group %in% cnst$labels_reth_dif[1:2]], aes(x = 2, y = -.9, label = gap_text),hjust = 0)+
  facet_grid(year ~ decomp_group )+
  ylim(c(-1,1.6))+
  scale_fill_manual('Cause',values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'right')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_e0_between_males


#Export 
fig$fig3 <- fig$fig_decomp_e0_between_females| fig$fig_decomp_e0_between_males

fig$fig3

ggsave("{wd}/out/main_figs/fig-3.pdf" %>% glue, fig$fig3, width = 13, height = 7, device = cairo_pdf)


## Figuire appendix for Hispanic-Black difference

fig$fig_decomp_e0_between_s4  <- ggplot(dat$fig_decomp_e0_between[decomp_group %in% cnst$labels_reth_dif[3]]) + 
  geom_bar(stat = "identity",position = "stack",show.legend = T,aes(x = age_group, y = contribution, fill = cause))+
  facet_grid(year ~ decomp_group + sex)+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_e0_between_s4

ggsave("{wd}/out/main_figs/fig-s4.pdf" %>% glue, fig$fig_decomp_e0_between_s4, width = 7, height = 7, device = cairo_pdf)



## figure with total contribution by cause of death

dat$fig_cause <- dat$fig_decomp_e0_between[, .(contribution = sum(contribution)), by = .(decomp_group,year,sex,cause)]


fig$fig_cause_females  <- ggplot(dat$fig_cause[sex == cnst$labels_sex[1]]) + 
  ggtitle(paste0(cnst$labels_sex[1],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = F,aes(x = cause, y = contribution, fill = cause))+
  facet_grid(year ~ decomp_group )+
  ylim(c(-1.5,2))+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_cause_females


#### Figures for males
fig$fig_cause_male  <- ggplot(dat$fig_cause[sex == cnst$labels_sex[2]]) + 
  ggtitle(paste0(cnst$labels_sex[2],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = F,aes(x = cause, y = contribution, fill = cause))+
  facet_grid(year ~ decomp_group )+
  #ylim(c(-1.5,2))+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_cause_male

fig$fig_s4 <- fig$fig_cause_females| fig$fig_cause_male

fig$fig_s4

ggsave("{wd}/out/main_figs/fig-4_appendix.pdf" %>% glue, fig$fig_s4, width = 13, height = 7, device = cairo_pdf)




# Figure of decomposition of lifespan inequality racial/ethnic groups --------------------

# regroup data as aprropriate

dat$decomp_sd_between[,age_group := cut(dat$decomp_sd_between$x +1, 
                                        breaks=c(cnst$age_cut,Inf),
                                        labels= c(paste0(cnst$age_cut[-length(cnst$age_cut)],'-',(cnst$age_cut[-1]-1)),'100+'))]

#aggregate over age
dat$fig_decomp_sd_between <- dat$decomp_sd_between[year %in% cnst$years_between , .(contribution =  sum(contribution)), by = .(decomp_group,year,sex,cause,age_group)]

dat$fig_gaps_sd <- dat$fig_decomp_sd_between[, .(gap = round(sum(contribution),1)), by = .(decomp_group,year,sex)]

dat$fig_gaps_sd[, gap_text:= paste0('Gap = ',gap)]


#### Figures for females
fig$fig_decomp_sd_between_females  <- ggplot(dat$fig_decomp_sd_between[sex == cnst$labels_sex[1]]) + 
  ggtitle('A) Age and cause contribution to gaps in lifespan inequality', subtitle =  paste0(cnst$labels_sex[1],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = F,aes(x = age_group, y = contribution, fill = cause))+
  geom_text(data = dat$fig_gaps_sd[sex == cnst$labels_sex[1],], aes(x = 2, y = -1.7, label = gap_text),hjust = 0)+
  facet_grid(year ~ decomp_group )+
  ylim(c(-2,0.6))+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_sd_between_females


#### Figures for males
fig$fig_decomp_sd_between_males  <- ggplot(dat$fig_decomp_sd_between[sex == cnst$labels_sex[2]]) + 
  ggtitle(paste0(' ', subtitle = cnst$labels_sex[2],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = T,aes(x = age_group, y = contribution, fill = cause))+
  geom_text(data = dat$fig_gaps_sd[sex == cnst$labels_sex[2],], aes(x = 2, y = -1.7, label = gap_text),hjust = 0)+
  facet_grid(year ~ decomp_group )+
  ylim(c(-2,0.6))+
  scale_fill_manual('Cause',values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'right')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_decomp_sd_between_males


#Export 
fig$figs5_a <- fig$fig_decomp_sd_between_females| fig$fig_decomp_sd_between_males

fig$figs5_a



## figure with total contribution by cause of death

dat$fig_cause_sd <- dat$fig_decomp_sd_between[, .(contribution = sum(contribution)), by = .(decomp_group,year,sex,cause)]


fig$fig_cause_females_sd  <- ggplot(dat$fig_cause_sd[sex == cnst$labels_sex[1]]) + 
  ggtitle('B) Cause contribution to gaps in lifespan inequality', subtitle =  paste0(cnst$labels_sex[1],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = F,aes(x = cause, y = contribution, fill = cause))+
  facet_grid(year ~ decomp_group )+
  ylim(c(-2.5,2))+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_cause_females_sd


#### Figures for males
fig$fig_cause_male_sd  <- ggplot(dat$fig_cause_sd[sex == cnst$labels_sex[2]]) + 
  ggtitle(paste0(cnst$labels_sex[2],'s'))+
  geom_bar(stat = "identity",position = "stack",show.legend = F,aes(x = cause, y = contribution, fill = cause))+
  facet_grid(year ~ decomp_group )+
  #ylim(c(-2.5,2))+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = 'Contribution (years)',size=10)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_cause_male_sd

fig$figs5_b <- fig$fig_cause_females_sd| fig$fig_cause_male_sd

fig$figs5_b

fig$figs5  <- fig$figs5_a / fig$figs5_b

fig$figs5

ggsave("{wd}/out/main_figs/fig-5_appendix.pdf" %>% glue, fig$figs5, width = 13, height = 13, device = cairo_pdf)
