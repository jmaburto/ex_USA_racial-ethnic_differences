

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
  ordered_cause = c('COVID-19','CVD','Cancer','Despair', 'Accidents', 'Respiratory','Infectious','Rest')
  colors_fig_cod_1 =c("#999999", "#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  time_cut = c(2010, 2019, 2020)
  age_cut = c(seq(0,100,10))
  years_between = c(2015,2019,2020)
  years_ind = c(2010,2019,2020)
  colors_fig_cod_2 = c( '#00525D', 
                        #'#A8B6BA', 
                        '#A70316')
  lyl_age = 95
})

dat <- list()
fig <- list()
tab <- list()

# Data ------------------------------------------------------------


dat$dxi_results <- data.table(readRDS(glue('{cnst$path_out}/dxi_results.rds')))

dat$lyl_results <- data.table(readRDS(glue('{cnst$path_out}/lyl_results.rds')))

dat$lt_ci <- data.table(readRDS(glue('{wd}/out/lt_CI.rds')))


# Reshape data with labels ------------------------------------------------

dat$dxi_results[, sex:= factor(x = sex,labels = cnst$labels_sex)]
dat$lyl_results[, sex:= factor(x = sex,labels = cnst$labels_sex)]
dat$lt_ci[, sex:= factor(x = sex,labels = cnst$labels_sex)]

dat$dxi_results[, reth:= factor(x = reth,labels = cnst$labels_reth)]
dat$lyl_results[, reth:= factor(x = reth,labels = cnst$labels_reth)]
dat$lt_ci[, reth:= factor(x = reth,labels = cnst$labels_reth)]

dat$lt_ci <- dat$lt_ci[x == 0 & year %in% cnst$years_ind]

## Need to summarize life expectancy, lifespan inequality, and LYL in this graph. 
dat$fig_data_lt <- dat$lt_ci[x == 0 & year %in% cnst$years_ind, 
                             c('reth','sex','year','ex','ex_q025','ex_q975','sd0','sd0_q025','sd0_q975','lyl','lyl_q025','lyl_q975')]

dat$fig_data_lt[,c('ex','ex_q025','ex_q975','sd0','sd0_q025','sd0_q975','lyl','lyl_q025','lyl_q975')] <-
  round(dat$fig_data_lt[,c('ex','ex_q025','ex_q975','sd0','sd0_q025','sd0_q975','lyl','lyl_q025','lyl_q975')],1)

dat$fig_data_lt

#export for Appendix
write.csv(x = dat$fig_data_lt, file = glue('{cnst$path_out}/main_tables/table_main_results_CI.csv'))


# Figure colors -----------------------------------------------------------

dat$fig_dxi <- melt.data.table(data = dat$dxi_results, id.vars = c('year','sex','reth','age'),variable.name = 'cause',value.name = 'lyl')
dat$fig_dxi <- dat$fig_dxi[age <= cnst$lyl_age & year %in% cnst$time_cut,]
dat$fig_dxi[, ymin := 1 - cumsum(lyl), by = .(year,sex,reth,age)]
dat$fig_dxi[, ymax := c(1,ymin[-length(ymin)]), by = .(year,sex,reth,age)]

dat$fig_dxi[, cause:= factor(x = cause,labels = cnst$labels_cause)]
dat$fig_dxi[, cause:= factor(cause,levels = rev(cnst$ordered_cause))]
dat$fig_dxi <- dat$fig_dxi[order(year,sex,reth,cause,age),]

dat$fig_data_lt[, LE := paste0('Life expectancy = ',ex)]
dat$fig_data_lt[, SD := paste0('Lifespan inequality = ',sd0)]
dat$fig_data_lt[, LYL := paste0('Years lost = ',lyl)]
dat$fig_data_lt[,age := 1]

#data set for lines
dat$fig_lines <- dat$fig_dxi

dat$fig_lines <- dat$fig_lines[cause == 'Rest',]
dat$fig_lines_2 <- dat$fig_lines[age %in% c(50,75),]

ggplot()+
  geom_line(data = dat$fig_lines,aes(x = age, y = ymin))+
  geom_segment(data = dat$fig_lines_2, aes(x = age, y = 0, xend = age, yend = ymin), linetype =2)+
  facet_grid(reth~year+sex)



#Survival functions and summary within graph

fig$lyl_female <-  ggplot(dat$fig_dxi[sex == cnst$labels_sex[1],], aes(age, label=lyl))+
  ggtitle(paste0('A) Probability of surviving, life expectancy, lifespan inequality and years lost'),
          subtitle = 'Females')+
  geom_segment(data = dat$fig_lines_2[sex == cnst$labels_sex[1],], aes(x = age, y = 0, xend = age, yend = ymin), linetype =2, col= 'brown1',alpha=.4)+
  geom_segment(data = dat$fig_lines_2[sex == cnst$labels_sex[1],], aes(x = 0, y = ymin, xend = age, yend = ymin), linetype =2, col= 'brown1',alpha=.4)+
  geom_ribbon(aes(ymin = ymin,ymax =ymax, group=fct_rev(cause), fill=cause), position = 'identity',show.legend = F)+
  geom_text(data = dat$fig_data_lt[sex == cnst$labels_sex[1],], aes(x = age, y = .4, label = LE),hjust = 0)+
  geom_text(data = dat$fig_data_lt[sex == cnst$labels_sex[1],], aes(x = age, y = .3, label = SD),hjust = 0)+
  geom_text(data = dat$fig_data_lt[sex == cnst$labels_sex[1],], aes(x = age, y = .2, label = LYL),hjust = 0)+
  facet_grid(reth ~ year)+
  scale_x_continuous('Age', c(seq(0,75,25),95))+
  scale_fill_manual('Cause of death', values = cnst$colors_fig_cod_1)+
  labs(x = "Age", y = "Probability of surviving and life years lost")+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
      legend.position = 'bottom',strip.text.y = element_blank())

fig$lyl_female


fig$lyl_male <-  ggplot(dat$fig_dxi[sex == cnst$labels_sex[2],], aes(age, label=lyl))+
  ggtitle(' ',subtitle = 'Males')+
  geom_segment(data = dat$fig_lines_2[sex == cnst$labels_sex[2],], aes(x = age, y = 0, xend = age, yend = ymin), linetype =2,  col= 'brown1',alpha=.4)+
  geom_segment(data = dat$fig_lines_2[sex == cnst$labels_sex[2],], aes(x = 0, y = ymin, xend = age, yend = ymin), linetype =2,  col= 'brown1',alpha=.4)+
  geom_ribbon(aes(ymin = ymin,ymax =ymax, group=fct_rev(cause), fill=cause), position = 'identity',show.legend = T)+
  geom_text(data = dat$fig_data_lt[sex == cnst$labels_sex[2],], aes(x = age, y = .4, label = LE),hjust = 0)+
  geom_text(data = dat$fig_data_lt[sex == cnst$labels_sex[2],], aes(x = age, y = .3, label = SD),hjust = 0)+
  geom_text(data = dat$fig_data_lt[sex == cnst$labels_sex[2],], aes(x = age, y = .2, label = LYL),hjust = 0)+
  guides(y = 'none')+
  facet_grid(reth ~ year)+
  scale_x_continuous('Age', c(seq(0,75,25),95))+
  scale_fill_manual('Cause of death', values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = NULL)+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'right') 


fig$lyl_male

# + 
#    theme(panel.background = element_rect(fill = "lightgrey",
#                                          colour = "lightgrey",
#                                          linetype = "solid"))


#Contribution of causes of death to LYL

fig$total_cause_lyl <- dat$fig_dxi[, .(lyl = sum(lyl)), by = .(year,sex,reth,cause)]

fig$total_cause_lyl[, prop := lyl/sum(lyl)*100, by = .(year,sex,reth)]

fig$fig_cause_lyl_female  <- ggplot(fig$total_cause_lyl[sex == cnst$labels_sex[1],], aes(x = cause, y = prop, fill = cause)) + 
  ggtitle(paste0('B) Proportion of years lost by cause of death'),
          subtitle = 'Females')+
  geom_bar(stat = "identity",position = "stack",show.legend = F)+
  facet_grid(reth ~  + year,scales = "free_x", space = "free_x" )+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = NULL,size=10)+
  scale_y_continuous(' ', c(seq(0,40,5)),limits = c(0,37))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom',strip.text.y = element_blank())+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_cause_lyl_female

fig$fig_cause_lyl_male  <- ggplot(fig$total_cause_lyl[sex == cnst$labels_sex[2],], aes(x = cause, y = prop, fill = cause)) + 
  ggtitle(' ',
          subtitle = 'Males')+
  geom_bar(stat = "identity",position = "stack",show.legend = F)+
  facet_grid(reth ~  + year,scales = "free_x", space = "free_x" )+
  scale_fill_manual(values = cnst$colors_fig_cod_1)+
  labs(x = NULL, y = NULL,size=10)+
  guides(y = 'none')+
  scale_y_continuous('Proportion (%)', c(seq(0,40,5)),limits = c(0,37))+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'bottom')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_cause_lyl_male


fig$fig_cause_lyl <- fig$fig_cause_lyl_female|fig$fig_cause_lyl_male
fig$fig_cause_lyl
  

# Last panel with changes in life expectancy, lifespan inequality and lyl
dat$fig_changes <- dat$fig_data_lt[,c('reth','sex','year','ex','sd0','lyl')]

dat$fig_changes <- dat$fig_changes[,.(change_e0 = diff(ex),
                                      change_sd = diff(sd0),
                                      change_lyl = diff(lyl),
                                      Period = c('2010-2019','2019-2020')),
                                   by = .(reth,sex)]



dat$fig_changes[,reth:= factor(reth,levels = rev(cnst$labels_reth))]



fig$fig_change_e0 <- ggplot(dat$fig_changes, aes(x = reth, y = change_e0, col = Period)) + 
  ggtitle(paste0('C) Changes from 2010-2020'),
          subtitle = 'Life expectancy')+
  geom_point(stat = "identity",show.legend = F, size = 3)+
  facet_wrap(sex  ~ . ,strip.position = 'top',ncol = 1)+
  scale_color_manual(values = cnst$colors_fig_cod_2)+
  labs(x = NULL, y = NULL,size=10)+
  scale_y_continuous('')+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'right')+
  geom_hline(yintercept = 0)+
  coord_flip()

fig$fig_change_e0


fig$fig_change_sd <- ggplot(dat$fig_changes, aes(x = reth, y = change_sd, col = Period)) + 
  ggtitle(paste0(' '),
          subtitle = 'Lifespan inequality')+
  geom_point(stat = "identity",show.legend = T, size = 3)+
  facet_wrap(sex  ~ . ,strip.position = 'top',ncol = 1)+
  scale_color_manual(values = cnst$colors_fig_cod_2)+
  labs(x = NULL, y = NULL,size=10)+
  scale_y_continuous('')+
  theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
        legend.position = 'right')+
  geom_hline(yintercept = 0)+
  guides(y = 'none')+
  coord_flip()

fig$fig_change_sd
# fig$fig_change_lyl <- ggplot(dat$fig_changes, aes(x = reth, y = change_lyl, col = Period)) + 
#   ggtitle('Change in years lost')+
#   geom_point(stat = "identity",show.legend = T, size = 3)+
#   facet_wrap(sex  ~ . ,strip.position = 'top',ncol = 1)+
#   scale_color_manual(values = cnst$colors_fig_cod_2)+
#   labs(x = NULL, y = NULL,size=10)+
#   scale_y_continuous('')+
#   theme(legend.text=element_text(size=10), legend.title = element_text(size=10),
#         legend.position = 'right')+
#   geom_hline(yintercept = 0)+
#   coord_flip()
# 


fig$summary_ind <- (fig$fig_change_e0 | fig$fig_change_sd )


fig$summary_ind




fig$fig_1_main <- (fig$lyl_female | fig$lyl_male)/(fig$fig_cause_lyl | fig$summary_ind)

#fig$fig_1_main


ggsave("{wd}/out/main_figs/fig-1.pdf" %>% glue, fig$fig_1_main, width = 17, height = 13, device = cairo_pdf)


