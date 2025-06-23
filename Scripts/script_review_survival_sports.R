rm(list=ls())

##-- Load libraries ------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)

source('functions_review_survival_sport.R')
source('parameters_review_survival_sport.R')

##-- Load data -----------------------------------------------------------------
d <- read_excel("../Data/database_survival_sports_reporting.xlsx", sheet='Articles',na = 'NA')
names(d) <- NEW_NAMES

##------------------------------------------------------------------------------
# First table: General Characteristics of the Articles
##------------------------------------------------------------------------------

##-- Countries
d <- d %>% mutate(country = if_else(country=='Canada, Denmark and Finland','Canada',country))
t_country <- sort(table(d$country), decreasing = TRUE)
p_country <- prop.table(t_country)
head(cbind(t_country,round(100*p_country,1)),8)

##-- Journal
t_journal <- sort(table(d$journal), decreasing = TRUE)
p_journal <- prop.table(t_journal)
head(cbind(t_journal,round(100*p_journal,1)),8)

##-- Continent
d <- merge(d, info_countries, by='country')

##-- Year
# table
t_year <- d %>% select(year) %>% count(year) 
t_year_grouped <- t_year %>%
  mutate(period = case_when(year >= 2013 & year <= 2015 ~ "2013-2015",
                            year >= 2016 & year <= 2018 ~ "2016-2018",
                            year >= 2019 & year <= 2023 ~ "2019-2023")) %>%
  group_by(period) %>%
  summarise(n = sum(n))  %>%
  mutate(percentage = round(n/sum(n) * 100, 2))

# plot
ggplot(t_year, aes(x=year, y=n)) + 
  geom_line(color='darkblue', linewidth=1) +
  geom_point(color='darkblue', size=3) +
  # geom_smooth(color='orange', linewidth=0.8, se = FALSE) +
  ylim(0,25) +
  scale_x_continuous(name = 'Year', breaks=2013:2023, minor_breaks = 2013:2023, limits = c(2013, 2023)) +
  ylab('no. articles')
# ggsave(filename='../../../Figures/figure_S1.jpeg', width = 8, height = 5, dpi = 300)

##-- Main goal
t_classification <- sort(table(d$main_goal), decreasing = TRUE)
p_classification <- prop.table(t_classification)
head(cbind(t_classification,round(100*p_classification,1)),8)

##-- Sample size
summary(d$N)

##------------------------------------------------------------------------------
# Second table: Sports characteristics
##------------------------------------------------------------------------------

##-- Sports
d_sports <- d %>% select(sport) %>% 
  mutate(sport = map(sport, separate_sport)) %>%
  unnest(sport) %>%
  mutate(sport = str_trim(sport)) %>% 
  mutate(sport = case_match(sport, 'Football' ~ 'Soccer', .default = sport))
t_sports <- d_sports %>% select(sport) %>% count(sport) %>% arrange(-n) %>% 
  mutate(perc=round(100*n/139,1))
t_sports2 <- head(t_sports,10)[-1,]
t_sports2$sport <- factor(t_sports2$sport, levels=t_sports2$sport)
ggplot(t_sports2, aes(x=sport, y=perc)) + 
  geom_bar(stat = 'identity',fill='darkblue') +
  xlab('Sport') +
  ylab('% articles') + 
  coord_flip()

##-- Sex
t_sex <- sort(table(d$gender), decreasing = TRUE)
p_sex <- prop.table(t_sex)
head(cbind(t_sex,round(100*p_sex,1)),8)

##-- Participant Level
t_participant <- sort(table(d$participant_level), decreasing = TRUE)
p_participant <- prop.table(t_participant)
head(cbind(t_participant,round(100*p_participant,1)),8)

##-- Sports
d_sports <- d %>% select(sport) %>% 
  mutate(sport = map(sport, separate_sport)) %>%
  unnest(sport) %>%
  mutate(sport = str_trim(sport))
t_sports <- d_sports %>% select(sport) %>% count(sport) %>% arrange(-n) %>% 
  mutate(perc=round(100*n/139,1))
t_sports2 <- head(t_sports,10)[-1,]
t_sports2$sport <- factor(t_sports2$sport, levels=t_sports2$sport)
ggplot(t_sports2, aes(x=sport, y=perc)) + 
  geom_bar(stat = 'identity',fill='darkblue') +
  ylab('% articles') + 
  coord_flip()
# ggsave(filename='../../../Figures/figure_sports.jpeg', width = 8, height = 5, dpi = 300)


##------------------------------------------------------------------------------
# Third table: Statistical analyses used in papers
##------------------------------------------------------------------------------

##-- KM estimator
t_km <- sort(table(d$kaplan_meier), decreasing = TRUE)
p_km <- prop.table(t_km)
head(cbind(t_km,round(100*p_km,1)),8)

##-- FH estimator
t_hf <- sort(table(d$harrington_fleming), decreasing = TRUE)
p_hf <- prop.table(t_hf)
head(cbind(t_hf,round(100*p_hf,1)),8)

##-- LR estimator
t_lr <- sort(table(d$log_rank), decreasing = TRUE)
p_lr <- prop.table(t_lr)
head(cbind(t_lr,round(100*p_lr,1)),8)

##-- Cox
t_cox <- sort(table(d$cox), decreasing = TRUE)
p_cox <- prop.table(t_cox)
head(cbind(t_cox,round(100*p_cox,1)),8)

##-- CoxR
t_coxr <- sort(table(d$cox_random), decreasing = TRUE)
p_coxr <- prop.table(t_coxr)
head(cbind(t_coxr,round(100*p_coxr,1)),8)

##-- Weibull
t_param <- sort(table(d$parametric_model), decreasing = TRUE)
p_param <- prop.table(t_param)
head(cbind(t_param,round(100*p_param,1)),8)

##-- Random survival forest
t_rsf <- sort(table(d$rsf), decreasing = TRUE)
p_rsf <- prop.table(t_rsf)
head(cbind(t_rsf,round(100*p_rsf,1)),8)

##-- Random survival forest
t_competing <- sort(table(d$competing), decreasing = TRUE)
p_competing <- prop.table(t_competing)
head(cbind(t_competing,round(100*p_competing,1)),8)

##-- All methods
d_methods0 <- d %>% select(all_of(VAR_METHODS))
names(d_methods0) <- VAR_METHODS_SHORT
d_methods <- d_methods0 %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "category") %>%
  na.omit() %>% 
  count(variable, category) %>%
  group_by(variable) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  filter(category=='Yes') %>% 
  select(variable, n, percentage) %>% 
  arrange(-n)

d_methods$variable <- factor(d_methods$variable, 
                             levels=arrange(d_methods,percentage) %>% 
                             select(variable) %>% pull)

ggplot(d_methods, aes(x=variable, y=percentage)) + 
  geom_bar(stat = 'identity',fill='darkblue') +
  ylab('% articles') + coord_flip() + xlab('') + 
  theme(axis.text = element_text(face='bold'),
        axis.title = element_text(face='bold'))

# ggsave(filename='../../../Figures/figure_S2.jpeg', width = 8, height = 5, dpi = 300)


##-- Software
d_software <- d %>% select(software) %>% 
  mutate(software = map(software, separate_software)) %>%
  unnest(software) %>%
  mutate(software = str_trim(software)) 
t_software <- sort(table(d_software), decreasing =TRUE)
p_software <- prop.table(t_software)
head(cbind(t_software,round(100*p_software,1)))

##-- Data sharing
t_share_d <- sort(table(d$data_sharing), decreasing = TRUE)
p_share_d <- prop.table(t_share_d)
head(cbind(t_share_d,round(100*p_share_d,1)),8)

##-- Code sharing
t_share_c <- sort(table(d$code_sharing), decreasing = TRUE)
p_share_c <- prop.table(t_share_c)
head(cbind(t_share_c,round(100*p_share_c,1)),8)


##------------------------------------------------------------------------------
# Fourth table: SAMPL
##------------------------------------------------------------------------------

d_sampl0 <- d %>% select(all_of(VAR_SAMPL))
names(d_sampl0) <- VAR_SAMPL_SHORT

##-- Table SAMPL (with NAs)
d_sampl_table <- d_sampl0 %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "category") %>%
  count(variable, category) %>%
  group_by(variable) %>%
  mutate(percentage = (n / sum(n[!is.na(category)])) * 100) %>%
  select(variable, category, n, percentage) %>%
  mutate(category = recode(category,!!!c(No = "Poorly",Partial = "Partially", Yes = "Well"))) %>% 
  mutate(variable = factor(variable, levels = VAR_SAMPL_SHORT)) %>% 
  mutate(category = factor(category, levels = c("Well", "Partially", "Poorly"))) %>%
  mutate(percentage = paste0(formatC(percentage, digits = 1, format = 'f'),"%")) %>% 
  arrange(variable, category)

write.table(d_sampl_table,'../Data/SAMPL_table.txt',append = FALSE,
            quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)

##-- Table purpose
t_purpose <- sort(table(d$reason_purpose_cat), decreasing = TRUE)
prop.table(t_purpose)

##-- Plot SAMPL (without NAs)
d_sampl <- d_sampl0 %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "category") %>%
  na.omit() %>% 
  count(variable, category) %>%
  group_by(variable) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  select(variable, category, n, percentage) %>%
  mutate(category = recode(category,!!!c(No = "Poorly",Partial = "Partially", Yes = "Well")),
         label    = if_else(percentage<3,"",paste0(formatC(percentage, format = 'f', digits=1),"%")),
         pos      = (cumsum(percentage) - (percentage / 2))/100)

d_sampl$variable <- factor(d_sampl$variable,
                           levels=arrange(d_sampl[d_sampl$category=='Well',],-percentage) %>% 
                             select(variable) %>% pull)
d_sampl$category <- factor(d_sampl$category, levels = c("Poorly", "Partially", "Well"))


ggplot(d_sampl, aes(fill=category, y=percentage, x=variable)) + 
  geom_bar(position=position_fill(reverse = TRUE), 
           stat="identity") +
  geom_text(aes(label = label, y = pos, color = category), 
            position = position_identity(), size = 2.5, fontface = "bold") +
  coord_flip() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text = element_text(face='bold'),
        axis.title = element_text(face='bold'),
        legend.text = element_text(face='bold'),
        plot.margin = margin(r = 10)) +
  xlab('') +
  ylab('Proportion of poorly reported studies') +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(expand   = c(0, 0),
                     sec.axis = sec_axis(transform = ~ 1 - . , 
                                         name      = "Proportion of well-reported studies")) + 
  scale_fill_manual(values=c("#fdb863", "#b8e186", "#286419")) +
  scale_color_manual(values = c("Poorly" = "black", "Partially" = "black", "Well" = "white")) +
  guides(color = guide_none())
  
# ggsave(filename='../../../Figures/figure_2.jpeg', width = 9, height = 7, dpi = 300)