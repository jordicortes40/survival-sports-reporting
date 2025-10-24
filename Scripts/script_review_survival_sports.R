rm(list=ls())

##-- Load libraries ------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)
library(janitor)

source('functions_review_survival_sport.R')
source('parameters_review_survival_sport.R')

##-- Load data -----------------------------------------------------------------
d <- read_excel("../Data/database_survival_sports_reporting.xlsx", sheet='Articles',na = 'NA')
names(d) <- NEW_NAMES

##------------------------------------------------------------------------------
# Table 2: General Characteristics of the Articles
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
# Table 3: Sports characteristics
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
# Table 4: Statistical analyses used in papers
##------------------------------------------------------------------------------

##-- Filterd data.frames
dH <- d %>% filter(main_goal=='Health')
dS <- d %>% filter(main_goal=='Sports Performance Analysis')

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

##-- All methods --> Filter by Health
d_methods0_H <- dH %>% select(all_of(VAR_METHODS))
names(d_methods0_H) <- VAR_METHODS_SHORT
d_methods_H <- d_methods0_H %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "category") %>%
  na.omit() %>% 
  count(variable, category) %>%
  group_by(variable) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  filter(category=='Yes') %>% 
  select(variable, n, percentage) %>% 
  arrange(-n)
d_methods_H$variable <- factor(d_methods_H$variable, 
                               levels = arrange(d_methods_H,percentage) %>% 
                               select(variable) %>% pull)

##-- All methods --> Filter by Sports Performance Analysis
d_methods0_S <- dS %>% select(all_of(VAR_METHODS))
names(d_methods0_S) <- VAR_METHODS_SHORT
d_methods_S <- d_methods0_S %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "category") %>%
  na.omit() %>% 
  count(variable, category) %>%
  group_by(variable) %>%
  mutate(percentage = (n / sum(n)) * 100) %>%
  filter(category=='Yes') %>% 
  select(variable, n, percentage) %>% 
  arrange(-n)
d_methods_S$variable <- factor(d_methods_S$variable, 
                               levels = arrange(d_methods_S,percentage) %>% 
                               select(variable) %>% pull)


# Cross-table methods between themselves
sum(d_methods0$`Kaplan Meier`=='Yes' & d_methods0$`Cox model`=='Yes')
sum(d_methods0$`Kaplan Meier`=='Yes' & d_methods0$`Log-rank` =='Yes')
sum(d_methods0$`Cox model`   =='Yes' & d_methods0$`Log-rank` =='Yes')

# Cross-table methods between type of data and sample size.
my_tab('main_goal','kaplan_meier')
my_tab('main_goal','cox')
my_tab('main_goal','log_rank') 

with(d,tapply(d$N,kaplan_meier,summary))
with(d,tapply(d$N,cox,summary))
with(d,tapply(d$N,log_rank,summary))

##-- Software
d_software <- d %>% select(software) %>% 
  mutate(software = map(software, separate_software)) %>%
  unnest(software) %>%
  mutate(software = str_trim(software)) 
t_software <- sort(table(d_software), decreasing =TRUE)
p_software <- prop.table(t_software)
head(cbind(t_software,round(100*p_software,1)))

##-- Software --> Filter by Health
d_software_H <- dH %>% select(software) %>% 
  mutate(software = map(software, separate_software)) %>%
  unnest(software) %>%
  mutate(software = str_trim(software)) 
t_software_H <- sort(table(d_software_H), decreasing =TRUE)
p_software_H <- prop.table(t_software_H)
head(cbind(t_software_H,round(100*p_software_H,1)),20)

##-- Software --> Filter by Sports Performance
d_software_S <- dS %>% select(software) %>% 
  mutate(software = map(software, separate_software)) %>%
  unnest(software) %>%
  mutate(software = str_trim(software)) 
t_software_S <- sort(table(d_software_S), decreasing =TRUE)
p_software_S <- prop.table(t_software_S)
head(cbind(t_software_S,round(100*p_software_S,1)),20)

##-- Data sharing
t_share_d <- sort(table(d$data_sharing), decreasing = TRUE)
p_share_d <- prop.table(t_share_d)
head(cbind(t_share_d,round(100*p_share_d,1)))
##-- Code sharing
t_share_c <- sort(table(d$code_sharing), decreasing = TRUE)
p_share_c <- prop.table(t_share_c)
head(cbind(t_share_c,round(100*p_share_c,1)))

##-- Data sharing --> Filter by Health
t_share_d_H <- sort(table(dH$data_sharing), decreasing = TRUE)
p_share_d_H <- prop.table(t_share_d_H)
head(cbind(t_share_d_H,round(100*p_share_d_H,1)))
##-- Code sharing --> Filter by Health
t_share_c_H <- sort(table(dH$code_sharing), decreasing = TRUE)
p_share_c_H <- prop.table(t_share_c_H)
head(cbind(t_share_c_H,round(100*p_share_c_H,1)))

##-- Data sharing --> Filter by Sports Performance
t_share_d_S <- sort(table(dS$data_sharing), decreasing = TRUE)
p_share_d_S <- prop.table(t_share_d_S)
head(cbind(t_share_d_S,round(100*p_share_d_S,1)))
##-- Code sharing --> Filter by Sports Performance
t_share_c_S <- sort(table(dS$code_sharing), decreasing = TRUE)
p_share_c_S <- prop.table(t_share_c_S)
head(cbind(t_share_c_S,round(100*p_share_c_S,1)))

##-- Censoring
t_censoring <- sort(table(d$censore_type), decreasing = TRUE)
p_censoring <- prop.table(t_censoring)
head(cbind(t_censoring,round(100*p_censoring,1)))

##-- Censoring --> Filter by Health
t_censoring_H <- sort(table(dH$censore_type), decreasing = TRUE)
p_censoring_H <- prop.table(t_censoring_H)
head(cbind(t_censoring_H,round(100*p_censoring_H,1)))

##-- Censoring --> Filter by Sports Performance
t_censoring_S <- sort(table(dS$censore_type), decreasing = TRUE)
p_censoring_S <- prop.table(t_censoring_S)
head(cbind(t_censoring_S,round(100*p_censoring_S,1)))




##------------------------------------------------------------------------------
# Fourth table: SAMPL
##------------------------------------------------------------------------------

d_sampl0 <- d %>% select(all_of(VAR_SAMPL))
names(d_sampl0) <- VAR_SAMPL_SHORT

##-- Table SAMPL (with NAs)  ---------------------------------------------------
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

##-- Plot SAMPL (without NAs)  -------------------------------------------------
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

##-- Plot SAMPL guideline (stratified by years) --------------------------------
d_sampl0_before <- d %>% filter(year<2020)  %>% select(all_of(VAR_SAMPL)) %>% mutate(year="<2020")
d_sampl0_after  <- d %>% filter(year>=2020) %>% select(all_of(VAR_SAMPL)) %>% mutate(year="≥2020")
names(d_sampl0_before) <- names(d_sampl0_after) <- c(VAR_SAMPL_SHORT,'year')
d_sampl0_year <- rbind(d_sampl0_before,d_sampl0_after)

d_sampl0_year_long <- d_sampl0_year %>%
  pivot_longer(cols = 1:14, names_to = "variable", values_to = "Value")

d_sampl0_year_prop <- d_sampl0_year_long %>%
  filter(!is.na(Value)) %>%
  group_by(year, variable) %>%
  summarise(n_yes    = sum(Value == "Yes"),
            n_total  = n(),
            prop_yes = n_yes / n_total,
            .groups  = "drop")


d_sampl0_year_prop$variable <- factor(d_sampl0_year_prop$variable,
                                      levels=arrange(d_sampl[d_sampl$category=='Well',],-percentage) %>% 
                                                     select(variable) %>% pull)

ggplot(d_sampl0_year_prop, aes(x = variable, y = prop_yes, fill = year)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::percent(prop_yes, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.2, size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    y = "Percentage of well-reported studies",
    x = "",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

# ggsave(filename='../../../Figures/figure_S3.jpeg', width = 12, height = 7, dpi = 300)


