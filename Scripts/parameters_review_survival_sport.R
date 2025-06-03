##-- VAR names
NEW_NAMES <- c('id','title','aim','aim2','include_model','author','country','year',
               'journal','journal_type','N','participant_type','age','sport',
               'sport2','gender','participant_level','data_source','main_goal',
               'model_type','model_type2','analysis_details','kaplan_meier',
               'cox','logreg_rates','log_rank','tarone','prentice_wilcoxon',
               'harrington_fleming','cox_random','msm','glm_pseudo_obs',
               'rsf','parametric_model','fine_gray','aalen_johanssen','competing',
               'bayes_multilevel_logreg','any_method','Paradigm',
               'software','macro_package','data_sharing','code_sharing','purpose',
               'identify_dates_or_events','circumstances_censored',
               'statistical_methods_rates','assumptions','estimated_surv_prob',
               'num_risk','plot_cumulative','median_surv','graph_or_table',
               'statistical_methods curves','p_value',
               'reg_model','risk_measure','additional_info','censore_type',
               'truncation','random_effects','time_dependent','reason_purpose','reason_purpose_cat')

##-- SAMPL items
VAR_SAMPL <- c('purpose','identify_dates_or_events','circumstances_censored',
               'statistical_methods_rates','assumptions','estimated_surv_prob',
               'num_risk','plot_cumulative','median_surv','graph_or_table',
               'statistical_methods curves','p_value',
               'reg_model','risk_measure')

VAR_SAMPL_SHORT <- c('Purpose','Dates def.','Censoring',
               'Survival rates','Assumptions','Survival prob.',
               'No. risk','Cumulative plot','Median','Graph/table',
               'Curves','P value', 'Reg. model','Risk Measure')

VAR_METHODS  <- c("kaplan_meier","cox","cox_random",
                  "log_rank","tarone", "prentice_wilcoxon",
                  "parametric_model","fine_gray","aalen_johanssen",
                  "rsf","msm",
                  "logreg_rates","glm_pseudo_obs","bayes_multilevel_logreg")

VAR_METHODS_SHORT  <- c("Kaplan Meier","Cox model","Cox model - Random effects",
                        "Log-rank","Tarone-Ware", "Prentice Wilcoxon",
                        "Parametric models","Fine-Gray","Aalen Johansen",
                        "Random Survival Forest","Multistate models",
                        "Logistic regression","GLM pseudo-observations",
                        "Bayes multilevel logregression")

##-- Info_countries
info_countries <- data.frame(
  country = c("Australia", "New Zealand", "Belgium", "Austria", "Ireland", "Iceland", 
              "Norway", "Switzerland", "UK", "Sweden", "Denmark", "Finland", 
              "France", "Germany", "Italy", "The Netherlands", "Luxembourg", "Spain",
              "Brazil", "Argentina", "USA", "Canada", "Japan", "China", "Russia", "Korea", "Qatar", 
              "Not reported", "."),
  continent = c(rep("Oceania", 2), rep("Europe", 16), rep("America", 4), 
                rep("Asia", 5), rep("Unknown", 2))
)


