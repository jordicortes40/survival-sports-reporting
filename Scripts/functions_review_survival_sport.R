##-- Function to split country names
separate_countries <- function(country_string) {
  countries <- unlist(str_split(country_string, ",\\s*|\\s+and\\s+"))
  return(countries)
}

# Split software
separate_software <- function(software_string) {
  software <- unlist(str_split(software_string, ",\\s*|\\s+and\\s+"))
  return(software)
}

# Split port
separate_sport <- function(sport_string) {
  sport <- unlist(str_split(unlist(str_split(sport_string, ",")),",\\s*|\\s+and\\s+"))
  return(sport)
}

# Table with absolute and relative frequencies using janitor R package
my_tab <- function(var1, var2){
  tab <- d %>% 
    tabyl(!!sym(var1), !!sym(var2)) %>% 
    adorn_totals("row") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns()
  tab
}