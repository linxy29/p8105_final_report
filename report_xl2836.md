report\_xl2836
================
Xinyi Lin
12/5/2018

Data
====

Data Source
-----------

This [data](https://www.kaggle.com/osmi/mental-health-in-tech-2016/downloads/mental-health-in-tech-2016.zip/1) comes from [OSMI Mental Health in Tech Survey](https://osmihelp.org/research/) which we first find in [kaggle](https://www.kaggle.com/osmi/mental-health-in-tech-2016).

This data includes 1433 observations and 63 variables.

Data cleaning
-------------

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

First, we should import data.

``` r
mentalhealth_2016_df = read_csv("./data/mental_health_in_tech_2016.csv") 
```

    ## Warning: Duplicated column names deduplicated: 'Why or why not?' => 'Why or
    ## why not?_1' [40]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Are you self-employed?` = col_integer(),
    ##   `Is your employer primarily a tech company/organization?` = col_integer(),
    ##   `Is your primary role within your company related to tech/IT?` = col_integer(),
    ##   `Do you have medical coverage (private insurance or state-provided) which includes treatment of  mental health issues?` = col_integer(),
    ##   `Do you have previous employers?` = col_integer(),
    ##   `Have you ever sought treatment for a mental health issue from a mental health professional?` = col_integer(),
    ##   `What is your age?` = col_integer()
    ## )

    ## See spec(...) for full column specifications.

Variables name in this data set are questions, so we change it to [short words](Variable%20name.pdf) for easy-reading.

``` r
colnames(mentalhealth_2016_df) <- c("self_employed", "num_employees", "tech_company", "tech_role", "benefits", "care_options", "employer_discussion", "employer_help", "anonymity", "medical_leave", "mental_health_consequences", "physical_health_consequences", "coworkers_discussion", "supervisor_discussion", "mental_vs_physical", "obs_consequence", "medical_coverage", "help_resourcces", "whether_reveal_business_contacts", "reveal_concequences_business_contects", "whether_reveal_coworkers", "reveal_concequences_coworkers", "productivity_affect", "work_time_affected", "preemployers", "preemployers_benefits", "preemployers_care_options", "preemployers_discussion", "preemployer_help", "pre_anonymity", "pre_mental_health_consequences", "pre_physical_health_consequences", "pre_coworkers_discussion", "pre_supervisors_discussion", "pre_mental_vs_physical", "pre_obs_consequence", "physical_health_interview", "physical_health_interview_reason", "mental_health_interview", "mental_health_interview_reason", "career_influence", "coworkers_view", "friends_family_share", "unsupportive_badly_handled", "less_likely_reveal", "family_history", "mental_health_previous", "mental_health_now", "condition_diagnosed", "possible_condition", "professional_diagnosed", "condition_professional_diagnosed", "seek_treatment", "work_interferes_treated", "work_interferes_untreated", "age", "gender", "country_live", "territory_live", "country_work", "territory_work", "work_position_kind", "work_remotely")
```

One observation in age is 323 which is impossible, so we exclude this observation. Besides, there are lots of different answers in `gender` variable which makes analysis process more difficult, so we use 'Female' and 'Male' to represent their gender.

``` r
mentalhealth_2016_tidied =
  mentalhealth_2016_df %>% 
  filter(age < 100) %>% 
  mutate(gender = str_replace(gender, "^[Ww]oman$", "Female"),     # female
         gender = str_replace(gender, "^[Ff]$", "Female"),
         gender = str_replace(gender, "^fem$", "Female"),
         gender = str_replace(gender, "^female$", "Female"),
         gender = str_replace(gender, "^female/woman$", "Female"),
         gender = str_replace(gender, "^fm$", "Female"),
         gender = str_replace(gender, "AFAB", "Female"),
         #gender = str_replace(gender, "AFAB", "Female"),
         gender = str_replace(gender, "^[Mm]$", "Male"),  # male
         gender = str_replace(gender, "^mail$", "Male"),  #?
         gender = str_replace(gender, "^MALE$", "Male"),
         gender = str_replace(gender, "^Male.$", "Male"),
         gender = str_replace(gender, "^Malr$", "Male"),    #?
         gender = str_replace(gender, "^[Mm]an$", "Male"),
         #gender = str_replace(gender, "^man$", "Male"),
         gender = str_replace(gender, "^male$", "Male"))

head(mentalhealth_2016_df)
```

    ## # A tibble: 6 x 63
    ##   self_employed num_employees tech_company tech_role benefits care_options
    ##           <int> <chr>                <int>     <int> <chr>    <chr>       
    ## 1             0 26-100                   1        NA Not eli… N/A         
    ## 2             0 6-25                     1        NA No       Yes         
    ## 3             0 6-25                     1        NA No       N/A         
    ## 4             1 <NA>                    NA        NA <NA>     <NA>        
    ## 5             0 6-25                     0         1 Yes      Yes         
    ## 6             0 More than 10…            1        NA Yes      I am not su…
    ## # ... with 57 more variables: employer_discussion <chr>,
    ## #   employer_help <chr>, anonymity <chr>, medical_leave <chr>,
    ## #   mental_health_consequences <chr>, physical_health_consequences <chr>,
    ## #   coworkers_discussion <chr>, supervisor_discussion <chr>,
    ## #   mental_vs_physical <chr>, obs_consequence <chr>,
    ## #   medical_coverage <int>, help_resourcces <chr>,
    ## #   whether_reveal_business_contacts <chr>,
    ## #   reveal_concequences_business_contects <chr>,
    ## #   whether_reveal_coworkers <chr>, reveal_concequences_coworkers <chr>,
    ## #   productivity_affect <chr>, work_time_affected <chr>,
    ## #   preemployers <int>, preemployers_benefits <chr>,
    ## #   preemployers_care_options <chr>, preemployers_discussion <chr>,
    ## #   preemployer_help <chr>, pre_anonymity <chr>,
    ## #   pre_mental_health_consequences <chr>,
    ## #   pre_physical_health_consequences <chr>,
    ## #   pre_coworkers_discussion <chr>, pre_supervisors_discussion <chr>,
    ## #   pre_mental_vs_physical <chr>, pre_obs_consequence <chr>,
    ## #   physical_health_interview <chr>,
    ## #   physical_health_interview_reason <chr>, mental_health_interview <chr>,
    ## #   mental_health_interview_reason <chr>, career_influence <chr>,
    ## #   coworkers_view <chr>, friends_family_share <chr>,
    ## #   unsupportive_badly_handled <chr>, less_likely_reveal <chr>,
    ## #   family_history <chr>, mental_health_previous <chr>,
    ## #   mental_health_now <chr>, condition_diagnosed <chr>,
    ## #   possible_condition <chr>, professional_diagnosed <chr>,
    ## #   condition_professional_diagnosed <chr>, seek_treatment <int>,
    ## #   work_interferes_treated <chr>, work_interferes_untreated <chr>,
    ## #   age <int>, gender <chr>, country_live <chr>, territory_live <chr>,
    ## #   country_work <chr>, territory_work <chr>, work_position_kind <chr>,
    ## #   work_remotely <chr>
