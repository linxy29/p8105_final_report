Report
================
12/5/2018

Motivation
----------

The purpose of this project is to apply the skills we learnt in “Data Science I” to access data in real world setting and preferably make some impact on a concerning public health issue. In this case, we choose to analyze the mental health data gathered from an online survey done by OSMI, and hope our report could promote workplace mental health awareness and be used as reference by job seeker while job haunting. However, after exploring our data, we found that we aren’t able to answers some question we listed in the proposal, since our data is very limited.

Related work
------------

Mental health has always been an interesting topic since it is closely related to our life, 20 percent of the total population have diagnosable mental disorder, also, recent study showed that graduate student are at more risk of depression. Therefore, while browsing Kaggle, the challenge of mental health in tech companies immediately draw our attention. As future biostatistician, It is highly possible that we will pursue a tech career. And knowing whether particular work type is more stressful, if some factor are more associated with depression, will help guide us through the job seeking process and maybe even provide some help when others are facing mental health issues.

Initial questions
-----------------

Initially, we want to answer the following questions: Do mental health illness and attitudes towards mental health vary by geographic location? How strong is the association between family history and mental health illness or other predictors? Attitudes toward mental health VS attitudes toward physical health consequence? Do tech companies provide more or less mental health benefits compared to non-tech company?

After exploring the data, we found that the we are not able to answer the latter two of our proposed question, since most of the participant in the survey are either from a tech company or is working as a tech in non-tech company. Also, the attitude towards mental health are very difficult to access since the questionnaire is asking what do participant think that their company would react if they had mental health illness, and this information could be highly biased since there is no standard measure. Instead, we decide to look into the number of people with mental health disorder vary by geographic location, and found that in general around 50% of participant of the survey is currently having mental health issue. Some states have higher percentage than other, but since some states only have one or two people who take the survey, those states tend to have extreme values like 100% or 0 %. For the second question, since mental health illness is a categorical variable, we are unable to use it as a response variable within a linear model. Therefore, we performed conditional inference tree and decision tree to analyze the data, and found family history of mental health illness and the size of the company is associated with current mental health illness. Although some of the question are not accessible, we found other interesting trend while exploring, such as the impact of work performance of mental health illness treatment, and people with particular mental health disease are more willing to seek treatment than others, and these questions will be further discussed in the exploration analysis session of the report.

Data
----

### Data Source

Our [data](https://www.kaggle.com/osmi/mental-health-in-tech-2016/downloads/mental-health-in-tech-2016.zip/1) comes from [OSMI Mental Health in Tech Survey](https://osmihelp.org/research/) which we first find in [kaggle](https://www.kaggle.com/osmi/mental-health-in-tech-2016).

This data includes 1433 observations and 63 variables.

### Data cleaning

``` r
library(tidyverse)
```

First, we should import data.

``` r
mentalhealth_2016_df = read_csv("./data/mental_health_in_tech_2016.csv") 
```

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

The age of one observation is 323 which is impossible, so we exclude this observation. Besides, there are lots of different forms of answers in `gender` variable which makes analysis process more difficult, so we use 'Female' and 'Male' to represent gender in each observation.

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
```

Discussion
----------

The dataset we conclude in this project contains all aspects of questions regarding mental health issues. Using the data and model building process, we find out that family history and the numbers of employees have relationship with existence of mental health disorders.

The plots in our project shows that people with personality disorder are less likely to actively seek treatment compared to people with post-traumatic stress disorder. The violin plot shows that for all kinds of disorders treatment can improve their mental health situation.

The first limitation of the project is that the data set we include is not large enough. Participants mostly lives in US so we cannot generate the conclusion in global scale. The second limitation of our project is that we fail to test the relationship between gender and mental health status since participants using different kinds of gender-identify strategies.
