## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup, echo=FALSE--------------------------------------------------------
library(DSjobtracker)
data("DStidy")

## ----eval=FALSE---------------------------------------------------------------
#  # install devtools if not already installed
#  # install.packages("devtools")
#  devtools::install_github("thiyangt/DSjobtracker")

## ----eval=FALSE---------------------------------------------------------------
#  library(DSjobtracker)

## ----eval=FALSE---------------------------------------------------------------
#  data("DStidy")

## -----------------------------------------------------------------------------
tibble::glimpse(DStidy)

## ----eval=FALSE---------------------------------------------------------------
#  ?DStidy

## ----warning=FALSE,message=FALSE,dev='png'------------------------------------
library(tidyr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(wordcloud2)
library(viridis)
library(forcats)

theme_set(theme_minimal())

skills_long <- DStidy %>%
  select(c(R:Bahasa_Malaysia)) %>%
  pivot_longer(c(R:Bahasa_Malaysia), values_to = "Value", names_to = "Name") %>%
  mutate(Value = as.numeric(levels(Value))[Value]) %>%
  group_by(Name) %>%
  summarize(Total = sum(Value)) %>%
  arrange(Total)

skills_long %>%
  mutate(Name = factor(Name, levels = .$Name)) %>%
  top_n(20) %>%
  ggplot(aes(x = Name, y = Total)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = Total),
    nudge_y = -10, size = 3.25,
    label.padding = unit(0.125, "lines")
  ) +
  coord_flip() +
  labs(
    title = "Top twenty skills required for data science jobs",
    x = "Skill Required", y = "No of job vacancies"
  )

## -----------------------------------------------------------------------------
not_software_columns <- c(
  "Presentation_Skills", "Data_visualization",
  "Spreadsheets", "BigData",
  "Communication", "BigData",
  "Data_warehouse", "cloud_storage",
  "Google_Cloud", "Machine_Learning",
  "Computer_vision", "Deep_Learning", "RDBMS",
  "web_design_and_development_tools", "AI",
  "Natural_Language_Processing(NLP)",
  "graphics_and_design_skills", "Data_marketing",
  "SEO", "Content_Management",
  "Data_Pipelines", "MPP_Platforms", "agile_execution",
  "Data_management", "Data_mining", "Data_science",
  "Web_Analytic_tools", "IOT",
  "Numerical_Analysis", "Finance_Knowledge", "Economic",
  "Investment_Knowledge", "Problem_Solving",
  "Korean_language", "Team_Handling",
  "Debtor_reconcilation", "Payroll_management",
  "Bayesian", "Optimization", "Bahasa_Malaysia"
)

## -----------------------------------------------------------------------------
indicators <- DStidy %>%
  select(c(R:Bahasa_Malaysia))
software_indicators <- indicators %>%
  select(colnames(.)[!colnames(.) %in% not_software_columns])

software_indicators_long <- software_indicators %>%
  pivot_longer(colnames(.), values_to = "Value", names_to = "Name") %>%
  mutate(Value = as.numeric(levels(Value))[Value]) %>%
  group_by(Name) %>%
  summarize(Total = sum(Value)) %>%
  arrange(Total)

wordcloud2(software_indicators_long %>%
  transmute(word = Name, freq = log(Total)),
size = 0.35,
minRotation = pi / 2, maxRotation = pi / 2,
color = viridis(nrow(software_indicators_long)),
fontFamily = "Montserrat"
)

## -----------------------------------------------------------------------------
count_data <- DStidy %>%
  select(Experience_Category, Edu_Category) %>%
  filter(!is.na(Edu_Category)) %>%
  count(Experience_Category, Edu_Category)

max_vacancies <- max(count_data$n)

count_data %>%
  ggplot(aes(x = fct_rev(Experience_Category), y = n, color = Edu_Category, size = n)) +
  geom_point() +
  geom_curve(
    data = tibble(
      x1 = c(4.4), x2 = c(4),
      y1 = c(62.5), y2 = c(58)
    ),
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  ) +
  coord_flip() +
  lims(y = c(0, 100)) +
  annotate("text",
    x = 4.65, y = 60, size = 2.8,
    label = paste("With less than 2 years of experience\n and a BSc degree \n you can still apply for", max_vacancies, "job vacancies")
  ) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  scale_size(guide = "none") +
  labs(
    x = "Years of Experience needed",
    y = "Number of job vacancies",
    color = "Minimum education qualification"
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, title.position = "top"))

## ----fig.height=14------------------------------------------------------------
# radar plot with job category and skills in a radar
job_skill_data <- DStidy %>% 
  select(R:Bahasa_Malaysia,Job_Category) %>% 
  filter(Job_Category != "Unimportant") %>% 
  pivot_longer(c(R:Bahasa_Malaysia),names_to="Name",values_to = "Value") %>%
  mutate(Value = as.numeric(levels(Value))[Value]) %>%
  group_by(Job_Category,Name) %>% 
  summarize(Total = sum(Value)) %>% 
  ungroup() %>% 
  filter(Total > 0) %>% 
  mutate(logTotal = log(Total)) %>%
  ungroup()

common_skills <- job_skill_data %>% 
  count(Name) %>% 
  filter(n == 3 & !(Name %in% not_software_columns)) %>% 
  .$Name

plot_data <- job_skill_data %>% 
  filter(Name %in% common_skills) %>% 
  mutate(Name = as.numeric(factor(Name,labels = common_skills)))

  plot_data %>% 
  ggplot(aes(x = Name,y = logTotal,fill = Job_Category,color = Job_Category))+
  geom_area(size = 0,position = position_dodge(width=0.9),alpha=0.1) + 
  geom_point(size=0.5) +
  geom_segment(aes(xend = Name,yend = logTotal,alpha = logTotal),
               y = 0,size = 1.25)+
  scale_x_continuous(labels = common_skills,breaks = 1:length(common_skills)) +
  theme(axis.text.y = element_blank(),
        legend.position = "none") + 
  labs(x = NULL,
       y = NULL) +
  scale_fill_brewer(palette = "Set1",type = "qual") + 
  coord_polar() + 
  facet_wrap(~ Job_Category,ncol=1)

