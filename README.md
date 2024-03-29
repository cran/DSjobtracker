
# DSjobtracker

<img src="man/figures/hexsticker.png" align="right" height="200"/>

What skills and qualifications are required for data science related
jobs?

# Installation

You can install the **development** version from
[GitHub](https://github.com/thiyangt/DSjobtracker):

``` r
#install.packages("devtools")
devtools::install_github("thiyangt/DSjobtracker")
library(DSjobtracker)
```

# Glimpse of tidy data

``` r
tibble::glimpse(DStidy)
```

    Rows: 1,172
    Columns: 109
    $ ID                                 <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, …
    $ Consultant                         <chr> "Thiyanga", "Jayani", "Jayani", "Ja…
    $ DateRetrieved                      <dttm> 2020-08-05, 2020-08-07, 2020-08-07…
    $ DatePublished                      <dttm> NA, 2020-07-31, 2020-08-06, 2020-0…
    $ Job_title                          <chr> NA, "Junior Data Scientist", "Engin…
    $ Company                            <chr> NA, "Dialog Axiata PLC", "London St…
    $ R                                  <dbl> 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0,…
    $ SAS                                <dbl> 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,…
    $ SPSS                               <dbl> 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,…
    $ Python                             <dbl> 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0,…
    $ MAtlab                             <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Scala                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ `C#`                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ `MS Word`                          <dbl> 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1,…
    $ `Ms Excel`                         <dbl> 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1,…
    $ `OLE/DB`                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ `Ms Access`                        <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1,…
    $ `Ms PowerPoint`                    <dbl> 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,…
    $ Spreadsheets                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Data_visualization                 <dbl> 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
    $ Presentation_Skills                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Communication                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ BigData                            <dbl> 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0,…
    $ Data_warehouse                     <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ cloud_storage                      <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Google_Cloud                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ AWS                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Machine_Learning                   <dbl> 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0,…
    $ `Deep Learning`                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Computer_vision                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Java                               <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0,…
    $ `C++`                              <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ C                                  <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ `Linux/Unix`                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ SQL                                <dbl> 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0,…
    $ NoSQL                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ RDBMS                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Oracle                             <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
    $ MySQL                              <dbl> 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0,…
    $ PHP                                <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
    $ SPL                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ web_design_and_development_tools   <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
    $ AI                                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ `Natural_Language_Processing(NLP)` <dbl> 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,…
    $ `Microsoft Power BI`               <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Google_Analytics                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ graphics_and_design_skills         <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
    $ Data_marketing                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ SEO                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Content_Management                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Tableau                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
    $ D3                                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
    $ Alteryx                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ KNIME                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Spotfire                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Spark                              <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,…
    $ S3                                 <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ Redshift                           <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ DigitalOcean                       <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ Javascript                         <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ Kafka                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Storm                              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Bash                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Hadoop                             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,…
    $ Data_Pipelines                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ MPP_Platforms                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Qlik                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Pig                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Hive                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,…
    $ Tensorflow                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ `Map/Reduce`                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Impala                             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Solr                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Teradata                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ MongoDB                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Elasticsearch                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ YOLO                               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ `agile execution`                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,…
    $ Data_management                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ pyspark                            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Data_mining                        <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ Data_science                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,…
    $ Web_Analytic_tools                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ IOT                                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Numerical_Analysis                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Economic                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Finance_Knowledge                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Investment_Knowledge               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Problem_Solving                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Team_Handling                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Debtor_reconcilation               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Payroll_management                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Bayesian                           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Optimization                       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ Knowledge_in                       <chr> NA, NA, "Elasticsearch, Logstash, K…
    $ City                               <chr> NA, "Colombo", "Colombo", "Colombo"…
    $ Educational_qualifications         <chr> NA, "Degree in Engineering / IT or …
    $ Salary                             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    $ URL                                <chr> NA, "https://www.google.com/search?…
    $ Search_Term                        <chr> NA, "Data Analysis Jobs in Sri Lank…
    $ Job_Category                       <chr> "Unimportant", "Data Science", "Dat…
    $ Experience_Category                <chr> "More than 2 and less than 5 years"…
    $ Location                           <chr> NA, "Sri Lanka", "Sri Lanka", "Sri …
    $ `Payment Frequency`                <chr> "unspecified", "unspecified", "unsp…
    $ BSc_needed                         <dbl> 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1,…
    $ MSc_needed                         <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    $ PhD_needed                         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    $ `English Needed`                   <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,…
    $ year                               <dbl> 2020, 2020, 2020, 2020, 2020, 2020,…

# 2021 survey data

``` r
df2021 <- get_data(2021)
df2021
```

    # A tibble: 382 × 115
          ID Consultant URL        Search_Term DateRetrieved DatePublished Job_Field
       <dbl> <chr>      <chr>      <chr>       <chr>         <chr>         <chr>    
     1     1 Abhishanya https://w… statistics… 11/10/2021    11/10/2021    Informat…
     2     2 Abhishanya https://w… statistics… 11/11/2021    11/11/2021    Education
     3     3 Abhishanya https://w… Data scien… 11/11/2021    11/9/2021     Other    
     4     4 Abhishanya https://w… Data scien… 11/11/2021    10/13/2021    Other    
     5     5 Abhishanya https://w… Statistics… 11/11/2021    10/14/2021    Informat…
     6     6 Abhishanya https://w… Data Scien… 11/11/2021    10/14/2020    Health   
     7     7 Abhishanya https://c… Data engin… 11/11/2021    11/8/2021     Other    
     8     8 Abhishanya https://l… Data engin… 11/11/2021    11/9/2021     Other    
     9     9 Abhishanya https://w… Data engin… 11/11/2021    10/20/2021    Informat…
    10    10 Abhishanya https://w… Data engin… 11/11/2021    10/21/2021    Finance  
    # ℹ 372 more rows
    # ℹ 108 more variables: Job_title <chr>, Company <chr>, Knowledge_in <chr>,
    #   `Minimum Experience in Years` <dbl>, City <chr>, Location <chr>,
    #   Educational_qualifications <chr>, `Payment Frequency` <chr>,
    #   Currency <chr>, Salary <dbl>, `English Needed` <dbl>,
    #   `English proficiency description` <chr>, Additional_languages <chr>,
    #   AI <dbl>, `Natural_Language_Processing(NLP)` <dbl>, Data_Pipelines <dbl>, …

# Preview of the tidy version of the dataset

``` r
head(DStidy)
```

    # A tibble: 6 × 109
         ID Consultant DateRetrieved       DatePublished       Job_title     Company
      <dbl> <chr>      <dttm>              <dttm>              <chr>         <chr>  
    1     1 Thiyanga   2020-08-05 00:00:00 NA                  <NA>          <NA>   
    2     2 Jayani     2020-08-07 00:00:00 2020-07-31 00:00:00 Junior Data … Dialog…
    3     3 Jayani     2020-08-07 00:00:00 2020-08-06 00:00:00 Engineer, An… London…
    4     4 Jayani     2020-08-07 00:00:00 2020-07-24 00:00:00 CI-Statistic… E.D. B…
    5     5 Jayani     2020-08-07 00:00:00 2020-07-24 00:00:00 DA-Data Anal… E.D. B…
    6     6 Jayani     2020-08-07 00:00:00 2020-08-13 00:00:00 Data Scienti… Emirat…
    # ℹ 103 more variables: R <dbl>, SAS <dbl>, SPSS <dbl>, Python <dbl>,
    #   MAtlab <dbl>, Scala <dbl>, `C#` <dbl>, `MS Word` <dbl>, `Ms Excel` <dbl>,
    #   `OLE/DB` <dbl>, `Ms Access` <dbl>, `Ms PowerPoint` <dbl>,
    #   Spreadsheets <dbl>, Data_visualization <dbl>, Presentation_Skills <dbl>,
    #   Communication <dbl>, BigData <dbl>, Data_warehouse <dbl>,
    #   cloud_storage <dbl>, Google_Cloud <dbl>, AWS <dbl>, Machine_Learning <dbl>,
    #   `Deep Learning` <dbl>, Computer_vision <dbl>, Java <dbl>, `C++` <dbl>, …

# Preview of the untidy version of the dataset

``` r
head(DSraw)
```

    # A tibble: 6 × 152
         ID Consultant DateRetrieved DatePublished Job_title     Company     R   SAS
      <dbl> <chr>      <chr>         <chr>         <chr>         <chr>   <dbl> <dbl>
    1     1 Thiyanga   05/08/2020    <NA>          <NA>          <NA>        1     1
    2     2 Jayani     07/08/2020    31/07/2020    Junior Data … Dialog…     1     0
    3     3 Jayani     07/08/2020    06/08/20      Engineer, An… London…     0     0
    4     4 Jayani     07/08/2020    24/07/2020    CI-Statistic… E.D. B…     1     1
    5     5 Jayani     07/08/2020    24/07/2020    DA-Data Anal… E.D. B…     0     1
    6     6 Jayani     07/08/2020    13/08/2020    Data Scienti… Emirat…     1     0
    # ℹ 144 more variables: SPSS <dbl>, Python <dbl>, MAtlab <dbl>, Scala <dbl>,
    #   `C#` <dbl>, `MS Word` <dbl>, `Ms Excel` <dbl>, `OLE/DB` <dbl>,
    #   `Ms Access` <dbl>, `Ms PowerPoint` <dbl>, Spreadsheets <dbl>,
    #   Data_visualization <dbl>, Presentation_Skills <dbl>, Communication <dbl>,
    #   BigData <dbl>, Data_warehouse <dbl>, cloud_storage <dbl>,
    #   Google_Cloud <dbl>, AWS <dbl>, Machine_Learning <dbl>,
    #   `Deep Learning` <dbl>, Computer_vision <dbl>, Java <dbl>, `C++` <dbl>, …
