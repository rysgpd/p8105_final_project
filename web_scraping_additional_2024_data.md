web_scraping_additional_2024_data
================
Laura Henze
2024-11-26

``` r
library(tidyverse)
library(rvest)
```

``` r
cleaned_data <- read.csv("data/cleaned_df.csv")

head(cleaned_data)
```

    ##      tourney_name tourney_date winner_id winner_seed winner_entry
    ## 1 Australian Open   2014-01-13    104745           1         <NA>
    ## 2 Australian Open   2014-01-13    106423          NA           WC
    ## 3 Australian Open   2014-01-13    106058          NA         <NA>
    ## 4 Australian Open   2014-01-13    104792          25         <NA>
    ## 5 Australian Open   2014-01-13    104312          24         <NA>
    ## 6 Australian Open   2014-01-13    105385          NA         <NA>
    ##          winner_name winner_hand winner_ht winner_ioc winner_age loser_id
    ## 1       Rafael Nadal           L       185        ESP       27.6   106071
    ## 2 Thanasi Kokkinakis           R       196        AUS       17.7   104997
    ## 3          Jack Sock           R       185        USA       21.3   104735
    ## 4       Gael Monfils           R       193        FRA       27.3   105992
    ## 5      Andreas Seppi           R       190        ITA       29.8   103720
    ## 6       Donald Young           L       183        USA       24.4   104898
    ##   loser_seed loser_entry     loser_name loser_hand loser_ht loser_ioc loser_age
    ## 1         NA        <NA>  Bernard Tomic          R      193       AUS      21.2
    ## 2         NA        <NA>  Igor Sijsling          R      190       NED      26.4
    ## 3         NA        <NA>   Tobias Kamke          R      180       GER      27.6
    ## 4         NA        <NA>  Ryan Harrison          R      183       USA      21.6
    ## 5         NA        <NA> Lleyton Hewitt          R      180       AUS      32.8
    ## 6         NA        <NA>    Robin Haase          R      190       NED      26.7
    ##                       score round minutes w_ace w_df w_svpt w_1st_in w_1st_won
    ## 1                   6-4 RET  R128      41     5    1     26       16        14
    ## 2     7-6(4) 0-6 7-6(3) 6-2  R128     182     6    9    147       79        55
    ## 3        7-6(5) 5-7 6-2 6-4  R128     242    16   12    148       74        54
    ## 4               6-4 6-4 6-4  R128     111    10    5     78       51        44
    ## 5    7-6(4) 6-3 5-7 5-7 7-5  R128     258    17    5    176      103        72
    ## 6 6-7(4) 7-6(2) 6-2 1-0 RET  R128     154     0    5    106       77        58
    ##   w_2nd_won w_sv_gms w_bp_saved w_bp_faced l_ace l_df l_svpt l_1st_in l_1st_won
    ## 1         6        5          0          0     3    1     28       19        15
    ## 2        36       19          5          9    20    4    123       73        57
    ## 3        43       21         10         12     6    8    136       66        51
    ## 4        17       15          0          0     2    1     93       57        42
    ## 5        40       29          6         13    23    5    177       90        76
    ## 6        16       16          3          4    11    6    124       81        55
    ##   l_2nd_won l_sv_gms l_bp_saved l_bp_faced winner_rank winner_rank_points
    ## 1         3        5          0          1           1              13130
    ## 2        22       19          2          5         570                 53
    ## 3        36       21          6         10          95                582
    ## 4        17       15          1          4          32               1245
    ## 5        38       28         11         18          25               1360
    ## 6        19       15          5          7          91                595
    ##   loser_rank loser_rank_points
    ## 1         57               810
    ## 2         73               681
    ## 3         78               651
    ## 4        110               504
    ## 5         43              1010
    ## 6         45               977

``` r
library(dplyr)
library(readr)
library(stringr)


url <- "http://www.tennis-data.co.uk/2024/frenchopen.csv"
tennis_data <- read_csv(url, show_col_types = FALSE)


processed_data <- tennis_data %>%
  mutate(
    score = paste0(
      ifelse(!is.na(W1), paste0(W1, "-", L1, " "), ""),
      ifelse(!is.na(W2), paste0(W2, "-", L2, " "), ""),
      ifelse(!is.na(W3), paste0(W3, "-", L3, " "), ""),
      ifelse(!is.na(W4), paste0(W4, "-", L4, " "), ""),
      ifelse(!is.na(W5), paste0(W5, "-", L5, " "), "")
    ) %>% str_trim(),
    tourney_date = as.Date(Date, "%d/%m/%Y") %>% format("%Y-%m-%d"),  # Format date to match your existing data
    tourney_name = str_replace(Tournament, "French Open", "Roland Garros")  # Rename tournament
  ) %>%
  select(
    tourney_name,
    tourney_date,
    winner_name = Winner,
    loser_name = Loser,
    score
  )

print(processed_data)
```

    ## # A tibble: 127 × 5
    ##    tourney_name  tourney_date winner_name  loser_name           score           
    ##    <chr>         <chr>        <chr>        <chr>                <chr>           
    ##  1 Roland Garros 2024-05-26   Nakashima B. Moreno De Alboran N. 6-1 6-7 6-3 6-2 
    ##  2 Roland Garros 2024-05-26   Rublev A.    Daniel T.            6-2 6-7 6-3 7-5 
    ##  3 Roland Garros 2024-05-26   Martinez P.  Tirante T.A.         5-7 6-4 3-6 6-4…
    ##  4 Roland Garros 2024-05-26   Sonego L.    Humbert U.           6-4 2-6 6-4 6-3 
    ##  5 Roland Garros 2024-05-26   Zhang Zh.    Vukic A.             6-4 4-6 6-3 7-5 
    ##  6 Roland Garros 2024-05-26   De Jong J.   Draper J.            7-5 6-4 6-7 3-6…
    ##  7 Roland Garros 2024-05-26   Hurkacz H.   Mochizuki S.         4-6 6-3 3-6 6-0…
    ##  8 Roland Garros 2024-05-26   Dimitrov G.  Kovacevic A.         6-4 6-3 6-4     
    ##  9 Roland Garros 2024-05-26   Marterer M.  Thompson J.          6-3 6-2 6-0     
    ## 10 Roland Garros 2024-05-26   Alcaraz C.   Wolf J.J.            6-1 6-2 6-1     
    ## # ℹ 117 more rows

``` r
#View(processed_data)
```

``` r
library(dplyr)
library(readr)
library(stringr)


url <- "http://www.tennis-data.co.uk/2024/usopen.csv"
us_open_data <- read_csv(url, show_col_types = FALSE)


processed_us_open_data <- us_open_data %>%
  mutate(
    score = paste0(
      ifelse(!is.na(W1), paste0(W1, "-", L1, " "), ""),
      ifelse(!is.na(W2), paste0(W2, "-", L2, " "), ""),
      ifelse(!is.na(W3), paste0(W3, "-", L3, " "), ""),
      ifelse(!is.na(W4), paste0(W4, "-", L4, " "), ""),
      ifelse(!is.na(W5), paste0(W5, "-", L5, " "), "")
    ) %>% str_trim(),
    tourney_date = as.Date(Date, "%d/%m/%Y") %>% format("%Y-%m-%d"),  # Format date to match your existing data
    tourney_name = str_replace(Tournament, "US Open", "US Open")  # Ensure tournament name is consistent
  ) %>%
  select(
    tourney_name,
    tourney_date,
    winner_name = Winner,
    loser_name = Loser,
    score
  )


print(processed_us_open_data)
```

    ## # A tibble: 127 × 5
    ##    tourney_name tourney_date winner_name        loser_name       score          
    ##    <chr>        <chr>        <chr>              <chr>            <chr>          
    ##  1 US Open      2024-08-26   Zverev A.          Marterer M.      6-2 6-7 6-3 6-2
    ##  2 US Open      2024-08-26   Bautista Agut R.   Nardi L.         7-5 7-6 7-6    
    ##  3 US Open      2024-08-26   Comesana F.        Stricker D.      4-6 6-3 7-6 6-3
    ##  4 US Open      2024-08-26   Humbert U.         Monteiro T.      6-3 6-4 6-4    
    ##  5 US Open      2024-08-26   Krueger M.         Grenier H.       4-6 6-3 6-4 7-5
    ##  6 US Open      2024-08-26   Carballes Baena R. Choinski J.      6-2 6-3 5-7 6-…
    ##  7 US Open      2024-08-26   Shang J.           Bublik A.        6-4 3-6 5-7 6-…
    ##  8 US Open      2024-08-26   Shelton B.         Thiem D.         6-4 6-2 6-2    
    ##  9 US Open      2024-08-26   Berrettini M.      Ramos-Vinolas A. 7-6 6-2 6-3    
    ## 10 US Open      2024-08-26   Ruud C.            Bu Y.            7-6 6-2 6-2    
    ## # ℹ 117 more rows

``` r
#View(processed_us_open_data)
```

``` r
library(dplyr)
library(readr)
library(stringr)


url <- "http://www.tennis-data.co.uk/2024/wimbledon.csv"
wimbledon_data <- read_csv(url, show_col_types = FALSE)


processed_wimbledon_data <- wimbledon_data %>%
  mutate(
    score = paste0(
      ifelse(!is.na(W1), paste0(W1, "-", L1, " "), ""),
      ifelse(!is.na(W2), paste0(W2, "-", L2, " "), ""),
      ifelse(!is.na(W3), paste0(W3, "-", L3, " "), ""),
      ifelse(!is.na(W4), paste0(W4, "-", L4, " "), ""),
      ifelse(!is.na(W5), paste0(W5, "-", L5, " "), "")
    ) %>% str_trim(),
    tourney_date = as.Date(Date, "%d/%m/%Y") %>% format("%Y-%m-%d"),  # Format date to match your existing data
    tourney_name = str_replace(Tournament, "Wimbledon", "Wimbledon")  # Ensure tournament name is consistent
  ) %>%
  select(
    tourney_name,
    tourney_date,
    winner_name = Winner,
    loser_name = Loser,
    score
  )

print(processed_wimbledon_data)
```

    ## # A tibble: 127 × 5
    ##    tourney_name tourney_date winner_name   loser_name        score              
    ##    <chr>        <chr>        <chr>         <chr>             <chr>              
    ##  1 Wimbledon    2024-07-01   Fognini F.    Van Assche L.     6-1 6-3 7-5        
    ##  2 Wimbledon    2024-07-01   Shang J.      Garin C.          7-5 6-4 6-4        
    ##  3 Wimbledon    2024-07-01   Ruud C.       Bolt A.           7-6 6-4 6-4        
    ##  4 Wimbledon    2024-07-01   Coric B.      Meligeni Alves F. 6-3 7-6 6-3        
    ##  5 Wimbledon    2024-07-01   Struff J.L.   Marozsan F.       6-4 6-7 6-2 6-3    
    ##  6 Wimbledon    2024-07-01   Dimitrov G.   Lajovic D.        6-3 6-4 7-5        
    ##  7 Wimbledon    2024-07-01   Shapovalov D. Jarry N.          6-1 7-5 6-4        
    ##  8 Wimbledon    2024-07-01   Tiafoe F.     Arnaldi M.        6-7 2-6 6-1 6-3 6-3
    ##  9 Wimbledon    2024-07-01   Thompson J.   Kotov P.          5-7 5-7 6-4 6-4 6-4
    ## 10 Wimbledon    2024-07-01   Zhang Zh.     Janvier M.        7-6 6-3 6-2        
    ## # ℹ 117 more rows

``` r
#View(processed_wimbledon_data)
```

``` r
library(dplyr)


cleaned_df <- read.csv("data/cleaned_df.csv")


cleaned_df_webscraping_data_added <- cleaned_df %>%
  bind_rows(
    processed_data,  # Roland Garros data
    processed_us_open_data,  # US Open data
    processed_wimbledon_data  # Wimbledon data
  )


print(head(cleaned_df_webscraping_data_added))
```

    ##      tourney_name tourney_date winner_id winner_seed winner_entry
    ## 1 Australian Open   2014-01-13    104745           1         <NA>
    ## 2 Australian Open   2014-01-13    106423          NA           WC
    ## 3 Australian Open   2014-01-13    106058          NA         <NA>
    ## 4 Australian Open   2014-01-13    104792          25         <NA>
    ## 5 Australian Open   2014-01-13    104312          24         <NA>
    ## 6 Australian Open   2014-01-13    105385          NA         <NA>
    ##          winner_name winner_hand winner_ht winner_ioc winner_age loser_id
    ## 1       Rafael Nadal           L       185        ESP       27.6   106071
    ## 2 Thanasi Kokkinakis           R       196        AUS       17.7   104997
    ## 3          Jack Sock           R       185        USA       21.3   104735
    ## 4       Gael Monfils           R       193        FRA       27.3   105992
    ## 5      Andreas Seppi           R       190        ITA       29.8   103720
    ## 6       Donald Young           L       183        USA       24.4   104898
    ##   loser_seed loser_entry     loser_name loser_hand loser_ht loser_ioc loser_age
    ## 1         NA        <NA>  Bernard Tomic          R      193       AUS      21.2
    ## 2         NA        <NA>  Igor Sijsling          R      190       NED      26.4
    ## 3         NA        <NA>   Tobias Kamke          R      180       GER      27.6
    ## 4         NA        <NA>  Ryan Harrison          R      183       USA      21.6
    ## 5         NA        <NA> Lleyton Hewitt          R      180       AUS      32.8
    ## 6         NA        <NA>    Robin Haase          R      190       NED      26.7
    ##                       score round minutes w_ace w_df w_svpt w_1st_in w_1st_won
    ## 1                   6-4 RET  R128      41     5    1     26       16        14
    ## 2     7-6(4) 0-6 7-6(3) 6-2  R128     182     6    9    147       79        55
    ## 3        7-6(5) 5-7 6-2 6-4  R128     242    16   12    148       74        54
    ## 4               6-4 6-4 6-4  R128     111    10    5     78       51        44
    ## 5    7-6(4) 6-3 5-7 5-7 7-5  R128     258    17    5    176      103        72
    ## 6 6-7(4) 7-6(2) 6-2 1-0 RET  R128     154     0    5    106       77        58
    ##   w_2nd_won w_sv_gms w_bp_saved w_bp_faced l_ace l_df l_svpt l_1st_in l_1st_won
    ## 1         6        5          0          0     3    1     28       19        15
    ## 2        36       19          5          9    20    4    123       73        57
    ## 3        43       21         10         12     6    8    136       66        51
    ## 4        17       15          0          0     2    1     93       57        42
    ## 5        40       29          6         13    23    5    177       90        76
    ## 6        16       16          3          4    11    6    124       81        55
    ##   l_2nd_won l_sv_gms l_bp_saved l_bp_faced winner_rank winner_rank_points
    ## 1         3        5          0          1           1              13130
    ## 2        22       19          2          5         570                 53
    ## 3        36       21          6         10          95                582
    ## 4        17       15          1          4          32               1245
    ## 5        38       28         11         18          25               1360
    ## 6        19       15          5          7          91                595
    ##   loser_rank loser_rank_points
    ## 1         57               810
    ## 2         73               681
    ## 3         78               651
    ## 4        110               504
    ## 5         43              1010
    ## 6         45               977

``` r
write.csv(cleaned_df_webscraping_data_added, "data/cleaned_df_webscraping_data_added.csv", row.names = FALSE)
#View(cleaned_df_webscraping_data_added)
```
