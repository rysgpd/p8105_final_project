data_cleaning
================
Rebecca Shyu
2024-11-20

``` r
slams = c("Australian Open", "Roland Garros", "Wimbledon", "Us Open", "US Open")

df_2014 = read_csv("data/atp_matches_2014.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

df_2015 = read_csv("data/atp_matches_2015.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

df_2016 = read_csv("data/atp_matches_2016.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

df_2017 = read_csv("data/atp_matches_2017.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

df_2018 = read_csv("data/atp_matches_2018.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

df_2019 = read_csv("data/atp_matches_2019.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

df_2020 = read_csv("data/atp_matches_2020.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  ) #wimbledon was cancelled this year due to COVID

df_2021 = read_csv("data/atp_matches_2021.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  ) 

df_2022 = read_csv("data/atp_matches_2022.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

df_2023 = read_csv("data/atp_matches_2023.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  )

seed_types = c("WC", "Q", "LL")

df_2024 = read_csv("data/atp_matches_2024.csv") %>% 
  janitor::clean_names() %>% 
  filter(
    tourney_name %in% slams
  ) %>% 
  mutate(
    winner_entry = ifelse(winner_seed %in% seed_types, winner_seed, winner_entry),
    winner_seed = ifelse(winner_seed %in% seed_types, NA, winner_seed),
    winner_seed = as.double(winner_seed),
    loser_entry = ifelse(loser_seed %in% seed_types, loser_seed, loser_entry),
    loser_seed = ifelse(loser_seed %in% seed_types, NA, loser_seed),
    loser_seed = as.double(loser_seed)
  )# only australian open since it goes up until april


total_df = bind_rows(df_2014, df_2015, df_2016, df_2017, df_2018, df_2019, df_2020, df_2021, df_2022, df_2023, df_2024) %>% 
  mutate(
    tourney_name = ifelse(tourney_name == "Us Open", "US Open", tourney_name),
    tourney_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
    tourney_name = as.factor(tourney_name),
    winner_hand = as.factor(winner_hand),
    winner_ioc = as.factor(winner_ioc),
    loser_hand = as.factor(loser_hand),
    loser_ioc = as.factor(loser_ioc),
    round = as.factor(round)
  ) %>% 
  select(-c(tourney_id, surface, draw_size, tourney_level, match_num, best_of))

summary(total_df)
```

    ##           tourney_name   tourney_date          winner_id       winner_seed   
    ##  Australian Open:1397   Min.   :2014-01-13   Min.   :100644   Min.   : 1.00  
    ##  Roland Garros  :1270   1st Qu.:2016-06-18   1st Qu.:104755   1st Qu.: 5.00  
    ##  US Open        :1270   Median :2018-11-05   Median :105550   Median :11.00  
    ##  Wimbledon      :1143   Mean   :2018-12-30   Mean   :116993   Mean   :12.81  
    ##                         3rd Qu.:2021-07-13   3rd Qu.:111446   3rd Qu.:20.00  
    ##                         Max.   :2024-01-15   Max.   :210506   Max.   :33.00  
    ##                                                               NA's   :2227   
    ##  winner_entry       winner_name        winner_hand   winner_ht    
    ##  Length:5080        Length:5080        L: 648      Min.   :170.0  
    ##  Class :character   Class :character   R:4427      1st Qu.:183.0  
    ##  Mode  :character   Mode  :character   U:   5      Median :188.0  
    ##                                                    Mean   :187.8  
    ##                                                    3rd Qu.:193.0  
    ##                                                    Max.   :211.0  
    ##                                                    NA's   :18     
    ##    winner_ioc     winner_age       loser_id        loser_seed   
    ##  ESP    : 572   Min.   :17.70   Min.   :100644   Min.   : 1.00  
    ##  FRA    : 484   1st Qu.:24.50   1st Qu.:104890   1st Qu.: 9.00  
    ##  USA    : 462   Median :27.60   Median :105668   Median :17.00  
    ##  SRB    : 306   Mean   :27.72   Mean   :116965   Mean   :16.98  
    ##  ITA    : 280   3rd Qu.:30.80   3rd Qu.:111456   3rd Qu.:25.00  
    ##  RUS    : 265   Max.   :40.80   Max.   :210530   Max.   :33.00  
    ##  (Other):2711                                    NA's   :3854   
    ##  loser_entry         loser_name        loser_hand    loser_ht    
    ##  Length:5080        Length:5080        L: 734     Min.   :170.0  
    ##  Class :character   Class :character   R:4329     1st Qu.:183.0  
    ##  Mode  :character   Mode  :character   U:  17     Median :185.0  
    ##                                                   Mean   :186.6  
    ##                                                   3rd Qu.:190.0  
    ##                                                   Max.   :211.0  
    ##                                                   NA's   :31     
    ##    loser_ioc      loser_age        score            round         minutes     
    ##  FRA    : 549   Min.   :16.70   Length:5080        F   :  40   Min.   :  0.0  
    ##  USA    : 535   1st Qu.:24.30   Class :character   QF  : 160   1st Qu.:118.0  
    ##  ESP    : 429   Median :27.50   Mode  :character   R128:2560   Median :147.0  
    ##  AUS    : 331   Mean   :27.64                      R16 : 320   Mean   :154.1  
    ##  ARG    : 277   3rd Qu.:30.80                      R32 : 640   3rd Qu.:186.0  
    ##  GER    : 276   Max.   :42.50                      R64 :1280   Max.   :396.0  
    ##  (Other):2683                                      SF  :  80   NA's   :386    
    ##      w_ace            w_df            w_svpt         w_1st_in     
    ##  Min.   : 0.00   Min.   : 0.000   Min.   :  8.0   Min.   :  3.00  
    ##  1st Qu.: 5.00   1st Qu.: 2.000   1st Qu.: 84.5   1st Qu.: 52.00  
    ##  Median : 9.00   Median : 3.000   Median :105.0   Median : 66.00  
    ##  Mean   :10.57   Mean   : 3.745   Mean   :109.9   Mean   : 68.94  
    ##  3rd Qu.:14.00   3rd Qu.: 5.000   3rd Qu.:132.0   3rd Qu.: 82.50  
    ##  Max.   :75.00   Max.   :26.000   Max.   :278.0   Max.   :198.00  
    ##  NA's   :21      NA's   :21       NA's   :21      NA's   :21      
    ##    w_1st_won        w_2nd_won        w_sv_gms       w_bp_saved   
    ##  Min.   :  3.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
    ##  1st Qu.: 41.00   1st Qu.:17.00   1st Qu.:14.00   1st Qu.: 2.00  
    ##  Median : 51.00   Median :22.00   Median :17.00   Median : 4.00  
    ##  Mean   : 52.51   Mean   :22.66   Mean   :17.94   Mean   : 4.71  
    ##  3rd Qu.: 62.00   3rd Qu.:27.00   3rd Qu.:21.00   3rd Qu.: 7.00  
    ##  Max.   :166.00   Max.   :58.00   Max.   :49.00   Max.   :24.00  
    ##  NA's   :21       NA's   :21      NA's   :20      NA's   :21     
    ##    w_bp_faced         l_ace             l_df            l_svpt     
    ##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 10.0  
    ##  1st Qu.: 3.000   1st Qu.: 3.000   1st Qu.: 2.000   1st Qu.: 91.0  
    ##  Median : 6.000   Median : 6.000   Median : 4.000   Median :111.0  
    ##  Mean   : 6.876   Mean   : 7.792   Mean   : 4.583   Mean   :115.1  
    ##  3rd Qu.:10.000   3rd Qu.:11.000   3rd Qu.: 6.000   3rd Qu.:137.0  
    ##  Max.   :30.000   Max.   :67.000   Max.   :26.000   Max.   :291.0  
    ##  NA's   :21       NA's   :21       NA's   :21       NA's   :21     
    ##     l_1st_in        l_1st_won        l_2nd_won        l_sv_gms    
    ##  Min.   :  3.00   Min.   :  2.00   Min.   : 0.00   Min.   : 0.00  
    ##  1st Qu.: 54.00   1st Qu.: 35.00   1st Qu.:15.00   1st Qu.:14.00  
    ##  Median : 68.00   Median : 45.00   Median :20.00   Median :17.00  
    ##  Mean   : 70.55   Mean   : 47.47   Mean   :20.75   Mean   :17.54  
    ##  3rd Qu.: 84.00   3rd Qu.: 58.00   3rd Qu.:26.00   3rd Qu.:21.00  
    ##  Max.   :218.00   Max.   :171.00   Max.   :55.00   Max.   :50.00  
    ##  NA's   :21       NA's   :21       NA's   :21      NA's   :20     
    ##    l_bp_saved      l_bp_faced    winner_rank     winner_rank_points
    ##  Min.   : 0.00   Min.   : 0.0   Min.   :  1.00   Min.   :   20     
    ##  1st Qu.: 4.00   1st Qu.: 9.0   1st Qu.: 10.00   1st Qu.:  814     
    ##  Median : 6.00   Median :11.0   Median : 29.00   Median : 1410     
    ##  Mean   : 6.74   Mean   :11.9   Mean   : 47.38   Mean   : 2556     
    ##  3rd Qu.: 9.00   3rd Qu.:15.0   3rd Qu.: 67.00   3rd Qu.: 3070     
    ##  Max.   :27.00   Max.   :35.0   Max.   :861.00   Max.   :16950     
    ##  NA's   :21      NA's   :21     NA's   :2        NA's   :2         
    ##    loser_rank      loser_rank_points
    ##  Min.   :   1.00   Min.   :    3    
    ##  1st Qu.:  36.00   1st Qu.:  553    
    ##  Median :  69.00   Median :  786    
    ##  Mean   :  87.52   Mean   : 1180    
    ##  3rd Qu.: 107.00   3rd Qu.: 1216    
    ##  Max.   :1415.00   Max.   :16950    
    ##  NA's   :13        NA's   :13

``` r
write_csv(total_df, "data/cleaned_df.csv")
```

``` r
# split into losers and winners dataset
df2 = read.csv("data/cleaned_df.csv")

winner_df = df2 %>% 
  select(1:10, 19:30, 40, 41) %>% 
  mutate(
    won = 1
  ) %>% 
  rename_all(~stringr::str_replace(.,"^winner_","")) %>% 
  rename_all(~stringr::str_replace(.,"^w_",""))


loser_df = df2 %>% 
  select(1, 2, 11:21, 31:39, 42, 43) %>% 
  mutate(
    won = 0
  ) %>% 
  rename_all(~stringr::str_replace(.,"^loser_","")) %>% 
  rename_all(~stringr::str_replace(.,"^l_",""))

player_df = rbind(winner_df, loser_df)

write_csv(player_df, "data/winners_losers_df.csv")
```
