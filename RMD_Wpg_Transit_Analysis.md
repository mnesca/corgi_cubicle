Playing Around with Transit Data
================
Marcello Nesca
July 2 2019

## Motivation(s) & Objectives

The idea of this practice dataset is:  
1\) to upkeep my data science skills in R  
2\) playing with version control using Github  
3\) Store my thoughts on data analysis  
4\) i want to practice more on Tidyverse code and stop using BASE R
coding

## Loading Datasets

I have obtained this dataset from the City of Winnipeg open dataset
archive I will be concentrating on utilizing the Transit-passups for
this practice project. Since this dataset gets updated daily, I have
gotten this dataset in June 29th 2019.

Ref: <https://data.winnipeg.ca/Transit/Transit-Pass-ups/mer2-irmb>

``` r
transitdata <- read_csv(here::here("data", "Transit_Pass-ups.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   `Pass-Up ID` = col_double(),
    ##   `Pass-Up Type` = col_character(),
    ##   Time = col_character(),
    ##   `Route Number` = col_double(),
    ##   `Route Name` = col_character(),
    ##   `Route Destination` = col_character(),
    ##   Location = col_character()
    ## )

## Descriptive Analysis on Variables

Here I want to describe what my variables initially look like before
further analysis. We first need to see what variables are included
without continiously looking at the dataframe. The idea is to try to be
efficient in your work. Here are a list of the variables:

``` r
sumvar <- summary(transitdata)
print(sumvar)
```

    ##    Pass-Up ID      Pass-Up Type           Time            Route Number   
    ##  Min.   : 363450   Length:115520      Length:115520      Min.   :  1.00  
    ##  1st Qu.:1235098   Class :character   Class :character   1st Qu.: 18.00  
    ##  Median :1864948   Mode  :character   Mode  :character   Median : 36.00  
    ##  Mean   :1794693                                         Mean   : 59.61  
    ##  3rd Qu.:2479886                                         3rd Qu.: 75.00  
    ##  Max.   :2934796                                         Max.   :185.00  
    ##                                                          NA's   :49      
    ##   Route Name        Route Destination    Location        
    ##  Length:115520      Length:115520      Length:115520     
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ## 

``` r
g <- ggplot(transitdata)
g + 
  geom_bar(aes("Pass-Up Type")) +
  xlab("Type of bus that passed")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Descriptive%20Analysis-1.png)<!-- -->

It seems this produced an error that I am trying to fix, the error in
this case is that its outputting the total count of the variable under
Route Destination which is not useful at all. I will try to fix this.

The issue was that the variables were characters to which i have to
change to factors.

AAAAND… urgggg\! It seems like transforming the variables didnt work
either\!\!

``` r
transitdata <- transitdata %>% 
  mutate_at(vars("Route Destination", "Route Name", "Pass-Up Type"), as.factor)

## I know this next bit of code is inefficient so i have to trouble shoot it later.
transitdata <- transitdata %>% 
  dplyr::rename(RouteDestination = `Route Destination`) %>%
  dplyr::rename(RouteName = `Route Name`) %>%
  dplyr::rename(PassUpType = `Pass-Up Type`) %>%
  dplyr::rename(RouteNumber = `Route Number`) %>%
  dplyr::rename(PassUpID = `Pass-Up ID`)
tibble(transitdata)
```

    ## # A tibble: 115,520 x 1
    ##    transitdata$Pas~ $PassUpType $Time $RouteNumber $RouteName
    ##               <dbl> <fct>       <chr>        <dbl> <fct>     
    ##  1          2934796 Wheelchair~ 06/2~           16 Selkirk-O~
    ##  2          2934787 Full Bus P~ 06/2~           15 Sargent-M~
    ##  3          2934745 Full Bus P~ 06/2~           14 St. Mary'~
    ##  4          2934683 Full Bus P~ 06/2~           21 Portage E~
    ##  5          2934614 Full Bus P~ 06/2~          162 Ft. Richm~
    ##  6          2934585 Full Bus P~ 06/2~           21 Portage E~
    ##  7          2934512 Full Bus P~ 06/2~           21 Portage E~
    ##  8          2934474 Wheelchair~ 06/2~           21 Portage E~
    ##  9          2934449 Full Bus P~ 06/2~           55 St.Anne's 
    ## 10          2934412 Full Bus P~ 06/2~           11 Portage-K~
    ## # ... with 115,510 more rows, and 2 more variables:
    ## #   $RouteDestination <fct>, $Location <chr>

UPDATE: 07-21-2019: So I finally figured out why my GGPLOT geom bar is
continiously breaking – it took me a week or so to figure this out. It
is because GGPLOT *CANNOT* have spaces in the variables. Furthermore,
spaces between variables is not good coding practice anyways, so… City
of Winnipeg, do *NOT* put spaces in your variables.

``` r
g <- ggplot(transitdata)
g + 
  geom_bar(aes(PassUpType, fill = PassUpType)) +
  xlab("Type of bus that passed")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/GGPLOT%20Test-1.png)<!-- -->

``` r
g + 
  geom_bar(aes(RouteDestination)) +
  coord_flip() +
  xlab("Where the bus went")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Now%20lets%20see%20other%20variables!-1.png)<!-- -->

``` r
g + 
  geom_bar(aes(RouteName)) +
  coord_flip() +
  xlab("Name of the Route")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Now%20lets%20see%20other%20variables!-2.png)<!-- -->

There are way too many values for these variables… Now a new problem to
solve\!

UPDATE 07-31-2019: definitely by practice, cleaning and visualizing data
is a circular process.

``` r
transitdata <- transitdata %>%
  separate(Location, into = c("Extra", "Lat1", "Long1"), sep = " ") %>%
  separate(Lat1, into = c("Extra1", "Latitude"), sep = 1) %>%
  separate(Long1, into = c("Longitude", "Extra2"), sep = -1) %>%
  select(-Extra, -Extra1, -Extra2)
tibble(transitdata)
```

    ## # A tibble: 115,520 x 1
    ##    transitdata$Pas~ $PassUpType $Time $RouteNumber $RouteName
    ##               <dbl> <fct>       <chr>        <dbl> <fct>     
    ##  1          2934796 Wheelchair~ 06/2~           16 Selkirk-O~
    ##  2          2934787 Full Bus P~ 06/2~           15 Sargent-M~
    ##  3          2934745 Full Bus P~ 06/2~           14 St. Mary'~
    ##  4          2934683 Full Bus P~ 06/2~           21 Portage E~
    ##  5          2934614 Full Bus P~ 06/2~          162 Ft. Richm~
    ##  6          2934585 Full Bus P~ 06/2~           21 Portage E~
    ##  7          2934512 Full Bus P~ 06/2~           21 Portage E~
    ##  8          2934474 Wheelchair~ 06/2~           21 Portage E~
    ##  9          2934449 Full Bus P~ 06/2~           55 St.Anne's 
    ## 10          2934412 Full Bus P~ 06/2~           11 Portage-K~
    ## # ... with 115,510 more rows, and 3 more variables:
    ## #   $RouteDestination <fct>, $Latitude <chr>, $Longitude <chr>
