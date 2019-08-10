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

## Data Cleaning and Checking Variables

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
ggplot(transitdata) +
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
## I know this next bit of code is inefficient so i have to trouble shoot it later.
transitdata <- transitdata %>% 
  dplyr::rename(RouteDestination = `Route Destination`) %>%
  dplyr::rename(RouteName = `Route Name`) %>%
  dplyr::rename(PassUpType = `Pass-Up Type`) %>%
  dplyr::rename(RouteNumber = `Route Number`) %>%
  dplyr::rename(PassUpID = `Pass-Up ID`) %>%
  mutate_at(vars("RouteDestination", "RouteName", "PassUpType", "RouteNumber"), as.factor)
```

UPDATE: 07-21-2019: So I finally figured out why my GGPLOT geom bar is
continiously breaking – it took me a week or so to figure this out. It
is because GGPLOT *CANNOT* have spaces in the variables. Furthermore,
spaces between variables is not good coding practice anyways, so… City
of Winnipeg, do *NOT* put spaces in your variables.

``` r
ggplot(transitdata) +
  geom_bar(aes(PassUpType, fill = PassUpType)) +
  xlab("Type of bus that passed")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/GGPLOT%20Test-1.png)<!-- -->

UPDATE 07-31-2019: definitely by practice, cleaning and visualizing data
is a circular process.

Below is where i changed longitude and latitude

``` r
transitdata <- transitdata %>%
  separate(Location, into = c("Extra", "Lat1", "Long1"), sep = " ") %>%
  separate(Lat1, into = c("Extra1", "Latitude"), sep = 1) %>%
  separate(Long1, into = c("Longitude", "Extra2"), sep = -1) %>%
  select(-Extra, -Extra1, -Extra2)
```

Here is where we clean the time variable - i want to lubridate the time
variable first, then split up time and date

``` r
transitdata <- transitdata %>%
  mutate(Time = mdy_hms(Time)) %>%
  separate(Time, into = c("Date", "Clock"), sep = " ")
```

``` r
transitdata <- transitdata %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date),
         ) %>%
  mutate_at(vars("year", "month"), as.factor)
```

It seems that I have cleaned everything I wish to clean, I have
separated all the variables in their component parts including the
dates\! So now lets explore some data finally\!

``` r
ggplot(transitdata) +
  geom_bar(aes(year, fill = year)) +
  xlab("Year")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/GGPLOT%20Data%20Exploration%20-%20year-1.png)<!-- -->

Wow\! Interesting exploratory information\! The rate of pass-ups are
increasing per year since 2015\! however in 2015 it did go down from
2014. 2019 is a half year since i downloaded the data end of June 2019.

``` r
ggplot(transitdata) +
  geom_bar(aes(month, fill = month)) +
  xlab("Month")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/GGPLOT%20Data%20Exploration%20-%20month-1.png)<!-- -->

and this is clearly very interesting\! most of the passups occur when –
you guessed it\! school is starting\!

``` r
ggplot(transitdata) +
  geom_bar(aes(RouteDestination)) +
  coord_flip() +
  xlab("Where the bus went")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Now%20lets%20see%20other%20variables!-1.png)<!-- -->

``` r
ggplot(transitdata) +
  geom_bar(aes(RouteName)) +
  coord_flip() +
  xlab("Name of the Route")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Now%20lets%20see%20other%20variables!-2.png)<!-- -->

I cannot seem to find a way to properly sort categorical variables but
ill definitely find out by next time\! There are way too many values for
these variables… Now a new problem to solve\!
