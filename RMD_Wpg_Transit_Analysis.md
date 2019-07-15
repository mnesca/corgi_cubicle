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
transitdata <- read_csv(here("data", "Transit_Pass-ups.csv"))
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
g <- ggplot(transitdata, aes("Pass-Up Type"))
g + 
  geom_bar() +
  xlab("Type of bus that passed")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Descriptive%20Analysis-1.png)<!-- -->

It seems this produced an error that I am trying to fix, the error in
this case is that its outputting the total count of the variable under
Route Destination which is not useful at all. I will try to fix this.

The issue was that the variables were characters to which i have to
change to factors.

``` r
transitdata <- transitdata %>% 
  mutate_at(vars("Route Destination", "Route Name", "Pass-Up Type"), as.factor)
tibble(transitdata)
```

    ## # A tibble: 115,520 x 1
    ##    transitdata$`Pa~ $`Pass-Up Type` $Time $`Route Number` $`Route Name`
    ##               <dbl> <fct>           <chr>           <dbl> <fct>        
    ##  1          2934796 Wheelchair Use~ 06/2~              16 Selkirk-Osbo~
    ##  2          2934787 Full Bus Pass-~ 06/2~              15 Sargent-Moun~
    ##  3          2934745 Full Bus Pass-~ 06/2~              14 St. Mary's-E~
    ##  4          2934683 Full Bus Pass-~ 06/2~              21 Portage Expr~
    ##  5          2934614 Full Bus Pass-~ 06/2~             162 Ft. Richmond~
    ##  6          2934585 Full Bus Pass-~ 06/2~              21 Portage Expr~
    ##  7          2934512 Full Bus Pass-~ 06/2~              21 Portage Expr~
    ##  8          2934474 Wheelchair Use~ 06/2~              21 Portage Expr~
    ##  9          2934449 Full Bus Pass-~ 06/2~              55 St.Anne's    
    ## 10          2934412 Full Bus Pass-~ 06/2~              11 Portage-Kild~
    ## # ... with 115,510 more rows, and 2 more variables: $`Route
    ## #   Destination` <fct>, $Location <chr>

``` r
g <- ggplot(transitdata, aes("Pass-Up Type"))
g + 
  geom_bar() +
  xlab("Type of bus that passed")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Mutating%20character%20variables%20to%20Factors%20then%20checking%20in%20GGPLOT2-1.png)<!-- -->

AAAAND… it still doesnt work\!\! urgggg\!
