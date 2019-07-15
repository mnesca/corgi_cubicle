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
g <- ggplot(transitdata, aes("Route Destination"))
g + 
  geom_bar() +
  xlab("This is where the bus was going")
```

![](RMD_Wpg_Transit_Analysis_files/figure-gfm/Descriptive%20Analysis-1.png)<!-- -->

It seems this produced an error that I am trying to fix, the error in
this case is that its outputting the total count of the variable under
Route Destination which is not useful at all. I will try to fix this.
