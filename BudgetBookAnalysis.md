BudgetBookAnalysis
================
Raka Adinugraha
November 21, 2019

## About the data

This data is a record of my personal spending from January 2019 to
August 2019. During this period, I record every spending that occurs in
every account that I have which is, cash money, bank account, and
various saving box. The data itself was exported from ‘My Budget Book’
android app in my phone. \#\# Purpose The purpose of this analysis is to
see the spending trend over this period and make it as a guidefor future
spending decisions to improve finances conditions. Other than that, it
also uses as an indicator of how good or not I at doing the records of
the spending.

## Processing the data

The datasets turned into a tibble. Preset Date and Amount as date and
number type respectively. The rest of the column will become character
type by default.

``` r
budget <- read_csv2("January-august_2019.csv", col_types = cols(
  Date = col_date(format = "%m/%d/%Y"),
  Amount = col_number()
))
```

    ## Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.

``` r
head(budget)
```

    ## # A tibble: 6 x 7
    ##   Date       Title     Comment `Main category` Subcategory Account   Amount
    ##   <date>     <chr>     <chr>   <chr>           <chr>       <chr>      <dbl>
    ## 1 2019-01-01 This sof~ <NA>    Office & Work   Software    Cash Mo~  -55000
    ## 2 2019-01-02 Extra fr~ <NA>    Income          <NA>        Saving ~    2000
    ## 3 2019-01-02 Yogurt    <NA>    Home            Food        Cash Mo~   -8100
    ## 4 2019-01-02 Dinner    <NA>    Home            Food        Cash Mo~  -18000
    ## 5 2019-01-02 Weekly c~ <NA>    Expenses        <NA>        Bank Ma~ -300000
    ## 6 2019-01-02 Weekly c~ <NA>    Expenses        <NA>        Cash Mo~  300000

Upon looking to the dataset, we can see there is some element of it we
can tweak. First we can see that ‘Main category’ column name have a
space in it, that in my experience could be problematic when try to use
it for plotting, so we need to change it. We also add new ‘Month\_Year’
column that will help our further analysis.

``` r
budget <- rename(budget, Main_category = 'Main category')
budget$Month_Year <- format(as_date(budget$Date), '%Y-%m')
head(budget)
```

    ## # A tibble: 6 x 8
    ##   Date       Title Comment Main_category Subcategory Account  Amount
    ##   <date>     <chr> <chr>   <chr>         <chr>       <chr>     <dbl>
    ## 1 2019-01-01 This~ <NA>    Office & Work Software    Cash M~  -55000
    ## 2 2019-01-02 Extr~ <NA>    Income        <NA>        Saving~    2000
    ## 3 2019-01-02 Yogu~ <NA>    Home          Food        Cash M~   -8100
    ## 4 2019-01-02 Dinn~ <NA>    Home          Food        Cash M~  -18000
    ## 5 2019-01-02 Week~ <NA>    Expenses      <NA>        Bank M~ -300000
    ## 6 2019-01-02 Week~ <NA>    Expenses      <NA>        Cash M~  300000
    ## # ... with 1 more variable: Month_Year <chr>

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](BudgetBookAnalysis_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
