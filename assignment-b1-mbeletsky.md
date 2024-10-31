assignment-b1-mbeletsky
================
2024-10-30

# Setup

First, we’ll load all the packages that we’ll be using.

``` r
library(datateachr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

I’ve included “datateachr” so that we have access to the
“vancouver_trees” dataset I’ll be using as an example. We’ll be using
“tidyverse” to take advantage of the “dplyr” package. I’ve loaded
“testthat” for the tests we’re going to run later on the package created
in this assignment.

# Exercises 1 and 2: Make and Document a Function

I will make a function that bundles a specific **group_by()** %\>%
**summarize()** workflow.

I want to turn the following workflow into a function: counting how many
unique observations there are for a variable.

``` r
exercise1_example <- vancouver_trees %>%
  group_by(neighbourhood_name) %>%
  summarize(n_species = n_distinct(species_name))
print(exercise1_example)
```

    ## # A tibble: 22 × 2
    ##    neighbourhood_name       n_species
    ##    <chr>                        <int>
    ##  1 ARBUTUS-RIDGE                  121
    ##  2 DOWNTOWN                        79
    ##  3 DUNBAR-SOUTHLANDS              161
    ##  4 FAIRVIEW                       119
    ##  5 GRANDVIEW-WOODLAND             146
    ##  6 HASTINGS-SUNRISE               176
    ##  7 KENSINGTON-CEDAR COTTAGE       159
    ##  8 KERRISDALE                     138
    ##  9 KILLARNEY                      122
    ## 10 KITSILANO                      171
    ## # ℹ 12 more rows

Here, we have grouped the “vancouver_trees” dataset by neighbourhood and
then summarized how many different species of tree there are in each
neighbourhood. This workflow could also be useful for: counting how many
genera there are per neighbourhood, or counting how many species there
are in each genus.

Now, I will make a function that can accomplish this workflow.

``` r
#' @title Group by, then summarize unique observations
#' @description Group your dataset by one variable, then summarize counts of unique observations for another variable. Rows will be sorted from highest counts to lowest. 
#' @param {Object} data - A dataframe you are working with. I named this argument "data" to keep things simple and to the point, and for existing functions, it's common for the argument "data" to be a dataframe. 
#' @param {Object} group_var - The variable (column) you wish to group your data by. I named this argument "group_var" because it's the variable (var) you are wanting to group by (group) and to distinguish it easily from the other variable we are using as an argument. 
#' @param {Object} count_var - The variable (column) you wish to count unique observations for. I named this argument "count_var" because it's the variable (var) you are wanting to count unique observations in (count) and to distinguish it easily from the other variable we are using as an argument. 
#' @return Tibble that shows counts of unique observations within one variable, for each group you specify. 
group_then_sumz <- function(data, group_var, count_var) {
  table <- data %>%
    drop_na({{ group_var }}) %>%
    group_by({{ group_var }}) %>%
    drop_na({{ count_var }}) %>%
    summarize(n_distinct = n_distinct({{ count_var }})) %>%
    arrange(desc(n_distinct))
print(table)
}
```

Here I’ve made a function called **group_then_sumz()** that takes in the
arguments “data” (a dataframe), “group_var” (name of the column you want
to group by), and “count_var” (name of the column you want to count
unique observations for). It outputs a tibble that shows you unique
counts for your variable of interest grouped by your other variable of
interest, and sorts the counts from high to low.

# Exercise 3: Examples

## Group by neighbourhood, count unique species

It’s time to show this function in action. Let’s begin by grouping by
neighbourhood and counting unique species.

``` r
group_then_sumz(vancouver_trees, neighbourhood_name, species_name)
```

    ## # A tibble: 22 × 2
    ##    neighbourhood_name       n_distinct
    ##    <chr>                         <int>
    ##  1 HASTINGS-SUNRISE                176
    ##  2 KITSILANO                       171
    ##  3 RENFREW-COLLINGWOOD             170
    ##  4 DUNBAR-SOUTHLANDS               161
    ##  5 KENSINGTON-CEDAR COTTAGE        159
    ##  6 RILEY PARK                      154
    ##  7 SHAUGHNESSY                     150
    ##  8 SUNSET                          147
    ##  9 GRANDVIEW-WOODLAND              146
    ## 10 MARPOLE                         139
    ## # ℹ 12 more rows

Here we have a nice table showing how many distinct tree species there
are in each Vancouver neighbourhood. We find out that Hastings-Sunrise,
Kitsilano, and Renfrew-Collingwood have the most unique tree species.

## Group by genus, count unique species

For the next example, let’s group by genus and count how many distinct
tree species there are in Vancouver that belong to each genus.

``` r
group_then_sumz(vancouver_trees, genus_name, species_name)
```

    ## # A tibble: 97 × 2
    ##    genus_name n_distinct
    ##    <chr>           <int>
    ##  1 ACER               31
    ##  2 PRUNUS             25
    ##  3 MAGNOLIA           20
    ##  4 QUERCUS            18
    ##  5 MALUS              14
    ##  6 PINUS              14
    ##  7 FRAXINUS           11
    ##  8 CORNUS             10
    ##  9 CRATAEGUS          10
    ## 10 PICEA              10
    ## # ℹ 87 more rows

Now we have a nice table showing how many distinct tree species there
are in Vancouver in each genus. The genera with the most unique species
found in Vancouver are *Acer*, *Prunus*, and *Magnolia*.

## Group by genera, count unique cultivars

For this example, we will group by genus again and count how many
cultivars there are in Vancouver in each genus.

``` r
group_then_sumz(vancouver_trees, genus_name, cultivar_name)
```

    ## # A tibble: 48 × 2
    ##    genus_name n_distinct
    ##    <chr>           <int>
    ##  1 ACER               64
    ##  2 PRUNUS             31
    ##  3 MAGNOLIA           29
    ##  4 FRAXINUS           22
    ##  5 MALUS              20
    ##  6 FAGUS              16
    ##  7 CORNUS             12
    ##  8 QUERCUS            11
    ##  9 GLEDITSIA           8
    ## 10 SORBUS              7
    ## # ℹ 38 more rows

Now that we’ve grouped by genus and summarized unique cultivars, we see
that *Acer*, *Prunus*, and *Magnolia* have the most unique cultivars
within them.

# Exercise 4: Test the Function

Now, we will make sure the function is working as it’s supposed to by
running tests.

## Test 1: expect_is()

Within our function, we print out a tibble with the results which should
also be a function because the output depends on the input data and
variables. We can test if the function **group_then_sumz()** and the
output object “table” are indeed functions by using **expect_is()** and
using the arguments object and “function”.

``` r
test_that("group_then_sumz and table are functions", {
expect_is(group_then_sumz, "function")
expect_is(table, "function")
})
```

    ## Test passed 😀

The test passes, so we have confirmed that **group_then_sumz()** and
“table” are both functions.

## Test 2: expect_no_error()

We can use the test function **expect_no_error()** to check that with
several non-redundant vector types, we don’t run into any error
messages.

``` r
test_that("different input variables produce no errors", {

# Vectors with no NA's
expect_no_error(group_then_sumz(vancouver_trees, neighbourhood_name, species_name), message = NULL, class = NULL)

# Vector with NA's
expect_no_error(group_then_sumz(vancouver_trees, genus_name, cultivar_name), message = NULL, class = NULL)

# Vector of different type (date)
expect_no_error(group_then_sumz(vancouver_trees, neighbourhood_name, date_planted), message = NULL, class = NULL)

# Vector of different type (numeric)
expect_no_error(group_then_sumz(vancouver_trees, neighbourhood_name, on_street_block), message = NULL, class = NULL)

})
```

    ## # A tibble: 22 × 2
    ##    neighbourhood_name       n_distinct
    ##    <chr>                         <int>
    ##  1 HASTINGS-SUNRISE                176
    ##  2 KITSILANO                       171
    ##  3 RENFREW-COLLINGWOOD             170
    ##  4 DUNBAR-SOUTHLANDS               161
    ##  5 KENSINGTON-CEDAR COTTAGE        159
    ##  6 RILEY PARK                      154
    ##  7 SHAUGHNESSY                     150
    ##  8 SUNSET                          147
    ##  9 GRANDVIEW-WOODLAND              146
    ## 10 MARPOLE                         139
    ## # ℹ 12 more rows
    ## # A tibble: 48 × 2
    ##    genus_name n_distinct
    ##    <chr>           <int>
    ##  1 ACER               64
    ##  2 PRUNUS             31
    ##  3 MAGNOLIA           29
    ##  4 FRAXINUS           22
    ##  5 MALUS              20
    ##  6 FAGUS              16
    ##  7 CORNUS             12
    ##  8 QUERCUS            11
    ##  9 GLEDITSIA           8
    ## 10 SORBUS              7
    ## # ℹ 38 more rows
    ## # A tibble: 22 × 2
    ##    neighbourhood_name       n_distinct
    ##    <chr>                         <int>
    ##  1 RENFREW-COLLINGWOOD            1284
    ##  2 KENSINGTON-CEDAR COTTAGE       1236
    ##  3 HASTINGS-SUNRISE               1122
    ##  4 SUNSET                         1031
    ##  5 DUNBAR-SOUTHLANDS               963
    ##  6 VICTORIA-FRASERVIEW             918
    ##  7 RILEY PARK                      838
    ##  8 MARPOLE                         772
    ##  9 KERRISDALE                      755
    ## 10 GRANDVIEW-WOODLAND              754
    ## # ℹ 12 more rows
    ## # A tibble: 22 × 2
    ##    neighbourhood_name       n_distinct
    ##    <chr>                         <int>
    ##  1 SHAUGHNESSY                      81
    ##  2 KERRISDALE                       78
    ##  3 SUNSET                           77
    ##  4 KENSINGTON-CEDAR COTTAGE         74
    ##  5 RENFREW-COLLINGWOOD              72
    ##  6 DUNBAR-SOUTHLANDS                70
    ##  7 HASTINGS-SUNRISE                 70
    ##  8 VICTORIA-FRASERVIEW              65
    ##  9 RILEY PARK                       61
    ## 10 ARBUTUS-RIDGE                    60
    ## # ℹ 12 more rows
    ## Test passed 😀

I’ve tested the function with four different inputs: vectors with no
NA’s (neighbourhood_name and species_name), vector with NA’s
(cultivar_name), a date vector (date_planted), and a numeric vector
(on_street_block). The test passed, meaning none of these input
variables that we might be interested in generate an error when the
function is run.

## Test 3: expect_output()

We can use the test function **expect_output()** to test that our
function is outputting the “table” tibble, as we want it to.

``` r
test_that("function produces output table", {
expect_output(
  group_then_sumz(vancouver_trees, neighbourhood_name, species_name),
  regexp = NULL
)
})
```

    ## Test passed 🎉

The test is passed, indicating that output is produced.
