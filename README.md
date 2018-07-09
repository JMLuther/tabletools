<!-- README.md is generated from README.Rmd. Please edit that file -->
    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

tabletools
==========

The goal of tabletools is to provide functions which are helpful for generating summary html tables. These functions are most useful in conjunction with an html table generating package::functions such as:

-   `htmlTable::htmlTable` [Tables with htmlTable and some alternatives](https://cran.r-project.org/web/packages/htmlTable/vignettes/tables.html)
-   `knitr::kable` see [Create Awesome HTML Table with knitr::kable and kableExtra](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html)
-   `formattable`

Example
-------

Using `dplyr` tools to make a summary dataframe- then converting results to an html table.

``` r
mtcars %>% 
  group_by(cyl) %>% 
  summarise(n = sum(!is.na(cyl)),
            pct_4gears = txt_pct_fr(gear, 4)) %>%  # shows what percentage have 4 gears (selected reference) and (with fraction)
  htmlTable::htmlTable(rnames=F, align = "c",
                       header = c("Cylinders", "n", "% with 4 gears"),
                       css.cell = rbind(rep("padding-left: .5em; padding-right: .5em;",times=ncol(.)),
                           matrix("padding:0 5px 0 5px;", ncol=ncol(.), nrow=nrow(.))))
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Cylinders
</th>
<th style="padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
n
</th>
<th style="padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
% with 4 gears
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding:0 5px 0 5px; text-align: center;">
4
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
11
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
72.7% ( <sup>8</sup>⁄<sub>11</sub> )
</td>
</tr>
<tr>
<td style="padding:0 5px 0 5px; text-align: center;">
6
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
7
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
57.1% ( <sup>4</sup>⁄<sub>7</sub> )
</td>
</tr>
<tr>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
8
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
14
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
0.0% ( <sup>0</sup>⁄<sub>14</sub> )
</td>
</tr>
</tbody>
</table>
