<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabletools

The goal of tabletools is to provide functions which are helpful for
generating summary html tables. For details on the functions, please see
help documentation in the package.

## Installation

Install from Github:
`devtools::install_github("JMLuther/tabletools", build_vignettes = TRUE)`

These functions are most useful in conjunction with an html table
generating package::functions such as:

-   `htmlTable::htmlTable` [Tables with htmlTable and some
    alternatives](https://cran.r-project.org/web/packages/htmlTable/vignettes/tables.html)  
-   `knitr::kable` see [Create Awesome HTML Table with knitr::kable and
    kableExtra](https://haozhu233.github.io/kableExtra/awesome_table_in_html.html)  
-   `formattable`

## `my_summary` custom summary

This summary function is customized to my preferences, and should
provide enough data to pipe into ggplot or a simple summary table. Just
provides a summary for a single continuous variable.

Table:

``` r
mtcars %>% 
  group_by(cyl) %>% 
  my_summary(mpg, digits = 1) %>% # override the default 2 decimal point rounding
  htmlTable::htmlTable(rnames=F, align = "c",
                       css.cell = rbind(rep("padding-left: .5em; padding-right: .5em;",times=ncol(.)),
                                        matrix("padding:0 5px 0 5px;", ncol=ncol(.), nrow=nrow(.))))
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
cyl
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
mpg_n
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
mpg_mean
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
mpg_sd
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
mpg_sem
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
mpg_median
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
mpg_ci_025
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
mpg_ci_975
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
26.7
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
4.5
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
1.4
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
26
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
21.4
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
33.5
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
19.7
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
1.5
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
0.5
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
19.7
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
17.8
</td>
<td style="padding:0 5px 0 5px; text-align: center;">
21.3
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
15.1
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
2.6
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
0.7
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
15.2
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
10.4
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: center;">
19
</td>
</tr>
</tbody>
</table>

Plot:

``` r
mtcars %>% 
  group_by(cyl) %>% 
  my_summary(mpg) %>% 
  ggplot(aes(factor(cyl), mpg_mean,
             ymin = mpg_ci_025, ymax = mpg_ci_975)) +
  geom_linerange() +
  geom_point(color = "red", size=2) +
  labs(x="No. of Cylinders",
       y= "Average MPG",
       title= "Average MPG by number of Cylinders") +
  theme_bw()
```

![](README-unnamed-chunk-3-1.png)

# Summary text functions

``` r
mtcars %>%
  summarise(mean_sd = txt_mean_sd(mpg),
            mean_sem = txt_mean_sem(mpg),
            mean_range = txt_mean_range(mpg),
            median_iqr = txt_median_iqr(mpg),
            pct_fr = txt_pct_fr(vs, 1)
            ) %>% 
  tidyr::gather(txt_f, output) %>% # this is better than using t() which creates a matrix
  htmlTable::htmlTable(rnames=F, align = "l", align.header = "l", 
                       caption = "Example results",
                       css.cell = rbind(rep("padding-left: .5em; padding-right: .5em;",times=ncol(.)),
                                        matrix("padding:0 5px 0 5px;", ncol=ncol(.), nrow=nrow(.))))
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
Example results
</td>
</tr>
<tr>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;">
txt_f
</th>
<th style="font-weight: 900; padding-left: .5em; padding-right: .5em; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;">
output
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="padding:0 5px 0 5px; text-align: left;">
mean_sd
</td>
<td style="padding:0 5px 0 5px; text-align: left;">
20.1±6.0
</td>
</tr>
<tr>
<td style="padding:0 5px 0 5px; text-align: left;">
mean_sem
</td>
<td style="padding:0 5px 0 5px; text-align: left;">
20.1±1.1
</td>
</tr>
<tr>
<td style="padding:0 5px 0 5px; text-align: left;">
mean_range
</td>
<td style="padding:0 5px 0 5px; text-align: left;">
20.1(10.4-33.9)
</td>
</tr>
<tr>
<td style="padding:0 5px 0 5px; text-align: left;">
median_iqr
</td>
<td style="padding:0 5px 0 5px; text-align: left;">
19.2\[15.4-22.8\]
</td>
</tr>
<tr>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: left;">
pct_fr
</td>
<td style="padding:0 5px 0 5px; border-bottom: 2px solid grey; text-align: left;">
43.8%(<sup>14</sup>⁄<sub>32</sub>)
</td>
</tr>
</tbody>
</table>

The following functions provide useful \# Session Info

<pre>
 R version 4.2.1 (2022-06-23 ucrt)
 Platform: x86_64-w64-mingw32/x64 (64-bit)
 Running under: Windows 10 x64 (build 19044)
 
 Matrix products: default
 
 attached base packages:
 [1] stats     graphics  grDevices utils     datasets  methods   base     
 
 other attached packages:
 [1] tidyr_1.2.1      tabletools_0.1.0 htmlTable_2.4.1  ggplot2_3.3.6   
 [5] dplyr_1.0.10    
 </pre>
 To cite R in publications use:
 <p>R Core Team (2022).
 <em>R: A Language and Environment for Statistical Computing</em>.
 R Foundation for Statistical Computing, Vienna, Austria.
 <a href="https://www.R-project.org/">https://www.R-project.org/</a>. 
 </p>
 
To cite the <tt>ggplot2</tt> package in publications use:

 <p>Wickham H (2016).
 <em>ggplot2: Elegant Graphics for Data Analysis</em>.
 Springer-Verlag New York.
 ISBN 978-3-319-24277-4, <a href="https://ggplot2.tidyverse.org">https://ggplot2.tidyverse.org</a>. 
 </p>
