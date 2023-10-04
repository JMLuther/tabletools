## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
library(dplyr)
library(ggplot2)
library(htmlTable)
library(tabletools)
library(tidyr)

## -----------------------------------------------------------------------------
mtcars %>% 
  group_by(cyl) %>% 
  my_summary(mpg, digits = 1) %>% # override the default 2 decimal point rounding
  htmlTable::htmlTable(rnames=F, align = "c",
                       css.cell = rbind(rep("padding-left: .5em; padding-right: .5em;",times=ncol(.)),
                                        matrix("padding:0 5px 0 5px;", ncol=ncol(.), nrow=nrow(.))))


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
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
  


## ----echo=F-------------------------------------------------------------------
Hmisc::markupSpecs$html$session()

