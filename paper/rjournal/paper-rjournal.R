## ---- include = FALSE---------------------------------------------------------
paper <- here::here("paper/index.Rmd")
meta <- rmarkdown::yaml_front_matter(paper)


## ----child=paper--------------------------------------------------------------

## ----setup, cache = FALSE, include = FALSE------------------------------------
library(tidyverse)
tocache <- TRUE
knitr::opts_chunk$set(echo = FALSE, 
                      cache = TRUE,
                      cache.path = "cache/",
                      fig.align = 'center', 
                      fig.pos = 'htbp', 
                      fig.width = 6,
                      message = FALSE,
                      warning = FALSE)

theme_set(
  theme(panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "lightgray"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.7),
        axis.ticks.length = unit(1.4, "mm"),
        axis.ticks = element_line(color = "black", size = 0.7),
        axis.title = element_text(color = "black", face = "bold"),
        strip.background = element_rect(color = "black",
                                        fill = "black"),
        strip.text = element_text(color = "white"),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", hjust = 0)))


