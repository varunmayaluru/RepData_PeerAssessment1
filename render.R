
# Commit your PA1_template.md and PA1_template.html files produced by processing
# your R markdown file with knit2html() function in R (from the knitr package) 
# by running the function from the console.

setwd("~/Coursera/5. Reproducible research/Week 2")

# As rmarkdown file produced is a v2 document, using render function of
# rmarkdown package

library(rmarkdown)

render(input = "PA1_template.Rmd", output_file = "PA1_template.html")
