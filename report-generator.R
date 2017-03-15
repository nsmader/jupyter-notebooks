#------------------------------------------------------------------------------#
#
### Drive Creation of Knitr Documents
#
#------------------------------------------------------------------------------#

setwd("~/GitHub/knitr-sandbox/")
n <- 20
df <- data.frame(org = sample(c("Org 1", "Org 2"), n, replace = TRUE),
                 program = paste("Program", sample(state.name, n)))

for (org in unique(df$org)){
  # knitr::knit2html(input = "report-template.Rmd",
  #                  output = paste0("Youth Wellbeing Report - ", org, ".html"))
  rmarkdown::render(input = "report-template.Rmd",
                    output_format = "html_document",
                    output_file = paste0("Youth Wellbeing Report - ", org, ".html"))
}
