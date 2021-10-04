# init.R
# Load/install libraries ----

my_packages = c("shinydashboard", "shinyjs", "tidyverse",
                "forecast", "plotly") 
install_if_missing = function(p) { 
  if (p %in% rownames(installed.packages()) == FALSE) { 
    install.packages(p) 
  } 
} 
invisible(sapply(my_packages, install_if_missing))