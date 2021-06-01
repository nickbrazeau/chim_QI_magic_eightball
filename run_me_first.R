packages <- c("tidyverse", "shiny", "DT", "ggrepel")
install.packages(setdiff(packages, rownames(installed.packages())))
