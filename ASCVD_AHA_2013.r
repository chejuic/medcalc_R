## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.csie.ntu.edu.tw/" 
options(repos=r)
})
## Encoding
options(encoding = "UTF-8")
# Install and load required R packages
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)}
ipak(c('shiny','shinydashboard','DT','gsheet','rsconnect'))


setwd('G:/我的雲端硬碟/Github/medcalc_R/ASCVD_AHA_2013\\')
runApp(choose.dir())
