setwd("C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK")
install.packages("usethis") #run this line if you need to install the usethis package

# you will need to install these packages if you don't have them already
# install.packages(c("here", "readxl", "tidyverse", "patchwork", "magrittr", 
# "broom", "ggrepel", "lubridate", "bacondecomp", "staggered", "did"))
# You may also need to download the developer version of `DRDID` before if you
# get an error. To do so use: 
# install.packages("devtools")
# devtools::install_github("pedrohcgs/DRDID")
# Finally, install the package emo from Hadley Wickham's GitHub:
# devtools::install_github("hadley/emo")

library(here) #nice file paths
library(readxl) #read in excel data
library(tidyverse) #collection of packages for data science
library(patchwork) #"stiches" together ggplots
library(magrittr) #pipes
library(broom) #tidy displays
library(ggrepel) #for labelling points on a ggplot
library(lubridate) #helps with date objects
library(bacondecomp) #Goodman Bacon Decomposition
library(did) #Callaway and Sant'Anna estimator for DID
library(staggered) #Sun and Abraham estimator for DID
