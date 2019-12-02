
### Setting RStudio ###
rm(list=ls())

if(!require("pacman"))install.packages("pacman")
pacman::p_load(tidyverse, curl)


###  Importing data from the Github repo ###
wi <-read.csv("https://raw.githubusercontent.com/alexmerk/mstat/master/w19_hu_mva/01/wdi_info.csv")
wd <-read.csv("https://raw.githubusercontent.com/alexmerk/mstat/master/w19_hu_mva/01/wdi_daten.csv")


### i) plot unemployment of all western EU against years using scatterplot
d1 <- wd %>% filter(Region=="Western Europe")

g1 <- ggplot (d1, aes(x=AL, y=Jahr, col=Land)) +
    geom_point()
g1



