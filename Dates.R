library(zoo)
library(tidyverse)
dta <- read.csv("C:/Users/cassidy.nicholas/OneDrive - IS/building-standards-dashboard/DummyData2.csv")

dta$qrt <- as.yearqtr(dta$Ended.date, format = "%d/%m/%Y") + 3/4
dta$qrt <- paste0(as.integer(dta$qrt) - 1, format(dta$qrt, "/%y Q%q"))
