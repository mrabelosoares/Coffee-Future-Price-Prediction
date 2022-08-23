library(tidyverse)
library(readxl)
WINFUT <- read_excel("Database.xlsx", sheet = "WINFUT")
DOLFUT <- read_excel("Database.xlsx", sheet = "DOLFUT")
ICFFUT <- read_excel("Database.xlsx", sheet = "ICFFUT")


names(WINFUT)[1] <- "date"
names(WINFUT)[2] <- "symbol"
names(WINFUT)[3] <- "open"
names(WINFUT)[4] <- "high"
names(WINFUT)[5] <- "low"
names(WINFUT)[6] <- "close"
names(WINFUT)[7] <- "volume"


  

WINFUTstat <- WINFUT %>% mutate(Order = order(Data, decreasing = TRUE),
                                VarFechamento = Fechamento)
WINFUTstat
summary(WINFUTstat)


min(WINFUT$Data)
min(DOLFUT$Data)
min(ICFFUT$Data)



