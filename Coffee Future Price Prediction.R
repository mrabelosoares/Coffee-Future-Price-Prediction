#check library and install packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(fields)) install.packages("fields", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

# Load library
library(caret)
library(data.table)
library(fields)
library(tidyverse)
library(knitr)
library(kableExtra)
library(grid)
library(ggplot2)
library(lattice)
library(gridExtra)
library(recosystem)
library(readxl)


##Data Clean

#create tempfile and download

dl <- tempfile()
download.file("https://github.com/mrabelosoares/Coffee-Future-Price-Prediction/raw/main/Database.xlsx", dl)

#read the file

WINFUT <- read_excel("Database.xlsx", sheet = "WINFUT")
DOLFUT <- read_excel("Database.xlsx", sheet = "DOLFUT")
ICFFUT <- read_excel("Database.xlsx", sheet = "ICFFUT")

#Future Contract of Coffee
ICFFUT
summary(ICFFUT)

#Day without trade
Notrade <- ICFFUT |> filter(`Volume Financeiro` == "0")
Notrade


#lag 1 day
returnICFFUT <- ICFFUT |> 
  arrange(Data) |> 
  mutate(ret = Fechamento / lag(Fechamento) - 1)
summary(returnICFFUT)


Notrade <- returnICFFUT |> filter(`Volume Financeiro` == "0")
Notrade

returnICFFUT |>
  ggplot(aes(x = Data, y = ret)) +
  geom_point()

Upper5 <- returnICFFUT |> filter(ret > 0.05)
summary(Upper5)


Upper5 |>
  ggplot(aes(x = Data, y = ret)) +
  geom_point()

Under5 <- returnICFFUT |> filter(ret < -0.05)
summary(Under5)

Under5 |>
  ggplot(aes(x = Data, y = ret)) +
  geom_point()

#lag 2 days

return2ICFFUT <- ICFFUT |> 
  arrange(Data) |> 
  mutate(ret = Fechamento / lag(Fechamento, n = 2) - 1)
summary(return2ICFFUT)


return2ICFFUT |>
  ggplot(aes(x = Data, y = ret)) +
  geom_point()

Upper52 <- return2ICFFUT |> filter(ret > 0.05)
summary(Upper52)


Upper52 |>
  ggplot(aes(x = Data, y = ret)) +
  geom_point()

Under52 <- returnICFFUT |> filter(ret < -0.05)
summary(Under52)

Under52 |>
  ggplot(aes(x = Data, y = ret)) +
  geom_point()
