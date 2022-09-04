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
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")

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
library(dplyr)
library(purrr)
library(zoo)

##Data Clean

#create tempfile and download

dl <- tempfile()
download.file("https://github.com/mrabelosoares/Coffee-Future-Price-Prediction/blob/2044135ec7c5bcfb6f357374de4ede11c9382fd8/CoffeDatabase.xlsx", dl)

#read the file
database <- read_xlsx("CoffeDatabase.xlsx")
WINFUT <- read_xlsx("CoffeDatabase.xlsx", sheet = "WINFUT", col_names = FALSE)
DOLFUT <- read_xlsx("CoffeDatabase.xlsx", sheet = "DOLFUT")
ICFFUT <- read_xlsx("CoffeDatabase.xlsx", sheet = "ICFFUT")
WINFUT
#Future Contract of Coffee
ICFFUT
summary(ICFFUT)

#Day without trade
Notrade <- ICFFUT |> filter(`Volume` == "0")
Notrade

returnICFFUT <- ICFFUT |> 
  arrange(Date) |> 
  mutate(return = Close - lag(Close) ,
         multiply = case_when(
          Decision == "Neutral" ~ 0,
          Decision == "Buy" ~ 1,
          Decision == "Sell" ~ -1) ,
         adjust = return * multiply)
summary(returnICFFUT)
df <- data.frame(returnICFFUT)
profityear <- df |>
  group_by(year(Date)) |>
  mutate(Profit = sum(adjust, na.rm = TRUE))
summary(profityear)

#lag 1, 5, 10, 20 days in points
returnICFFUTPTS <- ICFFUT |> 
  arrange(Date) |> 
  mutate(ret = Close - lag(Close),
         ret5 = Close - lag(Close, n = 5) ,
         ret10 = Close - lag(Close, n = 10),
         ret22 = Close - lag(Close, n = 22))
summary(returnICFFUTPTS)

#lag 1, 5, 10, 20 days in percent
returnICFFUTPER <- ICFFUT |> 
  arrange(Data) |> 
  mutate(ret = Fechamento / lag(Fechamento) - 1,
         ret5 = Fechamento / lag(Fechamento, n = 5) - 1,
         ret10 = Fechamento / lag(Fechamento, n = 10) - 1,
         ret22 = Fechamento / lag(Fechamento, n = 22) - 1)
summary(returnICFFUTPER)


#Outcome data visualization in points
p1 <- returnICFFUTPTS |>
  ggplot(aes(x = year(Data), y = ret)) +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
  
p5 <- returnICFFUTPTS |>
  ggplot(aes(x = year(Data), y = ret5)) +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
  
p10 <- returnICFFUTPTS |>
  ggplot(aes(x = year(Data), y = ret10))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()

p22 <- returnICFFUTPTS |>
  ggplot(aes(x = year(Data), y = ret22))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()

gridExtra::grid.arrange(p1, p5, p10, p22,
                        nrow = 2, 
                        top = "Return by trade days, year by year")

p2003 <- returnICFFUTPTS |>
  group_by(year(Data)) |>
  filter(year(Data) == 2003) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
p2003

p2007 <- returnICFFUTPTS |>
  group_by(year(Data)) |>
  filter(year(Data) == 2007) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
p2007

p2012 <- returnICFFUTPTS |>
  group_by(year(Data)) |>
  filter(year(Data) == 2012) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
p2012

p2020 <- returnICFFUTPTS |>
  group_by(year(Data)) |>
  filter(year(Data) == 2020) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
p2020


p2021 <- returnICFFUTPTS |>
  group_by(year(Data)) |>
  filter(year(Data) == 2021) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
p2021

p2022 <- returnICFFUTPTS |>
  group_by(year(Data)) |>
  filter(year(Data) == 2022) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 50, col="red") +
  geom_hline(yintercept = -50, col= "red") +
  geom_point()
p2022

#Outcome data visualization in percent
p1 <- returnICFFUTPER |>
  ggplot(aes(x = year(Data), y = ret, group = year(Data)))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()

p5 <- returnICFFUTPER |>
  ggplot(aes(x = year(Data), y = ret5, group = year(Data)))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()

p10 <- returnICFFUTPER |>
  ggplot(aes(x = year(Data), y = ret10, group = year(Data)))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()

p22 <- returnICFFUTPER |>
  ggplot(aes(x = month(Data), y = ret22, group = month(Data)))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()
p22

gridExtra::grid.arrange(p1, p5, p10, p22,
                        nrow = 2, 
                        top = "Return by trade days, year by year")


pweek22 <- returnICFFUTPER |>
  ggplot(aes(x = week(Data), y = ret22, group = week(Data)))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()
pweek22

pweek10 <- returnICFFUTPER |>
  ggplot(aes(x = week(Data), y = ret10, group = week(Data)))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()
pweek10

pweekdays <- returnICFFUTPER |>
  ggplot(aes(x = weekdays(Data), y = ret, group = weekdays(Data)))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()
pweekdays


p2003 <- returnICFFUTPER |>
  group_by(year(Data)) |>
  filter(year(Data) == 2021) |>
  ggplot(aes(x = Data, y = ret10))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()
p2003

p2007 <- returnICFFUTPER |>
  group_by(year(Data)) |>
  filter(year(Data) == 2007) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()
p2007

p2012 <- returnICFFUTPER |>
  group_by(year(Data)) |>
  filter(year(Data) == 2012) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()
p2012

p2020 <- returnICFFUTPER |>
  group_by(year(Data)) |>
  filter(year(Data) == 2020) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()
p2020


p2021 <- returnICFFUTPER |>
  group_by(year(Data)) |>
  filter(year(Data) == 2021) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()
p2021

p2022 <- returnICFFUTPER |>
  group_by(year(Data)) |>
  filter(year(Data) == 2022) |>
  ggplot(aes(x = Data, y = ret5))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_boxplot()
p2022


#Trade Strategy

returnICFFUTPER |> 