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

returnICFFUT <- ICFFUT |> 
  arrange(Data) |> 
  mutate(ret = Fechamento - lag(Fechamento))
returnICFFUT

#lag 1, 5, 10, 20 days in points
returnICFFUTPTS <- ICFFUT |> 
  arrange(Data) |> 
  mutate(ret = Fechamento - lag(Fechamento),
         ret5 = Fechamento - lag(Fechamento, n = 5) ,
         ret10 = Fechamento - lag(Fechamento, n = 10),
         ret22 = Fechamento - lag(Fechamento, n = 22))
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
  geom_hline(yintercept = 25, col="red") +
  geom_hline(yintercept = -25, col= "red") +
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

#Outcome data visualizantion in percent
p1 <- returnICFFUTPER |>
  ggplot(aes(x = year(Data), y = ret)) +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()

p5 <- returnICFFUTPER |>
  ggplot(aes(x = year(Data), y = ret5)) +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()

p10 <- returnICFFUTPER |>
  ggplot(aes(x = year(Data), y = ret10))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()

p22 <- returnICFFUTPER |>
  ggplot(aes(x = year(Data), y = ret22))  +
  geom_hline(yintercept = 0.1, col="red") +
  geom_hline(yintercept = -0.1, col= "red") +
  geom_point()

gridExtra::grid.arrange(p1, p5, p10, p22,
                        nrow = 2, 
                        top = "Return by trade days, year by year")

p2003 <- returnICFFUTPER |>
  group_by(year(Data)) |>
  filter(year(Data) == 2003) |>
  ggplot(aes(x = Data, y = ret5))  +
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
  geom_point()
p2022



Upper50 <- returnICFFUT |> filter(ret22 > 50)
summary(Upper50)


Upper15 |>
  ggplot(aes(x = Data, y = ret22)) +
  geom_point()

Under15 <- returnICFFUT |> filter(ret22 < - 0.15)
summary(Under15)

Under15 |>
  ggplot(aes(x = Data, y = ret22)) +
  geom_point()

#lag 2 days

return2ICFFUT <- ICFFUT |> 
  arrange(Data) |> 
  mutate(ret2 = Fechamento / lag(Fechamento, n = 2) - 1)
summary(return2ICFFUT)


return2ICFFUT |>
  ggplot(aes(x = Data, y = ret2)) +
  geom_point()

Upper52 <- return2ICFFUT |> filter(ret2 > 0.05)
summary(Upper52)


Upper52 |>
  ggplot(aes(x = Data, y = ret2)) +
  geom_point()

Under52 <- return2ICFFUT |> filter(ret2 < - 0.05)
summary(Under52)

Under52 |>
  ggplot(aes(x = Data, y = ret2)) +
  geom_point()


#lag 3 days

return3ICFFUT <- ICFFUT |> 
  arrange(Data) |> 
  mutate(ret3 = Fechamento / lag(Fechamento, n = 3) - 1)
summary(return3ICFFUT)


return3ICFFUT |>
  ggplot(aes(x = Data, y = ret3)) +
  geom_point()

Upper53 <- return3ICFFUT |> filter(ret3 > 0.05)
summary(Upper52)


Upper53 |>
  ggplot(aes(x = Data, y = ret3)) +
  geom_point()

Under53 <- return3ICFFUT |> filter(ret3 < - 0.05)
summary(Under53)

Under53 |>
  ggplot(aes(x = Data, y = ret3)) +
  geom_point()
