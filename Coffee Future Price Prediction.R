
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
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(runner)) install.packages("runner", repos = "http://cran.us.r-project.org")
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(xts)) install.packages("xts", repos = "http://cran.us.r-project.org")
if(!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
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
library(readxl)
library(dplyr)
library(purrr)
library(zoo)
library(runner)
library(quantmod)
library(rpart)
library(randomForest)
library(xts)
library(TTR)
library(corrplot)
library(rpart.plot)
##Data Clean

#create tempfile and download
dl <- tempfile()
download.file("https://github.com/mrabelosoares/Coffee-Future-Price-Prediction/blob/2044135ec7c5bcfb6f357374de4ede11c9382fd8/CoffeDatabase.xlsx", dl)

#read file XLSX format with decisions
ICFFUT <- read_xlsx("CoffeDatabase.xlsx", sheet = "ICFFUT")
#read file XLSX format with ICF Prices
ICFFUT_XLSX <- read_xlsx("CoffeDatabase.xlsx", sheet = "ICFFUT_XTS")
#Convert XLXS format to Data Frame
DFICFFUT_XTS <- as.data.frame(ICFFUT_XLSX)
#Convert Data Frame to XTS
ICFFUT_XTS <- xts(DFICFFUT_XTS[-1], order.by = as.Date(DFICFFUT_XTS$Date))

##Technical Indicators - Price-Based
#moving average 5 days
Moving_Average_5 <- SMA(ICFFUT_XTS$ICFFUT.Close, n=5)
#moving average 22 days
Moving_Average_22 <- SMA(ICFFUT_XTS$ICFFUT.Close, n=22)
#Bollinger Bands
Bollinger_Bands <- BBands(ICFFUT_XTS$ICFFUT.Close)
#Rate of Change Oscillator
Rate_Change_Oscillator <- ROC(ICFFUT_XTS$ICFFUT.Close, n=10)
#Relative Strength Index
Relative_Strength_Index <- RSI(ICFFUT_XTS$ICFFUT.Close)
#Stochastic Oscillator / Stochastic Momentum Index
Stochastic <- stoch(ICFFUT_XTS$ICFFUT.Close)
#MACD Oscillator
MACD <- MACD(ICFFUT_XTS$ICFFUT.Close)

#merge XTS
Full_ICFFUT <- merge(ICFFUT_XTS, 
                     Moving_Average_5, 
                     Moving_Average_22,
                     Bollinger_Bands,
                     Rate_Change_Oscillator,
                     Relative_Strength_Index,
                     Stochastic,
                     MACD)

#create data frame ICFFUT - 4/5 Arabica Coffee Futures
DFICFFUT <- data.frame(Date=index(Full_ICFFUT), coredata(Full_ICFFUT))

#Merge Data frame ICFFUT - 4/5 Arabica Coffee Futures and ICFFUT
CDFICFFUT <- left_join(DFICFFUT, 
                       ICFFUT |> select(Date, Decision, OpenInterest)) |>
  filter(Date > "2003-02-17") |> #excluding NA
  mutate(Decision  = as.factor(Decision)) #Decision as a factor = Y

##Data exploration and visualization

#Chart ICFFUT Price
chart_Series(ICFFUT_XTS,type = "candlesticks")

#Day without trade
Notrade <- CDFICFFUT |> 
  filter(ICFFUT.Volume == "0") |>
  select(Date, ICFFUT.Close, ICFFUT.Volume, Decision)
Notrade

#class, type and proprieties of data frame
as_tibble(CDFICFFUT)
summary(CDFICFFUT)
str(CDFICFFUT)

#distribution decision by year
p1 <- CDFICFFUT |>
  mutate(year = year(as.Date(Date)))|>
  filter(Decision == "Buy") |>
  ggplot(aes(x = year)) +
  geom_bar(stat="count") + 
  ylab("Buy")

p2 <- CDFICFFUT |>
  mutate(year = year(as.Date(Date)))|>
  filter(Decision == "Neutral") |>
  ggplot(aes(x = year)) +
  geom_bar(stat="count") + 
  ylab("Neutral")

p3 <- CDFICFFUT |>
  mutate(year = year(as.Date(Date)))|>
  filter(Decision == "Sell") |>
  ggplot(aes(x = year)) +
  geom_bar(stat="count") + 
  ylab("Sell")


#Buy, Neutral, Sell distribution throughout years
gridExtra::grid.arrange(p1, p2, p3, 
                        nrow = 3, 
                        top = "Buy, Neutral, Sell distribution throughout years")


#correlation between variables and outcome
NDFICFFUT <- CDFICFFUT |> mutate( #Decision as a number
  NDecision = case_when(
    Decision == "Neutral" ~ 0, #If Neutral, then O
    Decision == "Buy" ~ 1,#If Buy, then 1
    Decision == "Sell" ~ -1)#If sell, then -1
  ) |> select(-Decision, -Date) #less categorical and date column
C <- cor(NDFICFFUT) #correlation matrix
corrplot(C, method = 'square', type = 'lower') 

#empirical optimal results - sum last 22 days
profitICFFUT <- CDFICFFUT |> 
  arrange(Date) |> 
  mutate(return = ICFFUT.Close - lag(ICFFUT.Close) ,
         status = as.factor(case_when(
           Decision == lag(Decision) ~ "Hold",
           Decision != lag(Decision) ~ "Change")),
         multiplier = case_when(
           status == "Change" & lag(Decision) == "Neutral" ~ 0,
           status == "Hold" & Decision == "Neutral" ~ 0,
           status == "Change" & lag(Decision) == "Buy" ~ 1,
           status == "Hold" & Decision == "Buy" ~ 1,
           status == "Change" & lag(Decision) == "Sell" ~ -1,
           status == "Hold" & Decision == "Sell" ~ -1),
         adjust = return * multiplier,
         profit = case_when(
           Decision == "Neutral" ~ 0,
           Decision != "Neutral" ~ sum_run(adjust, k=22)
           ))
summary(profitICFFUT |> select(return, status, adjust, profit))

#profit adjust from the last 22 days
profitICFFUT |> 
  ggplot(aes(Date, profit)) + 
  geom_line()

#profit by year
profitY <- profitICFFUT |> mutate(Year = as.factor(year(as.Date(Date))))
profit_table <- as.data.frame(tapply(profitY$adjust, 
                                     profitY$Year, 
                                     FUN=sum, 
                                     na.rm = TRUE))
colnames(profit_table) <- "profit_year"
profit_table

#Modeling approach
#create a partition
separation_date <- as.Date("2020-01-02")
training_sample <- filter(CDFICFFUT, Date < separation_date)
testing_sample <- filter(CDFICFFUT, Date >= separation_date)
str(testing_sample)
#defining the predictors - Model 1
knn_fit <- knn3(Decision ~ ., 
                data = training_sample, k=5)
knn_fit
#probability and class of model
head(predict(knn_fit, testing_sample, type = "prob"))
head(predict(knn_fit, testing_sample, type = "class"))

#balanced accuracy - model 1
y_hat_knn <- predict(knn_fit, testing_sample, type = "class")
confusionMatrix(y_hat_knn, testing_sample$Decision)$overall["Accuracy"]
cm1 <- confusionMatrix(y_hat_knn, testing_sample$Decision)
cm1[["byClass"]][ , "Precision"]
cm1[["byClass"]][ , "Recall"]
KNN <- cm1[["byClass"]][ , "F1"]
KNN
#defining the predictors - Model 2
fit <- rpart(Decision ~ ., data = training_sample)
rpart.plot(fit)
#Structural break
as.POSIXlt(1.514808e+09,origin="1970-01-01")
as.POSIXlt(1.438603e+09,origin="1970-01-01")
as.POSIXlt(1.319285e+09,origin="1970-01-01")
as.POSIXlt(1.251158e+09,origin="1970-01-01")


#accuracy - model 2
y_hat_RT <- predict(fit, testing_sample, type = "class")
confusionMatrix(y_hat_RT, testing_sample$Decision)$overall["Accuracy"]
cm2 <- confusionMatrix(y_hat_RT, testing_sample$Decision)
cm2[["byClass"]][ , "Precision"]
cm2[["byClass"]][ , "Recall"]
Classification_Tree <- cm2[["byClass"]][ , "F1"]
Classification_Tree

#defining the predictors - Model 3
fit_RF <- randomForest(Decision ~., data = training_sample) 
#Variable Importance Plot
varImpPlot(fit_RF)
#accuracy - model 3
y_hat_RF <- predict(fit_RF, testing_sample, type = "class")
confusionMatrix(y_hat_RF, testing_sample$Decision)$overall["Accuracy"]
cm3 <- confusionMatrix(y_hat_RF, testing_sample$Decision)
cm3[["byClass"]][ , "Precision"]
cm3[["byClass"]][ , "Recall"]
Random_Forest <- cm3[["byClass"]][ , "F1"]
Random_Forest
#Results
knitr::kable(tibble(Decision = c("Buy", "Neutral", "Sell"), 
                    KNN, 
                    Classification_Tree, 
                    Random_Forest), 
             caption = "F1 Score by approach",
             position = "h")


