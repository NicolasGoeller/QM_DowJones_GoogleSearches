source("scrape_googletrends.R")
library(tidyverse)
library(lubridate)
library(tseries)
library(zoo)
library(xts)
library(ftplottools)
library(stargazer)
library(rugarch)

dj_data <- readRDS("Gtrends_DowJones.rds")
names(dj_data) <- c("date", "dj_hits", "dj_monrank", "dj_rank")
dji_data <- readRDS("Gtrends_DowJonesIndustrial.rds")
names(dji_data) <- c("date", "dji_hits", "dji_monrank", "dji_rank")
djia_data <- readRDS("Gtrends_DowJonesIndustrialAverage.rds")
names(djia_data) <- c("date", "djia_hits", "djia_monrank", "djia_rank")

gsv_data <- inner_join(dj_data, dji_data, by= "date")
gsv_data <- inner_join(gsv_data, djia_data, by= "date")
gsv_data$weekday <- wday(gsv_data$date)
gsv_data$weekday <- factor(gsv_data$weekday, c(2:7,1),  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
#gsv_data <- na.omit(gsv_data)

dj_series <- ts(read.zoo(gsv_data[,c("date", "dj_rank")]))
dji_series <- ts(read.zoo(gsv_data[,c("date", "dji_rank")]))
djia_series <- ts(read.zoo(gsv_data[,c("date", "djia_rank")]))

ggplot(data= gsv_data) + 
  geom_line(aes(x=date, y=dj_monrank*100), color= "red")+
  xlab("Date")+ylab("Search volume rank")+
  ft_theme()

ggplot(data= gsv_data) + 
  geom_line(aes(x=date, y=dj_rank), color= "black")+
  geom_line(aes(x=date, y=dj_monrank*100), color= "red")+
  xlab("Date")+ylab("Search volume rank")+
  ft_theme()

ggplot(data= gsv_data) + 
  geom_line(aes(x=date, y=dj_monrank*100), color= "red")+
  geom_line(aes(x=date, y=dji_monrank*100), color= "black")+
  geom_line(aes(x=date, y=djia_monrank*100), color= "blue")+
  xlab("Date")+ylab("Monthly search volume rank")+
  ft_theme()

ggplot(data= gsv_data) + 
  geom_line(aes(x=date, y=dj_rank), color= "red", alpha=0.5)+
  geom_line(aes(x=date, y=dji_rank), color= "black", alpha=0.5)+
  geom_line(aes(x=date, y=djia_rank), color= "blue", alpha=0.5)+
  xlab("Date")+ylab("Daily search volume rank")+
  ft_theme()

jarque.bera.test(djia_series)

adf.test(djia_series)

acf(djia_series)
pacf(djia_series)

djia_diff <- diff(djia_series, 7)

acf(djia_diff)
pacf(djia_diff)

jarque.bera.test(djia_diff)

adf.test(djia_diff)

gsv_data %>% 
  select(weekday, djia_rank) %>% 
  group_by(weekday) %>%
  summarise(mean_rank= mean(djia_rank))


ggplot(gsv_data, aes(x= weekday, y=djia_rank))+
  geom_boxplot()+
  xlab("Weekday")+ylab("Search volume distribution")+
  ft_theme()
  

fit1 <- arma(djia_diff, c(0,1))
summary(fit1)
fit2 <- arma(djia_diff, c(1,0))
summary(fit2)
fit3 <- arma(djia_diff, c(1,1))
summary(fit3)
fit4 <- arma(djia_diff, c(0,2))
summary(fit4)
fit5 <- arma(djia_diff, c(2,0))
summary(fit5)
fit6 <- arma(djia_diff, c(1,2)) #Best fit
summary(fit6)
fit7 <- arma(djia_diff, c(2,1))
summary(fit7)
fit8 <- arma(djia_diff, c(2,2))
summary(fit8)
fit9 <- arma(djia_diff, c(3,2))
summary(fit9)
fit10 <- arma(djia_diff, c(2,3))
summary(fit10)
fit11 <- arma(djia_diff, c(3,3))
summary(fit11)
fit12 <- arma(djia_diff, c(3,4))
summary(fit12)


model_names <- c("ARMA(0,1)","ARMA(1,0)", "ARMA(1,1)", "ARMA(0,2)", "ARMA(2,0)","ARMA(1,2)", "ARMA(2,1)", "ARMA(2,2)",
                 "ARMA(3,2)", "ARMA(2,3)", "ARMA(3,3)", "ARMA(3,4)")
aic_values <- c(round(summary(fit1)$aic,1), round(summary(fit2)$aic,1), round(summary(fit3)$aic,1), round(summary(fit4)$aic,1),
                round(summary(fit5)$aic,1), round(summary(fit6)$aic,1), round(summary(fit7)$aic,1), round(summary(fit8)$aic,1),
                round(summary(fit9)$aic,1), round(summary(fit10)$aic,1), round(summary(fit11)$aic,1), round(summary(fit12)$aic,1))
aic_table <- cbind(model_names, aic_values)
colnames(aic_table) <- c("Model specifications", "AIC")
stargazer(aic_table, column.labels =c("Model name","AIC value"), header=FALSE, type='html')

##Test for resid autocorr
Box.test(fit12$residuals, type = "Ljung-Box")

## test for ARCH effects
Box.test((fit12$residuals)^2, type = "Ljung-Box")

model=ugarchspec( variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                  mean.model = list(armaOrder = c(3, 4), 
                                    include.mean = FALSE), distribution.model = "sstd" ) 
#Check other GARCH
modelfit=ugarchfit(spec=model,data=djia_diff)
modelfit
## 0,2: AIC -6.1097
## 1,1: AIC -6.5624
## 1,0: AIC -6.3137
## 0,1: AIC -6.1101
## 2,1: AIC -6.5621
## 2,2: AIC -6.5619
#Box.test(fit6$residuals^2, type="Ljung-Box")
modelfit
## std: 2,2: -6.6341
## sstd: 2.2: -6.6420
## ssted: 2,1: -6.6422
plot(modelfit,which=8)

