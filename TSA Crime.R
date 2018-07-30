# Boston Crime Rate- TIME SERIES ANALYSIS

# Data visualization
library(xlsxjars)
library(xlsx)
violent <-read.xlsx("E:\\NEU\\IE 7275 Data mining\\R\\case\\violent crime.xlsx", sheetIndex = 3, header = T,stringsAsFactors = F)
str(violent)

# Parsing the date format
library(zoo)
violent$DATE1 <- as.yearmon(violent$DATE,"%Y/%m")
colnames(violent)[5] <- "Violent.Crime"

# Line chart
library(ggplot2)

# Time series plot using ggplot
ggplot(violent, aes(x=DATE1)) +
  geom_line(aes(y=AGGRAVATED.ASSAULT, color="AGGRAVATED.ASSAULT"))+
  geom_line(aes(y=ROBBERY, color="ROBBERY"))+
  geom_line(aes(y=HOMICIDE, color="HOMICIDE"))+
  geom_line(aes(y=Violent.Crime, color="Violent.Crime"))+
  scale_color_manual(values = c("firebrick2", "black", "cornflowerblue", "orange"))+
  labs(title =" Violent Crime 2012/07~ 2017/07", x = "", y = "Counts")

# Line plot for aggregate violent crime
ggplot(violent, aes(x=DATE1)) +
  geom_line(aes(y=Violent.Crime, color="Violent.Crime"))+
  scale_color_manual(values = c("orange"))+
  labs(title =" Violent.Crime 2012/07~ 2017/07", x = "", y = "Counts")

# Line plot for aggravated assault over time
ggplot(violent, aes(x=DATE1)) +
  geom_line(aes(y=AGGRAVATED.ASSAULT, color="AGGRAVATED.ASSAULT"))+
  scale_color_manual(values = c("firebrick2"))+
  labs(title =" AGGRAVATED.ASSAULT 2012/07~ 2017/07", x = "", y = "Counts")

# Line plot for robbery over time
ggplot(violent, aes(x=DATE1)) +
  geom_line(aes(y=ROBBERY, color="ROBBERY"))+
  scale_color_manual(values = c("cornflowerblue"))+
  labs(title =" ROBBERY 2012/07~ 2017/07", x = "", y = "Counts")

# Line plot for homicide over time
ggplot(violent, aes(x=DATE1)) +
  geom_line(aes(y=HOMICIDE, color="HOMICIDE"))+
  scale_color_manual(values = c("black"))+
  labs(title =" HOMICIDE 2012/07~ 2017/07", x = "", y = "Counts")

# Bar plot for violent crime over the seasons
str(violent)
sea <- c("spring","summer","fall","winter")
ggplot(violent.season, aes(x=season, y=Violent.crime,))+
  theme_bw()+
  geom_bar(stat = "identity",position="stack", width = 0.5)+
  scale_x_discrete(limits = sea)+
  scale_fill_manual(values=c('#fee8c8','#fdbb84','#e34a33'))+
  labs(title =" Violent Crime between seasons", x = "", y = "value")

# Bar plot for violent crime over the week
library(reshape2)
dat = melt(violent.week[-5], id.var="DAY_WEEK", variable.name="status")
week.or <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
ggplot(dat, aes(x=DAY_WEEK, y=value, fill=status))+
  theme_bw()+
  geom_bar(stat = "identity",position="stack", width = 0.5)+
  scale_x_discrete(limits = week.or)+
  scale_fill_manual(values=c("gray65","gray38","black"))+
  labs(title =" Violent Crime between Days of the week", x = "", y = "value")

# Bar plot for violent crime in districts
library(reshape2)
dat = melt(district[-5], id.var="district", variable.name="status")
ggplot(dat, aes(x=district, y=value, fill=status))+
  theme_bw()+
  geom_bar(stat = "identity",position="stack")+
  scale_fill_manual(values=c(AGGRAVATED.ASSAULT="gray65", ROBBERY="gray38", HOMICIDE="black"))+
  labs(title =" Violent Crime between districts", x = "districts", y = "value")

# Time series plot using ts()
Violent.Crime=ts(violent$Violent.Crime)

# Data partition 60%40%
violent.ts <- ts(Violent.Crime, start = c(2012,7), end = c(2017,6), frequency = 12)
train <- window(violent.ts, start = c(2012,7), end = c(2015,6), frequency = 12)
validation <- window(violent.ts, start = c(2015,7), end = c(2017,6), frequency = 12)

# Time series plot for training set
plot.ts(train, main="Time series plot of violent crime from 201207~201506")

# ACF PACF plot
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
acf(train, main="ACF")
pacf(train,main="PACF")
par(opar)

# ARIMA model selection
library(forecast)
auto.arima(train)

#ARIMA(0,0,0)(1,1,0)[12]
train_model <- arima0(train, order = c(0,0,0),seasonal = list(order =c(1,1,0),
                      period=12))


# Residual diagnostic
residuals <- train_model$residuals
par(mfrow = c(2,2))
ts.plot(residuals)
abline(h=0)
qqnorm(residuals)
qqline(residuals)
acf(residuals)
pacf(residuals)

# Test for Stationarity
library(TSA)
adf.test(residuals)
pp.test(residuals)
kpss.test(residuals)

# Test for normality of residuals
shapiro.test(residuals)

# Test for independence of residuals
Box.test(residuals, type = "Box-Pierce")
Box.test(residuals, type = "Ljung-Box")

# Developing a predictive model
pred <- predict(train_model,n.ahead = 24)
pred$pred
pred.real.ts <- ts(data.frame(validation, pred=pred$pred),start = c(2015,7), end = c(2017,6), frequency = 12)
pred.real.ts
library(ggfortify)
autoplot(pred.real.ts, facets = FALSE,ts.linetype = 1,xlim = , ylab = "Counts", main = "Prediction vs. real value from 201507~ 201706")

# Performance evaluation
library(Metrics)
mae(validation,pred$pred)