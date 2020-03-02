setwd("C:/ML/Amtrak")
data <- read.csv("Amtrak edit.csv", header = TRUE)

colnames(data)
colnames(data)[2] <- "Ridership"

plot(data$Ridership, type = "o")

#dummy variable
x <- data.frame(outer(rep(month.abb,length=159), month.abb, "==")+0)
colnames(x) <- month.abb
newdata <- cbind(data,x)
colnames(newdata)[2] <- "Ridership"


newdata["t"] <- 1:159
#adding log variable
newdata["log_rider"] <- log(newdata["Ridership"])
newdata["t squared"] <- newdata["t"]*newdata["t"]

#data partition
train <- newdata[1:108,] 
test <- newdata[109:120,]

### linear Model
linear_model <- lm(Ridership~t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval = "predict", newdata = test))
View(linear_pred)

rmse_linear <- sqrt(mean((test$Ridership-linear_pred$fit)^2, na.rm = T))                          
rmse_linear    

test$Ridership
linear_pred$fit

###Exponential

expo_model <- lm(log_rider~t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = "predict", newdata = test))
View(expo_pred)

rmse_expo <- sqrt(mean((test$Ridership-exp(expo_pred$fit))^2, na.rm = T))                          
rmse_expo

test$Ridership
linear_pred$fit

###Quadratic

colnames(newdata)[18] <- "tsquare"
colnames(train)[18] <- "tsquare"
colnames(test)[18] <- "tsquare"
quad_model <- lm(Ridership~t+tsquare, data = train)
summary(quad_model)
quad_pred <- data.frame(predict(quad_model, interval = "predict", newdata = test))
View(quad_pred)

rmse_quad <- sqrt(mean((test$Ridership-quad_pred$fit)^2, na.rm = T))                          
rmse_quad

test$Ridership
quad_pred$fit

####additive seasonality
add_sea <-lm(Ridership~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=train)
summary(add_sea)
addsea_pred <- data.frame(predict(add_sea, newdata = test, interval="predict"))
rmse_addsea <- sqrt(mean((test$Ridership-addsea_pred$fit)^2, na.rm = T))                          
rmse_addsea

test$Ridership
addsea_pred$fit

####additive seasonality with linear
addsea_linear <-lm(Ridership~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=train)
summary(addsea_linear)
addsea_linear_pred <- data.frame(predict(addsea_linear, newdata = test, interval="predict"))
rmse_addsealinear <- sqrt(mean((test$Ridership-addsea_linear_pred$fit)^2, na.rm = T))                          
rmse_addsealinear

test$Ridership
addsea_pred$fit

####additive seasonality with quadratic
addsea_quad <-lm(Ridership~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=train)
summary(addsea_quad)
addsea_quad_pred <- data.frame(predict(addsea_quad, newdata = test, interval="predict"))
rmse_addseaquad <- sqrt(mean((test$Ridership-addsea_quad_pred$fit)^2, na.rm = T))                          
rmse_addseaquad

test$Ridership
addsea_pred$fit

####multiplicative seasonality
multisea <-lm(log_rider~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=train)
summary(multisea)
multisea_pred <- data.frame(predict(multisea, newdata = test, interval="predict"))
rmse_multisea <- sqrt(mean((test$Ridership-exp(multisea_pred$fit))^2, na.rm = T))                          
rmse_multisea

test$Ridership
addsea_pred$fit

####multiplicative seasonality with linear trend
multisea_linear <-lm(log_rider~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=train)
summary(multisea_linear)
multisea_linear_pred <- data.frame(predict(multisea_linear, newdata = test, interval="predict"))
rmse_multisea_linear <- sqrt(mean((test$Ridership-exp(multisea_linear_pred$fit))^2, na.rm = T))                          
rmse_multisea_linear

test$Ridership
addsea_pred$fit

####multiplicative seasonality with quadratic
multisea_quad <-lm(log_rider~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=train)
summary(multisea_quad)
multisea_quad_pred <- data.frame(predict(multisea_quad, newdata = test, interval="predict"))
rmse_multisea_quad <- sqrt(mean((test$Ridership-exp(multisea_quad_pred$fit))^2, na.rm = T))                          
rmse_multisea_quad

test$Ridership
addsea_pred$fit

# RMSE TABLE
table_rmse <- data.frame("Model"=c("rmse_linear","rmse_expo","rmse_quad","rmse_addsea",
                                   "rmse_addsealinear","rmse_addseaquad","rmse_multisea",
                                   "rmse_multisea_linear","rmse_multisea_quad"), 
                         "RMSE"=c(rmse_linear,rmse_expo,rmse_quad,rmse_addsea,rmse_addsealinear,
                                  rmse_addseaquad,rmse_multisea,rmse_multisea_linear,rmse_multisea_quad))
View(table_rmse)

#### Combining RMSE and ARIMA
new_model <-lm(Ridership~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=train)
#dsnew <- data.frame(train+test)
summary(new_model)

resid <- residuals(new_model)
resid[1:10]
hist(resid)
acf(resid,lag.max = 10)

k <- arima(resid, order=c(1,0,0)) 
acf(k$residuals, lag.max = 15)
pred_arima <- predict(arima(k$residuals, order = c(0,1,1)),n.ahead=12)
str(pred_arima)
pred_arima$pred


to be contd>>>>>>


addsea_quad_pred <- data.frame(predict(addsea_quad, newdata = test, interval="predict"))
rmse_addseaquad <- sqrt(mean((test$Ridership-addsea_quad_pred$fit)^2, na.rm = T))                          
rmse_addseaquad

test$Ridership
addsea_pred$fit 

