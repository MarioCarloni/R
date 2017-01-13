library(quantmod)
library(tseries)

getSymbols('OSG', from = '2016-06-13')
getSymbols('CVV', from = '2016-06-13')

OSG <- data.frame(OSG)
CVV <- data.frame(CVV)

OSG.price <- unclass(OSG$OSG.Adjusted)
CVV.price <- unclass(CVV$CVV.Adjusted)

adf.test(OSG.price)
adf.test(CVV.price)

model1 <- lm(OSG.price ~ CVV.price)
model2 <- lm(CVV.price ~ OSG.price)

adf.test(model1$residuals, k=1)
adf.test(model2$residuals, k=1)


plot(model1$residuals, type = 'l', col = 'red')
par(new = T)
plot(model2$residuals, type = 'l', col = 'blue')
par(new = F)