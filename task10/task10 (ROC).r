A=read.csv2(file="~/Documents/RStudio(домахи)/data_2.2.csv")
#Используя выборку, логистическую регрессию, пакет pROC и презентацию с семинара:
library(pROC)

#1 задание: Исследуйте связь ЛНП(LDL) с АГ(code_AH). Постройте РОК-кривую.
mod1=glm(code_AH~LDL, data=A, family = binomial(link = "logit"))
summary(mod1)
model_predict1=predict(mod1, type = "response")
model_roc1=roc(predictor = model_predict1,response = A$code_AH)
plot(model_roc1)
#LDL значимый фактор 


#2 задание: Исследуйте связь пола с АГ. Постройте РОК-кривую. Почему она такая некрасивая?
mod2=glm(code_AH~SEX, data=A, family = binomial(link = "logit"))
summary(mod2)
model_predict2=predict(mod2, type = "response")
model_roc2=roc(predictor = model_predict2,response = A$code_AH)
plot(model_roc2)
#p-value тут по-больше, roc-кривая по-ниже, чем в 1 случае, но пол всё равно - значимый фактор

#3 задание: Исследуйте связь пола и ЛНП с АГ с помощью множественной регрессии.Постройте РОК-кривую.
mod3=glm(code_AH~SEX+LDL, data=A, family = binomial(link = "logit"))
summary(mod3)
model_predict3=predict(mod3, type = "response")
model_roc3=roc(predictor = model_predict3,response = A$code_AH)
plot(model_roc3)
#roc-кривая почти везде выше, чем в 1 и 2 случаях, auc больше, эта модель лучше объясняет АГ?


#4 задание: Постройте все три кривые на одном рисунке разными цветами. В легенде напишите тип модели и площадь под кривой (АУК).
plot.roc(model_roc1, col="green",lwd=2)
plot.roc(model_roc2,add=TRUE, col="red",lwd=2)
plot.roc(model_roc3,add=TRUE, col="blue",lwd=2)
#model_roc1$auc = 0.6223
#model_roc2$auc = 0.556
#model_roc3$auc = 0.6488
legend("bottomright",legend=c("mod1 AUC = 62.23 %","mod2 AUC = 55.6 %","mod3 AUC = 64.88 %"),col=c("green","red","blue"),lwd=2)
