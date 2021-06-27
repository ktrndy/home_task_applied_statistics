#ЗАДАЧА 1
#Зафиксируйте сид 7. Сгенерируйте выборку из 200 точек по правилу: Y = X2+sin(3X)+N(0,1), где Х – стандартная нормальная величина. Предположим, вы не хотите гадать зависимость и подходите к задаче регрессии Y ~ X с помощью деревьев. Постройте дерево и нарисуйте предсказание для:
#a. Глубины 1
#b. Глубины 2
#c. Глубины 5
#d. Глубины 10. Здесь не забудьте попросить алгоритм строить глубокое дерево, указав параметр cр достаточно маленьким, к примеру, cp = 0.00001
#e. Подберите оптимальное дерево с помощью прунинга и нарисуйте получившиеся предсказания. Опять, не забудьте переобучить дерево.

#ЗАДАЧА 2
#Исследуйте датасет kyphosis. Используйте решающие деревья, чтобы предсказать искривление позвоночника. Покажите, почему на малых выборках прунинг не работает. Откройте настоящее предназначение деревьев: представьте данные так, чтобы их поняла произвольная бабушка. В этом вам поможет пакет rpart.plot

set.seed(7)
X=rnorm(200)
E=rnorm(200)
Y=X^2+sin(3*X)+E #настоящая зависимость
plot(X,Y, pch=16)

library(rpart)

my_control1 = rpart.control(cp = 0.0001, xval = 100, maxdepth = 1)
mod1 = rpart(formula = Y~X, method = "anova", control = my_control1)
res1 = predict(mod1)
plot(X,Y, col=rgb(0,0,0,0.3),pch=16) 
lines(X[order(X)],res1[order(X)], type = "l", col=2,lwd=3) 


my_control2 = rpart.control(cp = 0.0001, xval = 100, maxdepth = 2)
mod2 = rpart(formula = Y~X, method = "anova", control = my_control2)
res2 = predict(mod2)
lines(X[order(X)],res2[order(X)], type = "l", col=3,lwd=3) 


my_control5 = rpart.control(cp = 0.0001, xval = 100, maxdepth = 5)
mod5 = rpart(formula = Y~X, method = "anova", control = my_control5)
res5 = predict(mod5)
lines(X[order(X)],res5[order(X)], type = "l", col=4,lwd=3) 


my_control10 = rpart.control(cp = 0.0001, xval = 100, maxdepth = 10)
mod10 = rpart(formula = Y~X, method = "anova", control = my_control10)
res10 = predict(mod10)
lines(X[order(X)],res10[order(X)], type = "l", col=5,lwd=3) 


my_control = rpart.control(cp = 0.00001, xval = 100)
mod = rpart(formula = Y~X, method = "anova",control = my_control)
plotcp(mod)
mod_pruned = prune(mod, cp = 0.066)  
res=predict(mod_pruned)
lines(X[order(X)],res[order(X)], type = "l", col=7,lwd=3) 


X1=rnorm(1000) 
E1=rnorm(1000) 
Y1=X1^2+sin(3*X1)+E1 
data_matrix1=data.frame(X1) 
names(data_matrix1)="X" 

mod_pruned = prune(mod, cp = 0.066) 

predict1=predict(mod_pruned, newdata = data_matrix1) 
plot(X1,Y1, col=rgb(0,0,0,0.3),pch=16) 
lines(X1[order(X1)],predict1[order(X1)], type = "l", col=2,lwd=3)