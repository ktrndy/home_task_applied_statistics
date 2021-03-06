#ЗАДАЧА 1
#Проверьте, что ЦПТ работает: Придумайте ужасающе распределённую случайную величину X. Проверьте численно, как в самом первом таске, существует ли у Х среднее и дисперсия. Смоделируйте 6 выборок: X, X+X, X+X+X, X+X+X+X, сумма двадцати Х и что-нибудь ещё. Постройте гистограммы и плотности и сравните нормальным распределением.
#Чтобы сделать 6 графиков на одном рисунке используйте par(mfrow=c(2,3))
#Придумывать жуткую случайную величину удобно с помощью цикла и функции.
#Пример:...

n=100
awfulness=function(k,n) #k-количество с.в в сумме, n-длина
{
  s=seq(1,n,by=1)*0
  for (j in 1:k){
    res=rep(NA, n) #NA-a missing value indicator, копирует значения в n ячеек
    for (i in 1:n){
      a=runif(1)  #generates random deviates от 0 до 1 1 раз
      b=rexp(1,a) # экспоненциальное распределение вектор длины 1 a-это лямбда в эксп.
      if ((b < 0.2)||(b>5)) b=rnorm(1) 
      if ( (b>2) & (b<4) ) b=-b
      res[i]=b}
    s=s+res
  }
  return(s)
}

X1=awfulness(1,n)
X2=awfulness(2,n)
X3=awfulness(3,n)
X4=awfulness(4,n)
X5=awfulness(20,n)
X6=awfulness(13,n)+rnorm(length(X2),mean=mean(X2),sd=sd(X2))

par(mfrow=c(2,3))
hist(X1,freq=FALSE)
lines(density(X1), col="green")
curve(dnorm(x,mean = mean(X1),sd=sd(X1)), add=T,col="violet")

hist(X2,freq=FALSE)
lines(density(X2), col="green")
curve(dnorm(x,mean = mean(X2),sd=sd(X2)), add=T,col="violet")

hist(X3,freq=FALSE)
lines(density(X3), col="green")
curve(dnorm(x,mean = mean(X3),sd=sd(X3)), add=T,col="violet")

hist(X4,freq=FALSE)
lines(density(X4), col="green")
curve(dnorm(x,mean = mean(X4),sd=sd(X4)), add=T,col="violet")

hist(X5,freq=FALSE)
lines(density(X5), col="green")
curve(dnorm(x,mean = mean(X5),sd=sd(X5)), add=T,col="violet")

hist(X6,freq=FALSE)
lines(density(X6), col="green")
curve(dnorm(x,mean = mean(X6),sd=sd(X6)), add=T,col="violet")


#ЗАДАЧА 2
#Для задания используйте случайную величину из предыдущего задания.
#Проверьте, правда ли, что оценка среднего и дисперсии распределены так, как мы сказали на семинаре. Для этого смоделируйте К раз выборку из N элементов, каждый раз подсчитывая среднее и дисперсию. В итоге вы получите выборку из К средних и К дисперсий. Постройте её гистограмму, плотность и сравните с нормальным распределением и с распределением хи-квадрат соответственно. Желательно нарисовать два графика на одном рисунке

my_samples=function(k,n) 
{
  means=seq(1,k,by=1)
  vars=medians=seq(1,k,by=1)
  for (j in 1:k){
    res=rep(NA, n) #NA-a missing value indicator, копирует значения в n ячеек
    for (i in 1:n){
      a=runif(1)  #generates random deviates от 0 до 1 1 раз
      b=rexp(1,a) # экспоненциальное распределение вектор длины 1 a-это лямбда в эксп.
      if ((b < 0.2)||(b>5)) b=rnorm(1) 
      if ( (b>2) & (b<4) ) b=-b
      res[i]=b}
    means[j]=mean(res)
    vars[j]=var(res)}
  return(c(means,vars))
}
k=800
n=40
MS=my_samples(k,n)
means=MS[1:k]    #выборочные средние
vars=MS[(k+1):(k+k)]  #выборочные дисперсии
par(mfrow=c(1,2))
hist(means,freq = FALSE)
lines(density(means),col="green")
curve(dnorm(x,mean=mean(means),sd=sd(means)),add=T,col="red")
hist(vars*(n/mean(vars)),freq = FALSE)
lines(density(vars*(n/mean(vars))),col="green")
curve(dchisq(x, df=(n-1), ncp = 0, log = FALSE),add=T,col="red")


#ЗАДАЧА 3
#Говорят, что нормальным распределением хорошо описываются ошибки при измерении. Я посчитал до десяти вслух и засёк время. И честно провёл этот эксперимент 30 раз. Данные в ten.csv

#a. Постройте гистограмму выборки и её плотность распределения на одном рисунке
#b. Нарисуйте поверх плотности выборки плотность нормального распределения с параметрами, оцененными по выборке
#c. Что можно сказать об утверждении: «Я в среднем я считаю до десяти ровно 10 секунд?»

A=read.csv(file="~/Documents/RStudio(домахи)/ten.csv")
par(mfrow=c(1,1))
hist(A$x,freq=FALSE)
lines(density(A$x),col="green")
curve(dnorm(x,mean = mean(A$x),sd=sd(A$x)),add=T,col="red")

#Что можно сказать об утверждении:
#«Я в среднем я считаю до десяти ровно 10 секунд?»

alpha = 0.05
#H0: mu = 10
#Ha: mu не = 10
t.test(A$x,alternative = "two.sided",mu=10)
#p-value < alpha = 0.05 => гипотезу H0 отклоняем.
