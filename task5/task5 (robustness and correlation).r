#(без проверки!)
#ЗАДАЧА 1
#Сравните между собой непарными тестами Вилкоксона и Стьюдента выборки из файла pair_1.csv. Постройте графики. Что происходит?

A=read.csv(file="~/Documents/RStudio(домахи)/pair_1.csv")
AA=A$A
AB=A$B
par(mfrow=c(1,2))
plot(density(AA,from=-10,to=20),col="red")
lines(density(AB,from=-10,to=20),col="green")
plot(density(AA-AB),col="green")
curve(dnorm(x,mean=0,sd=sd(AA-AB)),add = T,col="red")
t.test(AA-AB,alternative="two.sided")  #p-value < 2.2e-16 => true location is not equal to 0
wilcox.test(AA-AB,alternative = "two.sided")  #p-value < 2.2e-16 => true location is not equal to 0

#у AA и AB есть выбросы, именно поэтому не подтверждается гипотеза H0 о нормальности со средним 0
#даже несмотря на то, что распределение АА-АВ на графике похоже на нормальное (но это не точно, я пока не придумала лучшего объяснения)

#ЗАДАЧА 2
#Сравните между собой непарными тестами Вилкоксона и Стьюдента выборки из файла pair_2.csv. Постройте графики. Да господи, что происходит?

B=read.csv(file="~/Documents/RStudio(домахи)/pair_2.csv")
BA=B$A
BB=B$B
par(mfrow=c(1,2))
plot(density(BA,from=-20,to=20),col="red")
lines(density(BB,from=-20,to=20),col="green")
plot(density(BA-BB),col="green")
curve(dnorm(x,mean=0,sd=sd(BA-BB)),add = T,col="red")
t.test(BA-BB,alternative="two.sided")  #p-value = 6.569e-14 => true location is not equal to 0
wilcox.test(BA-BB,alternative = "two.sided")  #p-value = 1.999e-05 => true location is not equal to 0

#здесь та же история, что и в 1 задаче

#ЗАДАЧА 3
#Пощупайте понятия корреляции.

W=read.csv(file="~/Documents/RStudio(домахи)/std_correlations.csv")
a=W$A
b1=W$B1
b2=W$B2
b3=W$B3
b4=W$B4

#a) Для этого исследуйте корреляцию Пиросна случайной величины А с величинами В1, В2, В3 и В4 из файла std_correlations.csv . Сделайте это на четырёх графиках, построенных одновременно. Каждый график должен выглядеть как-то так:

r1=cor(x=a,y=b1,method = "pearson") #1  
r2=cor(x=a,y=b2,method = "pearson") #0.7088821  #С помощью коэффициента корреляции можно определить силу линейной взаимосвязи между переменными
r3=cor(x=a,y=b3,method = "pearson") #0.4456699
r4=cor(x=a,y=b4,method = "pearson") #0.04133461

par(mfrow=c(2,2))
plot(x=a,y=b1,type = "p",col="blue")
location = "bottomright"
legend(location, legend=c("r = 1"),cex = 0.7)
plot(x=a,y=b2,type = "p",col="blue")
legend(location, legend=c("r = 0.7088821"),cex = 0.7)
plot(x=a,y=b3,type = "p",col="blue")
legend(location, legend=c("r = 0.4456699"),cex = 0.7)
plot(x=a,y=b4,type = "p",col="blue")
legend(location, legend=c("r = 0.04133461"),cex = 0.7)

#b) На этих же данных исследуйте корреляцию Спирмена и Кендалла. График:
#не знаю пока, как в легенду графика вставлять имя переменной=значение переменной, поэтому пока сделала это вручную
tho1=cor(x=a,y=b1,method = "spearman") # 1
tho2=cor(x=a,y=b2,method = "spearman") # 0.6905024
tho3=cor(x=a,y=b3,method = "spearman") # 0.4271926
tho4=cor(x=a,y=b4,method = "spearman") # 0.03951613

tau1=cor(x=a,y=b1,method = "kendall") # 0.9999999
tau2=cor(x=a,y=b2,method = "kendall") # 0.5001273
tau3=cor(x=a,y=b3,method = "kendall") # 0.2925176
tau4=cor(x=a,y=b4,method = "kendall") # 0.02632247

par(mfrow=c(2,2))
plot(x=a,y=b1,type = "p",col="blue")
location = "bottomright"
legend(location, legend=c("tho = 1","tau = 0.9999999"),cex = 0.7)
plot(x=a,y=b2,type = "p",col="blue")
legend(location, legend=c("tho = 0.6905024","tau = 0.5001273"),cex = 0.7)
plot(x=a,y=b3,type = "p",col="blue")
legend(location, legend=c("tho = 0.4271926","tau = 0.2925176"),cex = 0.7)
plot(x=a,y=b4,type = "p",col="blue")
legend(location, legend=c("tho = 0.03951613","tau = 0.02632247"),cex = 0.7)

#тут спирмен везде больше кендалла 

#c) Сравните в общих словах Пирсона, Спирмена и Кендалла.
#Пирсон будет неустойчив к выбросам
#Спирмен сильнее реагирует на несогласие ранжировок(конкордантные/дисконкондартные пары), 
#чем Кендалл (это следует из формул для коэффциентов корреляции), поэтому в b) получили Спирмена больше Кендалла



#ЗАДАЧА 4
#Пощупайте понятия корреляции поплотнее. Для этого исследуйте корреляции Пирсона, Спирмена и Кендалла случайной величины А с величинами С1, С2, С3 и С4 из файла notstd_correlations.csv по схеме из предыдущего задания
#Предполагается, что вы возьмёте старый код и немного его адаптируете.

Q=read.csv(file="~/Documents/RStudio(домахи)/notstd_correlations.csv")
aq=Q$A
c1=Q$C1
c2=Q$C2
c3=Q$C3
c4=Q$C4

par(mfrow=c(2,2))
plot(x=aq,y=c1,type = "p",col="blue")
plot(x=aq,y=c2,type = "p",col="blue")
plot(x=aq,y=c3,type = "p",col="blue")
plot(x=aq,y=c4,type = "p",col="blue")

#на 2, 3 и 4 графиках видны выбросы, уберём их, чтобы корреляция была точнее

#с выбросами
rq1=cor(x=aq,y=c1,method = "pearson") # -0.01642752
rq2=cor(x=aq,y=c2,method = "pearson") # -0.009572435
rq3=cor(x=aq,y=c3,method = "pearson") # 0.4440186
rq4=cor(x=aq,y=c4,method = "pearson") # 0.00381705

#в случае 3 по значению корреляции Пирсона можно предположить, что линейная взаимосвязь есть, 
#но даже по графику видно, что это не так (ну вроде бы, хотя я могу и ошибаться) и поэтому я тут дальше убираю выбросы

#без выбросов

ind_aq=which(aq %in% boxplot.stats(aq)$out)
aqq=aq[-ind_aq]
ind_c1=which(c1 %in% boxplot.stats(c1)$out)
c1q=c1[-ind_c1]
ind_c2=which(c2 %in% boxplot.stats(c2)$out)
c2q=c2[-ind_c2]
ind_c3=which(c3 %in% boxplot.stats(c3)$out)
c3q=c3[-ind_c3]
ind_c4=which(c4 %in% boxplot.stats(c4)$out)
c4q=c4[-ind_c4]

cor(x=aqq,y=c1q[1:length(aqq)],method = "pearson") # было: -0.01642752 стало: -0.01144581 
cor(x=aqq,y=c2q[1:length(aqq)],method = "pearson") # было: -0.009572435 стало: -0.008564575
cor(x=c3q,y=aqq[1:length(c3q)],method = "pearson") # было: 0.4440186  стало: -0.01155698
cor(x=c4q,y=aqq[1:length(c4q)],method = "pearson") # было: 0.00381705 стало: -0.01028641

#с выбросами
cor(x=aq,y=c1,method = "spearman") # -0.009133402
cor(x=aq,y=c2,method = "spearman") # 0.6897809
cor(x=aq,y=c3,method = "spearman") # 0.6905024
cor(x=aq,y=c4,method = "spearman") # 0.4999971

#без
cor(x=aqq,y=c1q[1:length(aqq)],method = "spearman") # -0.01483261
cor(x=aqq,y=c2q[1:length(aqq)],method = "spearman") # -0.01116652
cor(x=aqq[1:length(c3q)],y=c3q,method = "spearman") # -0.006466429
cor(x=aqq[1:length(c4q)],y=c4q,method = "spearman") # -0.006750441

#с выбросами
cor(x=aq,y=c1,method = "kendall") # -0.006527953
cor(x=aq,y=c2,method = "kendall") # 0.4996757
cor(x=aq,y=c3,method = "kendall") # 0.5001273
cor(x=aq,y=c4,method = "kendall") # 9.814982e-05

#без
cor(x=aqq,y=c1q[1:length(aqq)],method = "kendall") # -0.009832267
cor(x=aqq,y=c2q[1:length(aqq)],method = "kendall") # -0.007462295
cor(x=aqq[1:length(c3q)],y=c3q,method = "kendall") # -0.004391201
cor(x=aqq[1:length(c4q)],y=c4q,method = "kendall") # -0.004472036

