#ЗАДАЧА 1
#Файл Sex_weight_TGs.csv содержит репрезентативную выборку населения одного российского города. Ответьте на вопросы ниже, используя подходящие тесты. Ответ приведите в виде «р-значение, оценка эффекта, 95% доверительный интервал». Пример: p=0.02, mean=75.1 (70.2-80.9)

A=read.csv2(file="~/Documents/RStudio(домахи)/Sex_weight_TGs.csv")
#Tr=A$triglycerides[-which(is.na(A$triglycerides)==TRUE)]

#a. Правда ли, что средний вес жителя этого города равен 75?

#двусторонний t-тест
W=A$weight
plot(density(W, from=0), lwd=3,ylab="Density")  
curve(dnorm(x,mean = 75,sd=sd(W)),add=T,col="violet")
#alpha=0.05
#H0: средний вес 75
#H1: средний вес не 75
t.test(W,mu=75,alternative="two.sided")
#p=2.425e-15, mean=78.62388 (77.73537-79.51239)
#p<0.05, отвергаем H0

#b. Кто тяжелее: мужчины или женщины?

#t-тест
#H0: средний вес одинаковый   
#Р1: кто-то тяжелее
t.test(weight~sex, data=A) 
#p < 2.2e-16, meam_men= 84.60380, mean_women=74.73147, CI=(8.133619,11.611029)
#отвергаем H0, по средним и CI мужчины тяжелее

#c. У кого выше уровень триглицеридов: у женщин или у мужчин?

plot(density(Tr, from=0), col="red", lwd=3,ylab="Density")
#выбросов по-больше, вилкоксон
#H0: уровень одинаковый   
#Р1: у кого-то больше
wilcox.test(triglycerides~sex,data=A,conf.int=TRUE)
#p-value = 4.525e-05, mean_men=1.524348, mean_women=1.233128, CI=(0.06005353,0.17002615)
#отвергаем H0, по средним и CI у мужчин выше
#у стьюдента почти такие же результаты p и CI, если убрать выбросы и применить оба теста, результат - тот же

#d. Значимо ли коррелирует вес с триглицеридами? 

plot(x=A$weight,y=A$triglycerides,type = "p",col="blue")
#выбросы
#H0: корреляция = 0
#H1: не 0
cor.test(A$weight,A$triglycerides)
#p < 2.2e-16, CI=(0.2548559,0.3467196)
#отклоняем H0 => значимо коррелируют 


Tr_w=A$triglycerides[A$sex==2]
ind_t=which(Tr_w %in% boxplot.stats(Tr_w)$out)
Tr_w=Tr_w[-ind_t]
Tr_m=A$triglycerides[A$sex==1]
ind_t=which(Tr_m %in% boxplot.stats(Tr_m)$out)
Tr_m=Tr_m[-ind_t]
#p чуть по-больше, CI не изменяется почти

#правильно отметила, что ТГ странно распределены, но тогда к ним нехорошо применять корреляцию Пирсона
