#В очередной раз дана выборка из 5000 россиян. В прошлый раз мы увидели намёк на связь между употреблением вина и гипертонией у женщин. Исследуем её подробнее.

#ЗАДАЧА 1
#Исследуем связь между употреблением вина и гипертонией.

#Ваш главный друг в этой домашке – функция table().
#В R тест Фишера сообщает в результате «отношение шансов». Не используйте его: это понятие мы разберём в следующий раз. Об него легко сломать мозг.
#Выводы о направлении связи (но не о значимости) делайте при помощи корреляции.

A=read.csv(file="~/Documents/RStudio(домахи)/wine_vs_ah.csv")

#a) Правда ли, что российские мужчины болеют гипертонией чаще, чем женщины?
#Проверьте это (и всё далее) точным тестом Фишера и тестом хи-квадрат.

tab=with(A, table(code_AH,SEX)) #таблица сопряжённости
#H0: взаимосвязи(пол~болезнь) нет;
#H0: взаимосвязь есть;
#alpha=0.05
chisq.test(tab) #p-value = 1.558e-06
fisher.test(tab) #p-value = 1.403e-06
#p-value < alpha => взаимосвязь пол~болезнь есть


#b) А что можно сказать о связи между употреблением вина и гипертонией в зависимости от пола?

W=A[A$SEX==2,]
M=A[A$SEX==1,]
tab_w=with(W, table(code_AH,WINE))
tab_m=with(M, table(code_AH,WINE))
chisq.test(tab_w) #p-value = 1.795e-11
fisher.test(tab_w) #p-value = 1.434e-11
chisq.test(tab_m) #p-value = 0.09326
fisher.test(tab_m) #p-value = 0.09256
#у женщин взаимосвязь вино~холестерин есть! у мужчин - нет

#c) А о связи между употреблением вина и полом?

tab_wine=with(A, table(SEX,WINE))
chisq.test(tab_wine) #p-value < 2.2e-16
fisher.test(tab_wine) #p-value < 2.2e-16
#взаимосвязь вино~пол есть!

#d) Примените на все три теста поправку на множественные сравнения с помощью p.adjust(). Изменилось ли что-нибудь?

p.adjust(chisq.test(tab)$p.value)
p.adjust(fisher.test(tab)$p.value)
p.adjust(chisq.test(tab_w)$p.value) 
p.adjust(fisher.test(tab_w)$p.value) 
p.adjust(chisq.test(tab_m)$p.value)
p.adjust(fisher.test(tab_m)$p.value)
p.adjust(chisq.test(tab_wine)$p.value)
p.adjust(fisher.test(tab_wine)$p.value)
#значения не изменились

#ненене, надо все тесты, соответствующие разным гипотезам запихнуть в одну поправку:

p.adjust(c(fisher.test(tab)$p.value,fisher.test(tab_w)$p.value,fisher.test(tab_m)$p.value,fisher.test(tab_wine)$p.value),method = "holm")

#правда, снова всё ок

#ЗАДАЧА 2
#Попробуем объяснить такой результат для женщин

#Точный тест Фишера тяжело подсчитать для настолько больших таблиц. Воспользуйтесь методом Монте-Карло с помощью simulate.p.value, установив число перестановок B достаточно большим. Иногда можно обойтись без Монте-Карло, увеличив доступную память с помощью параметра workspace.

#a) Нарисуйте график зависимости заболеваемости гипертонией в процентах от возраста по десятилетиям. Для удобства в базе уже есть кодировка code_age, где 1 это 25-34, 2 это 35-44 и т.д.
#Проверьте, есть ли зависимость с помощью точного теста Фишера и теста хи- квадрат.

tab_age_ah=with(W, table(code_age,code_AH))
X=c(1,2,3,4)
Y=c(tab_age_ah[5]/(tab_age_ah[5]+tab_age_ah[1]),tab_age_ah[6]/(tab_age_ah[6]+tab_age_ah[2]),tab_age_ah[7]/(tab_age_ah[7]+tab_age_ah[3]),tab_age_ah[8]/(tab_age_ah[8]+tab_age_ah[4]))
plot(X,Y,type="l",lwd=3)
chisq.test(tab_age_ah) #p-value < 2.2e-16
fisher.test(tab_age_ah, simulate.p.value = TRUE,B=300000) #-value = 3.333e-06
#зависимость возраст~гипертония для женщин есть

#b) Сделайте тоже самое с вином. Объясните, почему надо рассматривать влияние вина только внутри десятилетия

tab_age_wine=with(W, table(code_age,WINE))
Y_wine=c(tab_age_wine[5]/(tab_age_wine[5]+tab_age_wine[1]),tab_age_wine[6]/(tab_age_wine[6]+tab_age_wine[2]),tab_age_wine[7]/(tab_age_wine[7]+tab_age_wine[3]),tab_age_wine[8]/(tab_age_wine[8]+tab_age_wine[4]))
chisq.test(tab_age_wine) #p-value = 8.626e-06 
fisher.test(tab_age_wine, simulate.p.value = TRUE,B=300000) #p-value = 2.667e-05
#зависимость возраст~употребление вина для женщин есть


#почему надо рассматривать влияние вина только внутри десятилетия?
#потому что внутри каждого десятилетия употреблялось разное количество вина и влиять оно может по-разному

#Не совсем, взгляни:
plot(X,Y_wine,type="l",lwd=3)
#Может это не вино влияет, а тупо возраст: старше - меньше пьет, старше -  больше болеет

#c) Что можно сказать о влиянии вина внутри десятилетия?
tab_wine1=with(W[W$code_age==1,], table(code_AH,WINE))
chisq.test(tab_wine1) #p-value = 0.04389
fisher.test(tab_wine1) #p-value = 0.03349

tab_wine2=with(W[W$code_age==2,], table(code_AH,WINE))
chisq.test(tab_wine2) #p-value = 0.0002344
fisher.test(tab_wine2) #p-value = 0.0002339

tab_wine3=with(W[W$code_age==3,], table(code_AH,WINE))
chisq.test(tab_wine3) #p-value = 0.008448
fisher.test(tab_wine3) #p-value = 0.007721

tab_wine4=with(W[W$code_age==4,], table(code_AH,WINE))
chisq.test(tab_wine4) #p-value = 0.03367
fisher.test(tab_wine4) #p-value = 0.03289

#Что можно сказать о влиянии вина внутри десятилетия?
#несмотря на то, что в 4ом десятилетии вино употребляли меньше всего, зависимость болезнь~вино всё-равно есть
#p-value в 4ом случае очень похож на p-value в 1ом случае
#то есть в разном возрасте употребление вина по-разному влияет на заболеваемость гипертонией

#Нет, не то чтобы по-разному, это неочевижно, но везде есть

#d) Примените поправку на множественные сравнения для предыдущего пункта.Изменилось ли что-нибудь?
  
p.adjust(chisq.test(tab_wine1)$p.value) # и т.д., значения не изменились

#Надо пропихнуть 4 р-шки