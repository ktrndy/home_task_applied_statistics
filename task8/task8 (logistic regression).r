A=read.csv(file="~/Documents/RStudio(домахи)/logregr_data.csv")

#ЗАДАЧА 1
#Файл logregr_data.csv содержит выборку из 5000 россиян.

#a) С помощью логрегрессии исследуйте связь между возрастом и АГ. Интерпретируйте результат через отношение шансов. К примеру: «повышение возраста на 1 увеличивает шанс развития АГ в ХХ раз».

#H0: AGE не объясняет зависимую переменную code_AH (коэффиц. при нём = 0).
alpha=0.05
mod1=glm(formula=code_AH~AGE, data=A, family = binomial(link = "logit")) 
summary(mod1)
#p-value < 2e-16, отклоняем H0.
coef(mod1) #коэфициенты при факторах
exp(coef(mod1))
#отношение шансов заболеть увеличивается ~ в 1.1 раз при увеличении возраста на 1 год.  


#b) Сделайте тоже самое для пола и ЛНП (см. описание переменных)

#H0: SEX и LDL не объясняет зависимую переменную code_AH (коэффиц. при нём = 0).
mod2=glm(formula=code_AH~SEX+LDL, data=A, family = binomial(link = "logit")) 
summary(mod2)
#LDL - значимый фактор (p-value < 2e-16), SEX - нет (p-value = 0.398).
exp(coef(mod2))
#отношение шансов заболеть увеличивается ~ в 1.5 раз при увеличении LDL на 1 (и уменьшается в .. для женщин?). 


#c) Изучите связь сразу всех переменных из базы с АГ. Что-нибудь поменялось по сравнению с однофакторным анализом? Сравните однофакторные ОШ с многофакторными для пола, возраста и ЛНП и объясните результат

#H0: независимые переменные(SEX,AGE,WAI..) не объясняют зависимую(code_AH) (коэффиц. при них = 0)
mod3=glm(formula= code_AH~.-X, data=A, family = binomial(link = "logit")) 
summary(mod3)
#значимые факторы - все, кроме LDL. 
exp(coef(mod3))
#Jтношение шансов заболеть увеличивается ~ в 1.08 раз при увеличении возраста на 1 год и фиксированных
# остальных факторах, ~ в 1.07 раз при увеличении BMI на 1 и т.д.
#Отношение шансов для возраста и LDL и пола уменьшилось в многофакторном. 
#Добавленные в 3ю модель факторы сами по себе скорее всего зависят от пола и возраста из-за чего 
# ОШ для отдельных факторов при фиксированных остальных перераспределилось и, как следствие, уменьшилось.
#(то же самое с коэффициентами в многофакторном и однофакторном).

#ЗАДАЧА 2
#Для пункта с предыдущего задания используйте другие линки: пробит и Коши. Как можно объяснить изменения в статистических выводах?

mod4=glm(formula= code_AH~AGE, data=A, family = binomial(link = "probit")) 
summary(mod4) 
mod5=glm(formula= code_AH~AGE, data=A, family = binomial(link = "cauchit")) 
summary(mod5) 
mod6=glm(formula=code_AH~SEX+LDL, data=A, family = binomial(link = "probit")) 
summary(mod6) 
mod7=glm(formula=code_AH~SEX+LDL, data=A, family = binomial(link = "cauchit")) 
summary(mod7) 
mod8=glm(formula= code_AH~.-X, data=A, family = binomial(link = "probit")) 
summary(mod8)
mod9=glm(formula= code_AH~.-X, data=A, family = binomial(link = "cauchit")) 
summary(mod9) 

coef(mod1)
coef(mod4) 
coef(mod5) 

coef(mod2)
coef(mod6) 
coef(mod7)

coef(mod3)
coef(mod8)
coef(mod9)
# во всех 3х моделях немного изменились коэфициенты при факторах и свободные члены, p-value изменились незначительно. 

exp(coef(mod1))
exp(coef(mod4)) 
exp(coef(mod5)) 

exp(coef(mod2))
exp(coef(mod6)) 
exp(coef(mod7))

exp(coef(mod3))
exp(coef(mod8))
exp(coef(mod9))
 

# почти ничего не поменялось. зачем тогда брать неинтерпретируемые модели, если можно взять логрегрессию. так что надобность в пробите и коши просто отпадает
