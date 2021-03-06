set.seed(7) #��������� ��������� ���������
X=rnorm(200)
Z=rnorm(200)


Y=Z+X^2+2*sin(3*X) #��������� �����������
plot(X,Y, pch=16)

#�������� ������#####
data=data.frame(Y,X,X^2,sin(3*X)) #�����������, ��� �� ������� �������
mod_real=glm(Y~., data = data)
summary(mod_real)

predict_lm=predict(mod_real, newdata = data)
plot(X,predict_lm, type = "l") #��� ��������� �������� ����� ������������ ���� �������������

plot(X,Y, col=rgb(0,0,0,0.3),pch=16) #��������� ����� � rgb() ����� ����������������
lines(X[order(X)],predict_lm[order(X)], type = "l", col=2,lwd=3) #��������

library(glmnet) #��������� �������������

data_matrix=cbind(X,X^2,sin(3*X)) #��������! Glmnet ��� ������ ������� � ������ �� � ���� �������

cross_validation=cv.glmnet(x = data_matrix, y = Y, alpha = 0, nfolds = 10) #��������� �����-��������� �� ������ ������
plot(cross_validation) #������ ������ ����� ����, ������������� �� �����

model_L2=glmnet(x = data_matrix, y = Y, alpha = 0, lambda = 0) #������� � ����������� �������
coef(model_L2) #����� ���������� ������������

predict_L2=predict(model_L2, newx = data_matrix) #������ ������������


plot(X,Y, col=rgb(0,0,0,0.3),pch=16) #��������� ����� � rgb() ����� ����������������
lines(X[order(X)],predict_lm[order(X)], type = "l", col=2,lwd=3) #��������
lines(X[order(X)],predict_L2[order(X)], type = "l", col=4,lwd=3, lty=2) #������� �������


#����������#####

data=data.frame(Y,poly(X,degree = 10, raw = T)) #������� poly() ������ �� ������ �������� ������ �������.
#��� ���� �������� ����������####

#��� ����� ������������ ���: options(scipen=999); ��� ���� ����������� �������� ����� 6e-3 ��� 0.006

#��������� ������ � ���� ������#
