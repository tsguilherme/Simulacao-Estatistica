n=800
a=2
b=1

##### alpha=a e beta=b ##### função densidade f(x) ####
fx= function(x)(0.1e1 - exp(-b / x)) ^ a * a * b * exp(-b / x) / x ^ 2 / (0.1e1 - exp(-b / x))


a=2
b=1
plot(fx, xlim = c(0,5), ylim = c(0,1), col = "black")
a=1.5
plot(fx, add = T, xlim = c(0,5), ylim = c(0,1), col = "red")
a=0.5
plot(fx, add = T, xlim = c(0,5), ylim = c(0,1), col = "green")
a=1
plot(fx, add = T, xlim = c(0,5), ylim = c(0,1), col = "blue")
legend(x = c(3,5),y = c(0.5,0.9),
       legend = c(expression(paste(beta=="1.0"," ", alpha==2.0)),
                  expression(paste(beta=="1.0"," ", alpha==1.5)),
                  expression(paste(beta=="1.0"," ", alpha==0.5)),
                  expression(paste(beta=="1.0"," ", alpha==1.0))),
       lty =1, cex = 0.65,
       col = c("black","red","green","blue"))

############################ função acumulada F(x)

Fx = function(x) 0.1e1 - (0.1e1 - exp(-b / x)) ^ a

a=2
b=1
plot(Fx, xlim = c(0,5), ylim = c(0,1), col = "black")
a=1.5
plot(Fx, add = T, xlim = c(0,5), ylim = c(0,1), col = "red")
a=0.5
plot(Fx, add = T, xlim = c(0,5), ylim = c(0,1), col = "green")
a=1
plot(Fx, add = T, xlim = c(0,5), ylim = c(0,1), col = "blue")
legend(x = c(3,5),y = c(0.,0.4),
       legend = c(expression(paste(beta=="1.0"," ", alpha==2.0)),
                  expression(paste(beta=="1.0"," ", alpha==1.5)),
                  expression(paste(beta=="1.0"," ", alpha==0.5)),
                  expression(paste(beta=="1.0"," ", alpha==1.0))),
       lty =1, cex = 0.65,
       col = c("black","red","green","blue"))







######################## função de sobrevivência S(x) ########################3
Sx= function(x)(0.1e1 - exp(-b / x)) ^ a


a=2
b=1
plot(Sx, xlim = c(0,5), ylim = c(0,1.4), col = "black")
a=1.5
plot(Sx, add = T, xlim = c(0,5), ylim = c(0,1.4), col = "red")
a=0.5
plot(Sx, add = T, xlim = c(0,5), ylim = c(0,1.4), col = "green")
a=1
plot(Sx, add = T, xlim = c(0,5), ylim = c(0,1.4), col = "blue")
legend(x = c(3,5),y = c(0.7,1.2),
       legend = c(expression(paste(beta=="1.0"," ", alpha==2.0)),
                  expression(paste(beta=="1.0"," ", alpha==1.5)),
                  expression(paste(beta=="1.0"," ", alpha==0.5)),
                  expression(paste(beta=="1.0"," ", alpha==1.0))),
       lty =1, cex = 0.65,
       col = c("black","red","green","blue"))


##### alpha=c e beta=d #### função de risco h(y) ####

hx= function(x) a * b * exp(-b / x) / x ^ 2 / (0.1e1 - exp(-b / x))


a=2
b=1
plot(hx, xlim = c(0,5), ylim = c(0,1.4), col = "black")
a=1.5
plot(hx, add = T, xlim = c(0,5), ylim = c(0,1.4), col = "red")
a=0.5
plot(hx, add = T, xlim = c(0,5), ylim = c(0,1.4), col = "green")
a=1
plot(hx, add = T, xlim = c(0,5), ylim = c(0,1.4), col = "blue")
legend(x = c(3,5),y = c(0.7,1.2),
       legend = c(expression(paste(beta=="1.0"," ", alpha==2.0)),
                  expression(paste(beta=="1.0"," ", alpha==1.5)),
                  expression(paste(beta=="1.0"," ", alpha==0.5)),
                  expression(paste(beta=="1.0"," ", alpha==1.0))),
       lty =1, cex = 0.65,
       col = c("black","red","green","blue"))

warnings()





hx = function(x) lambda ^ alpha * x ^ (alpha - 1) * alpha * exp(lambda ^ alpha * x ^ alpha)
lambda=1
alpha=0.5
plot(hx, xlim = c(0,2), ylim = c(0,2), col = "green")
alpha=0.75
plot(hx, add = T, xlim = c(0,2), ylim = c(0,2), col = "red")
alpha=1
plot(hx, add = T, xlim = c(0,2), ylim = c(0,2), col = "blue")
alpha=1.5
plot(hx, add = T, xlim = c(0,2), ylim = c(0,2), col = "black")
alpha=1.75
plot(hx, add = T, xlim = c(0,2), ylim = c(0,2), col = "purple")
legend(x = c(1,2),y = c(0,1),
       legend = c(expression(paste(lambda=="1.0"," ", alpha==0.5)),
                  expression(paste(lambda=="1.0"," ", alpha==0.75)),
                  expression(paste(lambda=="1.0"," ", alpha==1.0)),
                  expression(paste(lambda=="1.0"," ", alpha==1.5)),
                  expression(paste(lambda=="1.0"," ", alpha==1.75))),
       lty =1, cex = 0.65,
       col = c("green","red","blue","black","purple"))

