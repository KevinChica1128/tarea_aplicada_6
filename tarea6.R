#Tarea 6 Aplicada II
#Kevin García - Alejandro Vargas
#Selección de variables
cadata <- read.csv("~/GitHub/tarea_aplicada_6/cadata.txt", sep="")
cadata<- cadata[-c(1:15528,16029:20640),]

modelo<- lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+
              cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+
              cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
summary(modelo)


#StepAIC
modelostepB<-step(modelo,direction = "backward") #Por AIC
summary(modelostepB) #AIC Backward
modelostepF<-step(modelo,direction = "forward")
summary(modelostepF)
modelostepBo<-step(mod0, scope=list(lower=mod0,upper=modelo) ,direction = "both") #StepAIC
summary(modelostepBo)

summary(stepAIC(modelo,direction = "both",trace = TRUE))
#Con el código 1
library(leaps)
modeloback <- regsubsets(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+
                            cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+
                            cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares,
                         data = cadata, method = "backward")
S<-summary(modeloback)
S$outmat
modeloR2<-which.max(S$adjr2) #El mejor modelo con el R2 ajus es el de 5 variables.
modeloCP<-which.min(S$cp) #Al igual que con el CP
modeloBIC<-which.min(S$bic) #Y con el BIC
coef(modeloback,5) #Coeficientes del mejor modelo

#Con el código 2
#Matriz X:
X<-matrix(c(cadata$Ingreso_mediano,cadata$Edad_mediana_de_la_vivienda,cadata$Total_de_habitaciones,
            cadata$Total_de_dormitorios,cadata$Poblacion,cadata$Hogares),nrow = 500,ncol = 6)
Y<-cadata$Valor_mediano_de_la_casa
modelo3<-leaps(X,Y,method = "adjr2")
which.max(modelo3$adjr2)

#Backward:
mod0<-lm(cadata$Valor_mediano_de_la_casa~1)
AIC(mod0)
summary(mod0)
mod.backward <- stepAIC(modelo, scope = list(lower = mod0),direction = "backward")
mod.forward <- stepAIC(mod0,scope=list(upper=modelo),direction = "forward")
