# PISA_PV1MATH

# Influencia del género y variables socioeconómicas en el rendimiento de matemáticas en los estudiantes para prueba PISA 2012.

## Evaluar las variables de género y socioeconómicas propuesta para determinar su influencia en las brechas de rendimiento de los estudiantes latinoamericanos en la prueba de matemática de Pisa 2012

### Marco Antonio Espinoza Marín B82760
### Isaura Gutiérrez Vargas B73591 
### Alison Lobo Salas B84339
### Felipe Moya Alvarado B85446 
### Daniela Vindas Torres B88619


## *Paquetes a utilizar*

```{r, message=FALSE, results='hide', warning=FALSE}
library(haven)
library(foreign)
library(tidyverse)
library(MASS)
library(Hmisc)
library(reshape2)
library(coefplot)
library(sjPlot)
library(see)
library(effects)
library(ggeffects)
library(margins)
```

Cargamos la base

```{r, message=FALSE}
setwd("C:/Users/Marco Espinoza/OneDrive/Documents/Universidad de Costa Rica/I semestre 2021/Modelos probabilísticos discretos/investigación")
base.pisa <- read_stata("base.dta")
```

# *Seleccionamos las variables dependientes*

```{r}

#VARIABLES INDEPENDIENTES Y DEPENDIENTES

#VARIABLE DEPENDIENTES
valores.log = base.pisa %>%
  dplyr::select(StIDStd,cnt,PV1MATH) %>%
  dplyr::filter(cnt == c("ARG", "BRA", "CHL","COL", "CRI", "MEX", "PER",
                         "URY"))
#REVISAMOS LA ESTRUCTURA DE NUESTRAS VARIABLES, ESTO ESPECIALMENTE PARA VER LA 
#CLASE DE CADA UNA Y VER SI NECESITAN RECODIFICACION
summary(valores.log)
str(valores.log)
#QUITAMOS EL IDENTIFICADOR Y EL PAIS PARA RECODIFICAR LOS VALORES
valores.log <- valores.log[,-c(1:2)]

valores.log1 = valores.log %>% 
  mutate(PV1MATH = case_when((PV1MATH <= 420) ~ 0,
                             (PV1MATH > 420) ~ 1))
valores.log2 = valores.log %>%
  mutate(PV1MATH = case_when((PV1MATH <= 482) ~ 0,
                             (PV1MATH > 482) ~ 1))

valores.log3 = valores.log %>% 
  mutate(PV1MATH = case_when((PV1MATH <= 545) ~ 0,
                             (PV1MATH > 545) ~ 1))

```


Percentiles utilizados acorde a los niveles de alfabetización
```{r}

percentile <- ecdf(valores.log$PV1MATH)
percentile(420)


percentile <- ecdf(valores.log$PV1MATH)
percentile(482)

percentile <- ecdf(valores.log$PV1MATH)
percentile(545)


```

# *Variables explicativas*
primeramente, definimos nuestras variables explicativas y procedemos a seleccionarlas

```{r}
variables.expl <- base.pisa %>% 
  dplyr::select(cnt,ST04Q01,escs,misced,
                immig) %>% 
  dplyr::filter(cnt == c("ARG", "BRA", "CHL","COL", "CRI", "MEX", "PER",
                         "URY"))


nvl.acadmadre = car::recode(variables.expl$misced, "0 = 0; 1 = 1;
                            2:3 = 2; 4 = 3; 5 = 4; 6 = 5")

#unimos las variables junto el vector ordinal
valores.logcom = cbind(variables.expl, valores.log1,valores.log2,
                       valores.log3)

colnames(valores.logcom) = c("pais","genero","ind_economico",
                             "nvl.acadmadre",
                             "status.migra",
                             "notas1","notas2","notas3")

valores.logcom = valores.logcom[,-4]

#cambiamos el nombre de nuestras variables

valores.logcom = cbind(valores.logcom, nvl.acadmadre)


#ahora revisamos la estructura de los datos
sapply(valores.logcom, class)
str(valores.logcom)
summary(valores.logcom$genero)
```

Cambio de clase de las variables

```{r}
sapply(valores.logcom, class)

valores.logcom$genero = as.factor(valores.logcom$genero)
valores.logcom$ind_economico = as.numeric(valores.logcom$ind_economico)
valores.logcom$status.migra = as.factor(valores.logcom$status.migra)
valores.logcom$nvl.acadmadre = as.factor(valores.logcom$nvl.acadmadre)
```



*Análisis descriptivo *

A continuación, se realizó un análisis descriptivo de las variables independientes según nuestra variable respuesta

```{r}

t.notas1Genero = table(valores.logcom$genero, valores.logcom$notas1, dnn = c("Género","Notas 1"))
rownames(t.notas1Genero) = c("Mujer","Hombre")

t.notas2Genero = table(valores.logcom$genero, valores.logcom$notas2,dnn = c("Género","Notas 2"))
rownames(t.notas2Genero) = c("Mujer","Hombre")

t.notas3Genero = table(valores.logcom$genero, valores.logcom$notas3, dnn = c("Género","Notas 3"))
rownames(t.notas3Genero) = c("Mujer","Hombre")

t.notas1Migra = table(valores.logcom$status.migra, valores.logcom$notas1, dnn = c("S.Migratorio","Notas 1"))

t.notas2Migra = table(valores.logcom$status.migra, valores.logcom$notas2, dnn = c("S.Migratorio","Notas 2"))

t.notas3Migra = table(valores.logcom$status.migra, valores.logcom$notas3, dnn = c("S.Migratorio","Notas 3"))

t.notas1Pais = table(valores.logcom$notas1,valores.logcom$pais)
t.notas2Pais = table(valores.logcom$notas2,valores.logcom$pais)
t.notas3Pais = table(valores.logcom$notas3,valores.logcom$pais)


t.notas1acadMadre = table(valores.logcom$nvl.acadmadre, valores.logcom$notas1, dnn = c("nvl.Académico Madre","Notas 1"))
t.notas2acadMadre = table(valores.logcom$nvl.acadmadre, valores.logcom$notas2, dnn = c("nvl.Académico Madre","Notas 2"))

t.notas3acadMadre = table(valores.logcom$nvl.acadmadre, valores.logcom$notas3, dnn = c("nvl.Académico Madre","Notas 3"))

```


```{r}
t.notas1Genero
t.notas2Genero
t.notas3Genero
t.notas1Migra
t.notas2Migra
t.notas3Migra
t.notas1acadMadre
t.notas2acadMadre
t.notas3acadMadre
View(valores.logcom)
colSums(is.na(valores.logcom))

cbind(t.notas1Migra,t.notas2Migra,t.notas3Migra)
cbind(t.notas1acadMadre,t.notas2acadMadre,t.notas3acadMadre)

```



## Modelo 1

```{r}

model <- glm(as.factor(notas1) ~ genero + ind_economico + nvl.acadmadre +
                 status.migra, data = valores.logcom, 
               family = binomial(link = "logit"), na.action = na.omit)

#vemos el modelo
model
#vemos el resumen del modelo
summary(model)
#revisamos el intervalo de confianza, donde los datos no tiene a 0
#lo que significa que son significantes para nuestro modelo
confint.default(model)

```

## Modelo 2

```{r}
#MODELO 2
model2 <- glm(as.factor(notas2) ~ genero + ind_economico + nvl.acadmadre +
                status.migra, data = valores.logcom, 
              family = binomial(link = "logit"), na.action = na.omit)
model2
summary(model2)
confint.default(model2)

```

## Modelo 3

```{r}
model3 <- glm(as.factor(notas3) ~ genero + ind_economico + nvl.acadmadre +
                status.migra, data = valores.logcom, 
              family = binomial(link = "logit"), na.action = na.omit)
model3
summary(model3)
confint.default(model3)
```

## Odd Ratio para el modelo 1

```{r}
(ci1 <- confint(model))

ORmodel = exp(cbind("OR model 1" = coef(model)))

```

## Odd ratio para el modelo 2

```{r}
(ci2 <- confint(model2))

ORmodel2 = exp(cbind("OR model 2" = coef(model2)))
```

## Odd ratio para el modelo 3

```{r}
(ci3 <- confint(model3))

ORmodel3 = exp(cbind("OR model 3" = coef(model3)))

```


## Efectos marginales promedio

```{r}
efectos1=margins_summary(model)
efectos2=margins_summary(model2)
efectos3=margins_summary(model3)


efectos1
efectos2
efectos3
```


```{r}

ORt.pvalues = cbind(ORmodel[-1, ],ORmodel2[-1, ],
      ORmodel3[-1, ])

colnames(ORt.pvalues) = c("OR model 1","OR model 2",
                         "OR model 3")
rownames(ORt.pvalues) = c("género","índice económico","nivel educativo madre 1",
                          "nivel educativo madre 2",
                          "nivel educativo madre 3","nivel educativo madre 4",
                          "nivel educativo madre 5","status migratorio 2",
                          "status migratorio 3")

coeficientes = cbind(model$coefficients, model2$coefficients, model3$coefficients)
colnames(coeficientes) = c("Modelo 1", "Modelo 2", "Modelo 3")
rownames(coeficientes) = c("intercept","género","índice económico","nivel educativo madre 1",
                          "nivel educativo madre 2",
                          "nivel educativo madre 3","nivel educativo madre 4",
                          "nivel educativo madre 5","status migratorio 2",
                          "status migratorio 3")


coeficientes
ORt.pvalues

```

## *Gráficos*

```{r}
coefplot(model)  + 
  theme_minimal() + 
  labs(title="Estimación de coeficientes con error estandar", 
       x="Estimación", 
       y="Variable", 
       caption="Exámenes PISA 2012") + theme_blank()

ggsave(filename="COEF.png", width=10, height=2*3, dpi=300)

#efecto marginales en el promedio
model1ggpredict  = ggpredict(model, terms = c("ind_economico[all]","nvl.acadmadre","genero"))
ggplot(model1ggpredict, aes(x, predicted, colour = group)) + geom_line() + facet_wrap(~ facet) + theme_blank()
#ggsave(filename="efectoMarginalEnelpromedio1.png", width=10, height=2*3, dpi=300)

model2ggpredict  = ggpredict(model2, terms = c("ind_economico[all]","nvl.acadmadre","genero"))
ggplot(model2ggpredict, aes(x, predicted, colour = group)) + geom_line() + facet_wrap(~ facet) + theme_blank()
#ggsave(filename="efectoMarginalEnelpromedio2.png", width=10, height=2*3, dpi=300)

model3ggpredict  = ggpredict(model3, terms = c("ind_economico[all]","nvl.acadmadre","genero"))
ggplot(model3ggpredict, aes(x, predicted, colour = group)) + geom_line() + facet_wrap(~ facet) + theme_blank()
#ggsave(filename="efectoMarginalEnelpromedio3.png", width=10, height=2*3, dpi=300)

#Efectos marginales promedio
plot_model(model, type = "eff", terms = c("ind_economico","nvl.acadmadre","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio1Eff.png", width=10, height=2*3, dpi=300)

plot_model(model, type = "pred", terms = c("ind_economico","nvl.acadmadre","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio1Pred.png", width=10, height=2*3, dpi=300)


plot_model(model2, type = "eff", terms = c("ind_economico","nvl.acadmadre","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio2Eff.png", width=10, height=2*3, dpi=300)

plot_model(model2, type = "pred", terms = c("ind_economico","nvl.acadmadre","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio2Pred.png", width=10, height=2*3, dpi=300)

plot_model(model3, type = "eff", terms = c("ind_economico","nvl.acadmadre","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio3Eff.png", width=10, height=2*3, dpi=300)

plot_model(model3, type = "pred", terms = c("ind_economico","nvl.acadmadre","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio3Pred.png", width=10, height=2*3, dpi=300)



```


```{r}
#efecto marginales en el promedio
model1ggpredict  = ggpredict(model, terms = c("ind_economico[all]","status.migra","genero"))
ggplot(model1ggpredict, aes(x, predicted, colour = group)) + geom_line() + facet_wrap(~ facet) + theme_blank()
#ggsave(filename="efectoMarginalEnelpromedio1Migra.png", width=10, height=2*3, dpi=300)


model2ggpredict  = ggpredict(model2, terms = c("ind_economico[all]","status.migra","genero"))
ggplot(model2ggpredict, aes(x, predicted, colour = group)) + geom_line() + facet_wrap(~ facet) + theme_blank()
#ggsave(filename="efectoMarginalEnelpromedio2Migra.png", width=10, height=2*3, dpi=300)


model3ggpredict  = ggpredict(model3, terms = c("ind_economico[all]","status.migra","genero"))
ggplot(model3ggpredict, aes(x, predicted, colour = group)) + geom_line() + facet_wrap(~ facet) + theme_blank()
#ggsave(filename="efectoMarginalEnelpromedio3Migra.png", width=10, height=2*3, dpi=300)


#Efectos marginales promedio
plot_model(model, type = "eff", terms = c("ind_economico","status.migra","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio1EffMigra.png", width=10, height=2*3, dpi=300)

plot_model(model, type = "pred", terms = c("ind_economico","status.migra","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio1PredMigra.png", width=10, height=2*3, dpi=300)

plot_model(model2, type = "eff", terms = c("ind_economico","status.migra","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio2EffMigra.png", width=10, height=2*3, dpi=300)

plot_model(model2, type = "pred", terms = c("ind_economico","status.migra","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio2PredMigra.png", width=10, height=2*3, dpi=300)

plot_model(model3, type = "eff", terms = c("ind_economico","status.migra","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio3EffMigra.png", width=10, height=2*3, dpi=300)

plot_model(model3, type = "pred", terms = c("ind_economico","status.migra","genero")) + theme_blank()
#ggsave(filename="efectoMarginalPromedio3PredMigra.png", width=10, height=2*3, dpi=300)


```



```{r}

valores.log = unlist(valores.log)
valores.log = as.data.frame(valores.log)
colnames(valores.log) = "PV1MATH"


ggplot(valores.log, aes(PV1MATH)) + geom_histogram(aes(y = ..density..),colour = "black", fill = "white") + xlab("PV1MATH") +
  geom_density(kernel = "gaussian", alpha=.3, fill="blue4") +  geom_vline(data=valores.log, aes(xintercept= quantile(PV1MATH,0.5992952)), linetype="dashed", size=0.8, colour="red") + geom_vline(data=valores.log, aes(xintercept = quantile(PV1MATH, 0.8326872)), linetype="dashed", size=0.8, colour="red") + geom_vline(data=valores.log, aes(xintercept = quantile(PV1MATH, 0.9530396)), linetype="dashed", size=0.8, colour="red") + theme_blank() + labs(x = "calificaciones (PV1MATH)", y = "densidad") + geom_text(aes(x=420, label="notas 1", y=0.006, angle = 0), colour="black", text=element_text(size=11)) + geom_text(aes(x=482, label="notas 2", y=0.006, angle = 0), colour="black", text=element_text(size=11)) + geom_text(aes(x=545, label="notas 3", y=0.006, angle = 0), colour="black", text=element_text(size=11))


```


Gráficos de proporciones

```{r}

proporciones = data.frame(decil = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                          prop = c(0.6242,0.6939,0.7762,0.7562,0.8758,0.9335,
                                   0.9126,1.001,1.2802,1.4835))

ggplot(proporciones, aes(x = decil, y = prop)) + geom_line(color = "black", size = 1.5) + theme_minimal() +
  labs(x = "Decil", y = "Proporción") + geom_vline(data = proporciones, aes(xintercept = decil), color = "red",
                                                   linetype = "dashed", size = 1)

#ggsave(filename="gráficoProporciones.png", width=10, height=2*3, dpi=300)

                                                   
```




