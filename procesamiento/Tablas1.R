## Tablas descriptivas y univariadas
rm(list=ls())
options(scipen=999)

#Librerias
pacman::p_load(sjlabelled,
               dplyr,
               stargazer,
               sjmisc,
               summarytools,
               kableExtra,
               sjPlot,
               corrplot,
               sessioninfo,
               ggplot2)
#Cargar base de datos
load(file = "data/latinobarometro_proc.RData")

#No trabajaremos con los datos perdidos, asi que los vamos a eliminar.
proc_data_original <-proc_data
dim(proc_data)
sum(is.na(proc_data))
proc_data <-na.omit(proc_data)
dim(proc_data)

#Con la nueva base con los na borrados, solo falta copiar las etiquetas.
proc_data <-sjlabelled::copy_labels(proc_data,proc_data_original)

#Código no está funcionando para copiar etiquetas, pues la BBDD no se está guardando con ellas
#asi que aquí va una sección con el cambio de etiquetas:
####################
proc_data$ns_demo <- set_label(x = proc_data$ns_demo,label = "Satisfacción Democracia")
get_label(proc_data$ns_demo)                                  

proc_data$demo_auto <- set_label(x = proc_data$demo_auto,label = "Pref. Gob. Democrático/Autoritario")
get_label(proc_data$demo_auto)

proc_data$na_demo <- set_label(x = proc_data$na_demo,label = "NA: Democracia mejor sistema de Gob.")         
get_label(proc_data$na_demo)

proc_data$na_nodemo <-set_label(x = proc_data$na_nodemo,label = "NA: Gob. no democrático llegara a resolver problemas")
get_label(proc_data$na_nodemo)

proc_data$na_spdemo <-set_label(x = proc_data$na_spdemo,label = "NA: Democracia permite solución problemas")
get_label(proc_data$na_spdemo)

proc_data$apoyo_gm <-set_label(x = proc_data$apoyo_gm,label = "Apoyo a gobierno militar")
get_label(proc_data$apoyo_gm)

proc_data$izq_der <-set_label(x = proc_data$izq_der, label = "Posicionamiento político izq-der")
get_label(proc_data$izq_der)

proc_data$cnf_pers <-set_label(x = proc_data$cnf_pers, label = "Confianza en personas")
get_label(proc_data$cnf_pers)

proc_data$cnf_FA <- set_label(x = proc_data$cnf_FA, label = "Confianza Fuerzas Armadas")
get_label(proc_data$cnf_FA)

proc_data$cnf_FP <- set_label(x = proc_data$cnf_FP, label = "Confianza Fuerzas Policiales")
get_label(proc_data$cnf_FP)

proc_data$cnf_PJ <- set_label(x = proc_data$cnf_PJ, label = "Confianza Poder Judicial")
get_label(proc_data$cnf_PJ)

proc_data$SEP <- set_label(x = proc_data$SEP, label = "Situación Económica País")
get_label(proc_data$SEP)

proc_data$PSEP <- set_label(x = proc_data$PSEP, label = "Sit. Eco. M/P que pasado")
get_label(proc_data$PSEP)

proc_data$FMSP <- set_label(x = proc_data$FMSP, label = "Sit. Eco. M/P que futuro")
get_label(proc_data$FMSP)

proc_data$SPF <- set_label(x = proc_data$SPF, label = "Sit. Eco. Personal y familiar")
get_label(proc_data$SPF)

proc_data$sexo <- set_label(x = proc_data$sexo, label = "Sexo")
proc_data$edad <- set_label(x = proc_data$edad, label = "Edad")
proc_data$val_demo <- set_label(x = proc_data$val_demo, label = "Valoración Democracia")

####################

#Tablas descriptivas de variables (sección metodológica)

sjmisc::descr(proc_data,
              show =c("label","range","mean","sd","NA.prc","n"))%>%
              kable(.,"markdown")
summarytools::dfSummary(proc_data, plain.ascii = FALSE)

##Tablas univariadas
#Valoración democracia (como sistema de gobierno)
ggplot()
ggplot(proc_data, aes(x = val_demo)) +
  geom_bar()
proc_data %>% ggplot(aes(x = val_demo)) +
  geom_bar(fill = "blue")+
  labs(title = "Valoración Democracia",
       x = "Valoración Democracia",
       y = "Frecuencia")
#Situación económica país..
ggplot()
ggplot(proc_data, aes(x = SEP)) +
  geom_bar()
proc_data %>% ggplot(aes (x = SEP)) +
  geom_bar(fill = "red")+
  labs(title = "Situación Económica País",
       x = "Situación Económica País",
       y = "Frecuencia")

