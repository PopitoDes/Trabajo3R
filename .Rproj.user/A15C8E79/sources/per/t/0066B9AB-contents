# Trabajo 3.
## Preparación de los datos:
rm(list = ls())
options(scipen=999)
pacman::p_load(tidyverse, car, sjmisc, sjlabelled, stargazer, haven)

## Abrir base de datos
load(file = "input/Latinobarometro.RData")
load(file = "C:/Users/UAH/Desktop/bbdd/Latinobarometro.RData")
data <- Latinobarometro_2023_Esp_v1_0

dim(data)
# 19205 observaciones de 274 variables.
View(data)
## Procesamiento de variables.
# Seleccionar variables que vamos a utilizar:
#Voy a seleccionar variables orientadas a visualizar la percepción hacia la democracia dentro de cada país, además de variables independientes
#Sexo, pais, edad y posicionamiento político.

# 1) P11STGBS.A Satisfacción democracia.
# 2) P10STGBS Preferencia en democracia
# 3) P18 A,b,D,I Democracia
# 4) P20STM Apoyar o no gobierno militar
# 5) P16ST En posición politica derecha/izquierda, donde se posiciona la gente.

data_proc <- data %>%
  select(P11STGBS.A, #Nivel satisfacción con la democracia en el país.
         P10STGBS, #Preferencia democracia o gobierno autoritario.
         P18ST.A, #La democracia es el mejor sistema de gobierno
         P18STM.B, #No importa si un gobierno no democrático llega si soluciona problemas
         P18N.D, #Para que la democracia funcione hay que garantizar variedad de fuentes de información de medios de comunicación.
         P18ST.I, #La democracia permite que se solucionen los problemas que tenemos.
         P20STM, #Apoyar o no gobierno militar.
         P16ST, #Posicionamiento izquierda/dereccha.
         idenpa, #País de identificación
         P9STGBS, #Confianza Personas
         P13STGBS.A, #Confianza Fuerzas armadas
         P13STGBS.B, #Confianza Carabineros
         P13ST.F, #Confianza Poder Judicial
         P5STGBS, #Situacion Económica país
         P6STGBS, #Situacion mejor/peor que hace 12 meses
         P7ST, # ¿Será mejor la situación país?
         P8STGBS, #Situación Económica familia/propia
         sexo,
         edad)
         

#Filtrar data para considerar únicamente la de Chile:
data_proc <- data_proc %>% dplyr::filter(idenpa==152)

## Etiquetas:
sjlabelled::get_label(data_proc)

proc_data <- data_proc %>% rename("ns_demo"=P11STGBS.A, #nivel satisfacción democracia
                                  "demo_auto"=P10STGBS, #Democracia/autoritarismo
                                  "na_demo"=P18ST.A, #nivel de acuerdo: democracia como mejor sistema de gobierno
                                  "na_nodemo"=P18STM.B, #nivel de acuerdo: gobierno no democrático solucione problemas
                                  "na_vinfo"=P18N.D, #Nivel de acuerdo: Para que funcione democracia hay que garantizar variedad de fuentes de información.
                                  "na_spdemo"=P18ST.I, #Nivel de acuerdo: Democracia permite que se solucionen problemas que tenemos.
                                  "apoyo_gm"=P20STM, #Apoyo a un gobierno militar. Variable dicotomica
                                  "izq_der"=P16ST, #Posicionamiento izquierda-derecha.
                                  "cnf_pers"=P9STGBS, #Confianza personas.
                                  "cnf_FA"=P13STGBS.A, #Confianza Fuerzas Armadas
                                  "cnf_FP"=P13STGBS.B, #Confianza Fuerzas Policiales
                                  "cnf_PJ"=P13ST.F, #Confianza Poder Judicial
                                  "SEP"=P5STGBS, #Situación económica país
                                  "PSEP"=P6STGBS, #Variación situación económica país mejor que antes
                                  "FMSP"=P7ST, #Variación situación económica país mejor que después
                                  "SPF"=P8STGBS) #Situación económica personal y familiar
                                  
                                  
#Una vez cambiados los nombres, cambiamos las etiquetas.
proc_data$ns_demo <- set_label(x = proc_data$ns_demo,label = "Satisfacción Democracia")
get_label(proc_data$ns_demo)                                  

proc_data$demo_auto <- set_label(x = proc_data$demo_auto,label = "Pref. Gob. Democrático/Autoritario")
get_label(proc_data$demo_auto)

proc_data$na_demo <- set_label(x = proc_data$na_demo,label = "NA: Democracia mejor sistema de Gob.")         
get_label(proc_data$na_demo)

proc_data$na_nodemo <-set_label(x = proc_data$na_nodemo,label = "NA: Gob. no democrático llegara a resolver problemas")
get_label(proc_data$na_nodemo)

proc_data$na_vinfo <-set_label(x = proc_data$na_vinfo,label = "NA: Variedad de Fuentes de Información")
get_label(proc_data$na_vinfo)

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

## Recodificación: eliminar NA.
#Eliminar NA en Satisfacción democracia.

frq(proc_data$ns_demo)

proc_data$ns_demo <- recode(proc_data$ns_demo, "c(-2,-1)=NA")

frq(proc_data$ns_demo)

#Eliminar NA en Pref Gob. Democrático/Autoritario.
frq(proc_data$demo_auto)

proc_data$demo_auto <- recode(proc_data$demo_auto, "c(-2,-1)=NA")

frq(proc_data$demo_auto)

#Eliminar NA en nivel de acuerdo: democracia mejor sistema de gobierno.

frq(proc_data$na_demo)

proc_data$na_demo <- recode(proc_data$na_demo, "c(-5)=NA")

frq(proc_data$na_demo)

#Eliminar NA en nivel de acuerdo: gobierno no democratico llegara a resolver problemas.

frq(proc_data$na_nodemo)

proc_data$na_nodemo <- recode(proc_data$na_nodemo, "c(-5)=NA")

frq(proc_data$na_nodemo)                              

#Eliminar Na en nivel de acuerdo: variedad de fuentes de información.

frq(proc_data$na_vinfo)

proc_data$na_vinfo <- recode(proc_data$na_vinfo, "c(-5)=NA")

frq(proc_data$na_vinfo)

#Eliminar NA en nivel de acuerdo: Democracia permite solución de problemas.

frq(proc_data$na_spdemo)

proc_data$na_spdemo <-recode(proc_data$na_spdemo, "c(-5)=NA")

frq(proc_data$na_spdemo)

#Eliminar NA en Apoyo a Gobierno Militar.

frq(proc_data$apoyo_gm)

proc_data$apoyo_gm <-recode(proc_data$apoyo_gm, "c(-2,-1)=NA")

frq(proc_data$apoyo_gm)

#Eliminar NA en posicionamiento político izquierda-derecha.

frq(proc_data$izq_der)

proc_data$izq_der <-recode(proc_data$izq_der, "c(-2,-1,97)=NA")

frq(proc_data$izq_der)

#Eliminar NA en confianza en personas

frq(proc_data$cnf_pers)

proc_data$cnf_pers <-recode(proc_data$cnf_pers, "c(-5)=NA")

frq(proc_data$cnf_pers)

#Eliminar NA en confianza en fuerzas armadas

frq(proc_data$cnf_FA)

proc_data$cnf_FA <-recode(proc_data$cnf_FA, "c(-2,-1)=NA")

frq(proc_data$cnf_FA)
#Eliminar NA en confianza en fuerzas policiales

frq(proc_data$cnf_FP)

proc_data$cnf_FP <-recode(proc_data$cnf_FP, "c(-2,-1)=NA")

frq(proc_data$cnf_FP)

#Eliminar NA en confianza en poder judicial 

frq(proc_data$cnf_PJ)

proc_data$cnf_PJ <-recode(proc_data$cnf_PJ, "c(-2,-1)=NA")

frq(proc_data$cnf_PJ)

#Eliminar NA en situación económica país

frq(proc_data$SEP)

proc_data$SEP <-recode(proc_data$SEP, "-2=NA")

frq(proc_data$SEP)

#Eliminar NA en Sit. Eco. M/P que pasado

frq(proc_data$PSEP)

proc_data$PSEP <-recode(proc_data$PSEP, "c(-2,-1)=NA")

frq(proc_data$PSEP)
#Eliminar NA en Sit. Eco M/P que futuro

frq(proc_data$FMSP)

proc_data$FMSP <-recode(proc_data$FMSP, "c(-2,-1)=NA")

frq(proc_data$FMSP)
#Eliminar NA en Sit. Eco personal y familiar

frq(proc_data$SPF)

proc_data$SPF <-recode(proc_data$SPF, "c(-2,-1)=NA")

frq(proc_data$SPF)

#Vamos a generar un nuevo item a partir de las variables: na_demo, na_nodemo y na_spdemo, que corresponden a formas de valoración de la
#democracia. Para ello, primero debemos tener todas las variables en un mismo sentido. Como el código para generar el nuevo item las
#"suma", lo más lógico es que las variables queden en que la mejor valoración sea 12, pues hay que hacer que el sentido de las variables
#sea que mayor valoración sea 4, y menor sea 1, para cada variable.

proc_data$na_demo <- recode(proc_data$na_demo, "1=4; 2=3; 3=2; 4=1")
proc_data$na_spdemo <- recode(proc_data$na_spdemo, "1=4; 2=3; 3=2; 4=1")

proc_data$val_demo <- (proc_data$na_demo+proc_data$na_nodemo+proc_data$na_spdemo)
frq(proc_data$val_demo)

#Recodificar la variable juntando valores:

proc_data$val_demo <- car::recode(proc_data$val_demo, "c(3,4,5)=1; c(6,7)=2; c(8,9)=3; c(10,11,12)=4;")
frq(proc_data$val_demo)

#Con la nueva variable lista, hay que cambiar la etiqueta

get_label(proc_data$val_demo)

proc_data$val_demo <-set_label(x = proc_data$val_demo,label ="Valoración Democracia")

get_label(proc_data$val_demo)

#Cambiar etiquetas.

proc_data$val_demo <- set_labels(proc_data$val_demo,
                                labels=c( "Muy Mala"=1,
                                           "Mala"=2,
                                           "Buena"=3,
                                           "Muy buena"=4))
summary(proc_data$val_demo)
frq(proc_data$val_demo)

#Recodificar para que valoraciones positivas sean los valores mayores.

frq(proc_data$SEP)
proc_data$SEP <-car::recode(proc_data$SEP, "1=5; 2=4;3=3;4=2;5=1")
frq(proc_data$SEP)

frq(proc_data$PSEP)
proc_data$PSEP <- car::recode(proc_data$PSEP, "1=5; 2=4;3=3;4=2;5=1")
frq(proc_data$PSEP)

frq(proc_data$FMSP)
proc_data$FMSP <- car::recode(proc_data$FMSP, "1=5; 2=4;3=3;4=2;5=1")
frq(proc_data$FMSP)

frq(proc_data$SPF)
proc_data$SPF <- car::recode(proc_data$SPF, "1=5; 2=4;3=3;4=2;5=1")
frq(proc_data$SPF)

frq(proc_data$cnf_pers)
proc_data$cnf_pers <-car::recode(proc_data$cnf_pers, "1=2;2=1")
frq(proc_data$cnf_pers)

frq(proc_data$cnf_FA)
proc_data$cnf_FA <-car::recode(proc_data$cnf_FA, "1=4;2=3;3=2;4=1")
frq(proc_data$cnf_FA)

frq(proc_data$cnf_FP)
proc_data$cnf_FP <-car::recode(proc_data$cnf_FP, "4=1;3=2;2=3;1=4")
frq(proc_data$cnf_FP)

frq(proc_data$cnf_PJ)
proc_data$cnf_PJ <-car::recode(proc_data$cnf_PJ, "4=1;3=2;2=3;1=4")
frq(proc_data$cnf_PJ)


##variables edad, sexo e identificación de país.

frq(proc_data$sexo)

#Recodoficamos por convención general donde mujer= 1 hombre=0

proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")
frq(proc_data$sexo)

frq(proc_data$sexo)
get_label(proc_data$sexo)

proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")

get_label(proc_data$sexo)

frq(proc_data$sexo)

proc_data$sexo <- set_labels(proc_data$sexo,
                                 labels=c("Mujer"=1,
                                          "Hombre"=0))
frq(proc_data$sexo)

frq(proc_data$edad)

#Recodificar edad no es necesario, pero cambiamos la etiqueta.
get_label(proc_data$edad)

proc_data$edad <- set_label(x = proc_data$edad,label ="Edad")

get_label(proc_data$edad)

#Guardar base de datos procesada.

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

#La variable "idenpa" genera problemas para calculos posteriormente, y es innceseario mantenerla
#asi que la eliminamos.

proc_data <- subset(proc_data, select = -c(idenpa))
stargazer(proc_data, type="text")

save(proc_data,file = "data/latinobarometro_proc.RData")

#####################################################################


