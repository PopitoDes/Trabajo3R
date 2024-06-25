#Trabajo 3

options(scipen=999)
rm(list=ls())
load(file ="data/latinobarometro_proc.RData")


pacman::p_load(sjlabelled,
               dplyr,
               stargazer,
               sjmisc,
               summarytools,
               kableExtra,
               sjPlot,
               corrplot,
               sessioninfo,
               ggplot2,
               psych)


#Eliminamos datos perdidos
proc_data_original <- proc_data
dim(proc_data)
sum(is.na(proc_data))
colSums(is.na(proc_data))
proc_data <-na.omit(proc_data)
dim(proc_data)

sjmisc::descr(proc_data,
              show = c("label","range","mean","sd","NA.prc","n")) %>%
              kable(.,"markdown")

#Copio Etiquetas:
proc_data <-sjlabelled::copy_labels(proc_data,proc_data_original)

#No está funcionando cambiar etiquetas así que aquí va una sección con las etiquetas:
##################
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


##################

#Tabla descriptiva
view(dfSummary(proc_data, headings=FALSE, graph.col = FALSE))

#Matriz de correlación

M <-cor(proc_data, use="complete.obs")
M
#En este caso, anteriormente la variable "Idenpa" generaba problemas al ser
#una variable con valor constante, tuve que eliminarla en el archivo de preparación.

#Matriz de correlación mas estético:

sjPlot::tab_corr(proc_data,
                 triangle = "lower")

#Voy a descartar las variables: na_demo, na_nodemo, na_spdemo, pues con ellas hice un nuevo item
#anteriormente, para que la información de la tabla sea más accesible.


proc_data <- subset(proc_data, select = -c(na_demo, na_nodemo, na_spdemo))

sjPlot::tab_corr(proc_data,
                 triangle = "lower")                                         

## Indice ponderado.
#Para realizar el índice ponderado, hay que cambiar todas las variables a numéricas.
indicadores2023 <- proc_data %>% mutate_all(~(as.numeric(.)))

# Luego, creamos una nueva variable para cada dimensión que contenga las medias de cada dimensión.
indicadores2023 = indicadores2023 %>%
  rowwise() %>%
  mutate(Eco = mean(c(SEP, PSEP, FMSP, SPF)),
         CNF = mean(c(cnf_pers, cnf_FA, cnf_FP, cnf_PJ)),
         VAL = mean(c(val_demo))) %>%
  ungroup()

# El indicador quedaría así:
indicadores2023 = indicadores2023 %>%
  rowwise() %>%
  mutate(ISN = mean(c(Eco, CNF, VAL))) %>%
  ungroup()

summary(indicadores2023$Eco)
summary(indicadores2023$CNF)
summary(indicadores2023$VAL)

#Observamos los primeros 10 casos del resultado:

indicadores2023 %>% select(ISN) %>% head(10)

indicadores2023 <- indicadores2023 %>% mutate(ISN = case_when(ISN>=0.33~"si",
                                                              ISN<0.33~"no")
                                              )
prop.table(table(indicadores2023$ISN))*100

#Construcción de escala.

data2 <- proc_data %>%
  select(SEP, PSEP, FMSP, SPF)

cor(data2)

#Alfa de Chronbach.

psych::alpha(data2)



#Recodifcamos datos para la escala:

frq(data2)

data2$SEP <-car::recode(data2$SEP, "1=0;2=1;3=2;4=3;5=4")
data2$PSEP <-car::recode(data2$PSEP, "1=0;2=1;3=2;4=3;5=4")
data2$FMSP <-car::recode(data2$FMSP, "1=0;2=1;3=2;4=3;5=4")
data2$SPF <-car::recode(data2$SPF, "1=0;2=1;3=2;4=3;5=4")                                  

frq(data2)

data2 <- data2 %>%
  rowwise() %>%
  mutate(DECO = sum(SEP, PSEP, FMSP, SPF))
summary(data2$DECO)

# Guardar base de datos:

save(proc_data, file = "data/data2.RData")

