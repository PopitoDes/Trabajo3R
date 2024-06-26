#Trabajo 4
#Carga y selección de variables:

rm(list = ls())
load(file ="data/data2.RData")

#Cargar paquetes:
pacman::p_load(dplyr, 
               car, sjmisc, 
               sjPlot, sjlabelled, 
               stargazer, 
               kableExtra,
               corrplot, 
               texreg, 
               ggplot2, 
               ggpubr)

#Eliminar datos perdidos
proc_data_original <- proc_data
dim(proc_data)
sum(is.na(proc_data))
colSums(is.na(proc_data))
proc_data <-na.omit(proc_data)
dim(proc_data)


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


# Regresiones

View(proc_data)
proc_data_2 <- subset(proc_data, select = c("izq_der", "val_demo","sexo", "edad","cnf_FA"))

proc_data_3 <- subset(proc_data, select = c("izq_der", "val_demo","sexo", "edad","cnf_FA", "cnf_FP", "SEP", "SPF"))

graph2 <- view(dfSummary(proc_data_3, headings= FALSE))

graph2
save(graph1)
graph1 <- ggplot(proc_data_2, aes (x = edad, y = val_demo)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Edad", y = "Valoración Democracia")
graph1
ggsave(graph1, file="output/graphs/graph1.png")

sjmisc::descr(proc_data_3,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

reg1 <- lm(val_demo ~ 1, data = proc_data_2)
stargazer(reg1, type="text")
# La constante de la variable Valoración democracia es 3.011.

# Modelos de regresión.


reg2 <- lm(val_demo ~ edad, data=proc_data_2)
reg3 <- lm(val_demo ~ sexo, data=proc_data_2)
reg4 <- lm(val_demo ~ izq_der, data=proc_data_2)
reg5 <- lm(cnf_FP ~ + sexo + izq_der, data=proc_data_3)
reg6 <- lm(SEP ~ sexo + izq_der, data=proc_data_3)

knitreg(list(reg6),
        custom.model.names = c("Modelo 1"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "Mujer <br> <i>(Ref. Hombre)</i>",
                              "Izq-Der"),
        caption = "Situación económica país",
        caption.above = TRUE)



knitreg(list(reg5),
       custom.model.names = c("Modelo 1"),
       custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
       custom.coef.names = c("Intercepto", 
                             "Mujer <br> <i>(Ref. Hombre)</i>",
                             "Izq-Der"),
       caption = "Confianza FP",
       caption.above = TRUE)

plot_model(reg6, 
           title = "", #quitar titulo
           show.values = TRUE, #mostrar valor de efectos
           dot.size = 3, #tamaño circulos
           line.size = 1, #tamaño CI
           value.size = 4, #tamaño valor efectoss
           spacing = 1, #espacio entre efectos
           vline.color = "red", # linea roja en punto neutro (0)
           axis.labels = rev(c("Mujer", 
                               "Izq-der")), #con rev porque automatico los tira en otro orden
           show.legend = FALSE) + # variables dependientes
  theme_bw()


                             
                             


plot_model(reg5, 
           title = "", #quitar titulo
           show.values = TRUE, #mostrar valor de efectos
           dot.size = 3, #tamaño circulos
           line.size = 1, #tamaño CI
           value.size = 4, #tamaño valor efectoss
           spacing = 1, #espacio entre efectos
           vline.color = "red", # linea roja en punto neutro (0)
           axis.labels = rev(c("Mujer", 
                               "Izq-der")), #con rev porque automatico los tira en otro orden
           show.legend = FALSE) + # variables dependientes
  theme_bw()
                               


knitreg(list(reg2, reg3, reg4),
        custo.model.names = c("Modelo 1",
                              "Modelo 2",
                              "Modelo 3"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto",
                              "Edad",
                              "Mujer (Ref. Hombre)",
                              "Posición política desde izq a der"),
        caption = "Valoración democracia",
        caption.above = TRUE)

knitreg(list(reg4),
custom.model.names = c("Modelo 1"),
custom.coef.names = c("Intercepto",
                      "Posición política"))

ggeffects::ggpredict(reg4, terms = c("izq_der")) %>%
  ggplot(aes(x=x, y=predicted)) +
  geom_bar(stat="identity", color="grey", fill="grey")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=.1) +
  labs(title="Posición política", x = "", y = "") +
  theme_bw() +
  scale_x_continuous(name = "",
                     breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("0","1", "2","3","4","5","6","7","8","9","10"))+
  scale_y_continuous(limits = c(0,16), 
                     breaks = seq(0,16, by = 1))

ggeffects::ggpredict(reg4, terms = c("")) %>%
  ggplot(aes(x=x, y=predicted)) +
  geom_bar(stat="identity", color="grey", fill="grey")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=.1) +
  labs(title="Posición política", x = "", y = "") +
  theme_bw() +
  scale_x_continuous(name = "",
                     breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                     labels = c("0","1", "2","3","4","5","6","7","8","9","10"))+
  scale_y_continuous(limits = c(0,16), 
                     breaks = seq(0,16, by = 1))
  
  
