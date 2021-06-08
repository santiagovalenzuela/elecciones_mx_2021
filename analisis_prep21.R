rm(list=ls(all=T))

library(tidyverse)

setwd("~/prep_2021")

# Cargamos los datos de votos por sección de la votación anterior
dips18 <-read.csv("data/2018_SEE_DIP_FED_MR_NAL_SEC.csv")

# Calculamos el porcentaje de voto de MORENA en  2018
dips18$votos_MORENA <- 0
dips18$votos_MORENA <- dips18$MORENA + round(dips18$PT_MORENA_ES/3) +
  round(dips18$PT_MORENA/2) + round(dips18$MORENA_ES/2)

dips18$pc_MORENA18 <- 0
dips18$pc_MORENA18 <- dips18$votos_MORENA/dips18$TOTAL_VOTOS

dips18$pc_participacion18 <- 0
dips18$pc_participacion18 <- dips18$TOTAL_VOTOS/dips18$LISTA_NOMINAL

##############################################################################


# Cargamos los datos del PREP
archivo_prep <- "data/20210606_2240_PREP_Diputaciones/Diputaciones_2021.csv" #Cambiar línea

partidos <- c("PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "RSP", "FXM", "PES")

c_VXM <- c("PAN.PRI.PRD", "PAN.PRI", "PAN.PRD", "PRI.PRD")

c_JHH <- c("PVEM.PT.MORENA", "PT.MORENA", "PVEM.MORENA", "PVEM.PT")

hora_prep <- readLines(con = archivo_prep, n = 3)[2]

nombres_resumen <- readLines(con = archivo_prep, n = 5)[3] %>%
  strsplit(split = "|", fixed = T) %>%
  unlist()

resumen_prep <- readLines(con = archivo_prep, n = 5)[4] %>%
  strsplit(split = "|", fixed = T) %>%
  unlist() %>%
  as_tibble_row(.name_repair = ~ nombres_resumen)

prep21 <-read.csv(file = archivo_prep,
                  header= T,
                  sep = ",",
                  skip = 5)

# Filtramos por actas contabilizadas
prep21 <- prep21 %>% filter(CONTABILIZADA ==1)

# Excluimos del análisis las casillas especiales y las que contienen el 
# Voto de las personas en prisión preventiva
prep21 <- prep21 %>%
  filter(!(TIPO_CASILLA == "S" | TIPO_CASILLA == "V"))

#Cambiamos el tipo de las columnas de voto a númerico

prep21[partidos] <- lapply(prep21[partidos], as.numeric)
prep21[c_VXM] <- lapply(prep21[c_VXM], as.numeric)
prep21[c_JHH] <- lapply(prep21[c_JHH], as.numeric)

prep21$TOTAL_VOTOS_CALCULADO <- as.numeric(prep21$TOTAL_VOTOS_CALCULADO)
prep21$LISTA_NOMINAL <- as.numeric(prep21$LISTA_NOMINAL)


# Obtenemos participación electoral

participacion21 <- 
prep21 %>%
  group_by(ID_ESTADO,SECCION) %>%
  summarise( total_seccional = sum(TOTAL_VOTOS_CALCULADO, na.rm = T),
             lista_nom_sec = sum(LISTA_NOMINAL, na.rm = T)) %>%
  mutate(pc_part_21 = total_seccional/lista_nom_sec)

participacion18 <- dips18 %>%
  select(ID_ESTADO, SECCION, pc_participacion18)

df_part <-inner_join(participacion18, participacion21, by=c("ID_ESTADO", "SECCION"))

# Obtenemos porcentaje de votos a Morena

prep21 <- prep21 %>%
  rowwise() %>%
  mutate(votos_morena_prep  = sum(MORENA,
                               round(PVEM.PT.MORENA/3),
                               round(PVEM.MORENA/2),
                               round(PT.MORENA/2), na.rm = T))

morena18 <- dips18 %>%
  select(ID_ESTADO, SECCION, pc_MORENA18)

morena21 <- prep21 %>%
  group_by(ID_ESTADO,SECCION) %>%
  summarise( votos_MORENA_sec = sum(votos_morena_prep, na.rm = T),
             total_seccional = sum(TOTAL_VOTOS_CALCULADO, na.rm = T)) %>%
  mutate(pc_morena21 = votos_MORENA_sec/total_seccional)

df_morena <-inner_join(morena18, morena21, by=c("ID_ESTADO", "SECCION"))

df_participacion18 <- inner_join(morena18, participacion18,  by=c("ID_ESTADO", "SECCION"))

df_participacion21 <- inner_join(morena21, participacion21,  by=c("ID_ESTADO", "SECCION"))

# Hacemos una regresión rápida para checar la tendencia entre el voto a Morena por 
# sección electoral en 2018 y en 2021

reg_mor <- lm(pc_morena21 ~ pc_MORENA18,
          data = df_morena) 

# Hacemos las gráficas
df_morena %>%
  ggplot(aes(pc_MORENA18, pc_morena21)) +
  geom_point(alpha = 0.1,
             color = "#c0311a") +
  
  geom_abline(intercept = 0, slope = 1, #Identidad
              size = 1,
              color = "black") +
  
  geom_abline(intercept = reg_mor$coefficients[1],
              slope = reg_mor$coefficients[2],
              linetype="dashed",
              size = 1,
              color = "#1aaac0") +
  
  labs(title= "Porcentaje de voto por sección a Morena",
       subtitle = "Cada punto representa una sección electoral. La línea negra es la línea de identidad.\nLa línea azul muestra la tendencia de la votación.",
       caption = paste0("Fuente: Elaboración propia con datos del PREP\nCorte:", hora_prep, "\n@santivalenz"),
       x = "Porcentaje de voto para diputaciones\n federales a Morena en 2018",
       y = "Porcentaje de voto para diputaciones\n federales a Morena en 2021") +
  
  theme_classic() +
  
  theme(
        plot.title = element_text(hjust = 0, face = "bold", color= "#c0311a"), ##347B98
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0))


#############################
# Plot 2: participacion y voto a Morena

lm(formula =  pc_morena21 ~ pc_part_21,
   data= df_participacion21) %>% summary()

df_participacion21 %>%
  ggplot(aes(pc_morena21, pc_part_21)) + 
           geom_point(alpha = 0.1,
                      color = "#c0311a")

df_participacion18 %>%
  ggplot(aes(pc_MORENA18, pc_participacion18)) + 
  geom_point(alpha = 0.1,
             color = "#c0311a")


#############################
