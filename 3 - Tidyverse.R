#3 Manipulación de data.frames
#Autor: Pepekike

#00 Importación de paquetes####
library(tidyverse)
library(GGally)
library(scales)

#01 Lectura de archivo####
setwd("C:/Users/jose.acuna/Desktop/Semana i/PobrezaCostaRica(1)/PobrezaCostaRica")

dfPobreza = read_csv("train.csv")

#02 Explorar los datos####
View(head(dfPobreza,5))

dfPobreza %>% head(5) %>% View()

dfPobreza_Cuant = dfPobreza %>% select(age, v2a1, tamhog, escolari)

plot(dfPobreza_Cuant$age, dfPobreza_Cuant$v2a1)
plot(dfPobreza_Cuant$tamhog, dfPobreza_Cuant$v2a1)

dfPobreza_Cuant %>%
  ggplot(aes(x = tamhog,
             y = v2a1,
             col = escolari)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  labs(x = "Tamaño de hogar",
       y = "Pago por renta",
       col = "Años de escolaridad") +
  scale_y_continuous(labels = comma, trans = "log")

rm(list = ls())

#03 Competencia de casas####
setwd("C:/Users/jose.acuna/Desktop/Semana i/CompetenciaPrecios")

dfPrecios = read_csv("train.csv")

options(scipen = 999) #Quitar notación científica

#Interes principal: Precios de las casas
dfPrecios %>%
  ggplot(aes(x = SalePrice,
             fill = HouseStyle)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(labels = comma,
                     breaks = seq(0,800000,100000))

dfPrecios %>%
  ggplot(aes(x = GrLivArea)) +
  geom_histogram()

dfPrecios %>%
  ggplot(aes(x = GrLivArea,
             y = SalePrice)) +
  geom_point(aes(col = HouseStyle)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

#Manipulación y limpieza
summary(dfPrecios)

dfPrecios %>%
  filter(PoolQC != "NA") %>%
  View()

dfPrecios_Piscina = dfPrecios %>%
  mutate(TienePiscina = ifelse(PoolQC != "NA", "Si", "No"))

dfPrecios_Piscina %>%
  ggplot(aes(x = TienePiscina,
             y = SalePrice)) +
  geom_boxplot()

dfPrecios_Piscina %>%
  ggplot(aes(x = SaleType,
             y = SalePrice)) +
  geom_boxplot()

dfPrecios_Piscina %>%
  group_by(SaleType) %>%
  summarise(PrecioProm = mean(SalePrice),
            PrecioDesv = sd(SalePrice),
            PrecioMax = max(SalePrice)) %>%
  View()

dfPrecios_Piscina %>%
  mutate(Residencial = ifelse(MSZoning %in% c("RH","RL","RP","RM"), T, F)) %>%
  ggplot(aes(x = Residencial,
             y = SalePrice,
             fill = Residencial)) +
  geom_boxplot()

#04 Información municipal####
setwd("C:/Users/jose.acuna/Desktop/Semana i/Datos/Datos_Municipios")

rm(list = ls())

dfCatMun = read_csv("CatalogoMunicipios.csv", locale = locale(encoding = "latin1"))
dfMunEducacion = read_csv("MunicipioEducacion.csv", skip = 4)
dfMunSalud = read_csv("MunicipioSalud.csv", skip = 4)
dfMunClima = read_csv("CatalogoClimasMunicipio.csv")

dfMunEducacion_Filt = dfMunEducacion %>%
  filter(id_municipio > 0) %>%
  select(id_estado, id_municipio, p15ymas_analfabeta, p15ymas_secundaria, grado_escolar)

dfMunSalud_Filt = dfMunSalud %>%
  filter(id_municipio > 0) %>%
  select(id_estado, id_municipio, p_con_derecho_imss)

dfMunSaludEducacion = dfMunEducacion_Filt %>%
  left_join(dfMunSalud_Filt,
            by = c("id_estado","id_municipio"))

dfMunClimaSaludEducacion = dfMunSaludEducacion %>%
  inner_join(dfMunClima,
            by = c("id_estado" = "ID_Estado", "id_municipio" = "ID_Municipio"))
