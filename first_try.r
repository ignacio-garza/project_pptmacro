#esta primera parte es para instalar los paquetes
#install.packages("Rblpapi")
#install.packages("dplyr")
#install.packages("ggplot2")

Sys.setlocale("LC_TIME", "es_ES.UTF-8") #Cambia idioma de las fechas.

#Cargar las librerías al ambiente global
library(Rblpapi) # API de Bloomberg
library(tidyr) # long <-> wide
library(dplyr) # Manipular los datos
library(ggplot2) # Gráficas
library(scales)
library(lubridate) #manejo de fechas

con <- blpConnect() 	# automatic if option("blpAutoConnect") is TRUE

start <- Sys.Date() %m-% months(18) #fecha de inicio para el query

oil <- bdh(securities = c("USCRWTIC Index", "CRAMMMIX Index", "EUCRBRDT Index"),
               fields = "PX_LAST",
               start.date = start,
               include.non.trading.days = FALSE)

glimpse(oil)

# Por borrar
library(readr)
oil <- read_csv("~/Downloads/oil_df.csv") %>%
        mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y"))
######

sh_theme <- theme_classic() +
  theme(axis.text.x = element_text(angle = 75),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "Montserrat"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = c(.9, .5),
        legend.background = element_blank())

## Gráfica de los precios de Brent, MME, WTI
oil_long <- oil %>%
        pivot_longer(-Fecha, names_to = "Mezcla", values_to = "Precio")

prices1 <- ggplot(oil_long, aes(Fecha, Precio, color = Mezcla)) +
            geom_line() +
            geom_hline(yintercept = 0) +
            scale_color_manual(values = c("#B38E5D", "#13322B", "#9D2449")) +
            scale_x_date(labels = date_format("%b-%Y"),
                date_breaks = "1 month") +
            scale_y_continuous(limits = c(-40, 80),
                breaks = seq(-40, 80, by = 20)) +
            labs(title = "PRECIOS DEL PETRÓLEO",
                 subtitle = "(Dólares por barril)",
                 caption = "Fuente: Bloomberg") +
            sh_theme

ggsave("precios.png", width = 8, height = 4.5)

## Gráfica de los diferenciales de precios
oil_diff <- oil %>% mutate(`Brent - MME` = Brent - MME,
        `WTI - MME` = WTI - MME) %>%
        select(Fecha, `Brent - MME`, `WTI - MME`) %>%
        pivot_longer(-Fecha, names_to = "Diferencial", values_to = "Precio")

diff1 <- ggplot(oil_diff, aes(Fecha, Precio, color = Diferencial)) +
        geom_line() +
        geom_hline(yintercept = 0) +
        scale_color_manual(values = c("#B38E5D", "#13322B", "#9D2449")) +
        scale_x_date(labels = date_format("%b-%Y"),
                date_breaks = "1 month") +
        scale_y_continuous(limits = c(-20, 30),
                breaks = seq(-20, 30, by = 20)) +
        labs(title = "DIFERENCIAL ENTRE DISTINTOS CRUDOS \n Y LA MEZCLA MEXICANA",
                subtitle = "(Dólares por Barril)",
                caption = "Fuente: Bloomberg") +
        sh_theme +
        theme(legend.position = c(.9, .7))

ggsave("diffs.png", width = 8, height = 4.5)