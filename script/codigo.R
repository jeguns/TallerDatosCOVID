
# Lectura -----------------------------------------------------------------

library(readr)
read_csv2('./data/positivos_covid.csv') -> datos_positivos

read_delim('./data/pm21Julio2021.csv',delim="|") -> datos_pcr

read_csv2('./data/fallecidos_covid.csv') -> datos_fallecidoscovid

read_delim('./data/fallecidos_sinadef.csv',delim="|") -> datos_sinadef

read_csv('./data/vacunas_covid.csv') -> datos_vacunas
  
# Limpieza ----------------------------------------------------------------

library(dplyr)
library(lubridate)

datos_positivos %>% str
datos_positivos$DEPARTAMENTO %>% table
datos_positivos$METODODX %>% table
datos_positivos %>% 
  mutate(FECHA_CORTE = ymd(FECHA_CORTE),
         FECHA_RESULTADO = ymd(FECHA_RESULTADO),
         METODODX = recode(METODODX,
                           `CONFIRMADOS COVID-19 04.08.2021.xlsx/CONFIRMADO_ANTIGENO`="ANTIGENO",
                           `CONFIRMADOS COVID-19 04.08.2021.xlsx/CONFIRMADOS PR`="RAPIDA")) -> datos_positivos
datos_positivos %>% str

datos_pcr %>% str
datos_pcr$RESULTADO %>% table
datos_pcr %>% 
  filter(FECHATOMAMUESTRA>=as.Date("2020-03-05") | (FECHATOMAMUESTRA>=as.Date("2020-01-01") & RESULTADO == "NEGATIVO")) %>% 
  filter(FECHATOMAMUESTRA<=as.Date(today())) %>% 
  filter(RESULTADO %in% c("POSITIVO","NEGATIVO")) %>% 
  filter(!edad %in% c('*','NULL')) -> datos_pcr
datos_pcr %>% str

datos_fallecidoscovid %>% str
library(skimr)
datos_fallecidoscovid %>% skim
datos_fallecidoscovid %>% 
  mutate(FECHA_CORTE = ymd(FECHA_CORTE),
         FECHA_FALLECIMIENTO = ymd(FECHA_FALLECIMIENTO)) %>% 
  filter(EDAD_DECLARADA>=0)-> datos_fallecidoscovid
datos_fallecidoscovid %>% skim

datos_sinadef %>% str
datos_sinadef %>% 
  mutate(EDAD = parse_number(EDAD)) -> datos_sinadef
datos_sinadef %>% str

datos_vacunas %>% str
datos_vacunas %>% 
  mutate(FECHA_CORTE = ymd(FECHA_CORTE),
         FECHA_VACUNACION = ymd(FECHA_VACUNACION),
         DOSIS = recode(DOSIS, `1` = "Primera", `2` = "Segunda")) -> datos_vacunas
datos_vacunas %>% str

# Visualización -----------------------------------------------------------

library(ggplot2)

corte_positivos = datos_positivos$FECHA_CORTE[1]
corte_positivos = paste0(day(corte), "/", month(corte), "/", year(corte))

datos_positivos %>% 
  dplyr::filter(!is.na(SEXO)) %>% 
  group_by(SEXO) %>% #count()
  summarise(prop = n() / nrow(.) ) %>% 
  ggplot(aes(x = "", y = prop, fill = SEXO, label = paste0(round(prop*100,2),"%"))) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("khaki2","palegreen2"),
                    name   = "Sexo", 
                    labels = c("Femenino", "Masculino")) +
  #scale_fill_brewer(palette="Dark2")+
  #scale_fill_brewer(palette="Blues")+
  geom_text(size = 5, position = position_stack(vjust = 0.5)) +
  labs(x = "",
       y = "",
       title = "Distribución de casos confirmados de COVID-19 por sexo",
       subtitle = paste0("Datos al ", corte_positivos),
       caption = "Fuente: MINSA") +
  theme(plot.title  = element_text(size = 12, face = "bold", hjust = 0.5),
        panel.background = element_rect(fill="white", color="white"),
        axis.text.x=element_blank()) -> grafico01

ggsave('./export/grafico01.png',grafico01)

datos_positivos %>%
  dplyr::filter(!is.na(FECHA_RESULTADO)) %>% 
  group_by(FECHA_RESULTADO) %>% 
  count() %>% 
  as.data.frame() %>% 
  dplyr::mutate(suma = cumsum(n)) %>% 
  ggplot(aes(x = FECHA_RESULTADO, y = suma)) +
  geom_line(stat = "identity", colour = "darkblue")+
  geom_point(stat = "identity", colour = "darkblue")+
  scale_x_date(limits      = c(min(datos_positivos$FECHA_RESULTADO),
                               max(datos_positivos$FECHA_RESULTADO)),
               date_breaks = "30 days",
               date_labels = "%d %b %y") +
  scale_y_continuous(trans = "log10",
                     labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "Fecha",
       y = "Log(Número de casos)",
       title = "Casos confirmados acumulados de COVID-19 a nivel nacional",
       subtitle = paste0("Datos al ", corte_positivos),
       caption = "Fuente: MINSA - Elaboración: Jesús Gamboa") +
  theme(plot.title  = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 8, angle = 30, hjust = 1),
        axis.title  = element_text(size = 10),
        plot.caption     = element_text(size = 7),
        panel.background = element_rect(fill="white", color="white"),
        panel.grid.major = element_line(color="gray60",size=0.5, linetype="dotted")) -> grafico02
  
ggsave('./export/grafico02.png',grafico02,width = 30,height = 15, units="cm")


fecha_pcr = datos_pcr$FECHATOMAMUESTRA %>% max
fecha_pcr = paste0(day(fecha_pcr), "/", month(fecha_pcr), "/", year(fecha_pcr))


library(zoo)
library(tidyr)
datos_pcr %>% 
  select(FECHATOMAMUESTRA,RESULTADO) %>% 
  mutate(MES = as.yearmon(FECHATOMAMUESTRA)) %>% 
  count(MES,RESULTADO) %>% 
  pivot_wider(names_from = RESULTADO, values_from = n) %>% 
  replace_na(list(POSITIVO=0)) %>% 
  mutate(POSITIVIDAD = POSITIVO/(POSITIVO+NEGATIVO)*100) %>% 
  ggplot(aes(x=MES,y=POSITIVIDAD,label=paste0(round(POSITIVIDAD,1),"%")))+
  geom_bar(stat="identity",fill="turquoise")+
  geom_text(position = position_stack(vjust=0.5), size = 4.25) +
  labs(X = "Mes",
       y = "%",
       title    = "Positividad mensual de pruebas moleculares",
       subtitle = paste0("Datos al ", fecha_pcr)) + 
  theme_minimal()-> grafico03

ggsave('./export/grafico03.png',grafico03,width = 30,height = 15, units="cm")


corte_vacunas = datos_vacunas$FECHA_CORTE[1]
corte_vacunas = paste0(day(corte_vacunas),"/",month(corte_vacunas),"/",year(corte_vacunas))

library(scales)

datos_vacunas %>% 
  replace_na(list(FABRICANTE = "NO ESPECIFICADO")) %>% 
  count(FABRICANTE,DOSIS) %>% 
  ggplot(aes(x=FABRICANTE,y=n,fill=DOSIS,label=n))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("brown1","mediumseagreen"))+
  geom_text(aes(label=ifelse(n>500,n," ")),position = position_stack(vjust=0.5))+
  scale_y_continuous(labels = NULL) + 
  labs(x = "Fabricante",
       y = "Cantidad de dosis",
       title = "Dosis aplicadas según fabricante y número de dosis",
       subtitle = paste0("Datos actualizados al ",corte_vacunas),
       caption  = "Fuente: MINSA, Elaboración: Jesús Gamboa") +
  theme_minimal() -> grafico04

ggsave('./export/grafico04.png',grafico04,width = 20,height = 15, units="cm")


datos_vacunas %>% 
  count(FECHA_VACUNACION,FABRICANTE) %>% 
  ggplot(aes(x=FECHA_VACUNACION,y=n,fill=FABRICANTE))+
  geom_area()+
  scale_fill_manual(values = c("dodgerblue1","forestgreen","gold"))+
  scale_x_date(limits = c(as.Date("2021-02-08"), today()+1),
               expand = c(0,0),
               labels = date_format("%d %b"),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month")) + 
  scale_y_continuous(labels = comma) +
  labs(x = "Fecha de vacunación",
       y = "Cantidad de dosis",
       title = paste0("Evolución de dosis aplicadas según fabricante de la vacuna"),
       subtitle = paste0("Datos actualizados al ",corte_vacunas),
       caption  = "Fuente: MINSA, Elaboración: Jesús Gamboa") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) -> grafico05

ggsave('./export/grafico05.png',grafico05,width = 20,height = 12.5, units="cm")




datos_vacunas %>% 
  count(FECHA_VACUNACION,FABRICANTE) %>% 
  ggplot(aes(x=FECHA_VACUNACION,y=n,fill=FABRICANTE))+
  geom_area()+
  scale_fill_manual(values = c("dodgerblue1","forestgreen","gold"))+
  scale_x_date(limits = c(as.Date("2021-02-08"), today()+1),
               expand = c(0,0),
               labels = date_format("%d %b"),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "3 months")) + 
  scale_y_continuous(labels = comma) +
  labs(x = "Fecha de vacunación",
       y = "Cantidad de dosis",
       title = paste0("Evolución de dosis aplicadas según fabricante de la vacuna"),
       subtitle = paste0("Datos actualizados al ",corte_vacunas),
       caption  = "Fuente: MINSA, Elaboración: Jesús Gamboa") +
  facet_wrap(~FABRICANTE,nrow=1)+
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) -> grafico06


ggsave('./export/grafico06.png',grafico06,width = 20,height = 12.5, units="cm")
