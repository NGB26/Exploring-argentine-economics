library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(plotly)
library(viridis)
library(hrbrthemes)
library(ggrepel)


df<-read_csv2("O:/INFANCIA/BECARIOS/NICOLAS/TESIS/base_resp_csv.csv")
df_r<-filter(df, EST_CLASE_7!=is.na(EST_CLASE_4))


df_1<-group_by(df, ENCUESTA, EST_CLASE_4)%>%
  summarise("ing"=weighted.mean(ing_lab_2021, PON_RESPONDENTE_SIN_ELEVAR, na.rm=T),
            "cant"=sum(PON_RESPONDENTE)/1000,
            "educ"=weighted.mean(EDUCACIO_ENTREV, PON_RESPONDENTE_SIN_ELEVAR))

df_1<-filter(df_1, ENCUESTA<20)
df_1<-df_1[1:16,]

df_1<-df_1%>%mutate(year=case_when(
  ENCUESTA==16~2017,
  ENCUESTA==17~2018,
  ENCUESTA==18~2019,
  ENCUESTA==19~2020,
  ENCUESTA==20~2021))

df_1<-df_1%>%mutate(Estrato=case_when(
    EST_CLASE_4==1~"Estrato medio profesional",
    EST_CLASE_4==2~"Estrato medio no profesional",
    EST_CLASE_4==3~"Estrato bajo integrado",
    EST_CLASE_4==4~"Estrato bajo marginal"))


b<-ggplot(df_1, aes(educ, ing, color=Estrato))+
  geom_point(aes(size=cant))+
  theme_minimal()+
  labs(title = "Ingreso promedio, educación y tamaño de los estratos",
        x="Años promedio de educación", y="Ingreso promedio",
        caption = "Año {frame_along}",
       color="Estrato socio-económico",
        subtitle = "El tamaño de las burbujas representa la cantidad en miles de personas")+
  transition_reveal(as.integer(year))+
  scale_size(range = c(.1, 20))+
  geom_text_repel(aes(label=as.character(round(cant,digits = 0))), size=5, 
                  hjust=-0.1, vjust=-0.5, position = position_dodge(width = 1))+
  theme(legend.position="bottom",panel.background = element_rect(fill='transparent'),
                                                                         panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                                                                         plot.title = element_text(hjust = 0.5),
                                                                         text=element_text(size=14,  family="serif"),
                                                                         legend.text = element_text(size=10),
                                                                         legend.margin=margin(),
                                                                         plot.caption=element_text(hjust = -0.1))+
  guides(size="none",colour=guide_legend(nrow = 2, byrow = T,override.aes = list(size = 5)))

b


setwd("O:/INFANCIA/BECARIOS/NICOLAS/Pruebas_R")

anim_save("estratos.gif", b)

