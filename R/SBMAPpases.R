#' SBMAPpases
#' @description  La funcion SBMAPpases() realiza un mapa de pases mediante
#' flechas donde el inicio de estas es el origen del pase y el final el destino.
#' Distingue entre pases completados, pases incompletados, asistencias de tiro y asistencias de gol.
#' Tambien calcula la cantidad de pases completados y el porcentaje de acierto.
#' @param events Dataframe Base de datos con los eventos de los partidos que se quiere estudiar
#' @param idJugador Integer identificador del jugador a estudiar
#' @param camporival Logico si se quiere filtrar los pases que se han hecho a campo rival
#' @return Grafico con los pases
#' @export
#' @examples
#' manCity<-seleccionEvens(eventsLF, idPartido = 2275037, idEquipo = 746, rival = F)
#' SBMAPpases(manCity)
#'

SBMAPpases<-function(events, camporival=F, idjugador=NA){
  require(tidyverse)
  require(ggplot2)
  pases<-events%>%
    filter(type.name=="Pass",
           ! play_pattern.name %in% c("From Corner", "From Free Kick", "From Throw In"))

  if(camporival){
    pases<-pases%>%
      filter(pass.end_location.x>60)
  }

  if(!is.na(idjugador)){
    pases<-pases%>%
      filter(player.id==idjugador)
  }

  pases<-pases %>%
    mutate(tipo.pase  = case_when(
      (is.na(pass.outcome.id)==F)~"Pase fallido",
      (is.na(pass.shot_assist)&is.na(pass.goal_assist)&is.na(pass.outcome.id)) ~ "pase simple",
      (pass.shot_assist==T & is.na(pass.goal_assist)) ~ "asistencia de tiro",
      pass.goal_assist==T  ~ "asistencia de gol"
    ))

  completados<-sum(is.na(pases$pass.outcome.id))


  total<-length(pases$id)
  acierto<-round(completados/total*100)

  h <- crear_StatsBombPitch(colorCesped = "#1D2852",horiz=T)
  p<-h+
    ggtitle(paste("Mapa de pases"),subtitle = paste("Completados \t",completados," \nAcierto \t",acierto,"%"))+
    geom_segment(data = pases,aes(x=location.x,y =location.y, xend=pass.end_location.x, yend=pass.end_location.y,color=tipo.pase), arrow = arrow(length = unit(0.03, "npc")))+
    scale_color_manual(values = c("Pase fallido"="red","pase simple"="#A9A9A9","asistencia de tiro"="yellow","asistencia de gol"="green"))+
    guides(color=guide_legend( "Tipo de pase" ))+
    theme(legend.position = "bottom")


  return(p)

}


