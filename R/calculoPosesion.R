#' @title calculoPosesion
#' @description El objetivo de la funcion calculoPosesion() es conseguir el porcentaje de posesion que tiene un equipo en particular en uno o mas partidos.
#' @param events Dataframe Base de datos con los eventos de los partidos que se estudiar
#' @param idEquipo Integer identificador del equipo a estudiar
#' @return numerico porcentaje de posesion
#' @export
#' @examples
#' calculoPosesion(eventsLF, idEquipo=972)
#'



calculoPosesion<-function(events,idEquipo){
  require(tidyverse)
  partidos <- unique(events$match_id[which(events$team.id==idEquipo)])
  posesion<-rep(0,length(partidos))
  partido<-events%>%
    filter(match_id%in%partidos, type.name!="Pressure")

  tiempoEquipo<-partido%>%
    filter(possession_team.id==idEquipo)%>%
    summarise(tiempo=sum(duration, na.rm = T))
  tiempoOtro<-partido%>%
    filter(possession_team.id!=idEquipo)%>%
    summarise(tiempo=sum(duration, na.rm = T))

  posesion<-tiempoEquipo$tiempo/(tiempoEquipo$tiempo+tiempoOtro$tiempo)*100



  return(mean(posesion))
}
