#' Seleccion de eventos
#' @description Funcion que selecciona eventos determinados por el usuario de una base de datos
#' @param eventos Dataframe Base de datos de la que queremos extraer los eventos
#' @param idJugador Integer Identificador del jugador que queremos seleccionar
#' @param idPartido Integer Identificador del partido que queremos seleccionar
#' @param idEquipo Integer Identificador del equipo que queremos seleccionar
#' @param rival Logico. Seleccion de los eventos de equipo rival
#' @return Dataframe con los eventos seleccionados
#' @export
#' @examples
#' datos<-seleccionEventos(eventsLF, idJugador=15555)
#'


seleccionEventos<-function(eventos, idJugador=0, idPartido=0, idEquipo=0, rival=F){
  require(tidyverse)
  require(dplyr)
  if(idEquipo!=0 & rival==T){
    partidos <- unique(eventos$match_id[which(eventos$team.id==idEquipo)])
    eventos<-eventos%>%
      filter(match_id%in%partidos)
  }
  if(idEquipo!=0 & rival==F){
    eventos<-eventos%>%
      filter(team.id==idEquipo)
  }
  if(idPartido!=0){
    eventos<-eventos%>%
      filter(match_id==idPartido)
  }
  if(idJugador!=0){
    eventos<-eventos%>%
      filter(player.id==idJugador)
  }
  return(eventos)
}





