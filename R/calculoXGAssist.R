#' calculoXGAssist
#' @description Calcula el expected Goal asistido generado por un determinado jugador
#' @param events Dataframe Base de datos con los eventos de los partidos que se quiere estudiar
#' @param idJugador Integer identificador del jugador a estudiar
#' @return numerico expected goal asistidpo generado por el jugador
#' @export
#' @examples
#' calculoXGAssist(eventsLF, idJugador=4643)
#'



calculoXGAssist<-function(events, idJugador){
  require(tidyverse)
  idEquipo<-unique(events$team.id[which(events$player.id==idJugador)])

  partidos <- unique(events$match_id[which(events$team.id==idEquipo)])

  eventos <- events%>%
    filter(match_id %in% partidos)

  partido<-eventos%>%
    filter(match_id%in%partidos)

  pases<-partido%>%
    filter(player.id==idJugador & type.name=="Pass" & pass.shot_assist==T)%>%
    select(pass.assisted_shot_id)

  shots<-partido%>%
    filter(id%in%pases$pass.assisted_shot_id)

  xGAssist<-sum(shots$shot.statsbomb_xg)


  return(xGAssist)
}

