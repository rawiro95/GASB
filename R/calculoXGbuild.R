#' calculoXGbuild
#' @description Calcula del expected goal construido por un jugador
#' @param events Dataframe Base de datos con los eventos de los partidos que se quiere estudiar
#' @param idJugador Integer identificador del jugador a estudiar
#' @return Numerico expected goal construido
#' @export
#' @examples
#' calculoXGbuild(eventsLF, idJugador=31703)
#'



calculoXGbuild<-function(events, idJugador){
  idEquipo<-unique(events$team.id[which(events$player.id==idJugador)])

  partidos <- unique(events$match_id[which(events$team.id==idEquipo)])

  eventos <- events%>%
    filter(match_id %in% partidos)

  xGbuild<-0

  for(i in 1:length(partidos)){

    partido<-eventos%>%
      filter(match_id==partidos[i])

    shots<-partido%>%
      filter(type.name=="Shot", team.id==idEquipo)

    pases<-partido%>%
      filter(player.id==idJugador & type.name=="Pass")

    tiros<-partido%>%
      filter(player.id==idJugador & (!is.na(pass.shot_assist) | type.name=="Shot"))


    posesiones<-unique(pases$possession)

    shots<-shots%>%
      filter(possession%in%posesiones)

    posesiones<-unique(tiros$possession)

    shots<-shots%>%
      filter(!possession%in%posesiones)

    xGbuild<-xGbuild+sum(shots$shot.statsbomb_xg)

  }
  return(xGbuild)
}
