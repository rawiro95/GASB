
#' minutosJugados
#' @description Calcula la cantidad de minutos jugados por un jugador en una cantidad de partidos
#' @param events Dataframe Base de datos con los eventos de los partidos que se quiere estudiar
#' @param idjugador Integer identificador del jugador a estudiar
#' @return Integer minutos jugados por el jugador
#' @export
#' @examples
#' minutosJugados(eventsLF, idJugador=15555)
#'



minutosJugados<-function(events,idJugador){

  idEquipo<-unique(events$team.id[which(events$player.id==idJugador)])[1]

  partidos <- unique(events$match_id[which(events$team.id==idEquipo)])

  eventos <- events%>%
    filter(match_id %in% partidos)


  playTime<-0

  for(i in 1:length(partidos)){
    ini<-0
    fin<-0
    partido<-eventos%>%
      filter(match_id==partidos[i])
    game.lineup=partido%>%filter(type.name=='Starting XI', team.id==idEquipo)

    sustituciones<-partido%>%
      filter(type.name=="Substitution")

    if((idJugador %in% game.lineup$tactics.lineup[[1]][[2]])){
      ini<-0
      fin<-90

    }else{
      if(idJugador%in%sustituciones$substitution.replacement.id){
        sustitucion<-sustituciones%>%
          filter(substitution.replacement.id==idJugador)
        ini<-min(sustitucion$minute,87)
        fin<-90

      }else{
        fin<-0

      }
    }


    if(idJugador %in% sustituciones$player.id){
      sustitucion<-sustituciones%>%
        filter(player.id==idJugador)
      fin<-sustitucion$minute

    }

    playTime<-playTime+fin-ini

  }


  return(playTime)
}


