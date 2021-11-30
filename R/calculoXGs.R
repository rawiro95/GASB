#' calculoXGs
#' @description Calcula el tiempo jugado, el expected goal asistido, el expected goal construido y el expected goal generado por el equipo contrario con
#' el jugador en el campo
#' @param events Dataframe Base de datos con los eventos de los partidos que se estudiar
#' @param idJugador Integer identificador del jugador a estudiar
#' @return Vector con los minutos, el xg asistido, el xg construido y el xg en contra
#' @export
#' @examples
#' calculoXGs(eventsLF, idJugador=15555)
#'


calculoXGs<-function(events,idJugador){

  idEquipo<-unique(events$team.id[which(events$player.id==idJugador)])[1]

  partidos <- unique(events$match_id[which(events$team.id==idEquipo)])

  eventos <- events%>%
    filter(match_id %in% partidos)

  pases<-eventos%>%
    filter(player.id==idJugador & type.name=="Pass" & pass.shot_assist==T)%>%
    select(pass.assisted_shot_id)

  shots<-eventos%>%
    filter(id%in%pases$pass.assisted_shot_id)

  xGAssist<-sum(shots$shot.statsbomb_xg)

  xgcontra<-0
  xGbuild<-0
  playTime<-0

  for(i in 1:length(partidos)){
    ini<-0
    fin<-0
    indicei<-1
    indicef<-1
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

    game.lineup=partido%>%filter(type.name=='Starting XI', team.id==idEquipo)

    sustituciones<-partido%>%
      filter(type.name=="Substitution")

    if((idJugador %in% game.lineup$tactics.lineup[[1]][[2]])){
      ini<-0
      indicei<-1
      fin<-90
      indicef<-length(partido$index)

    }else{
      if(idJugador%in%sustituciones$substitution.replacement.id){
        sustitucion<-sustituciones%>%
          filter(substitution.replacement.id==idJugador)
        ini<-min(sustitucion$minute,87)
        indicei<-sustitucion$index
        fin<-90
        indicef<-length(partido$index)

      }else{
        fin<-0
        indicef<-1

      }
    }


    if(idJugador %in% sustituciones$player.id){
      sustitucion<-sustituciones%>%
        filter(player.id==idJugador)
      fin<-sustitucion$minute

    }

    tiros<-partido%>%
      filter(type.name=="Shot" & team.id!=idEquipo[1] & index>=indicei & index<=indicef)

    xgcontra<-xgcontra+sum(tiros$shot.statsbomb_xg)
    playTime<-playTime+fin-ini

  }



  return(c(playTime,xGAssist,xGbuild,xgcontra))
}


