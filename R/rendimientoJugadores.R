#' rendimientoJugadores
#' @Description Calcula los valores de las variables de interes para estudiar el rendimiento de todos los jugadores
#' @param events Dataframe Base de datos con los eventos de los partidos que se estudiar
#' @return Dataframe con los valores de las variables de todos los jugadores
#' @export
#' @examples
#' rendimientoJugadores(eventsLF)
#'

rendimientoJugadores<-function(events){
  require(tidyverse)

  jugadores<-unique(events$player.id)

  jugadores<-jugadores[!is.na(jugadores)]

  names<-c("posicion","passing","deepProgresions","XgAssisted","succesDribbles","foulsWon","Turnovers","pressureRegains","pressure","PAdjTackles","PAdjInterceptions","fouls","tackDribbled","AerialWins","AerialWinsPor","PressuredLongBalls","UnPressuredlongBalls","Xgbuild","XgContra","player.id")

  datos<-data.frame(matrix(rep(0,length(jugadores)*length(names)),ncol = length(names)))

  names(datos)<-names

  for(i in 1 :length(jugadores)){
    idJugador<-jugadores[i]
    sumary<-calculoXGs(events,idJugador)
    idEquipo<-unique(events$team.id[which(events$player.id==idJugador)])

    eventsJugador<-events%>%
      filter(player.id==idJugador)

    pases<-eventsJugador%>%
      filter(type.name=="Pass",
             ! play_pattern.name %in% c("From Corner", "From Throw In"))

    minutos<-sumary[1]
    posesion<-calculoPosesion(events,idEquipo)

    frecuencias <- data.frame(table(eventsJugador$position.id))

    posicion <- as.numeric(as.character(frecuencias[which.max(frecuencias$Freq),1]))

    passing<-sum(is.na(pases$pass.outcome.id))/length(pases$id)*100

    XgAssisted<-sumary[2]/(minutos/90)

    deepProgresions<-(sum(eventsJugador$carry.end_location.x>80 & eventsJugador$location.x<80, na.rm = T)+sum(!is.na(pases$pass.recipient.id)&pases$pass.end_location.x>80 & pases$location.x<80, na.rm = T))/(minutos/90)

    XgBuild<-sumary[3]/(minutos/90)

    succesDribbles<-sum(eventsJugador$dribble.outcome.id==8,na.rm = T)/(minutos/90)

    foulsWon<-sum(eventsJugador$type.name=="Foul Won",na.rm = T)/(minutos/90)

    turnovers<-(sum(eventsJugador$type.name=="Miscontrol",na.rm = T)+sum(eventsJugador$dribble.outcome.id==9,na.rm = T))/(minutos/90)

    pressure<-sum(eventsJugador$type.name=="Pressure")/(minutos/90)

    pressureRegains<-sum(eventsJugador$type.name=="Pressure" & eventsJugador$counterpress==T,na.rm = T)/(minutos/90)

    pAdjTackles<-sum(eventsJugador$duel.outcome.id%in%c(15,16,17,4), na.rm = T)*2/(1+exp(-0.1*(posesion-50)))/(minutos/90)

    PadjInterceptions<-sum(eventsJugador$interception.outcome.id %in% c(15,16,17,4), na.rm = T)*2/(1+exp(-0.1*(posesion-50)))/(minutos/90)

    fouls<-sum(eventsJugador$foul_committed.type.id,na.rm = T)/(minutos/90)

    tackDribbled<-sum((sum((eventsJugador$duel.type.name=="Tackle"), na.rm = T)/(sum(eventsJugador$duel.type.name=="Tackle", na.rm = T)+sum(eventsJugador$type.name=="Dribbled Past"))),na.rm = T)*100

    AerialWins<-(sum(eventsJugador$shot.aerial_won,na.rm = T)+sum(eventsJugador$clearance.aerial_won,na.rm = T)+sum(eventsJugador$miscontrol.aerial_won,na.rm = T)+sum(eventsJugador$pass.aerial_won,na.rm = T))/(minutos/90)

    AerialWinsPor<-sum((sum(eventsJugador$shot.aerial_won,na.rm = T)+sum(eventsJugador$clearance.aerial_won,na.rm = T)+sum(eventsJugador$miscontrol.aerial_won,na.rm = T)+sum(eventsJugador$pass.aerial_won,na.rm = T))/((sum(eventsJugador$clearance.aerial_won,na.rm = T)+sum(eventsJugador$miscontrol.aerial_won,na.rm = T)+sum(eventsJugador$pass.aerial_won,na.rm = T))+sum(eventsJugador$duel.type.name=="Aerial Lost", na.rm = T)+sum(eventsJugador$shot.aerial_won,na.rm = T)),na.rm = T)*100

    PressuredLongBalls<-sum((pases$pass.length>40 & pases$pass.end_location.x>60 & pases$under_pressure==T),na.rm = T)/(minutos/90)

    UnPressuredlongBalls<-sum((pases$pass.length>40 & pases$pass.end_location.x>60 & is.na(pases$under_pressure)),na.rm = T)/(minutos/90)

    XgContra<-sumary[4]/(minutos/90)

    player.id<-eventsJugador$player.id


    datos[i,]<-c(posicion,passing,deepProgresions,XgAssisted,succesDribbles,foulsWon,turnovers,pressureRegains,pressure,pAdjTackles,PadjInterceptions,fouls,tackDribbled,AerialWins,AerialWinsPor,PressuredLongBalls,UnPressuredlongBalls,XgBuild,XgContra,player.id)

  }

  rownames(datos)<-jugadores

  datos[is.na(datos)]<-0

  return(datos)
}


