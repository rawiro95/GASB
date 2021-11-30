


analisisDefensivoEquipo<-function(events, idEquipo, presion=F){
  require(tidyverse)
  require(ggplot2)
  equipos<-unique(events$team.id)

  media<-matrix(rep(0,6*5),ncol=1)
  minimox<-matrix(rep(0,6*5),ncol=1)
  maximox<-matrix(rep(0,6*5),ncol=1)
  minimoy<-matrix(rep(0,6*5),ncol=1)
  maximoy<-matrix(rep(0,6*5),ncol=1)

  minimo<-matrix(rep(9999,6*5),ncol=1)
  maximo<-matrix(rep(0,6*5),ncol=1)

  pasesEquipo<-rep(0,length(equipos))


  for(i in 1:length(equipos)){
    print(i)
   if(presion==T){
      eventos<-events%>%
        filter(team.id==equipos[i])%>%
        filter(type.id==17)
   }else{
     eventos<-events%>%
       filter(team.id==equipos[i])%>%
       filter(type.id%in%c("10","22","17","33","6","4","9","2")&`50_50.outcome.id`%in%c(NA,108,147)&duel.outcome.id%in%c(4,NA,16,17,15))

    }
    pases<-seleccionEventos(events,idEquipo=equipos[i],rival = T)
    pases<-pases%>%
      filter(team.id!=equipos[i]&type.name=="Pass"&is.na(pass.outcome.id))

    for(l in 0:4){
      for(k in 0:5){
        j<-l*6+k+1
        eventosZona<-eventos%>%
          filter(location.x>k*20&location.x<(k+1)*20&location.y>l*16&location.y<(l+1)*16)
        pasesZona<-pases%>%
          filter(location.x>k*20&location.x<(k+1)*20&location.y>l*16&location.y<(l+1)*16)
        defensaZona<-length(eventosZona$id)/length(pasesZona$id)
        minimo[j]<-min(minimo[j],defensaZona)
        maximo[j]<-max(maximo[j],defensaZona)
        media[j]<-media[j]+defensaZona
        minimox[j]<-k*20
        maximox[j]<-(k+1)*20
        maximoy[j]<-80-l*16
        minimoy[j]<-80-(l+1)*16
        pasesEquipo[i]<-pasesEquipo[i]+length(pasesZona$id)
      }
    }
  }
  media<-media/length(equipos)

  if(presion==T){
    eventos<-events%>%
      filter(team.id==idEquipo)%>%
      filter(type.id==17)
  }else{
    eventos<-events%>%
      filter(team.id==idEquipo)%>%
      filter(type.id%in%c("10","22","17","33","6","4","9","2")&`50_50.outcome.id`%in%c(NA,108,147)&duel.outcome.id%in%c(4,NA,16,17,15))


  }
  pases<-seleccionEventos(events,idEquipo=idEquipo,rival = T)
  pases<-pases%>%
    filter(team.id!=idEquipo&type.name=="Pass"&is.na(pass.outcome.id))
  partidos<-length(unique(eventos$match_id))




  diferencia<-matrix(rep(0,6*5),ncol=1)

  for(l in 0:4){
    for(k in 0:5){
      j<-l*6+k+1
      eventosZona<-eventos%>%
        filter(location.x>k*20&location.x<(k+1)*20&location.y>l*16&location.y<(l+1)*16)
      pasesZona<-pases%>%
        filter(location.x>k*20&location.x<(k+1)*20&location.y>l*16&location.y<(l+1)*16)
      defensaZona<-length(eventosZona$id)/length(pasesZona$id)
      diferencia[j]<-defensaZona-media[j]
    }
  }

  for(i in 1:30){
    if(diferencia[i]>0){
      diferencia[i]<-diferencia[i]/(maximo[i]-media[i])
    }
    if(diferencia[i]<0){
      diferencia[i]<-diferencia[i]/(media[i]-minimo[i])
    }
  }

  datos<-as.data.frame(cbind(minimox,maximox,minimoy,maximoy,media,diferencia))

  names(datos)<-c("minimox","maximox","minimoy","maximoy","media","diferencia")

  posesion<-calculoPosesion(events,idEquipo)
  equipo<-events%>%
    filter(team.id==idEquipo)
  partidos<-length(unique(equipo$match_id))

  presion<-eventos%>%
    filter(type.name=="Pressure")
  presion<-round(dim(presion)[1]*2/(1+exp(-0.1*(posesion-50)))/partidos,2)

  bloqueos<-eventos%>%
    filter(type.name=="Block")
  bloqueos<-round(dim(bloqueos)[1]*2/(1+exp(-0.1*(posesion-50)))/partidos,2)

  intercepciones<-eventos%>%
    filter(type.name=="Interception")
  intercepciones<-round(dim(intercepciones)[1]*2/(1+exp(-0.1*(posesion-50)))/partidos,2)

  Faltas<-eventos%>%
    filter(type.name=="Foul Committed")
  Faltas<-round(dim(Faltas)[1]*2/(1+exp(-0.1*(posesion-50)))/partidos,2)

  duelos<-eventos%>%
    filter(type.name=="Duel")
  duelos<-round(dim(duelos)[1]*2/(1+exp(-0.1*(posesion-50)))/partidos,2)

  despejes<-eventos%>%
    filter(type.name=="Clearance")
  despejes<-round(dim(despejes)[1]*2/(1+exp(-0.1*(posesion-50)))/partidos,2)

  recuperaciones<-eventos%>%
    filter(type.name=="Ball Recovery")
  recuperaciones<-round(dim(recuperaciones)[1]*2/(1+exp(-0.1*(posesion-50)))/partidos,2)



  g1<-crear_StatsBombPitch()+
    geom_rect(data=datos,aes(xmin=minimox,xmax=maximox,ymin=minimoy,ymax=maximoy,fill=diferencia))+
    scale_fill_gradient2(low="black",mid="white",high = "red",midpoint = 0, limits=c(-1,1),guide=NULL)+
    geom_point(data=eventos,aes(location.x,80-location.y),colour="white",alpha=0.5,size=0.5)+
    ggtitle(eventos$team.name[1],subtitle = paste("Faltas ",Faltas,"\nIntercepciones ",intercepciones,"\nDuelos ",duelos,"\nBloqueos ",bloqueos,"\nDespejes ",despejes,"\nRecuperaciones ",recuperaciones,"\nPresion ",presion))

  return(g1)

}



