#' sonarJugador
#' @description La funcion sonarJugador() realiza un grafico sonar de pases. Distingue entre 24 direcciones. Cada direccion abarca un angulo de 15 grados. La frecuencia absoluta de pases se representa mediante el color de las barras. Cuanto mas calido sea el color, mayor frecuencia de pases tendra esa direccion. La longitud media de los pases se representa mediante las longitudes de las barras.
#' @param events Dataframe Base de datos con los eventos de los partidos que se quiere estudiar
#' @return Grafico sonar
#' @export
#' @examples
#' temp<-seleccionEventos(eventsLF,idJugador = 15555)
#' sonarJugador(temp)





sonarJugador<-function(events){
  require(viridis)
  require(ggplot2)
  round.angle=15
  pases<-events%>%
    filter(type.name=="Pass",
           ! play_pattern.name %in% c("From Corner", "From Free Kick", "From Throw In"))%>%
    mutate(angle.round=round(pass.angle*180/pi/round.angle)*round.angle)


  sonar<-pases%>%
    group_by(player.id,player.name, team.name)%>%
    mutate(N=n(),y=mean(location.x),x=mean(location.y))%>%
    ungroup()%>%
    group_by(player.name, team.name, angle.round)%>%
    mutate(n.angle=n())%>%
    ungroup()%>%
    group_by(player.name, team.name)%>%
    mutate(maxN=max(n.angle),
           angle.norm=n.angle)%>%
    ungroup()%>%
    group_by(player.id,angle.round, player.name, team.name,N,x,y,maxN)%>%
    summarize(angle.norm=mean(angle.norm),
              distance=mean(pass.length)*0.914,
              distance=ifelse(distance>40, 40,distance))
  nombre<-unique(events$player.name)

  completados<-sum(is.na(pases$pass.outcome.id))


  total<-length(pases$id)
  acierto<-round(completados/total*100)


  ggplot(sonar)+
    geom_bar(aes(x=angle.round, y=distance, fill=angle.norm), stat="identity")+
    geom_segment(aes(x = -180, y = 10, xend = 180, yend = 10),colour = "black",linetype=2)+
    geom_segment(aes(x = -180, y = 20, xend = 180, yend = 20),colour = "black",linetype=2)+
    geom_segment(aes(x = -180, y = 30, xend = 180, yend = 30),colour = "black",linetype=2)+
    geom_text(x=0,y=12, label="10m",size=3)+
    geom_text(x=0,y=22, label="20m",size=3)+
    geom_text(x=0,y=32, label="30m",size=3)+
    scale_y_continuous(limits=c(0,40))+
    scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
    coord_polar(start=pi, direction=1)+
    scale_fill_gradientn("Frecuencia absoluta",colours = c("#334CFF","#52FF33","#c00000"), na.value="#FDE725FF")+
    #guides(fill=guide_legend( "Frecuencia absoluta" ))+
    #scale_fill_continuous("Frecuencia absoluta",low="blue",high="red", na.value="#FDE725FF")+
    labs(x='', y='',title= nombre,subtitle = paste("Pases intentados ",sonar$N[1],"\n Completados ",completados,"\n Acierto ",acierto,"%"))+
    theme_void()+
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle =element_text(hjust=0.5),
          #legend.position = "none", #uncomment to remove colorbar
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))

}



