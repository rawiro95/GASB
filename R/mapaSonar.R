
#' mapaSonar
#' @description crea un mapa con los radares de pases de los jugadores en un partido
#' @param events dataframe Base de datos con los eventos de los partidos que se quiere estudiar
#' @param home Logico True si queremos el mapa del equipo local False para equipo visitante
#' @return Mapa de pases sonar
#' @export
#' @examples
#' partido<-seleccionEventos(eventsLF, idPartido = 19753)
#' mapaSonar(partido)




mapaSonar<-function(events,home=T){
  require(tidyverse)
  require(StatsBombR)
  require(dplyr)
  require(gridExtra)
  require(viridis)
  require(plotly)
  round.angle=15
  if(home==T){
    home<-1
  }else{
    home<-2
  }

  nombre<-unique(events$team.name)

  pases<-events%>%
    filter(team.id==events$team.id[home],type.name=="Pass",
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


  players<-unique(sonar$player.id)
  coor<-sonar%>%
    group_by(player.id, player.name)%>%
    summarize(y=mean(y),x=mean(x))



  game.lineup=events%>%filter(type.name=='Starting XI', team.id==events$team.id[home])
  game.players=game.lineup$tactics.lineup[[1]][["player.id"]]

  sonar<-sonar[with(sonar,order(sonar$player.id)),]
  game.players<-sort(game.players)
  sonar<-sonar%>%
    filter(player.id%in%game.players)
  coor<-coor%>%
    filter(player.id%in%game.players)
  coor<-coor[with(coor,order(coor$player.id)),]

  player.plots=list()
  for (i in 1:length(coor$player.id)){

    plot.data=sonar%>%filter(player.id==coor$player.id[i])

    player.plots[[i]]=ggplot(plot.data)+geom_bar(aes(x=angle.round, y=distance, fill=angle.norm), stat="identity")+
      scale_y_continuous(limits=c(0,40))+
      scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
      coord_polar(start=pi, direction=1)+
      scale_fill_gradientn("Frecuencia absoluta",colours = c("#334CFF","#52FF33","#c00000"), na.value="#FDE725FF")+
      theme_void()+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA),
            legend.position = "none")
    player.plots[[i]]=ggplotGrob(player.plots[[i]])

    if (i==length(game.players)){
      colorbar=
        ggplot(plot.data)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
        scale_y_continuous(limits=c(0,1))+
        scale_fill_viridis("", limits=c(0,30), na.value="#FDE725FF")+
        labs(x='', y='')+
        theme_void()+
        theme( legend.position = "bottom",
               plot.background = element_rect(fill = "transparent",colour = NA),
               panel.background = element_rect(fill = "transparent",colour = NA))
      colorbar=ggplotGrob(colorbar)
    }

  }

  df.grobs <- sonar%>%
    group_by(player.id, x, y)%>%
    do(subplots = ggplot(.)+
         geom_bar(aes(x=angle.round, y=distance, fill=angle.norm), stat="identity")+
         scale_y_continuous(limits=c(0,40))+
         scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
         coord_polar(start=pi, direction=1)+
         scale_fill_gradientn("Frecuencia absoluta",colours = c("#334CFF","#52FF33","#c00000"), na.value="#FDE725FF")+
         coord_polar(start=pi, direction=1)+
         theme_void()+
         theme(plot.background = element_rect(fill = "transparent",colour = NA),
               panel.background = element_rect(fill = "transparent",colour = NA),
               legend.position = "none"))%>%
    mutate(subgrobs = list(annotation_custom(ggplotGrob(subplots),
                                             x=x-15,
                                             y=y-15,
                                             xmax=x+15,
                                             ymax=y+15)))

  campo <- crear_StatsBombPitch("#538032", "#ffffff", "#538032", "#000000", BasicFeatures=F, horiz = F)
  p<-campo+
    ggtitle(paste("\nMapa sonar de pases de ",nombre)) +
    theme(legend.position="none")
  p<-p+df.grobs$subgrobs+
    geom_text(data = coor,aes(x=x,y=y),label=coor$player.name, size=3)
  return(p)


}


