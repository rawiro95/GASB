#' matrizPases
#' @description La función matrizPases() crea una matriz simétrica de puntos.
#'  color y el tamaño de los puntos representan las frecuencias absolutas de pases entre todos
#'  los jugadores de los eventos objetivo de estudio.
#' @param events Dataframe Base de datos con los eventos de los partidos que se quiere estudiar
#' @return Matriz interactiva de pases
#' @export
#' @examples
#' eventos<-seleccionEventos(eventsLF, idEquipo = 967,rival = F)
#' matrizPases(eventos)



matrizPases<-function(events){
  require(corrgram)
  require(corrplot)
  require(heatmaply)
  jugadores<-unique(events$player.name)
  jugadores<-jugadores[-1]


  mPases<-matrix(rep(0,length(jugadores)^2),nrow = length(jugadores))

  colnames(mPases)<-jugadores

  rownames(mPases)<-jugadores



  for(i in 1:length(jugadores)){
    for(j in 1:length(jugadores)){
      mPases[i,j]<-sum((events$pass.recipient.name==jugadores[j]&events$player.name==jugadores[i]),na.rm = T)+sum((events$pass.recipient.name==jugadores[i]&events$player.name==jugadores[j]),na.rm = T)
      if(mPases[i,j]==0){
        mPases[i,j]<-NA
      }
      }
  }


  return(heatmaply_cor(
    mPases,
    limits = c(0,max(mPases,na.rm=T)),
    node_type = "scatter",
    dendrogram="none",
    point_size_mat = mPases,
    label_names = c("Jugadorx", "Jugadory", "Pases"),tooltip = c("Jugadorx", "Jugadory", "Pases")))

}



