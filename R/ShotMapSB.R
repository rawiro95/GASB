#' shotMapSB
#'
#' @description La función ShotMapSB() crea un mapa recreando el campo contrario de un campo de futbol.
#' @param colorCesped Color del cesped.
#' @param colorLinea Color de las lineas.
#' @param colorFondo Color del fondo del grafico.
#' @param colorGol Color de la porteria.
#' @param BasicFEatures Logico. Lineas elementales o area pequeña y balcon del area.
#' @param horiz Logico. Posicion del campo.
#' @return Objeto ggplot.
#' @export
#' @examples
#' shotMapSB()

shotMapSB <- function(colorCesped = "#ffffff",colorLinea = "#A9A9A9", colorFondo = "#ffffff", colorGol = "#000000"){
  require(ggplot2)
  theme_blankPitch = function(size=12) {
    theme(
      #axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      #axis.ticks.margin=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background =element_rect(fill=colorFondo, colour=NA),
      legend.key=element_rect(colour=colorFondo,fill=colorFondo),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = colorFondo, fill = colorFondo, size = .5),
      panel.background=element_rect(fill=colorFondo,colour=colorFondo),
      #       panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background =element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour=colorFondo,size=size,angle=270),
      strip.text.x=element_text(size=size*1))
  }
  #Dimensiones
  ymin <- 60
  ymax <- 120
  xmin <- 0
  xmax <- 80

  #Accidentes a lo largo del campo
  frontArea <- 42+60
  frontPeq <- 54+60
  puntoPen <- 48+60

  #Accidentes a lo ancho
  areaLineaIzq <- 18
  areaLineaDer <- 62
  areaPeqIzq <- 30
  areaPeqDer <- 50
  paloizq <- 36
  paloDer <- 44
  puntoCentral <- 40


  circleFun <- function(centro = c(0,0), diameter=1,npoints = 1000){
    r = diameter/2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- centro[1]+r*cos(tt)
    yy <- centro[2] + r*sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  dArc <- circleFun(c(puntoCentral, puntoPen), 20)
  dArc <- dArc[which(dArc$y <= (frontArea)),]


  pitch <- ggplot() + xlim(c(xmin, xmax)) + ylim(c(ymin,ymax)) +
    theme_blankPitch() +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = colorCesped, colour =colorLinea) +
    geom_rect(aes(ymin=frontArea, ymax=ymax, xmin= areaLineaIzq, xmax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
    geom_rect(aes(ymin=frontPeq, ymax=ymax, xmin=areaPeqIzq, xmax=areaPeqDer), fill = colorCesped, colour = colorLinea) +
    geom_segment(aes(y = ymax, x = paloizq, yend=ymax, xend = paloDer),colour = colorGol, size = 2) +
    geom_point(aes(y = puntoPen , x = puntoCentral), colour = colorLinea) +
    geom_path(data=dArc, aes(x=x,y=y), colour = colorLinea)



  return(pitch)


}
shotMapSB()

