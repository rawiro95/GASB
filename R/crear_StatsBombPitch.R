#' crear_StatsBombPitch
#'
#' @description  Esta funcion crea un grafico ggplot con las dimensiones y coordenadas compatibles con los datos de StatsBomb.
#' @param colorCesped Color del cesped
#' @param colorLinea Color de las lineas
#' @param colorFondo Color del fondo del grafico
#' @param colorGol Color de la porteria
#' @param BasicFEatures Logico, lineas elementales o tambien circulo central, balcon del area y area pequena
#' @param horiz Logico. Posicion del campo, horizontal o vertical
#' @return Objeto ggplot que representa un campo de futbol.
#' @export
#' @examples
#' crear_StatsBombPitch()


crear_StatsBombPitch <- function(colorCesped="#ffffff",colorLinea="#A9A9A9", colorFondo="#ffffff", colorGol="#000000", BasicFeatures = F, horiz = T){
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
  ymin <- 0
  ymax <- 80
  xmin <- 0
  xmax <- 120

  #Accidentes a lo largo del campo
  frontAreaDef <- 18
  frontAreaOf <- 102
  medioCampo <- 60
  areaPeqDef <- 6
  areaPeqOf <- 114
  puntoPenDef <- 12
  puntoPenOf <- 108

  #Accidentes a lo ancho
  areaLineaIzq <- 18
  areaLineaDer <- 62
  areaPeqIzq <- 30
  areaPezDer <- 50
  paloizq <- 36
  paloDer <- 44
  puntoCentral <- 40

  #Otra dimension
  diametroCentro<- 20

  circleFun <- function(centro = c(0,0), diameter=1,npoints = 100){
    r = diameter/2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- centro[1]+r*cos(tt)
    yy <- centro[2] + r*sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  circuloCentral <- circleFun(c(medioCampo, puntoCentral), diametroCentro)

  if(BasicFeatures == TRUE){
    if(horiz == TRUE){
      pitch <- ggplot() + xlim(c(xmin, xmax)) + ylim(c(ymin,ymax)) +
      #theme_blankPitch() +
      geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = colorCesped, colour =colorLinea) +
      geom_rect(aes(xmin=xmin, xmax=frontAreaDef, ymin= areaLineaIzq, ymax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
      geom_rect(aes(xmin=frontAreaOf, xmax=xmax, ymin=areaLineaIzq, ymax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
      geom_segment(aes(x = medioCampo, y = ymin, xend = medioCampo, yend = ymax),colour = colorLinea) +
      geom_segment(aes(x = xmin, y = paloizq, xend = xmin, yend = paloDer),colour = colorGol, size = 2) +
      geom_segment(aes(x = xmax, y = paloizq, xend = xmax, yend = paloDer),colour = colorGol, size = 2)

    }else{
      pitch <- ggplot() + xlim(c(ymin, ymax)) + ylim(c(xmin,xmax)) +
      theme_blankPitch() +
      geom_rect(aes(ymin=xmin, ymax=xmax, xmin=ymin, xmax=ymax), fill = colorCesped, colour =colorLinea) +
      geom_rect(aes(ymin=xmin, ymax=frontAreaDef, xmin= areaLineaIzq, xmax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
      geom_rect(aes(ymin=frontAreaOf, ymax=xmax, xmin=areaLineaIzq, xmax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
      geom_segment(aes(y = medioCampo, x = ymin, yend = medioCampo, xend = ymax),colour = colorLinea) +
      geom_segment(aes(y = xmin, x = paloizq, yend = xmin, xend = paloDer),colour = colorGol, size = 2) +
      geom_segment(aes(y = xmax, x = paloizq, yend = xmax, xend = paloDer),colour = colorGol, size = 2)

    }
  }else{
    if(horiz == FALSE){
      pitch <- ggplot() + xlim(c(ymin, ymax)) + ylim(c(xmin,xmax)) +
        theme_blankPitch() +
        geom_rect(aes(ymin=xmin, ymax=xmax, xmin=ymin, xmax=ymax), fill = colorCesped, colour =colorLinea) +
        geom_rect(aes(ymin=xmin, ymax=frontAreaDef, xmin= areaLineaIzq, xmax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
        geom_rect(aes(ymin=frontAreaOf, ymax=xmax, xmin=areaLineaIzq, xmax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
        geom_rect(aes(ymin=areaPeqOf, ymax=xmax, xmin=areaPeqIzq, xmax=areaPezDer), fill = colorCesped, colour = colorLinea) +
        geom_rect(aes(ymin=xmin, ymax=areaPeqDef, xmin= areaPeqIzq, xmax=areaPezDer), fill = colorCesped, colour = colorLinea) +
        geom_segment(aes(y = medioCampo, x = ymin, yend = medioCampo, xend = ymax),colour = colorLinea) +
        geom_segment(aes(y = xmin, x = paloizq, yend = xmin, xend = paloDer),colour = colorGol, size = 2) +
        geom_segment(aes(y = xmax, x = paloizq, yend = xmax, xend = paloDer),colour = colorGol, size = 2) +
        geom_path(data=circuloCentral, aes(y=x,x=y), colour = colorLinea) +
        geom_point(aes(y = puntoPenDef , x = puntoCentral), colour = colorLinea) +
        geom_point(aes(y = puntoPenOf , x = puntoCentral), colour = colorLinea) +
        geom_point(aes(y = medioCampo , x = puntoCentral), colour = colorLinea)
    }else{
      pitch <- ggplot() + xlim(c(xmin, xmax)) + ylim(c(ymin,ymax)) +
        theme_blankPitch() +
        geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = colorCesped, colour =colorLinea) +
        geom_rect(aes(xmin=xmin, xmax=frontAreaDef, ymin= areaLineaIzq, ymax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
        geom_rect(aes(xmin=frontAreaOf, xmax=xmax, ymin=areaLineaIzq, ymax=areaLineaDer), fill = colorCesped, colour = colorLinea) +
        geom_segment(aes(x = medioCampo, y = ymin, xend = medioCampo, yend = ymax),colour = colorLinea) +
        geom_segment(aes(x = xmin, y = paloizq, xend = xmin, yend = paloDer),colour = colorGol, size = 2) +
        geom_segment(aes(x = xmax, y = paloizq, xend = xmax, yend = paloDer),colour = colorGol, size = 2) +
        geom_rect(aes(xmin=areaPeqOf, xmax=xmax, ymin=areaPeqIzq, ymax=areaPezDer), fill = colorCesped, colour = colorLinea) +
        geom_rect(aes(xmin=xmin, xmax=areaPeqDef, ymin= areaPeqIzq, ymax=areaPezDer), fill = colorCesped, colour = colorLinea) +
        geom_path(data=circuloCentral, aes(x=x,y=y), colour = colorLinea) +
        geom_point(aes(x = puntoPenDef , y = puntoCentral), colour = colorLinea) +
        geom_point(aes(x = puntoPenOf , y = puntoCentral), colour = colorLinea) +
        geom_point(aes(x = medioCampo , y = puntoCentral), colour = colorLinea)
    }
  }
  return(pitch)


}






