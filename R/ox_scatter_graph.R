#' ox_scatter_graph
#'
#' Draws scatter plots. Only allows one series per axis, but can be subset by time
#'
#' @param a Dataframe in long format containing 3 columns: Dates, variable and value
#' @param ttl Title of the graph
#' @param x_range X axis range
#' @param y_range Y axis range
#' @param srce Defaults to "Source: Haver Analytics, BIS Oxford Economics"
#' @param leg Legend entries. Defaults to variable names
#' @param leg_pos Positioning of legend in cartesian coordinate format
#' @param leg_col Number of columns in the legend. Defaults to 1
#' @param x_var The variable to go on the x axis. Defaults to first variable in the data frame
#' @param y_var The variable to go on the y axis. Defaults to second variable in the data frame
#' @param lobf Add a line of best fit. Binary
#' @param xlab Label for x axis. Defaults to first variable in the data frame
#' @param y_lab Label for y axis. Defaults to second variable in the data frame
#' @param colours Change the order of the colour pallete. Input as numbers of the positions you want used first
#' @param subs Date to split the sample at
#' @param no_leg Binary. Set to 1 if you want to supress the legend
#' @return h The graph as a list object (ggplot2)
#' @examples 
#' \donttest{ox_scatter_graph(x,"Labour market",x_range=c(4,8,1),y_range=c(1.5,4.5,0.5),xlab="Unemployment rate (%)",ylab="Wage growth (%)",subs=c('1/1/2008','1/1/2015'),connect=0,lobf=1)}
#'@export
ox_scatter_graph <- function(a,ttl,x_range,y_range,x_var=NULL,y_var=NULL,
                             srce="Source: Haver Analytics, BIS Oxford Economics",
                             leg=NULL,leg_pos=c(0.02,0.9),leg_col=1,subs=NULL,lobf=0,xlab=x_var,ylab=y_var,connect=0,
                             no_leg=0,colours=NULL){

  if(is.null(x_var)){x_var=levels(droplevels(a$variable))[1]} 
  if(is.null(y_var)){y_var=levels(droplevels(a$variable))[2]}

  #Define the colour pallette
  ox_colours <- ox_pallette()
  if(!is.null(colours)){ox_colours <- c(ox_colours[colours],ox_colours[-colours])}

  
  #Plot size and positioning
  scatter_theme<- function(){
    ox_theme(leg_pos)+
      theme(axis.title.y.left = element_text(angle = 90,margin=unit(c(0,0,0,0), "cm")),
            axis.title.x =element_text(angle = 0,margin=unit(c(0,0,0.25,0), "cm")),

            plot.subtitle=element_text(hjust=0.0,margin=unit(c(0.15,0,0.5,0),"cm"),size=0))
      }

  a <- spread(a,variable,value)
  drop_na(a)
  if(!is.null(subs)){
    a$id <- 1
    subs <- as.Date(subs,"%d/%m/%Y")
    
    for(i in seq(1,length(subs))){
    
    a$id[a$Dates>=subs[i]] <- i+1
    }
    a$id <- as.factor(a$id)
  }
  #Building the plot

  if(!is.null(subs)){
    h <- ggplot(a,aes(x=a[,x_var],y=a[,y_var],colour=a$id))+
      geom_point(size=2)+
      scatter_theme()+
      labs(y=ylab,x=xlab,caption=srce,title=ttl,subtitle="")#}
    if(lobf==1){h <- h+geom_smooth(method=lm,se=FALSE)}} else{

      h <- ggplot(a,aes(x=a[,x_var],y=a[,y_var]))+
        geom_point(colour="#BD1B21",size=2)+
        scatter_theme()+
        labs(y=ylab,x=xlab,caption=srce,title=ttl,subtitle="")#}
      if(lobf==1){h <- h+geom_smooth(method=lm,se=FALSE,colour="#BD1B21")}}


  if(is.null(leg)){h <- h+scale_colour_manual(values=ox_colours)}else{h <- h+scale_colour_manual(values=ox_colours,labels=leg)}

  h <- h+ scale_y_continuous(breaks=seq(y_range[1],y_range[2],y_range[3]),limits=c(y_range[1],y_range[2]),expand=c(0,0))

  h <- h+ scale_x_continuous(breaks=seq(x_range[1],x_range[2],x_range[3]),limits=c(x_range[1],x_range[2]),expand=c(0,0))

  h <- h+guides(col=guide_legend(override.aes = list(shape=15,size=5)))

  if(connect!=0){h <- h+geom_path()}

  if(leg_col!=1){h <- h+guides(col=guide_legend(ncol=leg_col))}
  if(no_leg==1){h <- h+theme(legend.position="none")}
  h <- titles_left(h)   #Edits can't be made after this step

  return(h)

}
