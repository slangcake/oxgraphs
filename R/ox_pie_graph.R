#' ox_pie_graph
#'
#' Draws a pie chart
#' @param a Dataframe in long format containing 2 columns: variable and value
#' @param ttl Title of the graph
#' @param lh_units Units for the LHS axis
#' @param srce Defaults to "Source: Haver Analytics, Oxford Economics"
#' @param leg Legend entries. Defaults to variable names
#' @param leg_pos Positioning of legend in cartesian coordinate format
#' @param leg_col Number of columns in the legend. Defaults to 1
#' @param colours Change the order of the colour pallete. Input as numbers of the positions you want used first
#' @return h The graph as a list object (ggplot2)
#'@export
ox_pie_graph <- function(a,ttl,lh_units="",srce="Source: Haver Analytics, Oxford Economics",
                            leg=NULL,leg_pos=c(0.02,0.9),leg_col=1,colours=NULL){

  #The best way to do this will be to create a stacked column chart, with everything in one column
  #(one option is to add a$category <- "1" to the data frame)
  #Then, it's a matter of converting the chart to polar coordinates: h <- h+coord_polar("y",start=0)
  #Need to set the max of the y axis to the sum of the variables, otherwise you get an incomplete pie

 #Then it's a matter of removing the axes, putting back the desired labels and working out the legend

  #Define the colour pallette
  ox_colours <- ox_pallette()
  if(!is.null(colours)){ox_colours <- c(ox_colours[colours],ox_colours[-colours])}

  #Plot size and positioning
  ox_pie_theme<- function(){
    theme_cowplot()+
      theme(text = element_text(angle=0,size=20,face="plain",colour="black"),
            line = element_line(colour="black",size=1),

            axis.title.y=element_blank(),
            axis.title.x =element_blank(),

            axis.ticks = element_blank(),

            axis.text = element_blank(),

            axis.line.x = element_blank(),
            axis.line.y = element_blank(),

            legend.key = element_rect(colour=NA, fill=NA),
            legend.margin = margin(0,0,0,0),
            legend.text = element_text(size = 20),
            legend.title = element_blank(),
            legend.background = element_rect(fill=alpha('white', 0)),
            legend.position=leg_pos,

            plot.title = element_text(margin=unit(c(0.2,0,0.15,0),"cm"),size=28, face = "bold",hjust=0.0),
            #plot.subtitle=element_text(hjust=0.0,margin=unit(c(0.15,0,0.5,0),"cm"),size=20),
            plot.caption=element_text(hjust=0.0,size=18,margin=unit(c(0,0,0.15,0),"cm")),
            plot.margin = margin(0,0,0,0),

            panel.background = element_rect(fill = "white")
      )
  }

  #Building the plot
  a$category <- 1

  h <- ggplot(a,aes(fill=variable))+
    geom_col(aes(category,value),colour="white",size=0.75)+
    ox_pie_theme()+
    #theme_void()+
    labs(y=lh_units,caption=srce,title=ttl)


  if(is.null(leg)){h <- h+scale_fill_manual(values=ox_colours)}else{h <- h+scale_fill_manual(values=ox_colours,labels=leg)}

  h <- h+ scale_y_continuous(limits=c(0,sum(a$value)),expand=c(0,0))

  if(leg_col!=1){h <- h+guides(fill=guide_legend(ncol=leg_col))}

  h <- h+coord_polar("y",start=0)

  h <- titles_left(h)

  return(h)
}
