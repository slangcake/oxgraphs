#' ox_area_graph
#'
#' Draws area graphs
#'
#' If using data from an Oxford databank, forecast lines will be automatically added if you run your data through the
#'  \code{\link{hist_end}} function. The information used is stored in a data frame called h_end. Note this will not work if
#'  you have changed variable names, or for newly constructed variables.
#'
#' @param a Dataframe in long format containing 3 columns: Dates, variable and value
#' @param ttl Title of the graph
#' @param lh_units Units for the LHS axis
#' @param x_range Date range: If x_range is entered as a number, will run from 1st observation of the first year, to end of the last year. Will use exact dates if x_range is entered in d/m/yy format
#' @param y_range Y axis range
#' @param thm Chart theme - function that defines style of chart. Defaults to pre-2022 OE house style. 'ox_theme_html' is a valid input for the new html style publications
#' @param x_break Frequency of x axis ticks. Defaults to 1 year.
#' @param x_seq Frequency of labels on ticks. Defaults to every third tick
#' @param x_format Date format on x axis. Defaults to YYYY. See https://www.r-bloggers.com/date-formats-in-r/ for alternatives.
#' @param srce Defaults to "Source: Haver Analytics, BIS Oxford Economics"
#' @param leg Legend entries. Defaults to variable names
#' @param leg_pos Positioning of legend in cartesian coordinate format
#' @param leg_col Number of columns in the legend. Defaults to 1
#' @param FY X axis in financial years (defaults to CY)
#' @param fc Adds a forecast line. FC==1 or FC==2 places forecast label next to, or above forecast line respectively.
#' @param fc_date Date at which the forecast line appears
#' @param colours Change the order of the colour pallete. Input as numbers of the positions you want used first
#' @param hlines Horizontal lines you want added to the chart
#' @param hlinestyle The style of the horizontal lines. Defaults to solid. Use numbers to define alternate styles i.e. 2= dashed
#' @param var_order Order you'd like to plot the variables in. You need to specify ALL variables by name
#' @param no_forc Supresses the automated forecast line
#' @param no_leg Binary. Set to 1 if you want to supress the legend
#' @return h The graph as a list object (ggplot2)
#' @examples
#' \donttest{ox_area_graph(a,ttl='Your title',lh_units='Units',x_range=c(2010,2019),y_range=c(0,400,100))}
#'@export
ox_area_graph <- function(a,ttl,lh_units,x_range,y_range,x_break="1 year",srce="Source: Haver Analytics, BIS Oxford Economics",
                          leg=NULL,leg_pos=c(0.02,0.9),leg_col=1,FY=0,colours=NULL,x_seq=3,x_format="%Y",fc=0,fc_date=NULL,hlines=NULL,
                          no_leg=0,hlinestyle=1,no_forc=0,var_order=NULL,thm = 'ox_theme'){

  if(fc==1 & is.null(fc_date)){stop("If you're going to have a forecast line, you need to specify the forecast date")}
  th <- ifelse(thm=='ox_theme_html',ox_theme_html,ox_theme)
  #Define the colour pallette
  ox_colours <- ox_pallette()
  if(!is.null(colours)){ox_colours <- c(ox_colours[colours],ox_colours[-colours])}

  #Defining the x axis range
  if(is.numeric(x_range)){
    x_range <- x_rng(x_range,a,FY)} else {
      x_range <- as.Date(x_range,"%d/%m/%Y")
    }
  #If no forecast line is selected, this checks whether data from the Oxford model are being plotted, and automates a forecast line
  if(fc==0 & exists('h_end') & no_forc==0){
    dts <-h_end[which(h_end$variable %in% unique(a$variable)),]
    if(!is_empty(dts$hist_end)){
      md <- min(dts$hist_end)
      aa <- drop_na(select(a,Dates,variable,value))
      if(!is.na(md) & md < x_range[2] & md < max(aa$Dates)){
        fc=2
        fc_date=md}}}
  #The sequence for ticks and labels on the x-axis
  b_seq <- seq.Date(x_range[1],x_range[2],x_break)
  if(FY==0){l_seq<-as.character(b_seq,x_format)}else{l_seq<-as.character(b_seq%m+%years(1),x_format)}
  l_seq[c(FALSE,rep(TRUE,x_seq-1))] <- ""

  if(!is.null(var_order)){a$variable <- factor(a$variable,levels=var_order)
  if(length(var_order)!=length(unique(droplevels(a$variable)))){stop("Your variable order doesn't equal the number of variables you want to plot")}
  }

  #Building the plot

  h <- ggplot(a)+

    geom_area(aes(Dates,value,fill=variable),size=1.05833)+

    th(leg_pos)+

    scale_x_date(breaks=b_seq,labels=l_seq,limits=x_range[1:2],expand=c(0,0))+

    labs(y="",caption=srce,title=ttl,subtitle=lh_units)+

    scale_y_continuous(breaks=seq(y_range[1],y_range[2],y_range[3]),limits=c(y_range[1],y_range[2]),expand=c(0,0))

  if(fc==1){
    h <- h+geom_vline(xintercept = as.numeric(as.Date(fc_date,"%d/%m/%Y")),size=1,
                      color = ifelse(thm=='ox_theme_html',"#495057", "black")) +
      annotate("text",label="Forecast",y_range[2],x=as.Date(fc_date,"%d/%m/%Y"),hjust=-0.05,vjust=1,
               family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
               size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465*oxscale),
               color = ifelse(thm=='ox_theme_html',"#495057", "black"))

  }else
    if(fc==2){h <- h+geom_vline(xintercept = as.numeric(as.Date(fc_date,"%d/%m/%Y")),size=1,
                                color = ifelse(thm=='ox_theme_html',"#495057", "black")) +
      annotate("label",label="Forecast",label.size=0,y_range[2],x=as.Date(fc_date,"%d/%m/%Y"),hjust=0.5,vjust=0.5,
               family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
               size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465*oxscale),
               color = ifelse(thm=='ox_theme_html',"#495057", "black"))
    }

  if(is.null(leg)){h <- h+scale_fill_manual(values=ox_colours)}else{h <- h+scale_fill_manual(values=ox_colours,labels=leg)}

  if(leg_col!=1){h <- h+guides(fill=guide_legend(ncol=leg_col))}

  if(FY==1){
    h <- h+annotate("text",label="FY",y=y_range[1],x=x_range[2],hjust=1,vjust=3.2,
                    family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
                    size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465*oxscale),
                    color = ifelse(thm=='ox_theme_html',"#495057", "black"))}

  if(!is.null(hlines)){h <- h+geom_hline(yintercept=hlines,size=1*oxscale,linetype=hlinestyle,
                                         color = ifelse(thm=='ox_theme_html',"#495057", "black"))}

  if(no_leg==1){h <- h+theme(legend.position="none")}
  h <- titles_left(h)
  return(h)
}
