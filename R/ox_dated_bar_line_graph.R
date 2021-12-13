#' ox_dated_bar_line_graph
#'
#' Draws dated bar graphs with a line series
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
#' @param line_ser The series you want plotted as a line
#' @param x_break Frequency of x axis ticks. Defaults to 1 year.
#' @param x_seq Frequency of labels on ticks. Defaults to every third tick
#' @param x_format Date format on x axis. Defaults to YYYY. See https://www.r-bloggers.com/date-formats-in-r/ for alternatives.
#' @param srce Defaults to "Source: Haver Analytics, BIS Oxford Economics"
#' @param leg Legend entries. Defaults to variable names
#' @param line_leg Legend for the line series. Defaults to 'Total'
#' @param leg_pos Positioning of legend in cartesian coordinate format
#' @param leg_col Number of columns in the legend. Defaults to 1
#' @param fc Adds a forecast line. FC==1 or FC==2 places forecast label next to, or above forecast line respectively.
#' @param fc_date Date at which the forecast line appears
#' @param y2_range Range for a second y axis
#' @param rh_units Units for second y axis
#' @param nudge_rh_units Positioning of rh axis label;ling can be messy - this nudges the label left/right
#' @param rhs_var Variable to be plotted on RHS axis
#' @param FY X axis in financial years (defaults to CY)
#' @param colours Change the order of the colour pallete. Input as numbers of the positions you want used first
#' @param eventdate Vertical line to signify a date 'event'  N.B. multiple events can be plotted by concatenating dates/labels etc
#' @param eventlinestyle The style of the vertical lines. Defaults to solid. "dashed" is a valid entry
#' @param event Label for your event
#' @param event_ypos Where the event label shows up on the y_axis (defaults to top of range)
#' @param eventhjust Nudge to event label left/right
#' @param var_order Order you'd like to plot the column variables in. You need to specify ALL variables by name
#' @param no_leg Binary. Set to 1 if you want to supress the legend
#' @param hlines Horizontal lines you want added to the chart
#' @param hlinestyle The style of the horizontal lines. Defaults to solid. Use numbers to define alternate styles i.e. 2= dashed
#' @param no_forc Supresses the automated forecast line
#' @param no_zero Supresses zero line
#' @param date_adj By default, dates are shifted so bars sit between ticks. This can be turned off (useful for high frequency charts)
#' @return h The graph as a list object (ggplot2)
#' @examples
#' \donttest{ox_dated_bar_line_graph(x,'Consumption','%, y/y',c(2012,2019),c(0,4,1),line_ser='line_series')}
#'@export

ox_dated_bar_line_graph <- function(a,ttl,lh_units,x_range,y_range,line_ser=NULL,x_break="1 year",srce="Source: Haver Analytics, BIS Oxford Economics",
                          leg=NULL,line_leg="Total",leg_pos=c(0.02,0.9),leg_col=1,fc=0,fc_date=NULL,y2_range=NULL,
                          no_leg=0,rh_units=lh_units,nudge_rh_units=0,rhs_var=NULL,FY=0,colours=NULL,hlines=NULL,
                          eventdate=NULL,eventlinestyle=1,event="Specify an event title",eventhjust=-0.02,event_ypos=y_range[2],
                          hlinestyle=1,x_seq=3,x_format="%Y",var_order=NULL,no_forc=0,date_adj=1,no_zero=0,thm = 'ox_theme'){

  th <- ifelse(thm=='ox_theme_html',ox_theme_html,ox_theme)
  #Some checks
  if(fc==1 & is.null(fc_date)){stop("If you're going to have a forecast line, you need to specify the forecast date")}
  if(is.null(y2_range)){second_axis <- 0}else{second_axis <- 1}
  if(second_axis==1 & is.null(rhs_var)){stop("If you're going to have a second axis, you need to specify at least one variable as the rhs_var")}
  if(is.null(line_ser)){stop("Need to specify a line series")}

  #Defining functions used within this function

  ox_colours <- ox_pallette()

  if(!is.null(colours)){ox_colours <- c(ox_colours[colours],ox_colours[-colours])}

  l <- length(line_ser)
  b <- length(unique(a$variable)) - l

  ox_colours_bar <- ox_colours[1:b]
  ox_colours_line <- ox_colours[b+1:l]

  if(second_axis==1){
    # Renaming and rescaling the variables that are on the RHS
    # Work out the linear transformation from primary to secondary axis

    y1n <- y_range[1] #first axis min
    y2n <- y2_range[1] #second axis min
    y1x <- y_range[2] #first axis max
    y2x <- y2_range[2] #second axis max

    a2 <- (y1x*y2n-y2x*y1n)/(y2n-y2x)
    if(y2n==0){a1 <- (y1x-a2)/y2x}else{a1 <- (y1n-a2)/y2n}

    trans <- ~ (. - a2) / a1

    for (j in rhs_var){
      a$value[a$variable==j] <- a$value[a$variable==j]*a1+a2
      # levels(a$variable)[levels(a$variable)==j] <- paste0(j," (RHS)")
    }
  }
  #Defining the x axis range

  if(is.numeric(x_range)){
    x_range <- x_rng(x_range,a,FY,bar=1)} else {
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
        fc_date <- md
        mg <- month(a$Dates[2])-month(a$Dates[1])

        if(mg==3|-9){fc_date <- fc_date%m-%months(2)}
        if(mg==0){
          ld <- a$Dates[a$Dates<=md] %>% max()
          fc_date <- ld%m+%months(1)}
      }}}

  #The sequence for ticks and labels on the x-axis
  b_seq <- seq.Date(x_range[1],x_range[2],x_break)
  if(FY==0){l_seq<-as.character(b_seq,x_format)}else{l_seq<-as.character(b_seq%m+%years(1),x_format)}
  l_seq[c(FALSE,rep(TRUE,x_seq-1))] <- ""

  # Changing dates so that bars sit between tick marks
  if(date_adj==1){
  b <- spread(a,variable,value)   #Convert to wide format

  eop <- b$Dates
  day(eop) <- days_in_month(eop)  #End of period dates
  dv <- round(as.numeric(diff(eop))/2)  %>%
    c(round(mean(.)),.)   #Vector of difference between dates. Take the mean of the vector for the first element
  b$Dates <- eop-dv   #Adjust the dates column by subtracting the difference

  a <- melt(b,"Dates")}  #Back to long format

  bars <- a[which(!(a$variable %in% line_ser)),] %>%droplevels(.)

  if(!is.null(var_order)){
    if(length(var_order)!=length(unique(bars$variable))){stop("Your variable order doesn't equal the number of variables you want to plot")}
    bars$variable <- factor(bars$variable,levels=var_order)}

  lns <- a[which(a$variable %in% line_ser),]

  #Building the plot

  h <- ggplot()+

    geom_col(data=bars,aes(Dates,value,fill=variable),size=1.05833)+

    geom_line(data=lns,aes(Dates,value,colour=variable),size=1.05833)+

    th(leg_pos)+
    theme(legend.spacing=unit(-0.2,"cm"))+

    scale_x_date(breaks=b_seq,labels=l_seq,limits=x_range[1:2],expand=c(0,0))+

    labs(y="",caption=srce,title=ttl,subtitle=lh_units)


  if(is.null(line_leg)){h <- h+scale_colour_manual(values=ox_colours_line)}else{h <- h+scale_colour_manual(values=ox_colours_line,labels=line_leg)}
  if(is.null(leg)){h <- h+scale_fill_manual(values=ox_colours_bar)}else{h <- h+scale_fill_manual(values=ox_colours_bar,labels=leg)}

  if(second_axis==1){

    h <- h+ scale_y_continuous(breaks=seq(y_range[1],y_range[2],y_range[3]),limits=c(y_range[1],y_range[2]),expand=c(0,0),
                               sec.axis = sec_axis(trans=trans,breaks=seq(y2_range[1],y2_range[2],y2_range[3])))+
      annotate("text",label=rh_units,y=y_range[2],x=x_range[2],hjust=0.5+nudge_rh_units,vjust=-1,
               family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
               size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465*oxscale),
               color = ifelse(thm=='ox_theme_html',"#495057", "black"))}
  else{h <- h+ scale_y_continuous(breaks=seq(y_range[1],y_range[2],y_range[3]),limits=c(y_range[1],y_range[2]),expand=c(0,0))}

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

  if(y_range[1]<0 & y_range[2]>0 & no_zero==0){
    h <- h+geom_hline(yintercept = 0,size=1*oxscale,color = ifelse(thm=='ox_theme_html',"#495057", "black"))}

  if(!is.null(hlines)){h <- h+geom_hline(yintercept=hlines,size=1,linetype=hlinestyle)}
  if(leg_col!=1){h <- h+guides(fill=guide_legend(ncol=leg_col))}

  if(!is.null(eventdate)){h <- h+geom_vline(xintercept = as.numeric(as.Date(eventdate,"%d/%m/%Y")),size=1,linetype=eventlinestyle) +
    # annotate("text",label=event,event_ypos,x=as.Date(eventdate,"%d/%m/%Y"),size=20/2.83465,hjust=eventhjust,vjust=1)}
    annotate("text",label=event,event_ypos,x=as.Date(eventdate,"%d/%m/%Y"),size=20/2.83465,hjust=eventhjust,vjust=1)}

  if(FY==1){
    h <- h+annotate("text",label="FY",y=y_range[1],x=x_range[2],hjust=1,vjust=3.2,
                    family = ifelse(thm=='ox_theme_html',"Segoe UI",""),
                    size = ifelse(thm=='ox_theme_html',18/2.83465, 20/2.83465*oxscale),
                    color = ifelse(thm=='ox_theme_html',"#495057", "black"))}

  if(no_leg==1){h <- h+theme(legend.position="none")}
  h <- titles_left(h)
  return(h)
}
