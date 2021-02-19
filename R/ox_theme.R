#' ox_theme
#' 
#' Sets chart borders, font, title size and positioning
#' 
#'@export

ox_theme<- function(leg_pos,oxscale=1,flip=0){
  if(flip==1){pm <- margin(0,30,0,5)*oxscale}else{pm <- margin(0,10,0,5)*oxscale}
  theme_cowplot()+
    theme(text = element_text(angle=0,size=20*oxscale,face="plain",colour="black"),
          line = element_line(colour="black",size=1*oxscale),
          
          axis.title.y.left = element_text(angle = 0,margin=unit(c(0,-1.3,0,0.75)*oxscale, "cm")),
          axis.title.y.right = element_text(angle=0,vjust=1,hjust=0,margin=unit(c(0,0,0,-2.5)*oxscale, "cm")),
          axis.title.x =element_blank(),
          
          axis.ticks = element_line(size=1*oxscale),
          axis.ticks.length=unit(0.15*oxscale, "cm"),
          
          axis.text = element_text(angle = 0,colour="black",size=20*oxscale),
          axis.text.x = element_text(margin=unit(c(0.35,0.35,0,0.5)*oxscale, "cm")),
          axis.text.y = element_text(margin=unit(c(0.5,0.35,0.5,0.5)*oxscale, "cm")),
          axis.text.y.right= element_text(margin=unit(c(0.5,0.5,0.5,0.35)*oxscale, "cm")),
          
          axis.line.x = element_line(size=1*oxscale),
          axis.line.y = element_line(size=1*oxscale),
          
          legend.key = element_rect(colour=NA, fill=NA),
          legend.margin = margin(0,0,0,0),
          legend.text = element_text(size = 20*oxscale),
          legend.title = element_blank(),
          legend.background = element_rect(fill=alpha('white', 0)),
          legend.spacing.x = unit(0,'cm'),
          legend.spacing.y = unit(0,'cm'),
          legend.position=leg_pos,
          
          plot.title = element_text(margin=unit(c(0.2,0,0.15,0)*oxscale,"cm"),size=28*oxscale, face = "bold",hjust=0.0),
          plot.subtitle=element_text(hjust=0.0,margin=unit(c(0.15,0,0.5,0)*oxscale,"cm"),size=20*oxscale),
          plot.caption=element_text(hjust=0.0,size=18*oxscale,margin=unit(c(0,0,0.15,0)*oxscale,"cm")),
          plot.margin = pm,
          
          panel.background = element_rect(fill = "white")
    )
}