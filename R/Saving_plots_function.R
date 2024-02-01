#### SAVING PLOTS ####
#Save graphs#
save_graph<-function(nameoffile,setwidth=NULL,setheight=NULL,setunits=NULL,width=NA,height=NA,units="in",dpi=500){
  require(ggplot2)
  if(!is.null(setwidth)){
    width<-setwidth
  }
  if(!is.null(setheight)){
    height<-setheight
  }
  if(!is.null(setunits)){
    units<-setunits
  }
  ggsave(filename = nameoffile,plot=last_plot(),units=units,height=height,width=width,dpi=dpi)
  message("Your graph has been saved in the following location:")
  message(getwd())
}
