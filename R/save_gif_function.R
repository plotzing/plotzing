save_gif<-function(nameoffile, setwidth=NULL,setheight=NULL,width=NULL,height=NULL){
  require(gganimate)
  require(magick)
  anim_save(filename=nameoffile,animation=last_animation())
  message("Your animation has been saved in the following location:")
  message(getwd())
  message("\nNOTE: You must save your animation as a .gif file in order for animation to be preserved. Saving your file in a standard image format, such as .png, will not work.")
  if(!is.null(height)||!is.null(width)||!is.null(setheight)||!is.null(setwidth)){
    message("WARNING: You cannot adjust the width or height of animations using this function. To adjust the width or height of animations, use the setanimationheight= and setanimationwidth= functions when generating the animation using the graph_violin() or graph_scatterplot() functions.")
  }
}