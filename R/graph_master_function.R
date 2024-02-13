graph_master <- function(dv=NULL,iv1=NULL,iv2=NULL,panelvariable=NULL,graphtype=NULL,setgraphtype=NULL,showlineplot=NULL,showbarplot=NULL,showscatterplot=NULL,showviolinplot=NULL, setbaroutlinecolor=NULL,setbarshorizontal=NULL,jitterheight=NULL,jitterwidth=NULL,setjitterheight=NULL,setjitterwidth=NULL,setjitter=NULL,internalfunctionautorotation=NULL,errorbars=NULL,seterrorbarthickness=NULL,splitx=NULL,splitgroup=NULL,splitpanel=NULL,showdata=NULL,showdatainback=NULL,bold=NULL,data=NULL,setdata=NULL,transparency=NULL,dottransparency=NULL,dotsize=NULL,title=NULL,settitle=NULL,setxaxislabel=NULL,setyaxislabel=NULL,setxaxistitle=NULL,setyaxistitle=NULL,colors=NULL,color1=NULL,color2=NULL,color3=NULL,color4=NULL,color5=NULL,color6=NULL,color7=NULL,color8=NULL,color9=NULL,color10=NULL,color=NULL,setcolor=NULL,setgrouplevels=NULL,setlegendlevels=NULL,level1=NULL,level2=NULL,level3=NULL,level4=NULL,level5=NULL,level6=NULL,level7=NULL,level8=NULL,level9=NULL,level10=NULL,legendtitle=NULL,titlesize=NULL,settitlesize=NULL,setxtitlesize=NULL,setytitlesize=NULL,setxaxissize=NULL,setyaxissize=NULL,setaxistextsize=NULL,setaxistitlesize=NULL,setlegendtitlesize=NULL,setlegendlevelsize=NULL,setpaneltitlesize=NULL,settitleface=NULL,setlegendtitleface=NULL,setxaxistitleface=NULL,setyaxistitleface=NULL,setpaneltitleface=NULL,seterrorbarwidth=NULL,seterrorbartransparency=NULL,showrotatedxlabels=NULL,rotatexaxislabels=NULL,setpanellevels=NULL,setxlevels=NULL,setystandardize=NULL,split1=NULL,split2=NULL,split3=NULL,setsplitx=NULL,setsplitgroup=NULL,setsplitpanel=NULL,seterrorbars=NULL,setcolors=NULL,showerrorbars=NULL,setlegendtitle=NULL,setdotsize=NULL,setdottransparency=NULL,setbaroutlinethickness=NULL,showcolorederrorbars=NULL,showspacebelowzero=NULL,setbartransparency=NULL,dodgewidth=NULL,setdodgewidth=NULL,setdotoutlinethickness=NULL,showdots=NULL,setdotoutlinecolor=NULL,showdotoutline=NULL,customdata=NULL,means=NULL,datapoints=NULL,setpositionhorizontalline=NULL,setpositiondottedhorizontalline=NULL,sethorizontallinecolor=NULL,sethorizontallinethickness=NULL,showanimation=NULL,setanimationid=NULL,setconfidencelevel=NULL,seterrorbarcolor=NULL,groupvariable=NULL,groupingvariable=NULL,showblackandwhitegraph=NULL,showdarkgraph=NULL,setreversecodex=NULL,setreversecodey=NULL,setreversecodegroup=NULL,setreversecodepanel=NULL,setreverseorderx=NULL,setreverseordergroup=NULL,setreverseorderlegend=NULL,setreverseorderpanel=NULL,reverseorderx=NULL,reverseordergroup=NULL,reverseorderlegend=NULL,reverseorderpanel=NULL,reversecodex=NULL,reversecodey=NULL,reversecodegroup=NULL,reversecodepanel=NULL,setxlevelorder=NULL,setgrouplevelorder=NULL,setpanellevelorder=NULL,setyaxisspacing=NULL,setyaxisend=NULL,setyaxisstart=NULL,showoutput=NULL,showgridlines=NULL,setlegendpositionleft=NULL,setlegendpositionbelow=NULL,setlegendpositionabove=NULL,showlegendleft=NULL,showlegendbelow=NULL,showlegendabove=NULL,showlegend=NULL,showboldedtitle=NULL,showboldedlegendtitle=NULL,showboldedxaxistitle=NULL,showboldedyaxistitle=NULL,showboldedpaneltitle=NULL,showboldedaxistitles=NULL,showline=NULL,showcolorblindgraph=NULL,showmeans=NULL,showintervalplot=NULL,setlinecapsize=NULL,setlinecapcolor=NULL,setlinecapoutlinecolor=NULL,setlinecapoutlinethickness=NULL,setlinethickness=NULL,setlinecolor=NULL,setviolinthickness=NULL,setviolintransparency=NULL,setboxplotcolor=NULL,setboxplotoutlinecolor=NULL,setboxplotthickness=NULL,setboxplottransparency=NULL,setboxplotwidth=NULL,showcoloredwhiskers=NULL,showcoloredboxplotoutline=NULL,showboxplot=NULL,showviolin=NULL,showcoloredshading=NULL,setshadingtransparency=NULL,setexclusionright=NULL,setexclusionabove=NULL,showshading=NULL,showloessline=NULL,setcustomintercept=NULL,setcustomslope=NULL,setlinetype=NULL,showdashedlines=NULL,showblankplot=NULL,...){

  require(ggplot2)

  settings_list <- list(setbaroutlinecolor=setbaroutlinecolor, setbarshorizontal=setbarshorizontal, jitterheight=jitterheight, jitterwidth=jitterwidth,
               setjitterheight=setjitterheight, setjitterwidth=setjitterwidth, setjitter=setjitter, internalfunctionautorotation=internalfunctionautorotation, errorbars=errorbars, seterrorbarthickness=seterrorbarthickness,
               splitx=splitx, splitgroup=splitgroup, splitpanel=splitpanel, showdata=showdata, showdatainback=showdatainback,bold=bold, data=data, setdata=setdata, transparency=transparency, dottransparency=dottransparency, dotsize=dotsize, title=title,
               settitle=settitle, setxaxislabel=setxaxislabel, setyaxislabel=setyaxislabel, setxaxistitle=setxaxistitle, setyaxistitle=setyaxistitle, colors=colors, color1=color1, color2=color2, color3=color3, color4=color4,
               color5=color5, color6=color6, color7=color7, color8=color8, color9=color9, color10=color10, color=color, setcolor=setcolor, setgrouplevels=setgrouplevels, setlegendlevels=setlegendlevels, level1=level1,
               level2=level2, level3=level3, level4=level4, level5=level5, level6=level6, level7=level7, level8=level8, level9=level9, level10=level10, legendtitle=legendtitle, titlesize=titlesize, settitlesize=settitlesize,
               setxtitlesize=setxtitlesize, setytitlesize=setytitlesize, setxaxissize=setxaxissize, setyaxissize=setyaxissize, setaxistextsize=setaxistextsize, setaxistitlesize=setaxistitlesize, setlegendtitlesize=setlegendtitlesize,
               setlegendlevelsize=setlegendlevelsize, setpaneltitlesize=setpaneltitlesize, settitleface=settitleface, setlegendtitleface=setlegendtitleface, setxaxistitleface=setxaxistitleface, setyaxistitleface=setyaxistitleface,
               setpaneltitleface=setpaneltitleface, seterrorbarwidth=seterrorbarwidth, seterrorbartransparency=seterrorbartransparency, showrotatedxlabels=showrotatedxlabels, rotatexaxislabels=rotatexaxislabels, setpanellevels=setpanellevels,
               setxlevels=setxlevels, setystandardize=setystandardize, split1=split1, split2=split2, split3=split3, setsplitx=setsplitx, setsplitgroup=setsplitgroup, setsplitpanel=setsplitpanel, seterrorbars=seterrorbars, setcolors=setcolors,
               showerrorbars=showerrorbars, setlegendtitle=setlegendtitle, setdotsize=setdotsize, setdottransparency=setdottransparency, setbaroutlinethickness=setbaroutlinethickness, showcolorederrorbars=showcolorederrorbars,
               showspacebelowzero=showspacebelowzero, setbartransparency=setbartransparency, dodgewidth=dodgewidth, setdodgewidth=setdodgewidth, setdotoutlinethickness=setdotoutlinethickness, showdots=showdots, setdotoutlinecolor=setdotoutlinecolor,
               showdotoutline=showdotoutline, customdata=customdata, means=means, datapoints=datapoints, setpositionhorizontalline=setpositionhorizontalline, setpositiondottedhorizontalline=setpositiondottedhorizontalline, sethorizontallinecolor=sethorizontallinecolor,
               sethorizontallinethickness=sethorizontallinethickness, showanimation=showanimation, setanimationid=setanimationid, setconfidencelevel=setconfidencelevel, seterrorbarcolor=seterrorbarcolor, groupvariable=groupvariable, groupingvariable=groupingvariable,
               showblackandwhitegraph=showblackandwhitegraph, showdarkgraph=showdarkgraph, setreversecodex=setreversecodex, setreversecodey=setreversecodey, setreversecodegroup=setreversecodegroup, setreversecodepanel=setreversecodepanel, setreverseorderx=setreverseorderx,
               setreverseordergroup=setreverseordergroup, setreverseorderlegend=setreverseorderlegend, setreverseorderpanel=setreverseorderpanel, reverseorderx=reverseorderx, reverseordergroup=reverseordergroup, reverseorderlegend=reverseorderlegend,
               reverseorderpanel=reverseorderpanel, reversecodex=reversecodex, reversecodey=reversecodey, reversecodegroup=reversecodegroup, reversecodepanel=reversecodepanel, setxlevelorder=setxlevelorder, setgrouplevelorder=setgrouplevelorder,
               setpanellevelorder=setpanellevelorder, setyaxisspacing=setyaxisspacing, setyaxisend=setyaxisend, setyaxisstart=setyaxisstart, showoutput=showoutput, showgridlines=showgridlines, setlegendpositionleft=setlegendpositionleft,
               setlegendpositionbelow=setlegendpositionbelow, setlegendpositionabove=setlegendpositionabove, showlegendleft=showlegendleft, showlegendbelow=showlegendbelow, showlegendabove=showlegendabove, showlegend=showlegend,
               showboldedtitle=showboldedtitle, showboldedlegendtitle=showboldedlegendtitle, showboldedxaxistitle=showboldedxaxistitle, showboldedyaxistitle=showboldedyaxistitle, showboldedpaneltitle=showboldedpaneltitle, showboldedaxistitles=showboldedaxistitles,
               showline=showline,showcolorblindgraph=showcolorblindgraph,showmeans=showmeans,showintervalplot=showintervalplot,setlinecapsize=setlinecapsize,setlinecapcolor=setlinecapcolor,setlinecapoutlinecolor=setlinecapoutlinecolor,setlinecapoutlinethickness=setlinecapoutlinethickness,
               setlinethickness=setlinethickness,setlinecolor=setlinecolor,setviolinthickness=setviolinthickness,setviolintransparency=setviolintransparency,setboxplotcolor=setboxplotcolor,setboxplotoutlinecolor=setboxplotoutlinecolor,setboxplotthickness=setboxplotthickness,
               setboxplottransparency=setboxplottransparency,setboxplotwidth=setboxplotwidth,showcoloredwhiskers=showcoloredwhiskers,showcoloredboxplotoutline=showcoloredboxplotoutline,showboxplot=showboxplot,showviolin=showviolin,showcoloredshading=showcoloredshading,
               setshadingtransparency=setshadingtransparency,setexclusionright=setexclusionright,setexclusionabove=setexclusionabove,showshading=showshading,showloessline=showloessline,setcustomintercept=setcustomintercept,setcustomslope=setcustomslope,setlinetype=setlinetype,
               showdashedlines=showdashedlines,showblankplot=showblankplot)

  #Remove null entries
  settings_list <- Filter(function(x) !is.null(x), settings_list)

  if(!is.null(setgraphtype)){
    graphtype<-setgraphtype
  }

  # Initialize lists
  results <- list()
  variables <- list()
  combined_settings <- list()

  #Potential errors
  if(is.null(dv)){
    message("ERROR: You must specify one or more dependent variables using dv=")
    stop()
  }

  if(length(dv)==1&&is.null(iv1)){
    message("ERROR: You are specifying one dependent variable, but you must also specify an independent variable using iv1='variablename' or reshape multiple dvs using dv=c('variable1','variable2')")
    stop()
  }

  if(is.list(dv)==TRUE&&is.null(iv1)){
    message("ERROR: You are specifying multiple dependent variables, but you must also specify an independent variable using iv1='variablename'. If you trying to reshape your dependent variables to generate your independent variable, you may use c() instead of list(), as in the following example: dv=c('variable1','variable2')")
    stop()
  }

  counter<-0

  if(is.list(dv)==TRUE){
    counter<-counter+1
  }

  if(is.list(iv1)==TRUE){
    counter<-counter+1
  }

  if(is.list(iv2)==TRUE){
    counter<-counter+1
  }

  if(is.list(panelvariable)==TRUE){
    counter<-counter+1
  }

  if(counter>=2){
    message("ERROR: You may specify a list of multiple dependent variables, multiple x-axis variables, multiple group (color) variables, or multiple panel variables. However, you cannot specify multiple lists. For instance, you cannot specify a list of color variables AND a list of panel variables simultaneously (as you must pick one or the other). If you wish to use a list of variables and also reshape your dependent variable, use dv=c() rather than dv=list().")
    stop()
  }

  if(!is.null(showlineplot)){
    if(showlineplot==TRUE){
      graphtype<-"line"
    }
  }

  if(!is.null(showbarplot)){
    if(showbarplot==TRUE){
      graphtype<-"bar"
    }
  }

  if(!is.null(showviolinplot)){
    if(showviolinplot==TRUE){
      graphtype<-"violin"
    }
  }

  if(!is.null(showscatterplot)){
    if(showscatterplot==TRUE){
      graphtype<-"scatter"
    }
  }

  if(is.null(showlineplot)&&is.null(showscatterplot)&&is.null(showbarplot)&&is.null(showviolinplot)&&is.null(graphtype)&&!is.null(showviolin)){
    message("ERROR: The command showviolin is used to show or hide the violins produced when generating violin plots (to show only the boxplots, for example). Did you mean to use showviolinplot=TRUE instead?")
    stop()
  }

  if(is.null(showlineplot)&&is.null(showscatterplot)&&is.null(showbarplot)&&is.null(showviolinplot)&&is.null(graphtype)&&!is.null(showline)){
    message("ERROR: The command showline is used to show or hide lines when using scatterplots and line graphs. Did you mean to use showlineplot=TRUE instead?")
    stop()
  }

  if(is.null(graphtype)){
    message("ERROR: You must specify a graphtype using graphtype=. For example, graphtype='violin'")
    stop()
  }
  if(!is.null(showanimation)){
    if(showanimation==TRUE){
      message("ERROR: You cannot set showanimation to TRUE when using this function.")
      stop()
    }
  }

  if(!is.null(dv)){
  if(is.list(dv)==FALSE&&is.list(iv1)==FALSE&&is.list(iv2)==FALSE&&is.list(panelvariable)==FALSE&&is.list(dv)==FALSE) {
    # Get the graphing variables if specified by the user
    variables <- list(dv = dv, iv1 = iv1, iv2 = iv2, panelvariable = panelvariable)
    combined_settings <- modifyList(variables, settings_list)

    if(!is.null(graphtype)){
      if(graphtype=="line"||graphtype=="l"||graphtype=="lineplot"||graphtype=="line_plot"||graphtype=="Lineplot"||graphtype=="Line_plot"||graphtype=="L"||current_graphtype=="Line"){
        showlineplot<-TRUE
      }
      if(graphtype=="bar"||graphtype=="b"||graphtype=="barplot"||graphtype=="bar_plot"||graphtype=="Barplot"||graphtype=="Bar"||graphtype=="B"||graphtype=="Bar_plot"){
        showbarplot<-TRUE
      }
      if(graphtype=="scatter"||graphtype=="s"||graphtype=="scatterplot"||graphtype=="scatter_plot"||graphtype=="Scatterplot"||graphtype=="Scatter"||current_graphtype=="S"||current_graphtype=="Scatter_plot"){
        showscatterplot<-TRUE
      }
      if(graphtype=="violin"||graphtype=="v"||graphtype=="violinplot"||graphtype=="violin_plot"||graphtype=="Violin"||graphtype=="Violin_plot"||graphtype=="V"||graphtype=="Violinplot"){
        showviolinplot<-TRUE
      }
    }

    if (!is.null(showlineplot) && showlineplot==TRUE) {
      results <- do.call("graph_line", combined_settings)
    }
    if (!is.null(showbarplot) && showbarplot==TRUE) {
      results <- do.call("graph_bar", combined_settings)
    }
    if (!is.null(showviolinplot) && showviolinplot==TRUE) {
      results <- do.call("graph_violin", combined_settings)
    }
    if (!is.null(showscatterplot) && showscatterplot==TRUE) {
      results <- do.call("graph_scatterplot", combined_settings)
    }
  }
    if(is.list(iv1)==TRUE&&is.list(iv2)==FALSE&&is.list(panelvariable)==FALSE&&is.list(panelvariable)==FALSE){
      iv1<-unlist(iv1)
      iv1<-as.vector(iv1)

      for (i in 1:length(iv1)) {
        variables[[i]] <- list(dv = dv, iv1 = iv1[[i]], iv2=iv2, panelvariable=panelvariable)
        combined_settings[[i]] <- modifyList(variables[[i]], settings_list)

        if(!is.null(graphtype)){
          if(length(graphtype)>1){
            current_graphtype<-graphtype[i]
          }
          if(length(graphtype)==1){
            current_graphtype<-graphtype
          }
          if(current_graphtype=="line"||current_graphtype=="l"||current_graphtype=="lineplot"||current_graphtype=="line_plot"||current_graphtype=="Lineplot"||current_graphtype=="Line_plot"||current_graphtype=="L"||current_graphtype=="Line"){
            results[[i]]<-do.call("graph_line",combined_settings[[i]])
          }
          if(current_graphtype=="bar"||current_graphtype=="b"||current_graphtype=="barplot"||current_graphtype=="bar_plot"||current_graphtype=="Barplot"||current_graphtype=="Bar"||current_graphtype=="B"||current_graphtype=="Bar_plot"){
            results[[i]]<-do.call("graph_bar",combined_settings[[i]])
          }
          if(current_graphtype=="scatter"||current_graphtype=="s"||current_graphtype=="scatterplot"||current_graphtype=="scatter_plot"||current_graphtype=="Scatterplot"||current_graphtype=="Scatter"||current_graphtype=="S"||current_graphtype=="Scatter_plot"){
            results[[i]]<-do.call("graph_scatterplot",combined_settings[[i]])
          }
          if(current_graphtype=="violin"||current_graphtype=="v"||current_graphtype=="violinplot"||current_graphtype=="violin_plot"||current_graphtype=="Violin"||current_graphtype=="Violin_plot"||current_graphtype=="V"||current_graphtype=="Violinplot"){
            results[[i]]<-do.call("graph_violin",combined_settings[[i]])
          }
        }
        # Run the appropriate graphing function with the combined settings
        if (!is.null(showlineplot) && showlineplot==TRUE) {
          results[[i]] <- do.call("graph_line", combined_settings[[i]])
        }
        if (!is.null(showbarplot) && showbarplot==TRUE) {
          results[[i]] <- do.call("graph_bar", combined_settings[[i]])
        }
        if (!is.null(showviolinplot) && showviolinplot==TRUE) {
          results[[i]] <- do.call("graph_violin", combined_settings[[i]])
        }
        if (!is.null(showscatterplot) && showscatterplot==TRUE) {
          results[[i]] <- do.call("graph_scatterplot", combined_settings[[i]])
        }
      }
    }

    if(is.list(iv2)==TRUE&&is.list(iv1)==FALSE&&is.list(panelvariable)==FALSE&&is.list(dv)==FALSE){
      iv2<-unlist(iv2)
      iv2<-as.vector(iv2)

      for (i in 1:length(iv2)) {
        variables[[i]] <- list(dv = dv, iv1 = iv1, iv2=iv2[[i]], panelvariable=panelvariable)
        combined_settings[[i]] <- modifyList(variables[[i]], settings_list)

        if(!is.null(graphtype)){
          if(length(graphtype)>1){
            current_graphtype<-graphtype[i]
          }
          if(length(graphtype)==1){
            current_graphtype<-graphtype
          }
          if(current_graphtype=="line"||current_graphtype=="l"||current_graphtype=="lineplot"||current_graphtype=="line_plot"||current_graphtype=="Lineplot"||current_graphtype=="Line_plot"||current_graphtype=="L"||current_graphtype=="Line"){
            results[[i]]<-do.call("graph_line",combined_settings[[i]])
          }
          if(current_graphtype=="bar"||current_graphtype=="b"||current_graphtype=="barplot"||current_graphtype=="bar_plot"||current_graphtype=="Barplot"||current_graphtype=="Bar"||current_graphtype=="B"||current_graphtype=="Bar_plot"){
            results[[i]]<-do.call("graph_bar",combined_settings[[i]])
          }
          if(current_graphtype=="scatter"||current_graphtype=="s"||current_graphtype=="scatterplot"||current_graphtype=="scatter_plot"||current_graphtype=="Scatterplot"||current_graphtype=="Scatter"||current_graphtype=="S"||current_graphtype=="Scatter_plot"){
            results[[i]]<-do.call("graph_scatterplot",combined_settings[[i]])
          }
          if(current_graphtype=="violin"||current_graphtype=="v"||current_graphtype=="violinplot"||current_graphtype=="violin_plot"||current_graphtype=="Violin"||current_graphtype=="Violin_plot"||current_graphtype=="V"||current_graphtype=="Violinplot"){
            results[[i]]<-do.call("graph_violin",combined_settings[[i]])
          }
        }
        # Run the appropriate graphing function with the combined settings
        if (!is.null(showlineplot) && showlineplot==TRUE) {
          results[[i]] <- do.call("graph_line", combined_settings[[i]])
        }
        if (!is.null(showbarplot) && showbarplot==TRUE) {
          results[[i]] <- do.call("graph_bar", combined_settings[[i]])
        }
        if (!is.null(showviolinplot) && showviolinplot==TRUE) {
          results[[i]] <- do.call("graph_violin", combined_settings[[i]])
        }
        if (!is.null(showscatterplot) && showscatterplot==TRUE) {
          results[[i]] <- do.call("graph_scatterplot", combined_settings[[i]])
        }
      }
    }

    if(is.list(panelvariable)==TRUE&&is.list(iv1)==FALSE&&is.list(iv2)==FALSE&&is.list(dv)==FALSE){
      panelvariable<-unlist(panelvariable)
      panelvariable<-as.vector(panelvariable)

      for (i in 1:length(panelvariable)) {
        variables[[i]] <- list(dv = dv, iv1 = iv1, iv2=iv2, panelvariable=panelvariable[[i]])
        combined_settings[[i]] <- modifyList(variables[[i]], settings_list)

        if(!is.null(graphtype)){
          if(length(graphtype)>1){
            current_graphtype<-graphtype[i]
          }
          if(length(graphtype)==1){
            current_graphtype<-graphtype
          }
          if(current_graphtype=="line"||current_graphtype=="l"||current_graphtype=="lineplot"||current_graphtype=="line_plot"||current_graphtype=="Lineplot"||current_graphtype=="Line_plot"||current_graphtype=="L"||current_graphtype=="Line"){
            results[[i]]<-do.call("graph_line",combined_settings[[i]])
          }
          if(current_graphtype=="bar"||current_graphtype=="b"||current_graphtype=="barplot"||current_graphtype=="bar_plot"||current_graphtype=="Barplot"||current_graphtype=="Bar"||current_graphtype=="B"||current_graphtype=="Bar_plot"){
            results[[i]]<-do.call("graph_bar",combined_settings[[i]])
          }
          if(current_graphtype=="scatter"||current_graphtype=="s"||current_graphtype=="scatterplot"||current_graphtype=="scatter_plot"||current_graphtype=="Scatterplot"||current_graphtype=="Scatter"||current_graphtype=="S"||current_graphtype=="Scatter_plot"){
            results[[i]]<-do.call("graph_scatterplot",combined_settings[[i]])
          }
          if(current_graphtype=="violin"||current_graphtype=="v"||current_graphtype=="violinplot"||current_graphtype=="violin_plot"||current_graphtype=="Violin"||current_graphtype=="Violin_plot"||current_graphtype=="V"||current_graphtype=="Violinplot"){
            results[[i]]<-do.call("graph_violin",combined_settings[[i]])
          }
        }
        # Run the appropriate graphing function with the combined settings
        if (!is.null(showlineplot) && showlineplot==TRUE) {
          results[[i]] <- do.call("graph_line", combined_settings[[i]])
        }
        if (!is.null(showbarplot) && showbarplot==TRUE) {
          results[[i]] <- do.call("graph_bar", combined_settings[[i]])
        }
        if (!is.null(showviolinplot) && showviolinplot==TRUE) {
          results[[i]] <- do.call("graph_violin", combined_settings[[i]])
        }
        if (!is.null(showscatterplot) && showscatterplot==TRUE) {
          results[[i]] <- do.call("graph_scatterplot", combined_settings[[i]])
        }
      }
    }

  if(is.list(dv)==TRUE){
    dv<-unlist(dv)
    dv<-as.vector(dv)

    for (i in 1:length(dv)) {
      variables[[i]] <- list(dv = dv[[i]], iv1 = iv1, iv2=iv2, panelvariable=panelvariable)
      combined_settings[[i]] <- modifyList(variables[[i]], settings_list)

      if(!is.null(graphtype)){
        if(length(graphtype)>1){
          current_graphtype<-graphtype[i]
        }
        if(length(graphtype)==1){
          current_graphtype<-graphtype
        }
          if(current_graphtype=="line"||current_graphtype=="l"||current_graphtype=="lineplot"||current_graphtype=="line_plot"||current_graphtype=="Lineplot"||current_graphtype=="Line_plot"||current_graphtype=="L"||current_graphtype=="Line"){
            results[[i]]<-do.call("graph_line",combined_settings[[i]])
          }
          if(current_graphtype=="bar"||current_graphtype=="b"||current_graphtype=="barplot"||current_graphtype=="bar_plot"||current_graphtype=="Barplot"||current_graphtype=="Bar"||current_graphtype=="B"||current_graphtype=="Bar_plot"){
            results[[i]]<-do.call("graph_bar",combined_settings[[i]])
          }
          if(current_graphtype=="scatter"||current_graphtype=="s"||current_graphtype=="scatterplot"||current_graphtype=="scatter_plot"||current_graphtype=="Scatterplot"||current_graphtype=="Scatter"||current_graphtype=="S"||current_graphtype=="Scatter_plot"){
            results[[i]]<-do.call("graph_scatterplot",combined_settings[[i]])
          }
          if(current_graphtype=="violin"||current_graphtype=="v"||current_graphtype=="violinplot"||current_graphtype=="violin_plot"||current_graphtype=="Violin"||current_graphtype=="Violin_plot"||current_graphtype=="V"||current_graphtype=="Violinplot"){
            results[[i]]<-do.call("graph_violin",combined_settings[[i]])
          }
      }
      # Run the appropriate graphing function with the combined settings
      if (!is.null(showlineplot) && showlineplot==TRUE) {
        results[[i]] <- do.call("graph_line", combined_settings[[i]])
      }
      if (!is.null(showbarplot) && showbarplot==TRUE) {
        results[[i]] <- do.call("graph_bar", combined_settings[[i]])
      }
      if (!is.null(showviolinplot) && showviolinplot==TRUE) {
        results[[i]] <- do.call("graph_violin", combined_settings[[i]])
      }
      if (!is.null(showscatterplot) && showscatterplot==TRUE) {
        results[[i]] <- do.call("graph_scatterplot", combined_settings[[i]])
      }
    }
  }
    message("\n NOTE: Click through the arrows in the plotting panel (typically located on the right-hand side) to see all your graphs.")
    return(results)
  }
}
