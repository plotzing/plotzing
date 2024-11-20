#### LINE PLOT ####
graph_line<-function(dv=NULL,iv1=NULL,iv2=NULL,panelvariable=NULL,showdata=TRUE,setystandardize=FALSE,jitterheight=0.2,jitterwidth=0.2,setjitter=NULL,setjitterheight=NULL,setjitterwidth=NULL,dotsize=.8,dottransparency=NULL,transparency=NULL,errorbars="default",setidvariable=NULL,splitx=FALSE,splitgroup=FALSE,splitlegend=NULL,splitpanel=FALSE,bold=FALSE,setlinecapcolor=NULL,setlinecapsize=4.5,showlinecaps=TRUE,showmeans=NULL,title=NULL,settitle=NULL,setxaxislabel=NULL,setyaxislabel=NULL,setxaxistitle=NULL,setyaxistitle=NULL,legendtitle=NULL,linecolor=NULL,dotcolor=NULL,setlinecolor="#1D4E5D",setdotcolor="#3BA0BF",data=df,setdata=NULL,setgrouplevels=NULL,setlegendlevels=NULL,colors=NULL,color1=-1,color2=-2,color3=-1,color4=-1,color5=-1,color6=-1,color7=-1,color8=-1,color9=-1,color10=-1,level1=NULL,level2=NULL,level3=NULL,level4=NULL,level5=NULL,level6=NULL,level7=NULL,level8=NULL,level9=NULL,level10=NULL,highlightabove=-1,titlesize=NULL,settitlesize=NULL,setxaxistitlesize=NULL,setyaxistitlesize=NULL,setxaxistextsize=NULL,setyaxistextsize=NULL,setaxistextsize=NULL,setaxistitlesize=NULL,setxtitlesize=NULL,setytitlesize=NULL,setxaxissize=NULL,setyaxissize=NULL,setlegendtitlesize=NULL,setlegendtextsize=NULL,setlegendlevelsize=NULL,setpaneltitlesize=NULL,showcolorederrorbars=NULL,colorful=FALSE,seterrorbarcolor=NULL,seterrorbarwidth=.1,seterrorbartransparency=NULL,showrotatedxlabels=FALSE,rotatexaxislabels=FALSE,setpanellevels=NULL,setxlevels=NULL,setxaxislevels=NULL,split1=FALSE,split2=FALSE,split3=FALSE,showerrorbars=TRUE,seterrorbars=NULL,setdotsize=NULL,setdottransparency=NULL,linethickness=NULL,setlinethickness=1.15,seterrorbarthickness=NULL,setlegendtitle=NULL,setsplitx=NULL,setsplitgroup=NULL,setsplitlegend=NULL,setsplitpanel=NULL,setcolors=NULL,dodgewidth=0.9,setdodgewidth=NULL,setdotoutlinethickness=0.5,setdotoutlinecolor=NULL,showdotoutline=TRUE,showdots=NULL,customdata=FALSE,means=NULL,datapoints=NULL,setpositionhorizontalline=NULL,setpositiondottedhorizontalline=NULL,sethorizontallinecolor="black",sethorizontallinethickness=1,internalfunctionautorotation=FALSE,overrideerrorbarcolor=FALSE,showcolorblindgraph=FALSE,color=NULL,setcolor=NULL,setconfidencelevel=0.95,showintervalplot=FALSE,showline=TRUE,setmeancolor=NULL,setlinecapoutlinethickness=NA,setlinecapoutlinecolor=NULL,setmeanoutlinecolor=NULL,setmeanoutlinethickness=NULL,showdatainback=FALSE,showdotsinback=NULL,groupvariable=NULL,groupingvariable=NULL,showblackandwhitegraph=FALSE,showdarkgraph=NULL,setreversecodex=FALSE,setreversecodey=FALSE,setreversecodegroup=FALSE,setreversecodepanel=FALSE,setreverseorderx=FALSE,setreverseordergroup=FALSE,setreverseorderlegend=NULL,setreverseorderpanel=FALSE,reverseorderx=NULL,reverseordergroup=NULL,reverseorderlegend=NULL,reverseorderpanel=NULL,reversecodex=NULL,reversecodey=NULL,reversecodegroup=NULL,reversecodepanel=NULL,setxlevelorder=NULL,setgrouplevelorder=NULL,setlegendlevelorder=NULL,setpanellevelorder=NULL,setyaxisspacing=NULL,setyaxisend=NULL,setyaxisstart=NULL,showoutput=NULL,showgridlines=TRUE,setlegendpositionleft=FALSE,setlegendpositionbelow=FALSE,setlegendpositionabove=FALSE,showlegendleft=FALSE,showlegendbelow=FALSE,showlegendabove=FALSE,showlegend=TRUE,settitleface="bold",setlegendtitleface="plain",setxaxistitleface=NULL,setyaxistitleface=NULL,setpaneltitleface=NULL,showboldedtitle=NULL,showboldedlegendtitle=NULL,showboldedxaxistitle=NULL,showboldedyaxistitle=NULL,showboldedpaneltitle=NULL,showboldedaxistitles=NULL,setylowerbound=NULL,setyupperbound=NULL,showlargerfonts=FALSE,setsubtitle=NULL,subtitle=NULL,setsubtitlesize=NULL,setsubtitleface=NULL,..){

  require(Rmisc)
  require(ggplot2)

  #Creating the "Not in" function
  '%!in%' <- function(x,y)!('%in%'(x,y))

  #### Setting up parameters ####
  if(!is.null(setsubtitle)){
    subtitle<-setsubtitle
  }
  if(is.null(setsubtitlesize)){
    setsubtitlesize<-10
  }
  if(is.null(setsubtitleface)){
    setsubtitleface<-"plain"
  }
  if(is.null(showdarkgraph)){
    showdarkgraph<-FALSE
  }
  if(!is.null(setsplitlegend)){
    splitgroup<-setsplitlegend
  }
  if(!is.null(splitlegend)){
    splitgroup<-splitlegend
  }
  if(is.null(showoutput)){
    showoutput<-TRUE
  }
  if(!is.null(setdata)){
    data<-setdata
  }
  if(!is.null(setxaxistitlesize)){
    setxtitlesize<-setxaxistitlesize
  }
  if(!is.null(setyaxistitlesize)){
    setytitlesize<-setyaxistitlesize
  }
  if(!is.null(setxaxislevels)){
    setxlevels<-setxaxislevels
  }
  if(!is.null(setxaxistitlesize)){
    setxaxissize<-setxaxistitlesize
  }
  if(!is.null(setyaxistitlesize)){
    setyaxissize<-setyaxistitlesize
  }
  if(!is.null(setyupperbound)||!is.null(setylowerbound)){
    if(is.null(setylowerbound)){
      message("ERROR: To adjust the bounds of your y-axis, you must also specify a lower bound using setylowerbound=")
      stop()
    }
    if(is.null(setyupperbound)){
      message("ERROR: To adjust the bounds of your y-axis, you must also specify a lower bound using setyupperbound=")
      stop()
    }
  }
  if(!is.null(setreverseorderlegend)){
    setreverseordergroup<-setreverseorderlegend
  }
  if(!is.null(reverseorderlegend)){
    setreverseordergroup<-reverseorderlegend
  }
  if(!is.null(setlegendlevelorder)){
    setgrouplevelorder<-setlegendlevelorder
  }
  if(setlegendpositionabove==TRUE){
    showlegendabove<-TRUE
  }
  if(setlegendpositionbelow==TRUE){
    showlegendbelow<-TRUE
  }
  if(setlegendpositionleft==TRUE){
    showlegendleft<-TRUE
  }
  if(!is.null(setxaxislabel)){
    setxaxistitle<-setxaxislabel
  }
  if(!is.null(setyaxislabel)){
    setyaxistitle<-setyaxislabel
  }
  if(!is.null(setgrouplevels)){
    setlegendlevels<-c(setgrouplevels)
  }
  if(is.null(setdotoutlinecolor)){
    if(showdarkgraph==FALSE){
      setdotoutlinecolor<-"black"
    }
    if(showdarkgraph==TRUE){
      setdotoutlinecolor<-"#323232"
    }
  }

  if(!is.null(reversecodex)){
    setreversecodex<-reversecodex
  }
  if(!is.null(reversecodey)){
    setreversecodey<-reversecodey
  }
  if(!is.null(reversecodegroup)){
    setreversecodegroup<-reversecodegroup
  }
  if(!is.null(reversecodepanel)){
    setreversecodepanel<-reversecodepanel
  }
  if(!is.null(reverseorderx)){
    setreverseorderx<-reverseorderx
  }
  if(!is.null(reverseordergroup)){
    setreverseordergroup<-reverseordergroup
  }
  if(!is.null(reverseorderpanel)){
    setreverseorderpanel<-reverseorderpanel
  }
  if(!is.null(showboldedtitle)){
    if(showboldedtitle==TRUE){
      settitleface<-"bold"
    }
    if(showboldedtitle==FALSE){
      settitleface<-"plain"
    }
  }
  if(!is.null(showboldedlegendtitle)){
    if(showboldedlegendtitle==TRUE){
      setlegendtitleface<-"bold"
    }
    if(showboldedlegendtitle==FALSE){
      setlegendtitleface<-"plain"
    }
  }
  if(!is.null(showboldedaxistitles)){
    if(showboldedaxistitles==TRUE){
      showboldedxaxistitle<-TRUE
      showboldedyaxistitle<-TRUE
    }
  }
  if(!is.null(showboldedxaxistitle)){
    if(showboldedxaxistitle==TRUE){
      setxaxistitleface<-"bold"
    }
    if(showboldedxaxistitle==FALSE){
      setxaxistitleface<-"plain"
    }
  }
  if(!is.null(showboldedyaxistitle)){
    if(showboldedyaxistitle==TRUE){
      setyaxistitleface<-"bold"
    }
    if(showboldedyaxistitle==FALSE){
      setyaxistitleface<-"plain"
    }
  }
  if(!is.null(showboldedpaneltitle)){
    if(showboldedpaneltitle==TRUE){
      setpaneltitleface<-"bold"
    }
    if(showboldedpaneltitle==FALSE){
      setpaneltitleface<-"plain"
    }
  }

  if(showdarkgraph==TRUE){
    if(setdotcolor=="#3BA0BF"&&is.null(linecolor)){
      setdotcolor<-"yellow"
    }
    if(setlinecolor=="#1D4E5D"&&is.null(dotcolor)){
      setlinecolor<-"yellow"
    }
  }
  if(!is.null(groupvariable)){
    iv2<-groupvariable
  }

  if(!is.null(groupingvariable)){
    iv2<-groupingvariable
  }

  if(!is.null(showdotsinback)){
    showdatainback<-showdotsinback
  }

  if(!is.null(seterrorbarcolor)){
    showcolorederrorbars<-FALSE
  }
  if(!is.null(linecolor)){
    setlinecolor<-linecolor
  }
  if(!is.null(dotcolor)){
    setdotcolor<-dotcolor
  }
  if(!is.null(linethickness)){
    setlinethickness<-linethickness
  }

  #If showcolorederrorbars is manually set, override error bar colors
  if(!is.null(showcolorederrorbars)){
    if(showcolorederrorbars==FALSE){
      overrideerrorbarcolor<-TRUE
    }
  }

  if(!is.null(setmeancolor)){
    setlinecapcolor<-setmeancolor
  }

  if(is.null(setlinecapcolor)){
    setlinecapcolor<-setlinecolor
  }

  if(!is.null(setmeanoutlinecolor)){
    setlinecapoutlinecolor<-setmeanoutlinecolor
  }

  if(!is.null(setmeanoutlinethickness)){
    setlinecapoutlinethickness<-setmeanoutlinethickness
  }

  if(!is.null(setlinecapoutlinecolor)||(!is.na(setlinecapoutlinethickness))){
    if(showcolorblindgraph==TRUE){
      message("ERROR: You cannot adjust your line cap (mean) outline when using a color-blind friendly graph.")
      stop()
    }
  }

  if(!is.null(setlinecapoutlinecolor)){
    setlinecapoutlinethickness<-2
  }
  if(is.null(setlinecapoutlinecolor)){
    if(showdarkgraph==FALSE){
      setlinecapoutlinecolor<-"#F2F2F2"
    }
    if(showdarkgraph==TRUE){
      setlinecapoutlinecolor<-"black"
    }
  }

  if(showline==FALSE){
    showintervalplot<-TRUE
  }

  #By default, if not manually specified, set showcolorederrorbars to FALSE.
  #This default will change for interval plots.
  if(is.null(showcolorederrorbars)){
    if(showintervalplot==FALSE){
      showcolorederrorbars<-FALSE
    }
    if(showintervalplot==TRUE){
      showcolorederrorbars<-TRUE
    }
  }

  if(showintervalplot==TRUE){
    if(is.null(seterrorbarthickness)){
      seterrorbarthickness<-0.8
    }
    if(is.null(seterrorbartransparency)){
      seterrorbartransparency<-1
    }
  }

  if(showintervalplot==FALSE){
    if(is.null(seterrorbarthickness)){
      seterrorbarthickness<-0.55
    }
    if(is.null(seterrorbartransparency)){
      seterrorbartransparency<-1
    }
  }

  if(is.null(seterrorbarcolor)){
    seterrorbarcolor<-"gray"
  }

  if(!is.null(showmeans)){
    showlinecaps<-showmeans
  }

  if(!is.null(setcolor)){
    color<-setcolor
  }

  if(length(color)>1){
    setcolors<-c(color)
    color<-NULL
  }
  ytickspecs<-0

  if(!is.null(setyaxisspacing)){
    ytickspecs<-ytickspecs+1
  }

  if(!is.null(setyaxisend)){
    ytickspecs<-ytickspecs+1
  }

  if(!is.null(setyaxisstart)){
    ytickspecs<-ytickspecs+1
  }

  if(!is.null(color)){
    setdotcolor<-color
    setlinecolor<-color
    setlinecapcolor<-color
    if(overrideerrorbarcolor==FALSE){
      seterrorbarcolor<-color
    }
  }

  if(setdotoutlinethickness==0||showdotoutline==FALSE){
    setdotoutlinethickness<-NA
  }
  if(is.null(titlesize)){
    titlesize<-20
  }
  if(is.null(setytitlesize)){
    setytitlesize<-17.5
  }
  if(is.null(setxtitlesize)){
    setxtitlesize<-17.5
  }
  if(is.null(setxaxistextsize)){
    setxaxistextsize<-11
  }
  if(is.null(setyaxistextsize)){
    setyaxistextsize<-11
  }
  if(is.null(setlegendlevelsize)){
    setlegendlevelsize<-11
  }
  if(is.null(setlegendtitlesize)){
    setlegendtitlesize<-15.5
  }

  if(showlargerfonts==TRUE){
    titlesize<-titlesize+4
    setytitlesize<-setytitlesize+3
    setxtitlesize<-setxtitlesize+3
    setxaxistextsize<-setxaxistextsize+3
    setyaxistextsize<-setyaxistextsize+3
    setlegendlevelsize<-setlegendlevelsize+3
    setlegendtitlesize<-setlegendtitlesize+3
  }

  if(is.data.frame(dv)||is.data.frame(dv)&&is.data.frame(iv1)||!is.null(means)||!is.null(datapoints)||is.null(dv)){
    customdata<-TRUE
    if(is.null(dv)&&is.null(means)&&is.null(datapoints)&&is.null(iv1)){
      dv<-df
    }
    if(!is.null(dv)&&!is.null(iv1)){
      means<-dv
      datapoints<-iv1
      dv<-NULL
    }
  }

  if(customdata==TRUE){
    message("Note: To change the names of your axes, use setyaxistitle= and setxaxistitle=. To change the name of your legend, use setlegendtitle=")
  }

  if(!is.null(showdots)){
    showdata<-showdots
  }

  if(!is.null(setjitterheight)){
    jitterheight<-setjitterheight
  }

  if(!is.null(setjitterwidth)){
    jitterwidth<-setjitterwidth
  }

  if(!is.null(setjitter)){
    if(setjitter==FALSE){
      jitterheight<-0
      jitterwidth<-0
    }else{
      jitterheight<-setjitter
      jitterwidth<-setjitter
    }
  }

  if(!is.null(setdodgewidth)){
    dodgewidth<-setdodgewidth
  }

  if(!is.null(settitlesize)){
    titlesize<-settitlesize
  }

  if(is.null(titlesize)){
    titlesize<-20
  }

  if(!is.null(setaxistitlesize)){
    setxtitlesize<-setaxistitlesize
    setytitlesize<-setaxistitlesize
  }

  if(is.null(setxtitlesize)){
    setxtitlesize<-17.5
  }

  if(is.null(setytitlesize)){
    setytitlesize<-17.5
  }

  if(is.null(setlegendtitlesize)){
    setlegendtitlesize<-17.5
  }

  if(!is.null(setaxistextsize)){
    setyaxissize<-setaxistextsize
    setxaxissize<-setaxistextsize
  }

  if(is.null(setyaxissize)){
    setyaxistextsize<-11
  }

  if(is.null(setxaxissize)){
    setxaxistextsize<-11
  }

  if(is.null(setpaneltitlesize)){
    setpaneltitlesize<-11
  }

  if(!is.null(setlegendtextsize)){
    setlegendlevelsize<-setlegendtextsize
  }

  if(is.null(setlegendlevelsize)){
    setlegendlevelsize<-11
  }

  if(!is.null(settitle)){
    title<-settitle
  }

  if(setlinecolor[[1]]=="Alisa"){
    setlinecolor[[1]]<-'#FF00FF'
  }

  if(setdotcolor[[1]]=="Alisa"){
    setdotcolor[[1]]<-'#FF00FF'
  }

  if(showrotatedxlabels!=TRUE&&showrotatedxlabels!=FALSE&&rotatexaxislabels!=TRUE&&rotatexaxislabels!=FALSE&&!is.null(showrotatedxlabels)){
    message("To rotate your x-axis labels to be vertical, rather than horizontal, use rotatexaxislabels=TRUE or showrotatedxlabels=TRUE")
  }

  if(!is.null(setcolors)&&is.null(colors)){
    colors<-setcolors
  }
  if(!is.null(colors)&&is.null(setcolors)){
    setcolors<-colors
  }

  if(!is.null(setsplitx)){
    splitx<-setsplitx
  }

  if(!is.null(setsplitgroup)){
    splitgroup<-setsplitgroup
  }

  if(!is.null(setlegendtitle)){
    legendtitle<-setlegendtitle
  }

  if(!is.null(setdotsize)){
    dotsize<-setdotsize
  }

  if(!is.null(seterrorbars)){
    errorbars<-seterrorbars
  }

  if(!is.null(iv1) && length(dv)>1 && !is.null(iv2)){
    panelvariable<-iv2
    iv2<-NULL
  }

  if(!is.null(setsplitpanel)){
    splitpanel<-setsplitpanel
  }

  #If error bars are default, set errorbars to between-subjects 95% confidence intervals
  if(errorbars=="default"){
    errorbars<-"ci"
  }

  if(split1==TRUE){
    splitx<-TRUE
  }

  if(split2==TRUE){
    splitgroup<-TRUE
  }

  if(split3==TRUE){
    splitpanel<-TRUE
  }

  if(colorful==TRUE){
    showcolorederrorbars<-TRUE
  }
  if(!is.null(dottransparency)&&is.null(transparency)){
    transparency<-dottransparency
  }

  if(!is.null(setdottransparency)&&is.null(transparency)){
    transparency<-setdottransparency
  }

  if(rotatexaxislabels==TRUE){
    showrotatedxlabels<-TRUE
  }

  dotsize<-as.numeric(dotsize)

  if(is.null(dotsize)){
    dotsize<-1.1
  }
  if(is.null(transparency)){
    transparency<-0.3
  }

  if(!is.null(transparency)){
    if(transparency>1&&transparency<10.01){
      setdottransparency<-transparency
      transparency<-transparency/10
    }
  }

  #If you don't have a grouping (color) variable
  if(is.null(iv2)&&!is.null(iv1)&&length(dv)==1&&customdata==FALSE || is.null(iv2)&&is.null(iv1)&&length(dv)>1&&customdata==FALSE||(customdata==TRUE&&"groupvariable" %!in% colnames(dv)&&"groupvariable" %!in% colnames(means))){

    #Set parameters if showblackandwhitegraph is set to TRUE
    if(showblackandwhitegraph==TRUE){
      setlinecolor<-"black"
      setdotcolor<-"gray"
      setlinecapcolor<-"black"
      showcolorederrorbars<-FALSE
      seterrorbarcolor<-"gray"
      overrideerrorbarcolor<-TRUE
      setdotoutlinecolor<-"light gray"
    }

    #If the user is not specifying a custom table of means
    if(customdata==FALSE){
      #If not reshaping
      if(length(dv)==1){

        #If no panel variable is specified
        if(is.null(panelvariable)){
          graphvariables <- data[, c(dv, iv1)]

          if(is.character(dv) && is.character(iv1)){
            colnames(graphvariables) <- c(dv, iv1)
          }
        }

        #If a panel variable is speciified
        if(!is.null(panelvariable)){
          graphvariables <- data[, c(dv, iv1, panelvariable)]

          if(is.character(dv) && is.character(iv1) && is.character(panelvariable)){
            colnames(graphvariables) <- c(dv, iv1, panelvariable)
          }
        }
      }
      #If reshaping
      if(length(dv)>1){
        require(reshape2)

        #If no panelvariable is specified
        if(is.null(panelvariable)){
          reshapeddata<-data[, c(dv)]
          reshapeddata$ID<-1:NROW(reshapeddata)
          reshapeddata<-melt(reshapeddata,id.vars=c("ID"))
          if(showoutput==TRUE){
            print(reshapeddata)
          }
          message("Your variable has been re-shaped. Check the data printed above to ensure your data were reshaped correctly.")

          graphvariables<-reshapeddata[,c(3,2)] #dv, iv
          colnames(graphvariables) <- c("yvariable","xvariable")
        }

        #If a panel variable is specified
        if(!is.null(panelvariable)){
          panelvariable2<-data[,panelvariable]
          ID<-1:NROW(data)
          reshapeddata<-data.frame(ID,panelvariable2,data[, c(dv)])
          reshapeddata<-melt(reshapeddata,id.vars=c(1,2)) #First variable is ID, second is panelvariable
          message("Your variable has been re-shaped. Check the data printed below to ensure your raw data were reshaped correctly.")
          graphvariables<-reshapeddata[,c(4,3,2)] #dv, iv, and paneling variable
          colnames(graphvariables) <- c("yvariable", "xvariable", panelvariable)
        }
      }

      #Create a duplicate dataframe
      summarydata<-graphvariables

      #If you have a panel variable, set the column names for
      #the summarydata dataframe to be 'yvariable', 'xvariable', and
      #'facetvariable. Otherwise, exclude facetvariable.
      if(length(colnames(summarydata))>2){
        colnames(summarydata) <- c("yvariable", "xvariable","facetvariable")
      }else{
        colnames(summarydata)<-c("yvariable","xvariable")
      }

      #Remove NAs
      summarydata<-na.omit(summarydata)

      #If reverse coding or reversing the order of variables
      if(setreversecodex==TRUE){
        summarydata$xvariable<-(max(summarydata$xvariable,na.rm=T)+min(summarydata$xvariable,na.rm=T))-summarydata$xvariable
      }
                                #If performing a median split, don't use this code
      if(setreverseorderx==TRUE&&splitx==FALSE){
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c(rev(levels(summarydata$xvariable))))
      }
      if(setreversecodey==TRUE){
        summarydata$yvariable<-(max(summarydata$yvariable,na.rm=T)+min(summarydata$yvariable,na.rm=T))-summarydata$yvariable
      }
      if(setreversecodepanel==TRUE){
        summarydata$facetvariable<-(max(summarydata$facetvariable,na.rm=T)+min(summarydata$facetvariable,na.rm=T))-summarydata$facetvariable
      }
      if(setreverseorderpanel==TRUE&&splitpanel==FALSE){
        summarydata$facetvariable<-as.factor(summarydata$facetvariable)
        summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c(rev(levels(summarydata$facetvariable))))
      }
      if(!is.null(setxlevelorder)){
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c(setxlevelorder))
        if(setreverseorderx==TRUE){
          message("You can either reverse the order of the levels of your x-axis variable or specify a custom order. However, you cannot do both.")
          stop()
        }
      }
      if(!is.null(setpanellevelorder)){
        summarydata$facetvariable<-as.factor(summarydata$facetvariable)
        summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c(setpanellevelorder))
        if(setreverseorderpanel==TRUE){
          message("You can either reverse the order of the levels of your panel variable or specify a custom order. However, you cannot do both.")
          stop()
        }
      }
      if(setystandardize==TRUE){
        summarydata$yvariable<-scale(summarydata$yvariable)
        message("Note: Your y-variable has been standardized.")
      }

      #If performing a median split
      if(splitx==TRUE){
        summarydata$xvariable<-as.numeric(summarydata$xvariable)
        recordedmedianx<-median(summarydata$xvariable,na.rm=T)
        summarydata$xvariable<-ifelse(summarydata$xvariable>median(summarydata$xvariable,na.rm=T),"High",ifelse(summarydata$xvariable<=median(summarydata$xvariable,na.rm=T),"Low",NA))
        if(setreverseorderx==FALSE){
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c("Low","High"))
        }
        if(showoutput==TRUE){
          print(table(summarydata$facetvariable))
        }
        message(sprintf("NOTE: A median split is being performed on your x-axis variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianx))
      }

      if(splitpanel==TRUE&&!is.null(panelvariable)){
        summarydata$facetvariable<-as.numeric(summarydata$facetvariable)
        recordedmedianfacet<-median(summarydata$facetvariable,na.rm=T)
        summarydata$facetvariable<-ifelse(summarydata$facetvariable<=median(summarydata$facetvariable,na.rm=T),"Low",ifelse(summarydata$facetvariable>median(summarydata$facetvariable,na.rm=T),"High",NA))
        if(setreverseorderpanel==FALSE){
        summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c("Low","High"))
        }
        if(showoutput==TRUE){
          print(table(summarydata$facetvariable))
        }
        message(sprintf("NOTE: A median split is being performed on your panel variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianfacet))
      }

      #If setting custom levels for the levels of your x-variable
      if(!is.null(setxlevels)){
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        originallevels<-levels(summarydata$xvariable)
        levels(summarydata$xvariable)<- c(setxlevels)
        newlevels<-levels(summarydata$xvariable)
        message("Levels of x variable have been converted:")

        #Print your original levels and your new levels unless
        #showoutput is set to FALSE by the user
        if(showoutput==TRUE){
          print(originallevels)
          print(newlevels)
        }
      }

      #Change the x-variable to a factor
      summarydata$xvariable<-as.factor(summarydata$xvariable)

      #Set the setconfidencelevel and errorbars parameter values
      if(errorbars=="ci95"||errorbars=="ci_95"||errorbars=="95ci"||errorbars=="95_ci"||errorbars=="95_CI"||errorbars=="95CI"||errorbars=="CI_95"||errorbars=="CI95"){
        setconfidencelevel<-0.95
        errorbars<-"ci"
      }

      if(errorbars=="ci99"&&errorbars=="ci_99"&&errorbars=="99ci"&&errorbars=="99_ci"&&errorbars=="99_CI"&&errorbars=="99CI"&&errorbars=="CI_99"&&errorbars=="CI99"){
        setconfidencelevel<-0.99
        errorbars<-"ci"
      }

      #Compute the means, standard errors, and 95 or 99% confidence intervals
      #with a panel variable included
      #NOTE: summarySE computes both the standard error and the confidence
      #interval. We specify which to use below.

      if(!is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('facetvariable','xvariable'),conf.interval=setconfidencelevel)
      }

      #Compute the means, standard errors, and 95 or 99% confidence intervals
      #with no panel variable included
      if(is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('xvariable'),conf.interval=setconfidencelevel)
      }

      #Create a column called errors in the table of means computed above
      #which corresponds to the confidence intervals, standard error, or
      #other error bar specified by the user

      #Standard errors
      if(errorbars=="se"||errorbars=="SE"){
        summary$errors<-summary$se
        message("Error bars reflect standard errors.")
      }
      #95% or 99% confidence intervals
      if(errorbars=="ci"||errorbars=="CI"){
        summary$errors<-summary$ci
        message(sprintf("Error bars reflect %s percent confidence intervals.",setconfidencelevel))
      }
      #Standard deviations as error bars
      if(errorbars=="sd"||errorbars=="SD"){
        summary$errors<-summary$sd
        message("Error bars reflect a standard deviation above or below each mean.")
      }

      #No error bars
      if(errorbars=="none"||errorbars=="None" || showerrorbars==FALSE){
        summary$errors<-rep(0,NROW(summary))
        seterrorbartransparency<-0
      }
    }

    #If the user is giving you a table of means/errorbars instead of having
    #you compute it for them
    if(customdata==TRUE){
      message("Note: No grouping (color) variable was found in your dataset. If this is incorrect, make sure you have a variable with the column name 'groupvariable' in your dataframe.")

      #If the user doesn't specify means and datapoints separately, assume
      #the table they are providing is the means in the position of
      #what would otherwise be the dependent variable
      if(is.null(means)&&is.null(datapoints)){
        summary<-dv
        graphvariables<-dv
        showdata<-FALSE
      }

      #If the user is specifying means and datapoints separately
      #Note that the summarydata dataframe, which includes the
      #individual datapoints, is distinct from the summary
      #dataframe, which includes only the means, CIs, etc.

      #Means
      if(!is.null(means)){
        summary<-means
        graphvariables<-means
      }
      summary$xvariable<-as.factor(summary$xvariable)
      summary$yvariable<-as.numeric(summary$yvariable)

      #Datapoints
      if(!is.null(datapoints)){
      summarydata<-datapoints
      summarydata$xvariable<-as.factor(summarydata$xvariable)
      summarydata$yvariable<-as.numeric(summarydata$yvariable)
      if("panelvariable" %in% colnames(summarydata)){
          summarydata$facetvariable<-summary$panelvariable
        }
      }
      if(is.null(datapoints)){
        showdata<-FALSE
      }

      #Extract the errorbars column, which could go by multiple
      #names
      if(!is.null(summary)&&"errors"%!in%colnames(summary)){
        if("errorbars" %in% colnames(summary)){
          summary$errors<-summary$errorbars
        }
        if("error" %in% colnames(summary)){
          summary$errors<-summary$error
        }
        if("ci" %in% colnames(summary)){
          summary$errors<-summary$ci
        }
        if("CI" %in% colnames(summary)){
          summary$errors<-summary$CI
        }
        if("se" %in% colnames(summary)){
          summary$errors<-summary$se
        }
        if("SE" %in% colnames(summary)){
          summary$errors<-summary$SE
        }
      }

      #If no error bars are specified or the user sets errorbars to none
      if("errors" %!in% colnames(summary)||errorbars=="none"||errorbars=="None"||showerrorbars==FALSE){
        summary$errors<-rep(0,NROW(summary))
        seterrorbartransparency<-0
      }

      #Create a column for the facet variable if needed
      if("panelvariable" %in% colnames(summary)&&"facetvariable"%!in%colnames(summary)){
        summary$facetvariable<-summary$panelvariable
      }

      #Rename the graphvariables dataframe to be xvariable, yvariable, etc.
      colnames(graphvariables)[1]<-"yvariable"
      colnames(graphvariables)[2]<-"xvariable"
      if("facetvariable" %in% colnames(summary)){
        colnames(graphvariables)[3]<-"facetvariable"
      }
    }

    #When the override parameter is FALSE, show colored error bars. Otherwise,
    #leave them the default gray color.
    if(overrideerrorbarcolor==FALSE){
      showcolorederrorbars<-TRUE
    }

    #Set the colors if needed
    if(!is.null(setcolors)){
      setlinecapcolor<-c(setcolors)
      if(overrideerrorbarcolor==FALSE){
        seterrorbarcolor<-c(setcolors)
      }
      setdotcolor<-c(setcolors)
      if(length(setcolors)==1){
        setlinecolor<-setcolors
      }
    }

    #Prevent users from setting the labels and order
    #of a panel variable simultaneously, which would cause issues based
    #on how panel variable labels are set
    if(!is.null(setpanellevels)){
      if(!is.null(setpanellevelorder)){
        message("ERROR: Unfortunately, you cannot set the panel level order and specify custom panel labels in the present version. However, you may reverse the order of your panels using setreverseorderpanel=TRUE.")
        stop()
      }

      #Make the facet variable a factor in both the summary and
      #summarydata dataframes and drop any unused levels
      summarydata$facetvariable<-as.factor(summarydata$facetvariable)
      summary$facetvariable<-as.factor(summary$facetvariable)
      summarydata$facetvariable<-droplevels(summarydata$facetvariable)
      summary$facetvariable<-droplevels(summary$facetvariable)

      #Create a vector of the original levels of the facet variable
      originallevels<-levels(summarydata$facetvariable)

      #Update the levels of the facet variable if specified by the user
      levels(summarydata$facetvariable)<- c(setpanellevels)
      levels(summary$facetvariable)<- c(setpanellevels)

      #Create a vector of the new levels of your facet variable
      newlevels<-levels(summarydata$facetvariable)

      #Print the new and old levels unless showoutput is set to FALSE
      message("Levels of paneling variable have been converted:")
      if(showoutput==TRUE){
        print(originallevels)
        print(newlevels)
      }
    }

    #Print the dataframe unless showoutput is set to FALSE
    if(showoutput==TRUE){
      print(summarydata)
    }

    #Print the summary table, but change errors to errorbars
    #to make it easier for users to read.
    summary2<-summary
    summary2$errorbars<-summary2$errors
    if(!is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1:2,4,9)])
    }

    if(is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1,3,8)])
    }

    #Code below was used for automatically rotating the x-axis labels
    #if the user has many levels with long labels. However,
    #this caused issues in the past:

    # if(is.null(showrotatedxlabels)){
    #   if(sum(nchar(levels(summarydata$xvariable)))>80){
    #     showrotatedxlabels<-TRUE
    #     internalfunctionautorotation<-TRUE
    #   }
    # }
    # if(is.null(showrotatedxlabels)){
    #   if(sum(nchar(levels(summarydata$xvariable)))<=80){
    #     showrotatedxlabels<-FALSE
    #   }
    # }

    #### Core graph when no color variable is specified ####
    graph<-ggplot(data=summary, aes(x=xvariable, y=yvariable,group=1)) +
      ylab(colnames(graphvariables)[1]) +
      xlab(colnames(graphvariables)[2]) +
      expand_limits(y=0) +
      theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))

    #If datapoints are to be shown in the front
    if(showdatainback==FALSE){

      #If showing colored error bars
      if(showcolorederrorbars==TRUE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors),linetype="solid", color=setlinecolor, alpha=seterrorbartransparency,width=seterrorbarwidth,linewidth=seterrorbarthickness,position=position_dodge(0.1))
      }

      #If showing gray error bars
      if(showcolorederrorbars==FALSE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors), linetype="solid",color=seterrorbarcolor, width=seterrorbarwidth, alpha=seterrorbartransparency,linewidth=seterrorbarthickness,position=position_dodge(0.1))
      }

      #If showline is set to FALSE or showintervalplot set to TRUE (hiding
      #the line)
      if(showintervalplot==FALSE){
        graph<-graph+geom_line(data=summary,linewidth=setlinethickness,color=setlinecolor)
      }

      #If showing line caps for means
      if(showlinecaps==TRUE){
        graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable,group=1),shape=21,stroke=c(setlinecapoutlinethickness),fill=c(setlinecapcolor),color=c(setlinecapoutlinecolor),position=position_dodge(0.1), size=c(setlinecapsize))
      }

      #If showing datapoints
      if(showdata==TRUE){

        #If specifying a single color for the datapoints
        if(length(setdotcolor)==1){
          graph<-graph+geom_point(data=summarydata,shape=21,stroke=setdotoutlinethickness,fill=c(setdotcolor),color=c(setdotoutlinecolor),size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
        }

        #If specifying multiple colors
        if(length(setdotcolor)>1){
          graph<-graph+geom_point(data=summarydata,aes(fill=xvariable),shape=21,stroke=setdotoutlinethickness,color=c(setdotoutlinecolor),size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
          graph<-graph+scale_fill_manual(values=c(setdotcolor))
        }
      }
      message("Datapoints are shown in the front by default. To move them to the back, add showdatainback=TRUE")
    }

    #If datapoints are to be shown in the back
    if(showdatainback==TRUE){
      if(showdata==TRUE){
        if(length(setdotcolor)==1){
          graph<-graph+geom_point(data=summarydata,shape=21,stroke=setdotoutlinethickness,fill=c(setdotcolor),color=c(setdotoutlinecolor),size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
        }
        if(length(setdotcolor)>1){
          graph<-graph+geom_point(data=summarydata,aes(fill=xvariable),shape=21,stroke=setdotoutlinethickness,color=c(setdotoutlinecolor),size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
          graph<-graph+scale_fill_manual(values=c(setdotcolor))
        }
      }
      if(showcolorederrorbars==TRUE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors),linetype="solid", color=setlinecolor, alpha=seterrorbartransparency,width=seterrorbarwidth,linewidth=seterrorbarthickness,position=position_dodge(0.1))
      }
      if(showcolorederrorbars==FALSE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors), linetype="solid",color=seterrorbarcolor, width=seterrorbarwidth, alpha=seterrorbartransparency,linewidth=seterrorbarthickness,position=position_dodge(0.1))
      }
      if(showintervalplot==FALSE){
        graph<-graph+geom_line(data=summary,linewidth=setlinethickness,color=setlinecolor)
      }
      if(showlinecaps==TRUE){
        graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable,group=1),shape=21,stroke=c(setlinecapoutlinethickness),fill=c(setlinecapcolor),color=c(setlinecapoutlinecolor),position=position_dodge(0.1), size=c(setlinecapsize))
      }
    }

    #Hide the legend since there should be no color variable under this
    #if statement
    graph<-graph+guides(fill="none",color="none")

    #Messages to users
    if(showintervalplot==TRUE){
      message("Because you are showing an interval plot, the default settings for errorbars have now been changed. To adjust the color of errorbars, use seterrorbarcolor = . To adjust the thickness of errorbars, use seterrorbarthickness = .")
    }
    if(showlinecaps==FALSE){
      message("Mean values have been removed. To re-include them, use showlinecaps = TRUE or showmeans = TRUE")
    }

    #If titles are not specified by the user
    if (!is.null(title)) {
      graph <- graph + ggtitle(title)
    }
    if(!is.null(subtitle)){
      graph <- graph + labs(subtitle=subtitle)
    }

    if (!is.null(setxaxistitle)) {
      graph <- graph + xlab(setxaxistitle)
    }
    if (!is.null(setyaxistitle)) {
      graph <- graph + ylab(setyaxistitle)
    }

    #If the user is specifying a horizontal solid or dotted line
    if(!is.null(setpositionhorizontalline)){
      graph<-graph+geom_hline(yintercept=setpositionhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness)
    }
    if(!is.null(setpositiondottedhorizontalline)){
      graph<-graph+geom_hline(yintercept=setpositiondottedhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness,linetype="dotted")
    }

    #For users to adjust the axis, they must specify all three commands
    #(setyaxisstart, setyaxisend end, and setyaxisspacing
    #or the same commands for x). Users should not specify only one of these
    #(e.g., just setyaxisstart). This blocks them from doing so.
    if(ytickspecs>0&&ytickspecs<3){
      message("To adjust your y-axis, use setyaxisstart=, setyaxisend=, and setyaxisspacing=. All three parameters must be specified to adjust the y-axis.")
      stop()
    }
    if(!is.null(setyaxisspacing)){
      graph<-graph+scale_y_continuous(breaks=seq(setyaxisstart,setyaxisend,by = setyaxisspacing))
    }

    #Add the panels to the graph if needed
    if(!is.null(panelvariable)||"facetvariable" %in% colnames(summary)){
      graph<-graph+facet_wrap(~facetvariable)
    }

    #More messages
    if(!is.null(setcolors)&&showintervalplot==FALSE&&length(setcolors)>1){
      message("\nTo set the color of the entire line, use setcolor= rather than setcolorS = or specify only one color. To separately set the color of different portions of your graph, use setlinecolor=, setdotcolor=, seterrorbarcolor=, and setlinecapcolor=.")
    }
    if(!is.null(setcolors)||!is.null(colors)){
      message("\nNOTE: Setting colors overrides other options, such as seterrorbarcolor, setlinecapcolor, setdotcolor, etc. You can use these commands instead of setcolors to separately adjust the desired portions of the graph.")
    }
    if(!is.null(setcolor)||!is.null(color)){
      message("\nNOTE: Setting a color overrides other options, such as seterrorbarcolor, setlinecapcolor, setdotcolor, etc. You can use these commands instead of setcolor to separately adjust the desired portions of the graph.")
    }

    #If showdarkgraph is set to TRUE, make the graph black and the text white
    if(showdarkgraph==TRUE){
      graph<-graph+theme(
        plot.background = element_rect(fill = "black"),panel.background=element_rect(fill="black"),legend.background=element_rect(fill="black"), legend.key=element_rect(fill="black"),axis.text=element_text(color="white"),legend.text=element_text(color="white"),axis.title=element_text(color="white"),legend.title=element_text(color="white"))
    }

    #If transparency set by the user exceeds 1 (should be a 0 - 1 scale)
    if(!is.null(setdottransparency)){
      if(setdottransparency>1&&setdottransparency<10.1){
        message("NOTE: Transparency is typically set on a scale from 0 (completely invisible) - 1 (not at all transparent). Because you specified a transparency value greater than 1, we divided this value by 10, effectively making a scale from 1.1 - 10. However, specifying a transparency of 1 will trigger the default 0 - 1 scale and remove all transparency. To avoid confusion, we recommend using the default 0 - 1 scale in the future.")
      }
    }

    #Bolding, italics, font size, etc.
    if(!is.null(setyaxistitleface)){
      graph<-graph+theme(axis.title.y=element_text(face=setyaxistitleface))
    }
    if(!is.null(setxaxistitleface)){
      graph<-graph+theme(axis.title.x=element_text(face=setxaxistitleface))
    }
    if(!is.null(setpaneltitleface)){
      graph<-graph+theme(strip.text=element_text(face=setpaneltitleface))
    }
    if(!is.null(setpaneltitlesize)){
      graph<-graph+theme(strip.text=element_text(size=setpaneltitlesize))
    }

    #If the user wishes to hide the gridlines
    if(showgridlines==FALSE){
      graph<-graph+theme_classic()+theme(legend.position="none")
    }

    #Redundant, but useful in case a legend ever appears through
    #some sort of glitch
    if(showlegend==FALSE){
      graph<-graph+theme(legend.position = "none")
    }

    #If the user wishes to "zoom in" or "zoom out" of their graph
    if(!is.null(setyupperbound)){
      graph<-graph+ylim(setylowerbound,setyupperbound)
    }
    
    if(max(as.numeric(summarydata$yvariable))<3){
      if(jitterheight>.1){
        if(showdata==TRUE){
          message("WARNING: Because you have a relatively narrow range of values in your y-axis variable, jitter may make your datapoints appear to be in a different location than is likely to be appropriate. We recommend adjusting your jitter using the setjitterheight and setjitterwidth commands (or use the setjitter command to set both height and width simultaneously--for example, setjitter=.01).")  
        }
      }
    }

    #If the user is rotating the x-axis labels to make them fit
    if(showrotatedxlabels==TRUE){
      if(internalfunctionautorotation==TRUE){
        message("NOTE: X-axis labels have been rotated by default to avoid overlapping labels. To override this, add showrotatedxlabels=FALSE")
      }
      return(graph + theme(axis.text.x = element_text(angle = 90,vjust=0.5)))
    }

    if(showrotatedxlabels==FALSE){
      return(graph)
    }
  }
  #### LINE GRAPH PART 2 (Grouping/color variable included) ####
  if((!is.null(iv2) && customdata==FALSE) || ((!is.null(iv1) && length(dv)>1) && customdata==FALSE)||(customdata==TRUE&&"groupvariable" %in% colnames(dv))||(customdata==TRUE&&"groupvariable" %in% colnames(means))){

    #If the user is not supplying their own table of means/CIs
    if(customdata==FALSE){
      #If there is no panel (facet) variable
      if(is.null(panelvariable)){
        #If the user is not reshaping
        if(length(dv)==1){
          graphvariables <- data[, c(dv, iv1,iv2)]
          #If the user is specifying variable names rather than
          #index positions, use these to set the column names.
          if(is.character(dv) && is.character(iv1) && is.character(iv2)){
            colnames(graphvariables) <- c(dv, iv1, iv2)
          }
        }
        #If the user is reshaping
        if(length(dv)>1){
          require(reshape2)
          iv_variable<-data[,iv1]
          ID<-1:NROW(data)
          reshapeddata<-data.frame(ID,iv_variable,data[, c(dv)])
          reshapeddata<-melt(reshapeddata,id.vars=c(1,2)) #First variable is ID, second is iv
          message("Your variable has been re-shaped. Check the data printed below to ensure your raw data were reshaped correctly.")
          graphvariables<-reshapeddata[,c(4,3,2)] #dv, reshaped iv, and iv specified by user
          colnames(graphvariables) <- c("yvariable", "xvariable", iv1)
        }
        summarydata<-graphvariables
        colnames(summarydata) <- c("yvariable", "xvariable", "groupvariable")
      }
      #If the user is including a panel (facet) variable
      if(!is.null(panelvariable)){
        #If the user is not reshaping
        if(length(dv)==1){
          graphvariables <- data[, c(dv,iv1,iv2,panelvariable)]

          if(is.character(dv) && is.character(iv1) && is.character(iv2) && is.character(panelvariable)){
            colnames(graphvariables) <- c(dv, iv1, iv2,panelvariable)
          }
        }
        #If the user is reshaping
        if(length(dv)>1){
          require(reshape2)
          iv_variable<-data[,iv1]
          panelvariable2<-data[,panelvariable]
          ID<-1:NROW(data)
          reshapeddata<-data.frame(ID,iv_variable,panelvariable2,data[, c(dv)])
          reshapeddata<-melt(reshapeddata,id.vars=c(1,2,3)) #First variable is ID, second is iv specified by user, third is panel variable
          message("Your variable has been re-shaped. Check the data printed below to ensure your raw data were reshaped correctly.")
          graphvariables<-reshapeddata[,c(5,4,2,3)] #dv, reshaped iv, iv specified by user, panel variable
          colnames(graphvariables) <- c("yvariable", "xvariable", iv1, panelvariable)
        }
        summarydata<-graphvariables
        colnames(summarydata) <- c("yvariable", "xvariable", "groupvariable","facetvariable")
      }

      summarydata<-na.omit(summarydata)

      #If they're reverse-coding a variable or reversing the order
      #in which the levels of a variable appear on the graph
      if(setreversecodex==TRUE){
        summarydata$xvariable<-(max(summarydata$xvariable,na.rm=T)+min(summarydata$xvariable,na.rm=T))-summarydata$xvariable
      }
      if(setreverseorderx==TRUE&&splitx==FALSE){
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c(rev(levels(summarydata$xvariable))))
      }
      if(setreversecodey==TRUE){
        summarydata$yvariable<-(max(summarydata$yvariable,na.rm=T)+min(summarydata$yvariable,na.rm=T))-summarydata$yvariable
      }
      if(setreversecodegroup==TRUE){
        summarydata$groupvariable<-(max(summarydata$groupvariable,na.rm=T)+min(summarydata$groupvariable,na.rm=T))-summarydata$groupvariable
      }
      if(setreverseordergroup==TRUE&&splitgroup==FALSE){
        summarydata$groupvariable<-as.factor(summarydata$groupvariable)
        summarydata$groupvariable<-factor(summarydata$groupvariable,levels=c(rev(levels(summarydata$groupvariable))))
      }
      if(setreversecodepanel==TRUE){
        summarydata$facetvariable<-(max(summarydata$facetvariable,na.rm=T)+min(summarydata$facetvariable,na.rm=T))-summarydata$facetvariable
      }
      if(setreverseorderpanel==TRUE&&splitpanel==FALSE){
        summarydata$facetvariable<-as.factor(summarydata$facetvariable)
        summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c(rev(levels(summarydata$facetvariable))))
      }
      #If the user is specifying a custom level order
      if(!is.null(setxlevelorder)){
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c(setxlevelorder))
        if(setreverseorderx==TRUE){
          message("You can either reverse the order of the levels of your x-axis variable or specify a custom order. However, you cannot do both.")
          stop()
        }
      }
      if(!is.null(setgrouplevelorder)){
        summarydata$groupvariable<-as.factor(summarydata$groupvariable)
        summarydata$groupvariable<-factor(summarydata$groupvariable,levels=c(setgrouplevelorder))
        if(setreverseordergroup==TRUE){
          message("You can either reverse the order of the levels of your grouping variable or specify a custom order. However, you cannot do both.")
          stop()
        }
      }
      if(!is.null(setpanellevelorder)){
        summarydata$facetvariable<-as.factor(summarydata$facetvariable)
        summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c(setpanellevelorder))
        if(setreverseorderpanel==TRUE){
          message("You can either reverse the order of the levels of your panel variable or specify a custom order. However, you cannot do both.")
          stop()
        }
      }
      #If they are standardizing their y-axis variable
      if(setystandardize==TRUE){
        summarydata$yvariable<-scale(summarydata$yvariable)
        message("Note: Your y-variable has been standardized.")
      }
      #Median splitting
      if(splitx==TRUE){
        summarydata$xvariable<-as.numeric(summarydata$xvariable)
        recordedmedianx<-median(summarydata$xvariable,na.rm=T)
        summarydata$xvariable<-ifelse(summarydata$xvariable>median(summarydata$xvariable,na.rm=T),"High",ifelse(summarydata$xvariable<=median(summarydata$xvariable,na.rm=T),"Low",NA))
        if(setreverseorderx==FALSE){
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c("Low","High"))
        }
        if(showoutput==TRUE){
          print(table(summarydata$facetvariable))
        }
        message(sprintf("NOTE: A median split is being performed on your x-axis variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianx))
      }

      if(splitpanel==TRUE&&!is.null(panelvariable)){
        summarydata$facetvariable<-as.numeric(summarydata$facetvariable)
        recordedmedianfacet<-median(summarydata$facetvariable,na.rm=T)
        summarydata$facetvariable<-ifelse(summarydata$facetvariable<=median(summarydata$facetvariable,na.rm=T),"Low",ifelse(summarydata$facetvariable>median(summarydata$facetvariable,na.rm=T),"High",NA))
        if(setreverseorderpanel==FALSE){
        summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c("Low","High"))
        }
        if(showoutput==TRUE){
          print(table(summarydata$facetvariable))
        }
        message(sprintf("NOTE: A median split is being performed on your panel variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianfacet))
      }

      if(splitgroup==TRUE){
        summarydata$groupvariable<-as.numeric(summarydata$groupvariable)
        recordedmediangroup<-median(summarydata$groupvariable,na.rm=T)
        summarydata$groupvariable<-ifelse(summarydata$groupvariable<=median(summarydata$groupvariable,na.rm=T),"Low",ifelse(summarydata$groupvariable>median(summarydata$groupvariable,na.rm=T),"High",NA))
        if(setreverseordergroup==FALSE){
        summarydata$groupvariable<-factor(summarydata$groupvariable,levels=c("Low","High"))
        }
        if(showoutput==TRUE){
          print(table(summarydata$facetvariable))
        }
        message(sprintf("NOTE: A median split is being performed on your grouping variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmediangroup))
      }
      #If changing the labels associated with your x-axis variable
      if(!is.null(setxlevels)){
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        originallevels<-levels(summarydata$xvariable)
        levels(summarydata$xvariable)<- c(setxlevels)
        newlevels<-levels(summarydata$xvariable)
        #Print the original levels and the new ones unless user
        #sets showoutput to FALSE
        message("Levels of x variable have been converted:")
        if(showoutput==TRUE){
          print(originallevels)
          print(newlevels)
        }
      }

      summarydata$xvariable<-as.factor(summarydata$xvariable)
      summarydata$groupvariable<-as.factor(summarydata$groupvariable)

      #Setting parameters for error bars
      if(errorbars=="ci95"||errorbars=="ci_95"||errorbars=="95ci"||errorbars=="95_ci"||errorbars=="95_CI"||errorbars=="95CI"||errorbars=="CI_95"||errorbars=="CI95"){
        setconfidencelevel<-0.95
        errorbars<-"ci"
      }

      if(errorbars=="ci99"&&errorbars=="ci_99"&&errorbars=="99ci"&&errorbars=="99_ci"&&errorbars=="99_CI"&&errorbars=="99CI"&&errorbars=="CI_99"&&errorbars=="CI99"){
        setconfidencelevel<-0.99
        errorbars<-"ci"
      }

      #Computing means, CIs, standard errors, etc.
      if(!is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('xvariable','groupvariable','facetvariable'),conf.interval=setconfidencelevel)
      }

      if(is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('xvariable','groupvariable'),conf.interval=setconfidencelevel)
      }

      #Create a column called errors that pulls the SE, CI, or SD depending on
      #what is specified by the user
      if(errorbars=="se"||errorbars=="SE"){
        summary$errors<-summary$se
        message("Error bars reflect standard errors.")
      }
      if(errorbars=="ci"||errorbars=="CI"){
        summary$errors<-summary$ci
        message(sprintf("Error bars reflect %s percent confidence intervals.",setconfidencelevel))
      }
      if(errorbars=="sd"||errorbars=="SD"){
        summary$errors<-summary$sd
        message("Error bars reflect a standard deviation above or below each mean.")
      }
      if(errorbars=="none"||errorbars=="None" || showerrorbars==FALSE){
        summary$errors<-rep(0,NROW(summary))
        seterrorbartransparency<-0
      }
    }

    #If the user is supplying their own table of means/CIs
    if(customdata==TRUE){
      message("Note: A grouping (color) variable was found in your dataset.")
      if(is.null(means)&&is.null(datapoints)){
        summary<-dv
        graphvariables<-dv
        showdata<-FALSE
      }
      if(!is.null(means)){
        summary<-means
        graphvariables<-means
      }

      #Convert variables to factors
      summary$xvariable<-as.factor(summary$xvariable)
      summary$groupvariable<-as.factor(summary$groupvariable)
      summary$yvariable<-as.numeric(summary$yvariable)

      #If the user is supplying means and individual datapoints
      if(!is.null(datapoints)){
        summarydata<-datapoints
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        summarydata$groupvariable<-as.factor(summarydata$groupvariable)
        summarydata$yvariable<-as.numeric(summarydata$yvariable)

        #If the user is also using a facet variable
        if("panelvariable" %in% colnames(summarydata)){
          summarydata$facetvariable<-summary$panelvariable
        }
      }

      #If they aren't providing individual datapoints, set showdata
      #to FALSE
      if(is.null(datapoints)){
        showdata<-FALSE
      }

      #If they're supplying error bars, add these to the errors column
      if(!is.null(summary)){
        if("errors"%!in%colnames(summary)){
          if("errorbars" %in% colnames(summary)&&"errors"%!in%colnames(summary)){
            summary$errors<-summary$errorbars
          }
          if("error" %in% colnames(summary)){
            summary$errors<-summary$error
          }
          if("ci" %in% colnames(summary)){
            summary$errors<-summary$ci
          }
          if("CI" %in% colnames(summary)){
            summary$errors<-summary$CI
          }
          if("se" %in% colnames(summary)){
            summary$errors<-summary$se
          }
          if("SE" %in% colnames(summary)){
            summary$errors<-summary$SE
          }
        }
        #If they aren't providing error bars,
        #repeat 0 for the errors column and set error bar transparency to 0
        if("errors" %!in% colnames(summary)||errorbars=="none"||errorbars=="None"||showerrorbars==FALSE){
          summary$errors<-rep(0,NROW(summary))
          seterrorbartransparency<-0
        }
        if("panelvariable" %in% colnames(summary)&&"facetvariable"%!in%colnames(summary)){
          summary$facetvariable<-summary$panelvariable
        }

        #Set the graphvariables dataframe
        colnames(graphvariables)[1]<-"yvariable"
        colnames(graphvariables)[2]<-"xvariable"
        colnames(graphvariables)[3]<-"groupvariable"
        if("facetvariable" %in% colnames(summary)){
          colnames(graphvariables)[4]<-"facetvariable"
        }
      }
    }

    #Block the user from setting the levels and adjusting their order
    if(!is.null(setpanellevels)){
      if(!is.null(setpanellevelorder)){
        message("ERROR: Unfortunately, you cannot set the panel level order and specify custom panel labels in the present version. However, you may reverse the order of your panels using setreverseorderpanel=TRUE.")
        stop()
      }

      #If they're adjusting the levels
      summarydata$facetvariable<-as.factor(summarydata$facetvariable)
      summary$facetvariable<-as.factor(summary$facetvariable)
      summarydata$facetvariable<-droplevels(summarydata$facetvariable)
      summary$facetvariable<-droplevels(summary$facetvariable)
      originallevels<-levels(summarydata$facetvariable)
      levels(summarydata$facetvariable)<- c(setpanellevels)
      levels(summary$facetvariable)<- c(setpanellevels)
      newlevels<-levels(summarydata$facetvariable)
      message("Levels of paneling variable have been converted:")

      #If showoutput is set to TRUE, print the old and new levels
      if(showoutput==TRUE){
        print(originallevels)
        print(newlevels)
      }
    }

    #If showoutput is set to true, print the summarydata dataframe
    if(showoutput==TRUE){
      print(summarydata)
    }
    summary2<-summary
    summary2$errorbars<-summary2$errors

    #Print the means/error bars if showoutput is set to TRUE and
    #there is not a facet variable
    if(is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1,2,4,9)])
    }
    #If there is a facet variable
    if(!is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1:3,5,10)])
    }

    #Old:
    # if(is.null(showrotatedxlabels)){
    #   if(sum(nchar(levels(summarydata$xvariable)))>80){
    #     showrotatedxlabels<-TRUE
    #     internalfunctionautorotation<-TRUE
    #   }
    # }
    # if(is.null(showrotatedxlabels)){
    #   if(sum(nchar(levels(summarydata$xvariable)))<=80){
    #     showrotatedxlabels<-FALSE
    #   }
    # }

    #Core graph
    graph<-ggplot(data=NULL, aes(x=xvariable, y=yvariable, colour=groupvariable,fill=groupvariable,group=groupvariable)) +
      ylab(colnames(graphvariables)[1]) +
      xlab(colnames(graphvariables)[2]) +
      expand_limits(y=0) +
      theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))

    #If showing the datapoints in the front
    if(showdatainback==FALSE){
      if(showcolorederrorbars==TRUE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors,color=groupvariable), linetype="solid",width=seterrorbarwidth, linewidth=seterrorbarthickness,alpha=seterrorbartransparency,position=position_dodge(0.1))
      }
      #If you don't want colored error bars
      if(showcolorederrorbars==FALSE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors), linetype="solid",colour=c(seterrorbarcolor), linewidth=seterrorbarthickness,width=seterrorbarwidth, alpha=seterrorbartransparency,position=position_dodge(0.1))
      }
      #If you're showing the lines (and not just an interval plot)
      if(showintervalplot==FALSE){
        if(showcolorblindgraph==FALSE){
          graph<-graph+geom_line(data=summary,position=position_dodge(0.1),linewidth=setlinethickness)
        }
        #If you're showing a colorblind graph set the lines to be dashed,
        #solid, etc.
        if(showcolorblindgraph==TRUE){
          graph<-graph+geom_line(data=summary,aes(linetype=groupvariable),position=position_dodge(0.1),linewidth=setlinethickness)
        }
      }
      #If you're showing linecaps
      if(showlinecaps==TRUE){
        if(setlinecapcolor[[1]]==setlinecolor[[1]]){
          if(showcolorblindgraph==FALSE){
          graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, fill=groupvariable,group=groupvariable),shape=21,stroke=c(setlinecapoutlinethickness),color=c(setlinecapoutlinecolor),position=position_dodge(0.1), size=c(setlinecapsize))
        }
        #If you're showing a colorblind graph, set the line caps to be
        #different shapes
        if(showcolorblindgraph==TRUE){
          graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, colour=groupvariable,group=groupvariable,shape=groupvariable),position=position_dodge(0.1), size=c(setlinecapsize))
        }
        }
        #If they're setting linecap colors that don't match the line color
        if(setlinecapcolor[[1]]!=setlinecolor[[1]]){
          if(showcolorblindgraph==FALSE){
            graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, group=groupvariable),shape=21,stroke=c(setlinecapoutlinethickness),color=c(setlinecapoutlinecolor),fill=c(setlinecapcolor),position=position_dodge(0.1), size=c(setlinecapsize))
          }
          if(showcolorblindgraph==TRUE){
            graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, group=groupvariable,shape=groupvariable),position=position_dodge(0.1), color=c(setlinecapcolor),size=c(setlinecapsize))
          }
        }
      }

      #If they're showing the individual datapoints
      if(showdata==TRUE){
        graph<-graph+geom_point(data=summarydata,shape=21,stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=transparency,position=position_jitter(width=jitterwidth,height=jitterheight))
      }
    }

    if(showdatainback==TRUE){
      if(showdata==TRUE){
        graph<-graph+geom_point(data=summarydata,shape=21,stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=transparency,position=position_jitter(width=jitterwidth,height=jitterheight))
      }
      if(showcolorederrorbars==TRUE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors,color=groupvariable), linetype="solid",width=seterrorbarwidth, linewidth=seterrorbarthickness,alpha=seterrorbartransparency,position=position_dodge(0.1))
      }
      if(showcolorederrorbars==FALSE){
        graph<-graph+geom_errorbar(data=summary,aes(ymin=yvariable-errors, ymax=yvariable+errors), linetype="solid",colour=c(seterrorbarcolor), linewidth=seterrorbarthickness,width=seterrorbarwidth, alpha=seterrorbartransparency,position=position_dodge(0.1))
      }
      if(showintervalplot==FALSE){
        if(showcolorblindgraph==FALSE){
          graph<-graph+geom_line(data=summary,position=position_dodge(0.1),linewidth=setlinethickness)
        }
        if(showcolorblindgraph==TRUE){
          graph<-graph+geom_line(data=summary,aes(linetype=groupvariable),position=position_dodge(0.1),linewidth=setlinethickness)
        }
      }
      if(showlinecaps==TRUE){
        if(setlinecapcolor[[1]]==setlinecolor[[1]]){
          if(showcolorblindgraph==FALSE){
            graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, fill=groupvariable,group=groupvariable),shape=21,stroke=c(setlinecapoutlinethickness),color=c(setlinecapoutlinecolor),position=position_dodge(0.1), size=c(setlinecapsize))
          }
          if(showcolorblindgraph==TRUE){
            graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, colour=groupvariable,group=groupvariable,shape=groupvariable),position=position_dodge(0.1), size=c(setlinecapsize))
          }
        }
        if(setlinecapcolor[[1]]!=setlinecolor[[1]]){
          if(showcolorblindgraph==FALSE){
            graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, group=groupvariable),shape=21,stroke=c(setlinecapoutlinethickness),color=c(setlinecapoutlinecolor),fill=c(setlinecapcolor),position=position_dodge(0.1), size=c(setlinecapsize))
          }
          if(showcolorblindgraph==TRUE){
            graph<-graph+geom_point(data=summary,aes(x=xvariable, y=yvariable, group=groupvariable,shape=groupvariable),position=position_dodge(0.1), color=c(setlinecapcolor),size=c(setlinecapsize))
          }
        }
      }
    }
    if(showintervalplot==TRUE){
      message("Because you are showing an interval plot, the default settings for errorbars have now been changed. To use uncolored error bars, use seterrorbarcolor = . To adjust the thickness of errorbars, use seterrorbarthickness = .")
    }
    if(showlinecaps==FALSE){
      message("Mean values have been removed. To re-include them, use showlinecaps = TRUE or showmeans=TRUE")
    }
    if(!is.null(legendtitle)){
      graph<-graph + labs(color = legendtitle,fill=legendtitle,linetype=legendtitle,shape=legendtitle)
    }
    if(is.null(legendtitle)){
      graph<-graph + labs(color = colnames(graphvariables)[[3]],fill = colnames(graphvariables)[[3]],linetype = colnames(graphvariables)[[3]],shape=colnames(graphvariables)[[3]])
    }
    if(showlegendleft==TRUE){
      graph<-graph+theme(legend.position="left")
    }
    if(showlegendbelow==TRUE){
      graph<-graph+theme(legend.position="bottom")
    }
    if(showlegendabove==TRUE){
      graph<-graph+theme(legend.position="top")
    }
  }
  if (!is.null(title)) {
    graph <- graph + ggtitle(title)
  }
  if(!is.null(subtitle)){
    graph <- graph + labs(subtitle=subtitle)
  }

  if (!is.null(setxaxistitle)) {
    graph <- graph + xlab(setxaxistitle)
  }

  if (!is.null(setyaxistitle)) {
    graph <- graph + ylab(setyaxistitle)
  }

  if(!is.null(setlegendlevels)){
    if(color1!=-1){
      message("To set levels and colors at the same time, use colors = c('color1','color2', etc.) and setlegendlevels =c('level1','level2', etc.), rather than color1=, color2=, level1=, level2=, etc.")
    }

    if(!is.null(colors[1])){
      graph<-graph+scale_color_manual(values=c(colors),labels=c(setlegendlevels))+scale_fill_manual(values=c(colors))
      graph<-graph+guides(fill="none")
      }
    if(is.null(colors[1])){
      graph <- graph + scale_color_discrete(labels=c(setlegendlevels))+scale_fill_discrete(labels=c(setlegendlevels))
      graph<-graph+guides(fill="none")
    }
  }

  if(is.null(setlegendlevels)){
    if(!is.null(colors[1])){
      graph<-graph+scale_color_manual(values=c(colors))+scale_fill_manual(values=c(colors))
    }
  }

  if(!is.null(setpositionhorizontalline)){
    graph<-graph+geom_hline(yintercept=setpositionhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness)
  }
  if(!is.null(setpositiondottedhorizontalline)){
    graph<-graph+geom_hline(yintercept=setpositiondottedhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness,linetype="dotted")
  }
  if(ytickspecs>0&&ytickspecs<3){
    message("To adjust your y-axis, use setyaxisstart=, setyaxisend=, and setyaxisspacing=. All three parameters must be specified to adjust the y-axis.")
    stop()
  }
  if(!is.null(setyaxisspacing)){
    graph<-graph+scale_y_continuous(breaks=seq(setyaxisstart,setyaxisend,by = setyaxisspacing))
  }
  if(!is.null(panelvariable)||"facetvariable" %in% colnames(summary)){
    graph<-graph+facet_wrap(~facetvariable)
  }
  if(showblackandwhitegraph==TRUE){
    graph<-graph+scale_color_grey()+scale_fill_grey()
  }
  if(showdarkgraph==TRUE){
    graph<-graph+theme(
      plot.background = element_rect(fill = "black"),panel.background=element_rect(fill="black"),legend.background=element_rect(fill="black"), legend.key=element_rect(fill="black"),axis.text=element_text(color="white"),legend.text=element_text(color="white"),axis.title=element_text(color="white"),legend.title=element_text(color="white"))
  }
  if(!is.null(setdottransparency)){
    if(setdottransparency>1&&setdottransparency<10.1){
      message("NOTE: Transparency is typically set on a scale from 0 (completely invisible) - 1 (not at all transparent). Because you specified a transparency value greater than 1, we divided this value by 10, effectively making a scale from 1.1 - 10. However, specifying a transparency of 1 will trigger the default 0 - 1 scale and remove all transparency. To avoid confusion, we recommend using the default 0 - 1 scale in the future.")
    }
  }
  if(!is.null(setyaxistitleface)){
    graph<-graph+theme(axis.title.y=element_text(face=setyaxistitleface))
  }
  if(!is.null(setxaxistitleface)){
    graph<-graph+theme(axis.title.x=element_text(face=setxaxistitleface))
  }
  if(!is.null(setpaneltitleface)){
    graph<-graph+theme(strip.text=element_text(face=setpaneltitleface))
  }
  if(!is.null(setpaneltitlesize)){
    graph<-graph+theme(strip.text=element_text(size=setpaneltitlesize))
  }

  if(showlegend==TRUE){
    graph<-graph+theme(legend.title=element_text(face=setlegendtitleface))
  }
  if(showlegend==FALSE){
    graph<-graph+theme(legend.position = "none")
  }
  if(showgridlines==FALSE){
    graph<-graph+theme_classic()
  }
  if(!is.null(showboldedaxistitles)){
    if(showboldedaxistitles==TRUE){
      if(is.null(showboldedlegendtitle)||showboldedlegendtitle==FALSE){
        message("NOTE: You are bolding your axis titles. To also bold the legend title, use showboldedlegendtitle = TRUE")
      }
    }
  }

    if(setlinecolor!="#1D4E5D"||setdotcolor!="#3BA0BF"){
    message("NOTE: For graphs with multiple independent variables, set the line and dot color using setcolors=c(). For example: setcolors=c('purple','turquoise')")
    }

    if(length(levels(as.factor(summarydata$xvariable)))>20){
      message("NOTE: You seem to have more than 20 levels of your x-axis variable. If your x-axis variable is continuous, you may produce a scatterplot instead using the graph_scatterplot() function.")
    }
    if(!is.null(setyupperbound)){
    graph<-graph+ylim(setylowerbound,setyupperbound)
    }
    if(setlinecolor!="#1D4E5D"){
      message("WARNING: To adjust the color of your lines when setting a grouping (legend) variable, use the command setcolors=c()")
    }

  if(max(as.numeric(summarydata$yvariable))<3){
    if(jitterheight>.1){
      if(showdata==TRUE){
        message("WARNING: Because you have a relatively narrow range of values in your y-axis variable, jitter may make your datapoints appear to be in a different location than is likely to be appropriate. We recommend adjusting your jitter using the setjitterheight and setjitterwidth commands (or use the setjitter command to set both height and width simultaneously--for example, setjitter=.01).")  
      }
    }
  }
    #If they're rotating the x-axis labels
    if(showrotatedxlabels==TRUE){
    if(internalfunctionautorotation==TRUE){
      message("NOTE: X-axis labels have been rotated by default to avoid overlapping labels. To override this, add showrotatedxlabels=FALSE")
    }
    return(graph + theme(axis.text.x = element_text(angle = 90,vjust=0.5)))
  }
  if(showrotatedxlabels==FALSE){
    return(graph)
  }
}

