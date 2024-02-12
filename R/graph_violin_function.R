#### VIOLIN PLOT ####
graph_violin<-function(dv,iv1=NULL,iv2=NULL,panelvariable=NULL,showboxplot=TRUE,setboxplotcolor="default",splitx=FALSE,splitgroup=FALSE,splitpanel=FALSE,showdata=TRUE,errorbars="ci",internalfunctionautorotation=FALSE,jitterheight=.4,jitterwidth=.4,setjitterheight=NULL,setjitterwidth=NULL,setjitter=NULL,data=df,setdata=NULL,setviolintransparency=.4,dottransparency=NULL,setdottransparency=.65,setdotsize=NULL,dotsize=NULL,title=NULL,settitle=NULL,setxaxistitle=NULL,setxaxislabel=NULL,setyaxislabel=NULL,setyaxistitle=NULL,colors=NULL,color1=-1,color2=-1,color3=-1,color4=-1,color5=-1,color6=-1,color7=-1,color8=-1,color9=-1,color10=-1,color=NULL,setgrouplevels=NULL,setlegendlevels=NULL,level1=NULL,level2=NULL,level3=NULL,level4=NULL,level5=NULL,level6=NULL,level7=NULL,level8=NULL,level9=NULL,level10=NULL,legendtitle=NULL,titlesize=NULL,settitlesize=NULL,setxtitlesize=NULL,setytitlesize=NULL,setlegendtitlesize=NULL,setanimationtitlesize=16,setxaxissize=NULL,setyaxissize=NULL,setaxistextsize=NULL,setaxistitlesize=NULL,setlegendlevelsize=NULL,setpanellevels=NULL,setxlevels=NULL,split1=FALSE,split2=FALSE,split3=FALSE,showrotatedxlabels=FALSE,rotatexaxislabels=FALSE,setystandardize=FALSE,setsplitx=NULL,setsplitgroup=NULL,setsplitpanel=NULL,setcolors=NULL,setlegendtitle=NULL,showblankplot=FALSE,showviolin=TRUE,setboxplotwidth="default",setboxplottransparency="default",setwhiskerthickness=NULL,setboxplotthickness=0.5,setviolinthickness=1,setdotoutlinethickness=NULL,setdotoutlinecolor=NULL,showdotoutline=TRUE,showdots=NULL,setpositionhorizontalline=NULL,setpositiondottedhorizontalline=NULL,sethorizontallinecolor="black",sethorizontallinethickness=1,showanimation=FALSE,setanimationid=NULL,setpaneltitlesize=NULL,setanimationlevels=NULL,showboxplotoutliers=FALSE,showoutliers=NULL,groupvariable=NULL,groupingvariable=NULL,showblackandwhitegraph=FALSE,showdarkgraph=FALSE,setreversecodex=FALSE,setreversecodey=FALSE,setreversecodegroup=FALSE,setreversecodepanel=FALSE,setreverseorderx=FALSE,setreverseordergroup=FALSE,setreverseorderlegend=NULL,setreverseorderpanel=FALSE,reverseorderx=NULL,reverseordergroup=NULL,reverseorderlegend=NULL,reverseorderpanel=NULL,setxlevelorder=NULL,setgrouplevelorder=NULL,setpanellevelorder=NULL,reversecodex=NULL,reversecodey=NULL,reversecodegroup=NULL,reversecodepanel=NULL,setyaxisspacing=NULL,setyaxisend=NULL,setyaxisstart=NULL,showoutput=TRUE,showgridlines=TRUE,setlegendpositionleft=FALSE,setlegendpositionbelow=FALSE,setlegendpositionabove=FALSE,showlegend=TRUE,showlegendleft=FALSE,showlegendbelow=FALSE,showlegendabove=FALSE,setboxplotoutlinecolor="black",setwhiskercolor="black",showcoloredwhiskers=NULL,showcoloredboxplotoutline=FALSE,settitleface="bold",setlegendtitleface="plain",setxaxistitleface=NULL,setyaxistitleface=NULL,setpaneltitleface=NULL,showboldedtitle=NULL,showboldedlegendtitle=NULL,showboldedxaxistitle=NULL,showboldedyaxistitle=NULL,showboldedpaneltitle=NULL,showboldedaxistitles=NULL){

  require(ggplot2)

  '%!in%' <- function(x,y)!('%in%'(x,y))
  if(!is.null(setdata)){
    data<-setdata
  }
  if(!is.null(showcoloredwhiskers)){
    showcoloredboxplotoutline<-showcoloredwhiskers
  }
  if(!is.null(showoutliers)){
    showboxplotoutliers<-showoutliers
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
  if(!is.null(setreverseorderlegend)){
    setreverseordergroup<-setreverseorderlegend
  }
  if(!is.null(reverseorderlegend)){
    setreverseordergroup<-reverseorderlegend
  }
  if(!is.null(setxaxislabel)){
    setxaxistitle<-setxaxislabel
  }
  if(!is.null(setyaxislabel)){
    setyaxistitle<-setyaxislabel
  }
  if(!is.null(setwhiskercolor)){
    setboxplotoutlinecolor<-setwhiskercolor
  }
  if(!is.null(setwhiskerthickness)){
    setboxplotthickness<-setwhiskerthickness
  }
  if(!is.null(setgrouplevels)){
    setlegendlevels<-c(setgrouplevels)
  }
  if(showblankplot==TRUE){
    showviolin<-FALSE
    showdata<-FALSE
    showboxplot<-FALSE
  }
  if(!is.null(setdotoutlinecolor)&&is.null(setdotoutlinethickness)){
    setdotoutlinethickness<-0.5
  }
  if(is.null(setdotoutlinecolor)&&is.null(setdotoutlinethickness)){
    setdotoutlinethickness<-0
    setdotoutlinecolor<-"black"
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
  if(!is.null(groupvariable)){
    iv2<-groupvariable
  }
  if(!is.null(groupingvariable)){
    iv2<-groupingvariable
  }

  if(!is.null(setanimationid)){
    if(setanimationid %!in% colnames(data)){
      message("ERROR: The ID variable specified in setanimationid does not correspond to a valid variable name in your dataset")
      stop()
    }
  }

  if(setdotoutlinethickness==0||showdotoutline==FALSE){
    setdotoutlinethickness<-NA
  }

  if(is.null(setanimationid)){
    idvariablespecifiedbyuser<-NULL
  }

  if(!is.null(showdots)){
    showdata<-showdots
  }

  if(!is.null(setsplitpanel)){
    splitpanel<-setsplitpanel
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

  if(!is.null(settitlesize)){
    titlesize<-settitlesize
  }

  if(is.null(titlesize)){
    if(showanimation==FALSE){
      titlesize<-20
    }
    if(showanimation==TRUE){
      titlesize<-25
    }
  }

  if(!is.null(setaxistitlesize)){
    setxtitlesize<-setaxistitlesize
    setytitlesize<-setaxistitlesize
  }

  if(is.null(setxtitlesize)){
    if(showanimation==FALSE){
      setxtitlesize<-17.5
    }
    if(showanimation==TRUE){
      setxtitlesize<-20.5
    }
  }

  if(is.null(setytitlesize)){
    if(showanimation==FALSE){
      setytitlesize<-17.5
    }
    if(showanimation==TRUE){
      setytitlesize<-20.5
    }
  }

  if(is.null(setlegendtitlesize)){
    setlegendtitlesize<-17.5
  }

  if(!is.null(setaxistextsize)){
    setyaxissize<-setaxistextsize
    setxaxissize<-setaxistextsize
  }

  if(is.null(setyaxissize)){
    if(showanimation==FALSE){
      setyaxissize<-11
    }
    if(showanimation==TRUE){
      setyaxissize<-13
    }
  }

  if(is.null(setxaxissize)){
    if(showanimation==FALSE){
      setxaxissize<-11
    }
    if(showanimation==TRUE){
      setxaxissize<-13
    }
  }

  if(is.null(setpaneltitlesize)){
    if(showanimation==FALSE){
      setpaneltitlesize<-11
    }
    if(showanimation==TRUE){
      setpaneltitlesize<-15
    }
  }

  if(is.null(setlegendlevelsize)){
    setlegendlevelsize<-11
  }

  if(!is.null(settitle)){
    title<-settitle
  }

  if(setboxplottransparency=="default"&&showviolin==TRUE&&showboxplot==TRUE){
    setboxplottransparency<-.7
  }

  if(setboxplottransparency=="default"&&showviolin==FALSE&&showboxplot==TRUE){
    setboxplottransparency<-0.65
  }

  if(setboxplotwidth=="default"&&showviolin==TRUE&&showboxplot==TRUE){
    setboxplotwidth=.07
  }

  if(setboxplotwidth=="default"&&showviolin==FALSE&&showboxplot==TRUE){
    setboxplotwidth=.5
  }

  if(showrotatedxlabels!=TRUE&&showrotatedxlabels!=FALSE&&rotatexaxislabels!=TRUE&&rotatexaxislabels!=FALSE&&!is.null(showrotatedxlabels)){
    message("To rotate your x-axis labels to be vertical, rather than horizontal, use rotatexaxislabels=TRUE or showrotatedxlabels=TRUE")
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
  if(!is.null(setcolors)){
    colors<-setcolors
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

  if(!is.null(dottransparency)){
    setdottransparency<-dottransparency
  }

  if(setdottransparency>1&&setdottransparency<10.1){
    dottransparency<-setdottransparency
    setdottransparency<-setdottransparency/10
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

  if(!is.null(iv1) && length(dv)>1 && !is.null(iv2)){
    panelvariable<-iv2
    iv2<-NULL
  }
  if(rotatexaxislabels==TRUE){
    showrotatedxlabels=TRUE
  }

  if(is.null(dotsize)&&showanimation==FALSE){
    dotsize<-1.35
  }

  if(is.null(dotsize)&&showanimation==TRUE){
    dotsize<-2.5
  }

  dotsize<-as.numeric(dotsize)

  if(is.null(iv2)&&!is.null(iv1)&&length(dv)==1 || is.null(iv2)&&is.null(iv1)&&length(dv)>1){

    if(length(dv)==1){
      if(is.null(panelvariable)){
        graphvariables <- data[, c(dv, iv1)]

        if(is.character(dv) && is.character(iv1)){
          colnames(graphvariables) <- c(dv, iv1)
        }
      }
      if(!is.null(panelvariable)){
        graphvariables <- data[, c(dv, iv1, panelvariable)]

        if(is.character(dv) && is.character(iv1) && is.character(panelvariable)){
          colnames(graphvariables) <- c(dv, iv1, panelvariable)
        }
      }
    }
    if(length(dv)>1){
      require(reshape2)
      if(is.null(panelvariable)){
        reshapeddata<-data[, c(dv)]
        reshapeddata$ID<-1:NROW(reshapeddata)
        reshapeddata<-melt(reshapeddata,id.vars=c("ID"))
        message("Your variable has been re-shaped. Check the data printed below to ensure your data were reshaped correctly.")
        graphvariables<-reshapeddata[,c(3,2)] #dv, iv
        colnames(graphvariables) <- c("yvariable","xvariable")
      }
      if(!is.null(panelvariable)){
        panelvariable2<-data[,panelvariable]
        ID<-1:NROW(data)
        reshapeddata<-data.frame(ID,panelvariable2,data[, c(dv)])
        reshapeddata<-melt(reshapeddata,id.vars=c(1,2)) #First variable is ID, second is panelvariable
        message("Your variable has been re-shaped. Check the data printed below to ensure your data were reshaped correctly.")
        graphvariables<-reshapeddata[,c(4,3,2)] #dv, iv, and paneling variable
        colnames(graphvariables) <- c("yvariable", "xvariable", panelvariable)
      }
    }

    summarydata<-graphvariables
    if(length(colnames(summarydata))>2){
      colnames(summarydata) <- c("yvariable", "xvariable","facetvariable")
    }else{
      colnames(summarydata)<-c("yvariable","xvariable")
    }

    summarydata<-na.omit(summarydata)
    if(setreversecodex==TRUE){
      summarydata$xvariable<-(max(summarydata$xvariable,na.rm=T)+min(summarydata$xvariable,na.rm=T))-summarydata$xvariable
    }
    if(setreverseorderx==TRUE){
      summarydata$xvariable<-as.factor(summarydata$xvariable)
      summarydata$xvariable<-factor(summarydata$xvariable,levels=c(rev(levels(summarydata$xvariable))))
    }
    if(setreversecodey==TRUE){
      summarydata$yvariable<-(max(summarydata$yvariable,na.rm=T)+min(summarydata$yvariable,na.rm=T))-summarydata$yvariable
    }
    if(setreversecodepanel==TRUE){
      summarydata$facetvariable<-(max(summarydata$facetvariable,na.rm=T)+min(summarydata$facetvariable,na.rm=T))-summarydata$facetvariable
    }
    if(setreverseorderpanel==TRUE){
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
    if(splitx==TRUE){
      summarydata$xvariable<-as.numeric(summarydata$xvariable)
      recordedmedianx<-median(summarydata$xvariable,na.rm=T)
      summarydata$xvariable<-ifelse(summarydata$xvariable<=median(summarydata$xvariable,na.rm=T),"Low",ifelse(summarydata$xvariable>median(summarydata$xvariable,na.rm=T),"High",NA))
      summarydata$xvariable<-factor(summarydata$xvariable,levels=c("Low","High"))
      if(showoutput==TRUE){
        print(table(summarydata$xvariable))
      }
      message(sprintf("NOTE: A median split is being performed on your x-axis variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianx))
    }

    if(splitpanel==TRUE&&!is.null(panelvariable)){
      summarydata$facetvariable<-as.numeric(summarydata$facetvariable)
      recordedmedianfacet<-median(summarydata$facetvariable,na.rm=T)
      summarydata$facetvariable<-ifelse(summarydata$facetvariable<=median(summarydata$facetvariable,na.rm=T),"Low",ifelse(summarydata$facetvariable>median(summarydata$facetvariable,na.rm=T),"High",NA))
      summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c("Low","High"))
      if(showoutput==TRUE){
        print(table(summarydata$facetvariable))
      }
      message(sprintf("NOTE: A median split is being performed on your panel variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianfacet))
    }

    if(!is.null(setxlevels)){
      summarydata$xvariable<-as.factor(summarydata$xvariable)
      originallevels<-levels(summarydata$xvariable)
      levels(summarydata$xvariable)<- c(setxlevels)
      newlevels<-levels(summarydata$xvariable)
      message("Levels of x variable have been converted:")
      if(showoutput==TRUE){
        print(originallevels)
        print(newlevels)
      }
    }

    summarydata$xvariable<-as.factor(summarydata$xvariable)

    if(showblankplot==FALSE){
      #Checking for a minimum of 4 subjects per cell
      if(is.null(panelvariable)){
        frequencytable<-table(summarydata$xvariable)
        frequencytable<-as.data.frame(frequencytable)
        frequencytable<-subset(frequencytable,frequencytable$Freq>0)
        if(min(frequencytable$Freq)<4){
          if(showoutput==TRUE){
            print(table(summarydata$xvariable))
          }
          message("ERROR: A minimum of four subjects is needed across all levels of your independent variable. For details, check for cells with fewer than 4 subjects in the table printed above.")
          stop()
        }
      }

      if(!is.null(panelvariable)){
        frequencytable<-table(summarydata$xvariable,summarydata$facetvariable)
        frequencytable<-as.data.frame(frequencytable)
        frequencytable<-subset(frequencytable,frequencytable$Freq>0)
        if(min(frequencytable$Freq)<4){
          if(showoutput==TRUE){
            print(table(summarydata$xvariable,summarydata$facetvariable))
          }
          message("ERROR: A minimum of four subjects is needed across all levels of your independent variable. For details, check for cells with fewer than 4 subjects in the table printed above.")
          stop()
        }
      }
    }
    if(!is.null(setpanellevels)){
      if(!is.null(setpanellevelorder)){
        message("ERROR: Unfortunately, you cannot set the panel level order and specify custom panel labels in the present version. However, you may reverse the order of your panels using setreverseorderpanel=TRUE.")
        stop()
      }
      summarydata$facetvariable<-as.factor(summarydata$facetvariable)
      summarydata$facetvariable<-droplevels(summarydata$facetvariable)
      originallevels<-levels(summarydata$facetvariable)
      levels(summarydata$facetvariable)<- c(setpanellevels)
      newlevels<-levels(summarydata$facetvariable)
      message("Levels of paneling variable have been converted:")
      if(showoutput==TRUE){
        print(originallevels)
        print(newlevels)
      }
    }
    if(showoutput==TRUE){
      print(summarydata)
    }

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

    if(showviolin==TRUE){
      graph<-ggplot(summarydata, aes(x=xvariable,y=yvariable,color=xvariable,fill=xvariable))+
        geom_violin(aes(color=xvariable,fill=xvariable),trim=TRUE,linewidth=setviolinthickness,alpha=setviolintransparency)+
        theme(axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxissize),axis.text.y=element_text(size=setyaxissize))+
        theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface))+
        ylab(colnames(graphvariables)[1])+xlab(colnames(graphvariables)[2])
    }
    if(showviolin==FALSE){
      graph<-ggplot(summarydata, aes(x=xvariable,y=yvariable,color=xvariable,fill=xvariable))+
        theme(axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxissize),axis.text.y=element_text(size=setyaxissize))+
        theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface))+
        ylab(colnames(graphvariables)[1])+xlab(colnames(graphvariables)[2])
    }
    if(showboxplot==TRUE){
      if(showboxplotoutliers==FALSE){
        if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==FALSE){
          graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=setboxplotoutlinecolor,linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
        }
        if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==TRUE){
          graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
        }
        if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==FALSE){
          graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=setboxplotoutlinecolor,fill=c(setboxplotcolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
        }
        if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==TRUE){
          graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),fill=c(setboxplotcolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
        }
        message("NOTE: Outliers in boxplots are hidden by default. To display them, add showboxplotoutliers=TRUE")
      }
      if(showboxplotoutliers==TRUE){
        if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==FALSE){
          graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=setboxplotoutlinecolor,linewidth=setboxplotthickness,alpha=setboxplottransparency)
        }
        if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==TRUE){
          graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),linewidth=setboxplotthickness,alpha=setboxplottransparency)
        }
        if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==FALSE){
          graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=setboxplotoutlinecolor,fill=c(setboxplotcolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
        }
        if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==TRUE){
          graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),fill=c(setboxplotcolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
        }
      }
    }
    if(showdata==TRUE){
      graph<-graph+geom_point(aes(fill=xvariable),shape=21,stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=setdottransparency,position=position_jitterdodge(jitter.width=jitterwidth,jitter.height=jitterheight,dodge.width=.9))
    }
    graph<-graph+scale_fill_brewer(palette="Dark2")+scale_color_brewer(palette="Dark2")+theme(legend.position="none")

    if(showgridlines==FALSE){
      graph<-graph+theme_classic()+theme(legend.position="none")
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

    if(showanimation==TRUE){
      message("You must add a grouping variable to use animations.")
    }
  }
  #### VIOLIN PLOT PART 2 ####
  if(!is.null(iv2) || (!is.null(iv1) && length(dv)>1)){
    if(is.null(panelvariable)){
      if(length(dv)==1){
        if(is.null(setanimationid)){
          graphvariables <- data[, c(dv, iv1,iv2)]

          if(is.character(dv) && is.character(iv1) && is.character(iv2)){
            colnames(graphvariables) <- c(dv, iv1, iv2)
          }
        }
        if(!is.null(setanimationid)){
          graphvariables <- data[, c(dv, iv1,iv2,setanimationid)]

          if(is.character(dv) && is.character(iv1) && is.character(iv2)){
            colnames(graphvariables) <- c(dv, iv1, iv2,setanimationid)
          }
        }
      }
      if(length(dv)>1){
        require(reshape2)
        if(!is.null(setanimationid)){
          stop()
          message("Unfortunately, in the present version, you cannot set a custom ID variable when reshaping.")
        }

        iv_variable<-data[,iv1]

        idvariableforreshaping<-1:NROW(data)
        reshapeddata<-data.frame(idvariableforreshaping,iv_variable,data[, c(dv)])
        reshapeddata<-melt(reshapeddata,id.vars=c(1,2)) #First variable is ID, second is iv
        message("Your variable has been re-shaped. Check the data printed below to ensure your data were reshaped correctly. If the tickmarks on your x-axis are incorrect, adjust their spacing using setxaxisspacing= and the minimum and maximum values using setxaxisstart= and setxaxisend=. Note that all three of these parameters must be specified when changing an axis.")
        graphvariables<-reshapeddata[,c(4,3,2)]
        colnames(graphvariables) <- c("yvariable", "xvariable", iv1)
      }

      summarydata<-graphvariables

      #Remove the ID variable from graphvariables if it exists
      graphvariables<-graphvariables[,1:3]

      if(length(colnames(summarydata))>3){
        colnames(summarydata)<-c("yvariable","xvariable","groupvariable","idvariablespecifiedbyuser")
      }
      if(length(colnames(summarydata))==3){
        colnames(summarydata) <- c("yvariable", "xvariable", "groupvariable")
      }
    }

    if(!is.null(panelvariable)){
      if(length(dv)==1){
        if(is.null(setanimationid)){
          graphvariables <- data[, c(dv,iv1,iv2,panelvariable)]

          if(is.character(dv) && is.character(iv1) && is.character(iv2) && is.character(panelvariable)){
            colnames(graphvariables) <- c(dv, iv1, iv2,panelvariable)
          }
        }
        if(!is.null(setanimationid)){
          graphvariables <- data[, c(dv,iv1,iv2,panelvariable,setanimationid)]

          if(is.character(dv) && is.character(iv1) && is.character(iv2) && is.character(panelvariable)){
            colnames(graphvariables) <- c(dv, iv1, iv2,panelvariable,setanimationid)
          }
        }
      }
      if(length(dv)>1){
        require(reshape2)
        if(!is.null(setanimationid)){
          stop()
          message("Unfortunately, in the present version, you cannot set a custom ID variable when reshaping.")
        }
        iv_variable<-data[,iv1]
        panelvariable2<-data[,panelvariable]
        ID<-1:NROW(data)
        reshapeddata<-data.frame(ID,iv_variable,panelvariable2,data[, c(dv)])
        reshapeddata<-melt(reshapeddata,id.vars=c(1,2,3)) #First variable is ID, second is iv specified by user, third is panel variable
        message("Your variable has been re-shaped. Check the data printed below to ensure your data were reshaped correctly. If the tickmarks on your x-axis are incorrect, adjust their spacing using setxaxisspacing= and the minimum and maximum values using setxaxisstart= and setxaxisend=. Note that all three of these parameters must be specified when changing an axis.")
        graphvariables<-reshapeddata[,c(5,4,2,3)] #dv, reshaped iv, iv specified by user, panel variable
        colnames(graphvariables) <- c("yvariable", "xvariable", iv1, panelvariable)
      }
      summarydata<-graphvariables

      #Remove the ID variable from graphvariables if it exists
      graphvariables<-graphvariables[,1:4]

      if(length(colnames(summarydata))>4){
        colnames(summarydata)<-c("yvariable","xvariable","groupvariable","facetvariable","idvariablespecifiedbyuser")
      }
      if(length(colnames(summarydata))==4){
        colnames(summarydata) <- c("yvariable", "xvariable", "groupvariable","facetvariable")
      }
    }

    if(!is.null(setanimationlevels)){
      levels(summarydata$groupvariable)<-c(setanimationlevels)
    }

    summarydata<-na.omit(summarydata)
    if(setreversecodex==TRUE){
      summarydata$xvariable<-(max(summarydata$xvariable,na.rm=T)+min(summarydata$xvariable,na.rm=T))-summarydata$xvariable
    }
    if(setreverseorderx==TRUE){
      summarydata$xvariable<-as.factor(summarydata$xvariable)
      summarydata$xvariable<-factor(summarydata$xvariable,levels=c(rev(levels(summarydata$xvariable))))
    }
    if(setreversecodey==TRUE){
      summarydata$yvariable<-(max(summarydata$yvariable,na.rm=T)+min(summarydata$yvariable,na.rm=T))-summarydata$yvariable
    }
    if(setreversecodegroup==TRUE){
      summarydata$groupvariable<-(max(summarydata$groupvariable,na.rm=T)+min(summarydata$groupvariable,na.rm=T))-summarydata$groupvariable
    }
    if(setreverseordergroup==TRUE){
      summarydata$groupvariable<-as.factor(summarydata$groupvariable)
      summarydata$groupvariable<-factor(summarydata$groupvariable,levels=c(rev(levels(summarydata$groupvariable))))
    }
    if(setreversecodepanel==TRUE){
      summarydata$facetvariable<-(max(summarydata$facetvariable,na.rm=T)+min(summarydata$facetvariable,na.rm=T))-summarydata$facetvariable
    }
    if(setreverseorderpanel==TRUE){
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
    if(setystandardize==TRUE){
      summarydata$yvariable<-scale(summarydata$yvariable)
      message("Note: Your y-variable has been standardized.")
    }
    if(splitgroup==TRUE){
      summarydata$groupvariable<-as.numeric(summarydata$groupvariable)
      recordedmediangroup<-median(summarydata$groupvariable,na.rm=T)
      summarydata$groupvariable<-ifelse(summarydata$groupvariable>median(summarydata$groupvariable,na.rm=T),"High",ifelse(summarydata$groupvariable<=median(summarydata$groupvariable,na.rm=T),"Low",NA))
      summarydata$groupvariable<-factor(summarydata$groupvariable,levels=c("Low","High"))
      if(showoutput==TRUE){
        print(table(summarydata$groupvariable))
      }
      message(sprintf("NOTE: A median split is being performed on your grouping variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmediangroup))
    }

    if(splitx==TRUE){
      summarydata$xvariable<-as.numeric(summarydata$xvariable)
      recordedmedianx<-median(summarydata$xvariable,na.rm=T)
      summarydata$xvariable<-ifelse(summarydata$xvariable<=median(summarydata$xvariable,na.rm=T),"Low",ifelse(summarydata$xvariable>median(summarydata$xvariable,na.rm=T),"High",NA))
      summarydata$xvariable<-factor(summarydata$xvariable,levels=c("Low","High"))
      if(showoutput==TRUE){
        print(table(summarydata$xvariable))
      }
      message(sprintf("NOTE: A median split is being performed on your x-axis variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianx))
    }

    if(splitpanel==TRUE&&!is.null(panelvariable)){
      summarydata$facetvariable<-as.numeric(summarydata$facetvariable)
      recordedmedianfacet<-median(summarydata$facetvariable,na.rm=T)
      summarydata$facetvariable<-ifelse(summarydata$facetvariable<=median(summarydata$facetvariable,na.rm=T),"Low",ifelse(summarydata$facetvariable>median(summarydata$facetvariable,na.rm=T),"High",NA))
      summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c("Low","High"))
      if(showoutput==TRUE){
        print(table(summarydata$facetvariable))
      }
      message(sprintf("NOTE: A median split is being performed on your panel variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianfacet))
    }


    if(!is.null(setxlevels)){
      summarydata$xvariable<-as.factor(summarydata$xvariable)
      originallevels<-levels(summarydata$xvariable)
      levels(summarydata$xvariable)<- c(setxlevels)
      newlevels<-levels(summarydata$xvariable)
      message("Levels of x variable have been converted:")
      if(showoutput==TRUE){
        print(originallevels)
        print(newlevels)
      }
    }

    summarydata$xvariable<-as.factor(summarydata$xvariable)
    summarydata$groupvariable<-as.factor(summarydata$groupvariable)

    if(showblankplot==FALSE){
      #Checking for a minimum of 4 subjects per cell
      if(is.null(panelvariable)){
        frequencytable<-table(summarydata$xvariable,summarydata$groupvariable)
        frequencytable<-as.data.frame(frequencytable)
        frequencytable<-subset(frequencytable,frequencytable$Freq>0)
        if(min(frequencytable$Freq)<4){
          if(showoutput==TRUE){
            print(table(summarydata$xvariable,summarydata$groupvariable))
          }
          message("ERROR: A minimum of four subjects is needed across all combinations of the levels of your independent variables. For details, check for cells with fewer than 4 subjects in the table printed above.")
          stop()
        }
      }

      if(!is.null(panelvariable)){
        frequencytable<-table(summarydata$xvariable,summarydata$groupvariable,summarydata$facetvariable)
        frequencytable<-as.data.frame(frequencytable)
        frequencytable<-subset(frequencytable,frequencytable$Freq>0)
        if(min(frequencytable$Freq)<4){
          if(showoutput==TRUE){
            print(table(summarydata$xvariable,summarydata$groupvariable,summarydata$facetvariable))
          }
          message("ERROR: A minimum of four subjects is needed across all combinations of the levels of your independent variables. For details, check for cells with fewer than 4 subjects in the table printed above.")
          stop()
        }
      }
    }
    if(!is.null(setpanellevels)){
      if(!is.null(setpanellevelorder)){
        message("ERROR: Unfortunately, you cannot set the panel level order and specify custom panel labels in the present version. However, you may reverse the order of your panels using setreverseorderpanel=TRUE.")
        stop()
      }
      summarydata$facetvariable<-as.factor(summarydata$facetvariable)
      summarydata$facetvariable<-droplevels(summarydata$facetvariable)
      originallevels<-levels(summarydata$facetvariable)
      levels(summarydata$facetvariable)<- c(setpanellevels)
      newlevels<-levels(summarydata$facetvariable)
      message("Levels of paneling variable have been converted:")
      if(showoutput==TRUE){
        print(originallevels)
        print(newlevels)
      }
    }
    if(showoutput==TRUE){
      print(summarydata)
    }

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

    if(showanimation==TRUE){
      if(showviolin==TRUE){
        graph<-ggplot(summarydata, aes(x=xvariable,y=yvariable,color=xvariable,fill=xvariable))+
          geom_violin(aes(color=xvariable,fill=xvariable),trim=TRUE,linewidth=setviolinthickness,alpha=setviolintransparency)+
          theme(axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxissize),axis.text.y=element_text(size=setyaxissize))+
          theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface))+
          ylab(colnames(graphvariables)[1])+xlab(colnames(graphvariables)[2])
      }
      if(showviolin==FALSE){
        graph<-ggplot(summarydata, aes(x=xvariable,y=yvariable,color=xvariable,fill=xvariable))+
          theme(axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxissize),axis.text.y=element_text(size=setyaxissize))+
          theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface))+
          ylab(colnames(graphvariables)[1])+xlab(colnames(graphvariables)[2])
      }
      if(showboxplot==TRUE){
        if(showboxplotoutliers==FALSE){
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==TRUE){
            graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),fill=c(setboxplotcolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==TRUE){
            graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),fill=c(setboxplotcolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          message("NOTE: Outliers in boxplots are hidden by default. To display them, add showboxplotoutliers=TRUE")
        }
        if(showboxplotoutliers==TRUE){
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),fill=c(setboxplotcolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==TRUE){
            graph<-graph+geom_boxplot(aes(group=xvariable,color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),fill=c(setboxplotcolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
        }
      }
      if(showdata==TRUE){
        graph<-graph+geom_point(aes(group=idvariablespecifiedbyuser,fill=xvariable),shape=21,stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=setdottransparency,position=position_jitterdodge(jitter.width=jitterwidth,jitter.height=jitterheight,dodge.width=.9))
      }
      graph<-graph+scale_fill_brewer(palette="Dark2")+scale_color_brewer(palette="Dark2")+theme(legend.position="none")
    }

    if(showanimation==FALSE){
      if(showviolin==TRUE){
        graph<-ggplot(data=summarydata, aes(x=xvariable,y=yvariable,fill=groupvariable,color=groupvariable))+
          geom_violin(aes(group=interaction(xvariable,groupvariable)),trim=TRUE,linewidth=setviolinthickness,alpha=.5)+
          theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxissize),axis.text.y=element_text(size=setyaxissize))+
          guides(color="none")+ylab(colnames(graphvariables)[1])+xlab(colnames(graphvariables)[2])+
          scale_fill_discrete(name = colnames(graphvariables)[3])
      }
      if(showviolin==FALSE){
        graph<-ggplot(data=summarydata, aes(x=xvariable,y=yvariable,fill=groupvariable,color=groupvariable))+
          theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxissize),axis.text.y=element_text(size=setyaxissize))+
          guides(color="none")+ylab(colnames(graphvariables)[1])+xlab(colnames(graphvariables)[2])+
          scale_fill_discrete(name = colnames(graphvariables)[3])
      }
      if(showboxplot==TRUE){
        if(showboxplotoutliers==FALSE){
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable)),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==TRUE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable),color=groupvariable),width=setboxplotwidth,position=position_dodge(width=.9),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable)),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),fill=c(setboxplotcolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==TRUE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable),color=groupvariable),width=setboxplotwidth,position=position_dodge(width=.9),fill=c(setboxplotcolor),linewidth=setboxplotthickness,outlier.shape=NA,alpha=setboxplottransparency)
          }
          message("NOTE: Outliers in boxplots are hidden by default. To display them, add showboxplotoutliers=TRUE")
        }
        if(showboxplotoutliers==TRUE){
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable)),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]=="default"&&showcoloredboxplotoutline==TRUE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable),color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==FALSE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable)),width=setboxplotwidth,position=position_dodge(width=.9),color=c(setboxplotoutlinecolor),fill=c(setboxplotcolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
          if(setboxplotcolor[[1]]!="default"&&showcoloredboxplotoutline==TRUE){
            graph<-graph+geom_boxplot(aes(group=interaction(xvariable,groupvariable),color=xvariable),width=setboxplotwidth,position=position_dodge(width=.9),fill=c(setboxplotcolor),linewidth=setboxplotthickness,alpha=setboxplottransparency)
          }
        }
      }
      if(showdata==TRUE){
        graph<-graph+geom_point(aes(group=interaction(xvariable,groupvariable),fill=groupvariable),shape=21,stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=setdottransparency,position=position_jitterdodge(jitter.width=jitterwidth,jitter.height=jitterheight,dodge.width=.9))
      }
      suppressWarnings({
        if(showdarkgraph==TRUE){
          graph<-graph+scale_fill_brewer(palette="light1")+scale_color_brewer(palette="light1")
        }
      })
    }
    if(showviolin==FALSE&&showboxplot==TRUE){
      message("Because you have hidden violins, the default boxplot transparency and width have been changed. To adjust boxplot transparency, use setboxplottransparency = . To adjust boxplot width, use setboxplotwidth = .")
    }
    if(showgridlines==FALSE){
      graph<-graph+theme_classic()
    }

    if(!is.null(legendtitle)){
      graph<-graph + labs(fill = legendtitle)
      if(is.null(level1)&is.null(setlegendlevels)){
        graph<-graph + scale_fill_discrete(labels=c(factor(levels(summarydata$groupvariable))))
      }
    }

    if(is.null(legendtitle)){
      graph<-graph + labs(fill = colnames(graphvariables)[[3]])
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

  if (!is.null(setxaxistitle)) {
    graph <- graph + xlab(setxaxistitle)
  }

  if (!is.null(setyaxistitle)) {
    graph <- graph + ylab(setyaxistitle)
  }

  if(!is.null(setlegendlevels)){
    graph <- graph + scale_fill_discrete(labels=c(setlegendlevels))
  }

  if(!is.null(level1)&!is.null(level2)&is.null(level3)&is.null(level4)&is.null(level5)&is.null(level6)&is.null(level7)&is.null(level8)&is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&is.null(level4)&is.null(level5)&is.null(level6)&is.null(level7)&is.null(level8)&is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&!is.null(level4)&is.null(level5)&is.null(level6)&is.null(level7)&is.null(level8)&is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3,level4))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&!is.null(level4)&!is.null(level5)&is.null(level6)&is.null(level7)&is.null(level8)&is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3,level4,level5))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&!is.null(level4)&!is.null(level5)&!is.null(level6)&is.null(level7)&is.null(level8)&is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3,level4,level5,level6))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&!is.null(level4)&!is.null(level5)&!is.null(level6)&!is.null(level7)&is.null(level8)&is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3,level4,level5,level6,level7))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&!is.null(level4)&!is.null(level5)&!is.null(level6)&!is.null(level7)&!is.null(level8)&is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3,level4,level5,level6,level7,level8))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&!is.null(level4)&!is.null(level5)&!is.null(level6)&!is.null(level7)&!is.null(level8)&!is.null(level9)&is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3,level4,level5,level6,level7,level8,level9))

  }
  if(!is.null(level1)&!is.null(level2)&!is.null(level3)&!is.null(level4)&!is.null(level5)&!is.null(level6)&!is.null(level7)&!is.null(level8)&!is.null(level9)&!is.null(level10)&is.null(setlegendlevels)){
    graph<-graph+scale_fill_discrete(labels=c(level1,level2,level3,level4,level5,level6,level7,level8,level9,level10))
  }

  if(!is.null(setlegendlevels)|!is.null(level1)){
    if(color1!=-1){
      message("To set levels and colors at the same time, use colors = c('color1','color2', etc.) and setlegendlevels =c('level1','level2', etc.), rather than color1=, color2=, level1=, level2=, etc.")
    }

    if(!is.null(colors)){
      graph<-graph+scale_fill_manual(values=c(colors),labels=c(setlegendlevels))+scale_color_manual(values=c(colors))
    }
  }

  if(!is.null(level1)){
    if(color1!=-1||!is.null(colors)){
      message("To set levels and colors at the same time, use colors = c('color1','color2', etc.) and setlegendlevels =c('level1','level2', etc.), rather than color1=, color2=, level1=, level2=, etc.")
    }
  }

  if(is.null(setlegendlevels)&is.null(level1)){
    suppressWarnings({
      if(color1!=-1&color2!=-1&color3==-1&color4==-1&color5==-1&color6==-1&color7==-1&color8==-1&color9==-1&color10==-1){
        graph<-graph+scale_fill_manual(values=c(color1,color2))+scale_color_manual(values=c(color1,color2))

      }
      if(color1!=-1&color2!=-1&color3!=-1&color4==-1&color5==-1&color6==-1&color7==-1&color8==-1&color9==-1&color10==-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3))+scale_color_manual(values=c(color1,color2,color3))
      }
      if(color1!=-1&color2!=-1&color3!=-1&color4!=-1&color5==-1&color6==-1&color7==-1&color8==-1&color9==-1&color10==-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3,color4))+scale_color_manual(values=c(color1,color2,color3,color4))
      }
      if(color1!=-1&color2!=-1&color3!=-1&color4!=-1&color5!=-1&color6==-1&color7==-1&color8==-1&color9==-1&color10==-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3,color4,color5))+scale_color_manual(values=c(color1,color2,color3,color4,color5))
      }
      if(color1!=-1&color2!=-1&color3!=-1&color4!=-1&color5!=-1&color6!=-1&color7==-1&color8==-1&color9==-1&color10==-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3,color4,color5,color6))+scale_color_manual(values=c(color1,color2,color3,color4,color5,color6))
      }
      if(color1!=-1&color2!=-1&color3!=-1&color4!=-1&color5!=-1&color6!=-1&color7!=-1&color8==-1&color9==-1&color10==-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3,color4,color5,color6,color7))+scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7))
      }
      if(color1!=-1&color2!=-1&color3!=-1&color4!=-1&color5!=-1&color6!=-1&color7!=-1&color8!=-1&color9==-1&color10==-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3,color4,color5,color6,color7,color8))+scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7,color8))
      }
      if(color1!=-1&color2!=-1&color3!=-1&color4!=-1&color5!=-1&color6!=-1&color7!=-1&color8!=-1&color9!=-1&color10==-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3,color4,color5,color6,color7,color8,color9))+scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7,color8,color9))
      }
      if(color1!=-1&color2!=-1&color3!=-1&color4!=-1&color5!=-1&color6!=-1&color7!=-1&color8!=-1&color9!=-1&color10!=-1&is.null(colors)){
        graph<-graph+scale_fill_manual(values=c(color1,color2,color3,color4,color5,color6,color7,color8,color9,color10))+scale_color_manual(values=c(color1,color2,color3,color4,color5,color6,color7,color8,color9,color10))
      }
    })

    suppressWarnings({
      if(!is.null(colors)&&is.null(setlegendlevels)){
        graph<-graph+scale_fill_manual(values=colors)+scale_color_manual(values=colors)
      }
    })
  }

  if(!is.null(color)){
    message("To color violin plots, use the command setcolors=c()")
  }

  if(!is.null(setpositionhorizontalline)){
    graph<-graph+geom_hline(yintercept=setpositionhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness)
  }
  if(!is.null(setpositiondottedhorizontalline)){
    graph<-graph+geom_hline(yintercept=setpositiondottedhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness,linetype="dotted")
  }

  if(showanimation==TRUE){
    require(gganimate)
    graph<-graph+transition_states(groupvariable,transition_length=1.5,state_length=2)+labs(subtitle="{closest_state}")+theme(plot.title=element_text(size=titlesize),axis.text.x = element_text(size=setxaxissize),axis.text.y = element_text(size=setyaxissize), axis.title.x = element_text(size=setxtitlesize), axis.title.y = element_text(size=setytitlesize),plot.subtitle = element_text(size=setanimationtitlesize,hjust=0.5))+exit_fade()+enter_fade()
    if(is.null(setanimationid)&&showdata==TRUE){
      message("NOTE: Although an ID variable may sometimes be correctly inferred, no explicit ID variable is set by default. If you are using a repeated-measures animation variable and want each datapoint to refer to the same subject across frames, it is best to add setanimationid= and specify an ID variable in your dataset.")
    }
  }
  if(ytickspecs>0&&ytickspecs<3){
    message("To adjust your y-axis, use setyaxisstart=, setyaxisend=, and setyaxisspacing=. All three parameters must be specified to adjust the y-axis.")
    stop()
  }
  if(!is.null(setyaxisspacing)){
    graph<-graph+scale_y_continuous(breaks=seq(setyaxisstart,setyaxisend,by = setyaxisspacing))
  }
  if(!is.null(panelvariable)){
    graph<-graph+facet_wrap(~facetvariable)+theme(strip.text=element_text(size=setpaneltitlesize))
  }
  if(showblackandwhitegraph==TRUE){
    graph<-graph+scale_color_grey()+scale_fill_grey()
  }
  if(showdarkgraph==TRUE){
    graph<-graph+theme(
      plot.background = element_rect(fill = "black"),panel.background=element_rect(fill="black"),legend.background=element_rect(fill="black"), legend.key=element_rect(fill="black"),axis.text=element_text(color="white"),legend.text=element_text(color="white"),axis.title=element_text(color="white"),legend.title=element_text(color="white"))
  }
  if(showrotatedxlabels==TRUE){
    if(internalfunctionautorotation==TRUE){
      message("NOTE: X-axis labels have been rotated by default to avoid overlapping labels. To override this, add showrotatedxlabels=FALSE")
    }
    graph<-graph + theme(axis.text.x = element_text(angle = 90,vjust=0.5))
  }
  if(showdata==FALSE){
    message("NOTE: Individual datapoints are hidden by default. To display these, add showdata=TRUE")
  }

  if(!is.null(dottransparency)){
    if(dottransparency>1&&dottransparency<10.1){
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

  if(showlegend==TRUE){
    graph<-graph+theme(legend.title=element_text(face=setlegendtitleface))
  }
  if(showlegend==FALSE){
    graph<-graph+theme(legend.position="none")
  }
  if(!is.null(showboldedaxistitles)){
    if(showboldedaxistitles==TRUE){
      if(is.null(showboldedlegendtitle)||showboldedlegendtitle==FALSE){
        message("NOTE: You are bolding your axis titles. To also bold the legend title, use showboldedlegendtitle = TRUE")
      }
    }
  }
  if(setboxplotoutlinecolor[[1]]!="black"){
    message("NOTE: If you would like the outline color and whiskers of your boxplots to match the color of your violins, use showcoloredwhiskers=TRUE or showcoloredboxplotoutlines=TRUE")
  }
    return(graph)
}

