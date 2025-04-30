#### BAR GRAPH ####
graph_bar2<-function(dv=NULL,iv1=NULL,iv2=NULL,panelvariable=NULL,setbaroutlinecolor="gray",setbarshorizontal=FALSE,jitterheight=NULL,jitterwidth=NULL,setjitterheight=NULL,setjitterwidth=NULL,setjitter=NULL,internalfunctionautorotation=FALSE,errorbars="default",seterrorbarthickness=NULL,splitx=FALSE,splitgroup=FALSE,splitlegend=NULL,splitpanel=FALSE,showdata=TRUE,bold=FALSE,data=df,setdata=NULL,transparency=NULL,dottransparency=NULL,dotsize=NULL,title=NULL,settitle=NULL,setxaxislabel=NULL,setyaxislabel=NULL,setxaxistitle=NULL,setyaxistitle=NULL,colors=NULL,color=NULL,setcolor=NULL,setgrouplevels=NULL,setlegendlevels=NULL,legendtitle=NULL,titlesize=NULL,settitlesize=NULL,setxtitlesize=NULL,setxaxistitlesize=NULL,setytitlesize=NULL,setyaxistitlesize=NULL,setxaxistextsize=NULL,setyaxistextsize=NULL,setaxistextsize=NULL,setaxistitlesize=NULL,setlegendtitlesize=NULL,setanimationheadingsize=30,setlegendlevelsize=NULL,setlegendtextsize=NULL,setpaneltitlesize=NULL,settitleface="bold",setlegendtitleface="plain",setxaxistitleface=NULL,setyaxistitleface=NULL,setpaneltitleface=NULL,seterrorbarwidth=.2,seterrorbartransparency=NULL,showrotatedxlabels=FALSE,rotatexaxislabels=FALSE,setpanellevels=NULL,setxlevels=NULL,setxaxislevels=NULL,setystandardize=FALSE,split1=FALSE,split2=FALSE,split3=FALSE,setsplitx=NULL,setsplitgroup=NULL,setsplitlegend=NULL,setsplitpanel=NULL,seterrorbars=NULL,setcolors=NULL,showerrorbars=TRUE,setlegendtitle=NULL,setdotsize=NULL,setdottransparency=NULL,setbaroutlinethickness=NULL,showcolorederrorbars=FALSE,showspacebelowzero=TRUE,setbartransparency=0.9,dodgewidth=0.9,setdodgewidth=NULL,setdotoutlinethickness=0.5,showdots=NULL,setdotoutlinecolor=NULL,showdotoutline=TRUE,customdata=FALSE,means=NULL,datapoints=NULL,setpositionhorizontalline=NULL,setpositiondottedhorizontalline=NULL,sethorizontallinecolor="black",sethorizontallinethickness=1,showanimation=FALSE,setanimationid=NULL,setconfidencelevel=0.95,seterrorbarcolor=NULL,groupvariable=NULL,groupingvariable=NULL,showblackandwhitegraph=FALSE,showdarkgraph=NULL,setreversecodex=FALSE,setreversecodey=FALSE,setreversecodegroup=FALSE,setreversecodepanel=FALSE,setreverseorderx=FALSE,setreverseordergroup=FALSE,setreverseorderlegend=NULL,setreverseorderpanel=FALSE,reverseorderx=NULL,reverseordergroup=NULL,reverseorderlegend=NULL,reverseorderpanel=NULL,reversecodex=NULL,reversecodey=NULL,reversecodegroup=NULL,reversecodepanel=NULL,setxlevelorder=NULL,setgrouplevelorder=NULL,setlegendlevelorder=NULL,setpanellevelorder=NULL,setyaxisspacing=NULL,setyaxisend=NULL,setyaxisstart=NULL,showoutput=NULL,showgridlines=TRUE,setlegendpositionleft=FALSE,setlegendpositionbelow=FALSE,setlegendpositionabove=FALSE,showlegendleft=FALSE,showlegendbelow=FALSE,showlegendabove=FALSE,showlegend=TRUE,showboldedtitle=NULL,showboldedlegendtitle=NULL,showboldedxaxistitle=NULL,showboldedyaxistitle=NULL,showboldedpaneltitle=NULL,showboldedaxistitles=NULL,setylowerbound=NULL,setyupperbound=NULL,showlargerfonts=FALSE,setsubtitle=NULL,subtitle=NULL,setsubtitlesize=NULL,setsubtitleface=NULL,setdotshape=21,setanimationtransitionlength=1.5,setanimationlength=2,setanimationheight=500,setanimationwidth=790,...){
  
  require(Rmisc)
  require(ggplot2)
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  #Extract other arguments not explicitly listed but specified by the user
  custom_args <- list(...) #Will use this later
  
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
  if(!is.null(setreverseorderlegend)){
    setreverseordergroup<-setreverseorderlegend
  }
  if(!is.null(reverseorderlegend)){
    setreverseordergroup<-reverseorderlegend
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
  if(!is.null(setxaxislabel)){
    setxaxistitle<-setxaxislabel
  }
  if(!is.null(setyaxislabel)){
    setyaxistitle<-setyaxislabel
  }
  if(!is.null(setgrouplevels)){
    setgrouplevels<-c(setlegendlevels)
  }
  if(showdarkgraph==FALSE&&is.null(setdotoutlinecolor)){
    setdotoutlinecolor<-"black"
  }
  if(showdarkgraph==TRUE&&is.null(setdotoutlinecolor)){
    setdotoutlinecolor<-"#323232"
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
  
  if(!is.null(groupvariable)){
    iv2<-groupvariable
  }
  
  if(!is.null(groupingvariable)){
    iv2<-groupingvariable
  }
  
  if(showanimation==TRUE){
    message("Unfortunately, animations for bar plots are not available in the present version.")
  }
  
  if(!is.null(seterrorbarcolor)){
    showcolorederrorbars<-FALSE
  }
  
  if(is.null(seterrorbarcolor)){
    seterrorbarcolor<-"#7F7F7F"
  }
  
  if(!is.null(setanimationid)){
    if(setanimationid %!in% colnames(data)){
      message("ERROR: The ID variable specified in setanimationid does not correspond to a valid variable name in your dataset")
      stop()
    }
  }
  
  if(is.null(setanimationid)){
    idvariablespecifiedbyuser<-NULL
  }
  
  if(setdotoutlinethickness==0||showdotoutline==FALSE){
    setdotoutlinethickness<-NA
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
  
  if(!is.null(setjitter)){
    if(setjitter==FALSE){
      jitterheight<-0
      jitterwidth<-0
    }else{
      jitterheight<-setjitter
      jitterwidth<-setjitter
    }
  }
  
  if(!is.null(setjitterwidth)){
    jitterwidth<-setjitterwidth
  }
  
  if(!is.null(setdodgewidth)){
    dodgewidth<-setdodgewidth
  }
  
  if(!is.null(setsplitpanel)){
    splitpanel<-setsplitpanel
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
    setlegendtitlesize<-15.5
  }
  
  if(!is.null(setaxistextsize)){
    setyaxistextsize<-setaxistextsize
    setxaxistextsize<-setaxistextsize
  }
  
  if(is.null(setyaxistextsize)){
    if(showanimation==FALSE){
      setyaxistextsize<-11
    }
    if(showanimation==TRUE){
      setyaxistextsize<-13
    }
  }
  
  if(is.null(setxaxistextsize)){
    if(showanimation==FALSE){
      setxaxistextsize<-11
    }
    if(showanimation==TRUE){
      setxaxistextsize<-13
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
  
  if(is.null(seterrorbarthickness)){
    if(showcolorederrorbars==TRUE){
      seterrorbarthickness<-1
    }
    if(showcolorederrorbars==FALSE&&is.null(iv2)&&is.null(panelvariable)){
      seterrorbarthickness<-.6
    }
    if(showcolorederrorbars==FALSE&&!is.null(iv2)||showcolorederrorbars==FALSE&&!is.null(panelvariable)){
      seterrorbarthickness<-.2
    }
  }
  
  if(is.null(seterrorbartransparency)){
    seterrorbartransparency<-1
  }
  
  if(showrotatedxlabels!=TRUE&&showrotatedxlabels!=FALSE&&rotatexaxislabels!=TRUE&&rotatexaxislabels!=FALSE){
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
  
  if(!is.null(setdottransparency)){
    dottransparency<-setdottransparency
  }
  
  if(!is.null(seterrorbars)){
    errorbars<-seterrorbars
  }
  
  if(errorbars=="default"){
    errorbars<-"ci"
  }
  
  if(!is.null(iv1) && length(dv)>1 && !is.null(iv2)){
    panelvariable<-iv2
    iv2<-NULL
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
  
  if(rotatexaxislabels==TRUE){
    showrotatedxlabels=TRUE
  }
  if(!is.null(dottransparency)&&is.null(transparency)){
    transparency<-dottransparency
  }
  
  if(is.null(dotsize)&&is.null(iv2)){
    dotsize<-1.1
  }
  if(is.null(dotsize)&&!is.null(iv2)){
    dotsize<-1
  }
  
  if(is.null(transparency)){
    transparency<-0.3
  }
  
  dotsize<-as.numeric(dotsize)
  transparency<-as.numeric(transparency)
  
  if(transparency>1){
    setdottransparency<-transparency
    transparency<-transparency/10
  }
  
  if(!is.null(setlegendtextsize)){
    setlegendlevelsize<-setlegendtextsize
  }
  
  if(transparency>10){
    transparency<-.55
    message("Note: Because you set transparency to a value greater than 10, transparency was set to an automatic default value.")
  }
  
  if(showlargerfonts==TRUE){
    titlesize<-settitlesize+4
    setytitlesize<-setytitlesize+3
    setxtitlesize<-setxtitlesize+3
    setxaxistextsize<-setxaxistextsize+3
    setyaxistextsize<-setyaxistextsize+3
    setlegendlevelsize<-setlegendlevelsize+3
    setlegendtitlesize<-setlegendtitlesize+3
  }
  
  if(is.null(iv2)&&!is.null(iv1)&&length(dv)==1 || is.null(iv2)&&is.null(iv1)&&length(dv)>1||(customdata==TRUE&&"groupvariable" %!in% colnames(dv)&&"groupvariable" %!in% colnames(means))){
    if(customdata==FALSE){
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
          message("Your variable has been re-shaped. Check the data printed below to ensure your raw data were reshaped correctly.")
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
      
      if(errorbars=="ci95"||errorbars=="ci_95"||errorbars=="95ci"||errorbars=="95_ci"||errorbars=="95_CI"||errorbars=="95CI"||errorbars=="CI_95"||errorbars=="CI95"){
        setconfidencelevel<-0.95
        errorbars<-"ci"
      }
      
      if(errorbars=="ci99"&&errorbars=="ci_99"&&errorbars=="99ci"&&errorbars=="99_ci"&&errorbars=="99_CI"&&errorbars=="99CI"&&errorbars=="CI_99"&&errorbars=="CI99"){
        setconfidencelevel<-0.99
        errorbars<-"ci"
      }
      
      if(!is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('facetvariable','xvariable'),conf.interval=setconfidencelevel)
      }
      
      if(is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('xvariable'),conf.interval=setconfidencelevel)
      }
      
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
        message("Error bars reflect one standard deviation above or below each mean.")
      }
      if(errorbars=="none"||errorbars=="None" || showerrorbars==FALSE){
        summary$errors<-rep(0,NROW(summary))
        seterrorbartransparency<-0
      }
    }
    if(customdata==TRUE){
      message("Note: No grouping (color) variable was found in your dataset. If this is incorrect, make sure you have a variable with the column name 'groupvariable' in your dataframe.")
      if(is.null(means)&&is.null(datapoints)){
        summary<-dv
        graphvariables<-dv
        showdata<-FALSE
      }
      if(!is.null(means)){
        summary<-means
        graphvariables<-means
      }
      summary$xvariable<-as.factor(summary$xvariable)
      summary$yvariable<-as.numeric(summary$yvariable)
      
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
      if("errors" %!in% colnames(summary)||errorbars=="none"||errorbars=="None"||showerrorbars==FALSE){
        summary$errors<-rep(0,NROW(summary))
        seterrorbartransparency<-0
      }
      if("panelvariable" %in% colnames(summary)&&"facetvariable"%!in%colnames(summary)){
        summary$facetvariable<-summary$panelvariable
      }
      
      colnames(graphvariables)[1]<-"yvariable"
      colnames(graphvariables)[2]<-"xvariable"
      if("facetvariable" %in% colnames(summary)){
        colnames(graphvariables)[3]<-"facetvariable"
      }
    }
    if(!is.null(setpanellevels)){
      if(!is.null(setpanellevelorder)){
        message("ERROR: Unfortunately, you cannot set the panel level order and specify custom panel labels in the present version. However, you may reverse the order of your panels using setreverseorderpanel=TRUE.")
        stop()
      }
      summarydata$facetvariable<-as.factor(summarydata$facetvariable)
      summary$facetvariable<-as.factor(summary$facetvariable)
      summarydata$facetvariable<-droplevels(summarydata$facetvariable)
      summary$facetvariable<-droplevels(summary$facetvariable)
      originallevels<-levels(summarydata$facetvariable)
      levels(summarydata$facetvariable)<- c(setpanellevels)
      levels(summary$facetvariable)<- c(setpanellevels)
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
    summary2<-summary
    summary2$errorbars<-summary2$errors
    if(!is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1:2,4,9)])
    }
    
    if(is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1,3,8)])
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
    
    #Adjusting the default jitter
    if(is.null(jitterheight)){
      jitterheight<-.4
      if(showdata==TRUE){
        if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<3){
          jitterheight<-.095
          message("\nNOTE: Given the narrow range of values on the y-axis, the default jitter height has been changed. You may adjust the jitter height using setjitterheight= or adjust the jitter width using setjitterwidth= .")  
        }
        if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<2){
          jitterheight<-.05
        }
        if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<1){
          jitterheight<-.01
        }
      }
    }
    if(is.null(jitterwidth)){
      jitterwidth<-.3
    }
    
    #Adjusting the default outline thickness based on the number of levels
    if(is.null(setbaroutlinethickness)){
      if(length(levels(as.factor(summarydata$xvariable)))<=25){
        setbaroutlinethickness<-.3
      }
      if(length(levels(as.factor(summarydata$xvariable)))>25){
        setbaroutlinethickness<-0
        message("NOTE: Bar outlines have been removed by default given the number of bars in your graph. To adjust this, use the command setbaroutlinethickness= and specify the preferred bar outline thickness.")
      }
    }
    
    if(showcolorederrorbars==FALSE){
      if(showdata==TRUE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable,fill=xvariable,color=xvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=c(setbaroutlinecolor),
                   linewidth=c(setbaroutlinethickness),alpha=c(setbartransparency)) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9),color=c(seterrorbarcolor))+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))
        
        if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
          graph<-graph+geom_point(data=summarydata,shape=setdotshape,stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
        }else{
          graph<-graph+geom_point(data=summarydata,shape=setdotshape,size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
        }
        
        if(length(levels(as.factor(summarydata$xvariable)))<=9){
          graph<-graph+scale_fill_brewer(palette=1)+scale_color_brewer(palette=1)+theme(legend.position="none")
        }
        
        if(length(levels(as.factor(summarydata$xvariable)))>9){
          graph<-graph+theme(legend.position="none")
        }
        
      }
      
      if(showdata==FALSE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable,fill=xvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=c(setbaroutlinecolor),
                   linewidth=c(setbaroutlinethickness),alpha=c(setbartransparency)) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9),color=c(seterrorbarcolor))+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))
        
        if(length(levels(as.factor(summarydata$xvariable)))<=9){
          graph<-graph+scale_fill_brewer(palette=1)+scale_color_brewer(palette=1)+theme(legend.position="none")
        }
        
        if(length(levels(as.factor(summarydata$xvariable)))>9){
          graph<-graph+theme(legend.position="none")
        }
      }
    }
    if(showcolorederrorbars==TRUE){
      if(showdata==TRUE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable,fill=xvariable,color=xvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=c(setbaroutlinecolor),
                   linewidth=c(setbaroutlinethickness),alpha=c(setbartransparency)) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9))+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))
        
        if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
          graph<-graph+geom_point(data=summarydata,shape=setdotshape,stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
        }else{
          graph<-graph+geom_point(data=summarydata,shape=setdotshape,size=dotsize,alpha=transparency,position=position_jitter(height=jitterheight,width=jitterwidth))
        }
        if(length(levels(as.factor(summarydata$xvariable)))<=9){
          graph<-graph+scale_fill_brewer(palette=1)+scale_color_brewer(palette=1)+theme(legend.position="none")
        }
        
        if(length(levels(as.factor(summarydata$xvariable)))>9){
          graph<-graph+theme(legend.position="none")
        }
      }
      
      if(showdata==FALSE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable,fill=xvariable,color=xvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=c(setbaroutlinecolor),
                   linewidth=c(setbaroutlinethickness),alpha=c(setbartransparency)) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9))+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))
        
        if(length(levels(as.factor(summarydata$xvariable)))<=9){
          graph<-graph+scale_fill_brewer(palette=1)+scale_color_brewer(palette=1)+theme(legend.position="none")
        }
        
        if(length(levels(as.factor(summarydata$xvariable)))>9){
          graph<-graph+theme(legend.position="none")
        }
      }
    }
    if(showgridlines==FALSE){
      graph<-graph+theme_classic()+theme(legend.position = "none")
    }
    if(length(levels(as.factor(summarydata$xvariable)))>9){
      message("\n NOTE: Because your x-variable contains more than 9 levels, the default color palette has changed. To adjust colors manually, use setcolors=c() (e.g., setcolors=c('red','blue','green'))\n")
    }
    if(showanimation==TRUE){
      message("ERROR: You must specify a grouping variable to generate an animated plot.")
    }
  }
  
  #### Bar graph part 2 ####
  if(!is.null(iv2) || (!is.null(iv1) && length(dv)>1)||(customdata==TRUE&&"groupvariable" %in% colnames(dv))||(customdata==TRUE&&"groupvariable" %in% colnames(means))){
    if(customdata==FALSE){
      if(is.null(panelvariable)){
        if(length(dv)==1){
          graphvariables <- data[, c(dv, iv1,iv2)]
          
          if(is.character(dv) && is.character(iv1) && is.character(iv2)){
            colnames(graphvariables) <- c(dv, iv1, iv2)
          }
        }
        if(length(dv)>1){
          require(reshape2)
          iv_variable<-data[,iv1]
          ID<-1:NROW(data)
          reshapeddata<-data.frame(ID,iv_variable,data[, c(dv)])
          reshapeddata<-melt(reshapeddata,id.vars=c(1,2)) #First variable is ID, second is iv
          if(showoutput==TRUE){
            print(reshapeddata)
          }
          message("Your variable has been re-shaped. Check the data printed above to ensure your raw data were reshaped correctly.")
          graphvariables<-reshapeddata[,c(4,3,2)] #dv, reshaped iv, and iv specified by user
          colnames(graphvariables) <- c("yvariable", "xvariable", iv1)
          if(showoutput==TRUE){
            print(graphvariables)
          }
        }
        summarydata<-graphvariables
        colnames(summarydata) <- c("yvariable", "xvariable", "groupvariable")
      }
      
      if(!is.null(panelvariable)){
        if(length(dv)==1){
          graphvariables <- data[, c(dv,iv1,iv2,panelvariable)]
          
          if(is.character(dv) && is.character(iv1) && is.character(iv2) && is.character(panelvariable)){
            colnames(graphvariables) <- c(dv, iv1, iv2,panelvariable)
          }
        }
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
        if(setreverseorderpanel==TRUE){
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
      
      if(errorbars=="ci95"||errorbars=="ci_95"||errorbars=="95ci"||errorbars=="95_ci"||errorbars=="95_CI"||errorbars=="95CI"||errorbars=="CI_95"||errorbars=="CI95"){
        setconfidencelevel<-0.95
        errorbars<-"ci"
      }
      
      if(errorbars=="ci99"&&errorbars=="ci_99"&&errorbars=="99ci"&&errorbars=="99_ci"&&errorbars=="99_CI"&&errorbars=="99CI"&&errorbars=="CI_99"&&errorbars=="CI99"){
        setconfidencelevel<-0.99
        errorbars<-"ci"
      }
      
      if(!is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('xvariable','groupvariable','facetvariable'),conf.interval=setconfidencelevel)
      }
      
      if(is.null(panelvariable)){
        summary <- summarySE(summarydata, measurevar='yvariable', groupvars=c('xvariable','groupvariable'),conf.interval=setconfidencelevel)
      }
      
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
        message("Error bars reflect one standard deviation above or below each mean.")
      }
      if(errorbars=="none"||errorbars=="None" || showerrorbars==FALSE){
        summary$errors<-rep(0,NROW(summary))
        seterrorbartransparency<-0
      }
    }
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
      summary$xvariable<-as.factor(summary$xvariable)
      summary$groupvariable<-as.factor(summary$groupvariable)
      summary$yvariable<-as.numeric(summary$yvariable)
      
      if(!is.null(datapoints)){
        summarydata<-datapoints
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        summarydata$groupvariable<-as.factor(summarydata$groupvariable)
        summarydata$yvariable<-as.numeric(summarydata$yvariable)
        if("panelvariable" %in% colnames(summarydata)){
          summarydata$facetvariable<-summary$panelvariable
        }
      }
      if(is.null(datapoints)){
        showdata<-FALSE
      }
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
      if("errors" %!in% colnames(summary)||errorbars=="none"||errorbars=="None"||showerrorbars==FALSE){
        summary$errors<-rep(0,NROW(summary))
        seterrorbartransparency<-0
      }
      if("panelvariable" %in% colnames(summary)&&"facetvariable"%!in%colnames(summary)){
        summary$facetvariable<-summary$panelvariable
      }
      
      colnames(graphvariables)[1]<-"yvariable"
      colnames(graphvariables)[2]<-"xvariable"
      colnames(graphvariables)[3]<-"groupvariable"
      if("facetvariable" %in% colnames(summary)){
        colnames(graphvariables)[4]<-"facetvariable"
      }
    }
    if(!is.null(setpanellevels)){
      if(!is.null(setpanellevelorder)){
        message("ERROR: Unfortunately, you cannot set the panel level order and specify custom panel labels in the present version. However, you may reverse the order of your panels using setreverseorderpanel=TRUE.")
        stop()
      }
      summarydata$facetvariable<-as.factor(summarydata$facetvariable)
      summary$facetvariable<-as.factor(summary$facetvariable)
      summarydata$facetvariable<-droplevels(summarydata$facetvariable)
      summary$facetvariable<-droplevels(summary$facetvariable)
      originallevels<-levels(summarydata$facetvariable)
      levels(summarydata$facetvariable)<- c(setpanellevels)
      levels(summary$facetvariable)<- c(setpanellevels)
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
    
    summary2<-summary
    summary2$errorbars<-summary2$errors
    if(is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1,2,4,9)])
    }
    
    if(!is.null(panelvariable)&&showoutput==TRUE){
      print(summary2[,c(1:3,5,10)])
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
    
    #Adjusting the default jitter
    if(is.null(jitterheight)){
      jitterheight<-.4
      if(showdata==TRUE){
        if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<3){
          jitterheight<-.095
          message("\nNOTE: Given the narrow range of values on the y-axis, the default jitter height has been changed. You may adjust the jitter height using setjitterheight= or adjust the jitter width using setjitterwidth= .")  
        }
        if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<2){
          jitterheight<-.05
        }
        if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<1){
          jitterheight<-.01
        }
      }
    }
    if(is.null(jitterwidth)){
      jitterwidth<-.3
    }
    
    #Adjusting the default outline thickness based on the number of levels
    if(is.null(setbaroutlinethickness)){
      if(length(levels(as.factor(summarydata$groupvariable)))*length(levels(as.factor(summarydata$xvariable)))<=25){
        setbaroutlinethickness<-1
      }
      if(length(levels(as.factor(summarydata$groupvariable)))*length(levels(as.factor(summarydata$xvariable)))>25){
        setbaroutlinethickness<-0
        message("NOTE: Bar outlines have been removed by default given the number of bars in your graph. To adjust this, use the command setbaroutlinethickness= and specify the preferred bar outline thickness.")
      }
    }
    
    if(showcolorederrorbars==FALSE){
      if(showdata==TRUE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable, fill=groupvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=setbaroutlinecolor,
                   linewidth=c(setbaroutlinethickness),alpha=setbartransparency) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9),color=seterrorbarcolor)+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
          scale_fill_discrete(name = colnames(graphvariables)[3])+guides(color = "none")
      
      if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){    
        graph<-graph+geom_point(data=summarydata,shape=setdotshape,aes(fill=groupvariable),stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=transparency,position=position_jitterdodge(jitter.height=jitterheight,jitter.width=jitterwidth,dodge.width=dodgewidth))
      }else{
        graph<-graph+geom_point(data=summarydata,shape=setdotshape,aes(color=groupvariable),size=dotsize,alpha=transparency,position=position_jitterdodge(jitter.height=jitterheight,jitter.width=jitterwidth,dodge.width=dodgewidth))  
      }
    }
      if(showdata==FALSE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable, fill=groupvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=setbaroutlinecolor,
                   linewidth=c(setbaroutlinethickness),alpha=setbartransparency) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9),color=seterrorbarcolor)+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
          scale_fill_discrete(name = colnames(graphvariables)[3])+guides(color="none")
      }
    }
    
    if(showcolorederrorbars==TRUE){
      if(showdata==TRUE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable, fill=groupvariable,color=groupvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=c(setbaroutlinecolor),
                   linewidth=c(setbaroutlinethickness),alpha=c(setbartransparency)) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9))+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
          scale_fill_discrete(name = colnames(graphvariables)[3])+guides(color = "none")
      
      if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){     
        graph<-graph+geom_point(data=summarydata,shape=setdotshape,aes(fill=groupvariable),stroke=setdotoutlinethickness,color=setdotoutlinecolor,size=dotsize,alpha=transparency,position=position_jitterdodge(jitter.height=jitterheight,jitter.width=jitterwidth,dodge.width=dodgewidth))
      }else{
        graph<-graph+geom_point(data=summarydata,shape=setdotshape,aes(color=groupvariable),size=dotsize,alpha=transparency,position=position_jitterdodge(jitter.height=jitterheight,jitter.width=jitterwidth,dodge.width=dodgewidth))
      }
    }
      
      if(showdata==FALSE){
        
        graph<-ggplot(summary, aes(x=xvariable, y=yvariable, fill=groupvariable,color=groupvariable)) +
          geom_bar(position=position_dodge(), stat="identity",
                   colour=c(setbaroutlinecolor),
                   linewidth=c(setbaroutlinethickness),alpha=c(setbartransparency)) +
          geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                        linewidth=seterrorbarthickness,
                        width=seterrorbarwidth,alpha=seterrorbartransparency,
                        position=position_dodge(.9))+
          ylab(colnames(graphvariables)[1]) +
          xlab(colnames(graphvariables)[2]) +
          expand_limits(y=0) +
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
          scale_fill_discrete(name = colnames(graphvariables)[3])+guides(color="none")
      }
    }
    
    if(showanimation==TRUE){
      graph<-ggplot(summary, aes(x=xvariable, y=yvariable, fill=xvariable,color=xvariable)) +
        geom_bar(position=position_dodge(), stat="identity",
                 colour=setbaroutlinecolor,
                 linewidth=c(setbaroutlinethickness),alpha=setbartransparency) +
        geom_errorbar(aes(ymin=yvariable-errors, ymax=yvariable+errors),
                      linewidth=seterrorbarthickness,
                      width=seterrorbarwidth,alpha=seterrorbartransparency,
                      position=position_dodge(.9),color=seterrorbarcolor)+
        ylab(colnames(graphvariables)[1]) +
        xlab(colnames(graphvariables)[2]) +
        expand_limits(y=0) +
        theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,
                                                 face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),
                         axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize))
    }
    
    suppressWarnings({
      if(showdarkgraph==TRUE){
        graph<-graph+scale_fill_brewer(palette="light1")+scale_color_brewer(palette="light1")
      }
    })
    if(showgridlines==FALSE){
      graph<-graph+theme_classic()
    }
    if(showlegend==FALSE){
      graph<-graph+theme(legend.position="none")
    }
    if(!is.null(legendtitle)){
      graph<-graph + labs(fill = legendtitle)
      if(is.null(setlegendlevels)){
        graph<-graph + scale_fill_discrete(labels=c(factor(levels(summarydata$groupvariable))))
      }
    }
    
    if(is.null(legendtitle)){
      graph<-graph + labs(fill = colnames(graphvariables)[[3]])
    }
    if(showlegend==TRUE){
      graph<-graph+theme(legend.title=element_text(face=setlegendtitleface))
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
    graph <- graph + scale_fill_discrete(labels=c(setlegendlevels))
  }
  
  if(!is.null(setlegendlevels)){
    if(!is.null(colors)){
      graph<-graph+scale_fill_manual(values=c(colors),labels=c(setlegendlevels))+scale_color_manual(values=c(colors),labels=c(setlegendlevels))
    }
  }
  if(is.null(setlegendlevels)){
    if(!is.null(colors)){
      graph<-graph+scale_fill_manual(values=c(colors))+scale_color_manual(values=c(colors))
    }
  }
  
  if(!is.null(color)){
    message("To color bar graphs, use the command setcolors=c()")
  }
  
  if(setbarshorizontal==TRUE){
    graph<-graph+coord_flip()
  }
  
  if(showspacebelowzero==FALSE){
    graph<-graph+coord_cartesian(expand = FALSE)
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
  
  if(showanimation==TRUE){
    require(gganimate)
    require(magick)
    
    graph<-graph+transition_states(groupvariable,transition_length=c(setanimationtransitionlength),state_length=c(setanimationlength))+labs(subtitle="{closest_state}",title=title)+
      theme(plot.title=element_text(size=titlesize),axis.text.x = element_text(size=setxaxistextsize),axis.text.y = element_text(size=setyaxistextsize), 
            axis.title.x = element_text(size=setxtitlesize), axis.title.y = element_text(size=setytitlesize),plot.subtitle = element_text(size=setanimationheadingsize,hjust=0.5))+
      enter_grow()+exit_shrink()+exit_fade()+enter_fade()+ease_aes("sine-in-out")
    
    if(is.null(colors)){
      if(length(levels(as.factor(summarydata$xvariable)))<=9){
        graph<-graph+scale_fill_brewer(palette=1)+theme(legend.position="none")
      }
      
      if(length(levels(as.factor(summarydata$xvariable)))>9){
        graph<-graph+theme(legend.position="none")
      }  
    }
    
    if(!is.null(setanimationid)){
      message("ERROR: Showing data or setting an animation ID when creating an animated bar plot is not available in the current version of Plotzing.")
      stop()
    }
    if(setanimationheight<150||setanimationwidth<150){
      if("animationoverride" %!in% names(custom_args)){
        message("\nERROR: Your animation is too small. Height and width are set in pixels. The default commands are setanimationheight=500 and setanimationwidth=790. If you wish to generate an extremely small animation and override this message, use the command animationoverride=TRUE")
        stop()  
      }
    }
    return(animate(graph,renderer=magick_renderer(),height=setanimationheight,width=setanimationwidth,units="px"))
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
      message("Note: Transparency is typically set on a scale from 0 (completely invisible) - 1 (not at all transparent). Because you specified a transparency value greater than 1, we divided this value by 10, effectively making a scale from 1.1 - 10. However, specifying a transparency of 1 will trigger the default 0 - 1 scale and remove all transparency. To avoid confusion, we recommend using the default 0 - 1 scale in the future.")
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
  if(!is.null(setyupperbound)){
    graph<-graph+ylim(setylowerbound,setyupperbound)
    if(setylowerbound>0){
      message("WARNING: To display the bars in a bar graph, the lower bound of your y-axis may need to be set to 0.")
    }
  }
  if(!is.null(showboldedaxistitles)){
    if(showboldedaxistitles==TRUE){
      if(is.null(showboldedlegendtitle)||showboldedlegendtitle==FALSE){
        message("NOTE: You are bolding your axis titles. To also bold the legend title, use showboldedlegendtitle = TRUE")
      }
    }
  }
  if(max(as.numeric(summarydata$yvariable))<3){
    if(jitterheight>.1){
      if(showdata==TRUE){
        message("WARNING: Because you have a relatively narrow range of values in your y-axis variable, jitter may make your datapoints appear to be in a different location than is likely to be appropriate. We recommend adjusting your jitter using the setjitterheight and setjitterwidth commands (or use the setjitter command to set both height and width simultaneously--for example, setjitter=.01).")  
      }
    }
  }
  
  if("setxaxissize" %in% names(custom_args)){
    message("The command setxaxissize is no longer available. Use the command setxaxistextsize instead.")
  }
  if("setyaxissize" %in% names(custom_args)){
    message("The command setyaxissize is no longer available. Use the command setyaxistextsize instead.")
  }
  if(showrotatedxlabels==TRUE){
    if(internalfunctionautorotation==TRUE){
      message("NOTE: X-axis labels have been rotated by default to avoid overlapping labels. To override this, add showrotatedxlabels=FALSE")
    }
    return(graph + theme(axis.text.x = element_text(angle = 90,vjust=0.5)))
  }
  if(setdotshape!=21&&setdotshape!=22&&setdotshape!=23&&setdotshape!=24&&setdotshape!=25&&setdotshape!="circle filled"&&setdotshape!="diamond filled"&&setdotshape!="square filled"&&setdotshape!="triangle down filled"&&setdotshape!="triangle filled"&&setdotoutlinethickness!=0&&!is.na(setdotoutlinethickness)){
    message("NOTE: Dot outlines have been removed based on the shape you specified. To restore these, use a shape with fill (e.g., setdotshape = 'triangle filled', setdotshape = 'square filled', etc.)")
  }
  
  if(showrotatedxlabels==FALSE){
    return(graph)
  }
}