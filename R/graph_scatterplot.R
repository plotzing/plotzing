#### SCATTERPLOT ####
graph_scatterplot<-function(dv,iv1=NULL,iv2=NULL,panelvariable=NULL,data=df,setdata=NULL,showline=TRUE,splitgroup=FALSE,splitlegend=NULL,jitterheight=NULL,jitterwidth=NULL,setjitter=NULL,setjitterheight=NULL,setjitterwidth=NULL,setlinetype="lm",showloessline=FALSE,showsimpleslopesplot=FALSE,dotsize=NULL,textsize=NULL,linethickness=NULL,dottransparency=NULL,transparency=NULL,dotcolor=NULL,linecolor=NULL,colors=NULL,title=NULL,settitle=NULL,setxaxislabel=NULL,setyaxislabel=NULL,setxaxistitle=NULL,setyaxistitle=NULL,legendtitle=NULL,setgrouplevels=NULL,setlegendlevels=NULL,showdata=TRUE,showblankplot=FALSE,color=NULL,setexclusionabove=NULL,setexclusionright=NULL,splitx=FALSE,splitpanel=FALSE,titlesize=NULL,settitlesize=NULL,setxtitlesize=NULL,setxaxistitlesize=NULL,setytitlesize=NULL,setyaxistitlesize=NULL,setxaxistextsize=NULL,setyaxistextsize=NULL,setaxistextsize=NULL,setaxistitlesize=NULL,setlegendtitlesize=NULL,setanimationheadingsize=30,setlegendlevelsize=NULL,setlegendtextsize=NULL,setpaneltitlesize=NULL,shading=NULL,showcoloredshading=NULL,shadingcolor="gray",colorful=FALSE,setpanellevels=NULL,showrotatedxlabels=FALSE,rotatexaxislabels=FALSE,setxstandardize=FALSE,setystandardize=FALSE,split1=FALSE,split2=FALSE,setxaxisspacing=NULL,setyaxisspacing=NULL,setxaxisstart=NULL,setxaxisend=NULL,setyaxisstart=NULL,setyaxisend=NULL,setlegendtitle=NULL,setdotsize=NULL,setdottransparency=NULL,setsplitx=NULL,setsplitgroup=NULL,setsplitlegend=NULL,setsplitpanel=NULL,setcolors=NULL,setcolor=NULL,showdots=NULL,setdotoutlinethickness=NULL,setdotoutlinecolor=NULL,showdotoutline=TRUE,showshading=NULL,setpositionhorizontalline=NULL,setpositiondottedhorizontalline=NULL,sethorizontallinecolor="black",sethorizontallinethickness=1,showdashedlines=FALSE,internalfunctionautorotation=FALSE,showcolorblindgraph=FALSE,setconfidencelevel=0.95,errorbars=NULL,seterrorshading="ci",showanimation=FALSE,setanimationid=NULL,setdotcolor=NULL,setlinecolor=NULL,setshadingcolor=NULL,setcustomintercept=NULL,setcustomslope=NULL,setlinethickness=NULL,groupvariable=NULL,groupingvariable=NULL,showblackandwhitegraph=FALSE,showdarkgraph=NULL,setreversecodex=FALSE,setreversecodey=FALSE,setreversecodegroup=FALSE,setreversecodepanel=FALSE,setreverseorderx=FALSE,setreverseordergroup=FALSE,setreverseorderlegend=NULL,setreverseorderpanel=FALSE,reverseordergroup=NULL,reverseorderlegend=NULL,reverseorderpanel=NULL,setgrouplevelorder=NULL,setlegendlevelorder=NULL,setpanellevelorder=NULL,reversecodex=NULL,reversecodey=NULL,reversecodegroup=NULL,reversecodepanel=NULL,showoutput=NULL,showgridlines=TRUE,setlegendpositionleft=FALSE,setlegendpositionbelow=FALSE,setlegendpositionabove=FALSE,showlegendleft=FALSE,showlegendbelow=FALSE,showlegendabove=FALSE,showlegend=TRUE,settitleface="bold",setlegendtitleface="plain",setxaxistitleface=NULL,setyaxistitleface=NULL,setpaneltitleface=NULL,showboldedtitle=NULL,showboldedlegendtitle=NULL,showboldedxaxistitle=NULL,showboldedyaxistitle=NULL,showboldedpaneltitle=NULL,showboldedaxistitles=NULL,setxlowerbound=NULL,setxupperbound=NULL,setylowerbound=NULL,setyupperbound=NULL,setreversecodelegend=NULL,showlargerfonts=FALSE,setsubtitle=NULL,subtitle=NULL,setsubtitlesize=NULL,setsubtitleface=NULL,setdotshape=21,seterrorshadingtransparency=NULL,setlinetransparency=1,setanimationheight=500,setanimationwidth=790,setanimationtransitionlength=1.5,setanimationlength=2,showsimpleslopes=NULL,...){
  
  require(ggplot2)
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  #Extract other arguments not explicitly listed but specified by the user
  custom_args <- list(...) #Will use this later
  
  if(!is.null(showsimpleslopes)){
    if(showsimpleslopes==TRUE){
      showsimpleslopesplot<-TRUE  
    }
  }
  
  if(is.null(seterrorshadingtransparency)){
    if(showsimpleslopesplot==FALSE){
      seterrorshadingtransparency<-.395
    }
    if(showsimpleslopesplot==TRUE){
      seterrorshadingtransparency<-.2
    }
  }
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
  if(setreverseorderx==TRUE){
    message("ERROR: You cannot reverse the levels of your x-variable when graphing a scatterplot because your x-variable must be continuous. To reverse-code your variable instead, use setreversecodex=TRUE")
    stop()
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
  if(!is.null(setxupperbound)||!is.null(setxlowerbound)){
    if(is.null(setxlowerbound)){
      message("To adjust the bounds of your x-axis, you must also specify a lower bound using setxlowerbound=")
      stop()
    }
    if(is.null(setxupperbound)){
      message("To adjust the bounds of your x-axis, you must also specify an upper bound using setxupperbound=")
      stop()
    }
  }
  if(!is.null(setxaxistitlesize)){
    setxtitlesize<-setxaxistitlesize
  }
  if(!is.null(setyaxistitlesize)){
    setytitlesize<-setyaxistitlesize
  }
  if(!is.null(setdata)){
    data<-setdata
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
  if(!is.null(setlegendlevelorder)){
    setgrouplevelorder<-setlegendlevelorder
  }
  if(!is.null(setxaxislabel)){
    setxaxistitle<-setxaxislabel
  }
  if(!is.null(setyaxislabel)){
    setyaxistitle<-setyaxislabel
  }
  if(showblankplot==TRUE){
    setdottransparency<-0
    showline<-FALSE
  }
  if(!is.null(setgrouplevels)){
    setlegendlevels<-c(setgrouplevels)
  }
  if(showdata==FALSE&&showline==FALSE){
    setdottransparency<-0
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
  if(!is.null(setreversecodelegend)){
    setreversecodegroup<-setreversecodelegend
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
  
  if(is.null(shading)){
    shading<-TRUE
  }
  
  if(!is.null(groupvariable)){
    iv2<-groupvariable
  }
  
  if(!is.null(groupingvariable)){
    iv2<-groupingvariable
  }
  if(!is.null(setcustomintercept)&&is.null(setcustomslope)){
    message("To create a custom line, you must also specify a custom slope using setcustomslope =")
  }
  
  if(!is.null(setcustomslope)&&is.null(setcustomintercept)){
    message("To create a custom line, you must also specify a custom intercept using setcustomintercept =")
  }
  if(!is.null(setcustomslope)&&!is.null(setcustomintercept)){
    showline<-FALSE
  }
  if(!is.null(setlinethickness)){
    linethickness<-setlinethickness
  }
  if(!is.null(setdotcolor)){
    dotcolor<-setdotcolor
  }
  if(!is.null(setlinecolor)){
    linecolor<-c(setlinecolor)
  }
  if(!is.null(setshadingcolor)){
    shadingcolor<-setshadingcolor
  }
  if(!is.null(setdotoutlinecolor)&&is.null(setdotoutlinethickness)){
    setdotoutlinethickness<-2
  }
  if(!is.null(setdotoutlinethickness)&&is.null(setdotoutlinecolor)){
    setdotoutlinecolor<-"black"
  }
  if(is.null(setdotoutlinecolor)&&is.null(setdotoutlinethickness)){
    setdotoutlinethickness<-0
    setdotoutlinecolor<-"black"
  }
  if(!is.null(setanimationid)){
    if(setanimationid %!in% colnames(data)){
      message("ERROR: The ID variable specified in setanimationid does not correspond to a valid variable name in your dataset")
      stop()
    }
  }
  
  if(!is.null(setcolor)){
    color<-setcolor
  }
  
  if(!is.null(errorbars)){
    seterrorshading<-errorbars
  }
  
  if(is.null(setanimationid)){
    idvariablespecifiedbyuser<-NULL
  }
  
  if(seterrorshading=="99ci"||seterrorshading=="99_ci"||seterrorshading=="99_CI"||seterrorshading=="99CI"||seterrorshading=="ci99"||seterrorshading=="CI99"||seterrorshading=="CI_99"||seterrorshading=="ci_99"){
    setconfidencelevel<-0.99
  }
  
  if(setconfidencelevel!=0.95){
    message(sprintf("Note: Error shading has been set to %s percent confidence intervals.", setconfidencelevel))
  }
  
  if(setdotoutlinethickness==0||showdotoutline==FALSE){
    setdotoutlinethickness<-NA
  }
  
  if(showcolorblindgraph==TRUE){
    showdashedlines<-TRUE
  }
  
  if(!is.null(showdots)){
    showdata<-showdots
  }
  
  if(!is.null(showshading)){
    shading<-showshading
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
  
  if(is.null(setyaxistextsize)){
    if(showanimation==FALSE){
      setyaxistextsize<-11
    }
    if(showanimation==TRUE){
      setyaxistextsize<-13
    }
  }
  
  if(!is.null(setaxistextsize)){
    setyaxistextsize<-setaxistextsize
    setxaxistextsize<-setaxistextsize
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
      setpaneltitlesize<-13
    }
    if(showanimation==TRUE){
      setpaneltitlesize<-16
    }
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
  
  if(!is.null(settitle)){
    title<-settitle
  }
  
  if(showrotatedxlabels!=TRUE&&showrotatedxlabels!=FALSE&&rotatexaxislabels!=TRUE&&rotatexaxislabels!=FALSE&&!is.null(showrotatedxlabels)){
    message("To rotate your x-axis labels to be vertical, rather than horizontal, use rotatexaxislabels=TRUE or showrotatedxlabels=TRUE")
  }
  
  if(!is.null(setcolors)){
    colors<-setcolors
  }
  
  if(!is.null(setsplitx)){
    splitx<-setsplitx
  }
  
  if(!is.null(setsplitpanel)){
    splitpanel<-setsplitpanel
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
  
  if(split1==TRUE){
    splitgroup<-TRUE
  }
  
  if(split2==TRUE){
    splitpanel<-TRUE
  }
  
  if(rotatexaxislabels==TRUE){
    showrotatedxlabels=TRUE
  }
  
  if(!is.null(iv1) && length(dv)>1 && !is.null(iv2)){
    panelvariable<-iv2
    iv2<-NULL
  }
  
  if(rotatexaxislabels==TRUE){
    showrotatedxlabels=TRUE
  }
  
  if(colorful==TRUE){
    showcoloredshading<-TRUE
  }
  
  if(showloessline==TRUE){
    setlinetype <- "loess"
  }
  
  if(splitx==TRUE){
    message("You cannot perform a median split on the x variable when using the scatterplot function.")
  }
  
  if(!is.null(dottransparency)&&is.null(transparency)){
    transparency<-dottransparency
  }
  
  
  if(is.null(dotsize)&&showanimation==FALSE){
    dotsize<-1.4
  }
  
  if(is.null(dotsize)&&showanimation==TRUE){
    dotsize<-2.3
  }
  
  if(is.null(transparency)){
    if(showsimpleslopesplot==FALSE){
      transparency<-0.25
    }
    if(showsimpleslopesplot==TRUE){
      transparency<-.4
    }
  }
  
  dotsize<-as.numeric(dotsize)
  transparency<-as.numeric(transparency)
  
  if(transparency>1&transparency<10.1){
    setdottransparency<-transparency
    transparency<-transparency/10
  }
  if(is.null(dotcolor)){
    dotcolor<-"#2eb1d1"
  }
  
  if(is.null(linethickness)){
    if(!is.null(setcustomintercept)&&!is.null(setcustomslope)){
      linethickness<-1.2
    }else{
      linethickness<-0.95
    }
  }
  
  if(is.null(linecolor)){
    linecolor<-c("#1e7388")
  }
  
  if(is.null(textsize)){
    textsize<-10.5
  }
  
  xtickspecs<-0
  ytickspecs<-0
  
  if(!is.null(setxaxisspacing)){
    xtickspecs<-xtickspecs+1
  }
  
  if(!is.null(setxaxisend)){
    xtickspecs<-xtickspecs+1
  }
  
  if(!is.null(setxaxisstart)){
    xtickspecs<-xtickspecs+1
  }
  
  if(!is.null(setyaxisspacing)){
    ytickspecs<-ytickspecs+1
  }
  
  if(!is.null(setyaxisend)){
    ytickspecs<-ytickspecs+1
  }
  
  if(!is.null(setyaxisstart)){
    ytickspecs<-ytickspecs+1
  }
  
  if(!is.null(color)&&is.null(iv2)){
    dotcolor<-c(color)
    linecolor<-c(color)
    message("To set the line and dot colors separately, use the commands setlinecolor= and setdotcolor =")
  }
  if(!is.null(colors)&&is.null(iv2)){
    message("To set the color for a scatterplot with one regression line, use the commands setlinecolor= and setdotcolor = (or, to set both simultaneously, use setcolor= )")
  }
  
  if(!is.null(color)&&!is.null(iv2)&&showanimation==FALSE){
    message("To set the colors for each line, use setcolors=c()")
  }
  
  if(!is.null(color)&&showanimation==TRUE){
    dotcolor<-color
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
  
  if(showdata==FALSE){
    transparency<-0
    message("Datapoints are present but set to maximum transparency, rendering them invisible. This means that the axes are still sized by default to account for all datapoints, but these datapoints are not visible. If the axes look unusual in size, it is likely because of an outlier in your data. You may see this using showdata=TRUE. Exclude outliers using setexclusionabove= or setexclusionright= to exclude values beyond a given threshold.")
    showdata<-TRUE
  }
  
  if(showdata==TRUE){
    if(is.null(iv2)&&!is.null(iv1)&&length(dv)==1 || is.null(iv2)&&is.null(iv1)&&length(dv)>1){
      if(showblackandwhitegraph==TRUE){
        linecolor<-"black"
        dotcolor<-"gray"
        shadingcolor<-"light gray"
      }
      
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
          message("Your variable has been re-shaped. Check the data printed below to ensure your data were reshaped correctly. If the tickmarks on your x-axis are incorrect, adjust their spacing using setxaxisspacing= and the minimum and maximum values using setxaxisstart= and setxaxisend=. Note that all three of these parameters must be specified when changing an axis.")
          graphvariables<-reshapeddata[,c(3,2)] #dv, iv
          colnames(graphvariables) <- c("yvariable","xvariable")
        }
        if(!is.null(panelvariable)){
          panelvariable2<-data[,panelvariable]
          ID<-1:NROW(data)
          reshapeddata<-data.frame(ID,panelvariable2,data[, c(dv)])
          reshapeddata<-melt(reshapeddata,id.vars=c(1,2)) #First variable is ID, second is panelvariable
          message("Your variable has been re-shaped. Check the data printed below to ensure your data were reshaped correctly. If the tickmarks on your x-axis are incorrect, adjust their spacing using setxaxisspacing= and the minimum and maximum values using setxaxisstart= and setxaxisend=. Note that all three of these parameters must be specified when changing an axis.")
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
      
      if(!is.null(setexclusionabove)){
        summarydata<-subset(summarydata,summarydata$yvariable<=setexclusionabove)
        message(sprintf("Note: Points on the y-axis greater than %s have been removed.", setexclusionabove))
      }
      
      if(!is.null(setexclusionright)){
        summarydata<-subset(summarydata,summarydata$xvariable<=setexclusionright)
        message(sprintf("Note: Points on the x-axis greater than %s have been removed.", setexclusionright))
      }
      if(setreversecodex==TRUE){
        summarydata$xvariable<-(max(summarydata$xvariable,na.rm=T)+min(summarydata$xvariable,na.rm=T))-summarydata$xvariable
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
      
      if(setxstandardize==TRUE){
        summarydata$xvariable<-scale(summarydata$xvariable)
        message("Note: Your x-variable has been standardized.")
      }
      
      summarydata$xvariable<-as.numeric(summarydata$xvariable)
      summarydata$yvariable<-as.numeric(summarydata$yvariable)
      
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
      
      if(is.null(showcoloredshading)){
        if(showdarkgraph==FALSE){
          showcoloredshading<-FALSE
        }
        if(showdarkgraph==TRUE){
          showcoloredshading<-TRUE
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
      if(!is.null(setcustomintercept)){
        if(is.null(setxaxisstart)&&min(summarydata$xvariable,na.rm=T)>-.001){
          setxaxisstart<-0
          setxaxisend<-round(max(summarydata$xvariable,na.rm=T),0)
          if(round(max(summarydata$xvariable,na.rm=T),0)>50){
            setxaxisspacing<-10
          }
          if(round(max(summarydata$xvariable,na.rm=T),0)<=50&&round(max(summarydata$xvariable,na.rm=T),0)>20){
            setxaxisspacing<-5
          }
          if(round(max(summarydata$xvariable,na.rm=T),0)<=20&&round(max(summarydata$xvariable,na.rm=T),0)>2){
            setxaxisspacing<-1
          }
          if(round(max(summarydata$xvariable,na.rm=T),0)<=2){
            setxaxisspacing<-.1
          }
        }
        if(is.null(setyaxisstart)&&min(summarydata$yvariable,na.rm=T)>-.001){
          setyaxisstart<-0
          setyaxisend<-round(max(summarydata$yvariable,na.rm=T),0)
          if(round(max(summarydata$yvariable,na.rm=T),0)>50){
            setyaxisspacing<-10
          }
          if(round(max(summarydata$yvariable,na.rm=T),0)<=50&&round(max(summarydata$yvariable,na.rm=T),0)>20){
            setyaxisspacing<-5
          }
          if(round(max(summarydata$yvariable,na.rm=T),0)<=20&&round(max(summarydata$yvariable,na.rm=T),0)>2){
            setyaxisspacing<-1
          }
          if(round(max(summarydata$yvariable,na.rm=T),0)<=2){
            setyaxisspacing<-.1
          }
        }
        if(showoutput==TRUE){
          print("Y-axis tick spacing set to:")
          print(setyaxisspacing)
          print("X-axis tick spacing set to:")
          print(setxaxisspacing)
        }
        message("You have set a custom slope and intercept. To adjust where the x and y-axis start and end, or to adjust the spacing of tickmarks, use setxaxisstart= , setxaxisend=, and setxaxisspacing = (or, for the y-axis, setyaxisstart =, setyaxisend=, etc.)")
      }
      
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
        jitterwidth<-.4
        if(showdata==TRUE){
          if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<3){
            jitterwidth<-.095
            message("\nNOTE: Given the narrow range of values on the x-axis, the default jitter width has been changed. You may adjust the jitter width using setjitterwidth= or adjust the jitter height using setjitterheight= .")  
          }
          if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<2){
            jitterwidth<-.05
          }
          if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<1){
            jitterwidth<-.01
          }
        }
      }
      if(showcoloredshading==TRUE){
        if(showline==TRUE&&shading==TRUE){
          if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
            graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
              geom_point(shape=setdotshape,stroke=setdotoutlinethickness,fill=dotcolor,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
              geom_smooth(method=setlinetype, level=setconfidencelevel, linetype="solid", color=scales::alpha(c(linecolor),setlinetransparency), fill=c(linecolor),linewidth=linethickness,fullrange=TRUE,alpha=seterrorshadingtransparency)+
              theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
              ylab(colnames(graphvariables)[1])+
              xlab(colnames(graphvariables)[2])
          }else{
            graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
              geom_point(shape=setdotshape,color=dotcolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
              geom_smooth(method=setlinetype, level=setconfidencelevel, linetype="solid", color=scales::alpha(c(linecolor),setlinetransparency), fill=c(linecolor),linewidth=linethickness,fullrange=TRUE,alpha=seterrorshadingtransparency)+
              theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
              ylab(colnames(graphvariables)[1])+
              xlab(colnames(graphvariables)[2])
          }
        }
      }
      
      if(showcoloredshading==FALSE){
        if(showline==TRUE&&shading==TRUE){
          if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
            graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
              geom_point(shape=setdotshape,stroke=setdotoutlinethickness,fill=dotcolor,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
              geom_smooth(method=setlinetype, level=setconfidencelevel,  linetype="solid", color=scales::alpha(c(linecolor),setlinetransparency), fill=c(shadingcolor),linewidth=linethickness,fullrange=TRUE,alpha=seterrorshadingtransparency)+
              theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
              ylab(colnames(graphvariables)[1])+
              xlab(colnames(graphvariables)[2])
          }else{
            graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
              geom_point(shape=setdotshape,color=dotcolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
              geom_smooth(method=setlinetype, level=setconfidencelevel,  linetype="solid", color=scales::alpha(c(linecolor),setlinetransparency), fill=c(shadingcolor),linewidth=linethickness,fullrange=TRUE,alpha=seterrorshadingtransparency)+
              theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
              ylab(colnames(graphvariables)[1])+
              xlab(colnames(graphvariables)[2])
          }
        }
      }
      
      if(showline==TRUE&&shading==FALSE){
        if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
          graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
            geom_point(shape=setdotshape,stroke=setdotoutlinethickness,fill=dotcolor,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
            geom_smooth(method=setlinetype, level=setconfidencelevel, se=F, linetype="solid", color=scales::alpha(c(linecolor),setlinetransparency), linewidth=linethickness,fullrange=TRUE)+
            theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
            ylab(colnames(graphvariables)[1])+
            xlab(colnames(graphvariables)[2])
        }else{
          graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
            geom_point(shape=setdotshape,color=dotcolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
            geom_smooth(method=setlinetype, level=setconfidencelevel, se=F, linetype="solid", color=scales::alpha(c(linecolor),setlinetransparency), linewidth=linethickness,fullrange=TRUE)+
            theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
            ylab(colnames(graphvariables)[1])+
            xlab(colnames(graphvariables)[2]) 
        }
      }
      
      if(showline==FALSE){
        if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
          graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
            geom_point(shape=setdotshape,stroke=setdotoutlinethickness,fill=dotcolor,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
            theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))
        }else{
          graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable))+
            geom_point(shape=setdotshape,color=dotcolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
            theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))  
        }
        if(!is.null(setcustomintercept)&&!is.null(setcustomslope)){
          graph<-graph+geom_abline(intercept=c(setcustomintercept),slope=c(setcustomslope),linetype="solid",color=c(linecolor),size=linethickness,alpha=seterrorshadingtransparency)
          if(linethickness==1.2){
            message("Because you specified a custom line, the default line thickness has changed. To adjust this, use setlinethickness=")
          }
        }
      }
      
      if (!is.null(title)) {
        graph <- graph + ggtitle(title)
      }
      
      if(!is.null(subtitle)){
        graph<-graph+labs(subtitle=subtitle)
      }
      
      if (!is.null(setxaxistitle)) {
        graph <- graph + xlab(setxaxistitle)
      }
      
      if (!is.null(setyaxistitle)) {
        graph <- graph + ylab(setyaxistitle)
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
      if(xtickspecs>0&&xtickspecs<3){
        message("To adjust your x-axis, use setxaxisstart=, setxaxisend=, and setxaxisspacing=. All three parameters must be specified to adjust the x-axis.")
        stop()
      }
      if(!is.null(setxaxisspacing)){
        graph<-graph+scale_x_continuous(breaks=seq(setxaxisstart,setxaxisend,by = setxaxisspacing))
      }
      if(!is.null(setyaxisspacing)){
        graph<-graph+scale_y_continuous(breaks=seq(setyaxisstart,setyaxisend,by = setyaxisspacing))
      }
      if(!is.null(panelvariable)){
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
      if(!is.null(setpaneltitlesize)){
        graph<-graph+theme(strip.text=element_text(size=setpaneltitlesize))
      }
      if(showlegend==FALSE){
        graph<-graph+theme(legend.position = "none")
      }
      if(!is.null(setyupperbound)){
        graph<-graph+ylim(setylowerbound,setyupperbound)
      }
      if(!is.null(setxupperbound)){
        graph<-graph+xlim(setxlowerbound,setxupperbound)
      }
      if(!is.null(colors)){
        message("ERROR: To set the colors when not using a grouping (legend) variable, use setlinecolor= and setdotcolor=")
      }
      if(showanimation==TRUE){
        message("ERROR: You must add a grouping variable to use animations.")
      }
      
      if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<3){
        if(jitterwidth>.1){
          if(showdata==TRUE){
            message("WARNING: Because you have a relatively narrow range of values in your x-axis variable, jitter may make your datapoints appear to be in a different location than is likely to be appropriate. We recommend adjusting your jitter using the setjitterheight and setjitterwidth commands (or use the setjitter command to set both height and width simultaneously--for example, setjitter=.01).")  
          }
        }
      }
      if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<3){
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
      if("setaxissize" %in% names(custom_args)){
        message("The command setaxissize is no longer available. Use the command setaxistextsize instead.")
      }
      if(showrotatedxlabels==TRUE){
        return(graph + theme(axis.text.x = element_text(angle = 90,vjust=0.5)))
      }
      
      if(showrotatedxlabels==FALSE){
        return(graph)
      }
    }
    
    #### SCATTERPLOT PART 2 ####
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
      
      summarydata<-na.omit(summarydata)
      
      if(!is.null(setexclusionabove)){
        summarydata<-subset(summarydata,summarydata$yvariable<=setexclusionabove)
        message(sprintf("Note: Points on the y-axis greater than %s have been removed.", setexclusionabove))
      }
      
      if(!is.null(setexclusionright)){
        summarydata<-subset(summarydata,summarydata$xvariable<=setexclusionright)
        message(sprintf("Note: Points on the x-axis greater than %s have been removed.", setexclusionright))
      }
      if(setreversecodex==TRUE){
        summarydata$xvariable<-(max(summarydata$xvariable,na.rm=T)+min(summarydata$xvariable,na.rm=T))-summarydata$xvariable
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
      
      if(setxstandardize==TRUE){
        summarydata$xvariable<-scale(summarydata$xvariable)
        message("Note: Your x-variable has been standardized.")
      }
      
      if(splitgroup==TRUE){
        summarydata$groupvariable<-as.numeric(summarydata$groupvariable)
        recordedmediangroup<-median(summarydata$groupvariable,na.rm=T)
        summarydata$groupvariable<-ifelse(summarydata$groupvariable<=median(summarydata$groupvariable,na.rm=T),"Low",ifelse(summarydata$groupvariable>median(summarydata$groupvariable,na.rm=T),"High",NA))
        if(setreverseordergroup==FALSE){
          summarydata$groupvariable<-factor(summarydata$groupvariable,levels=c("Low","High"))
        }
        if(showoutput==TRUE){
          print(table(summarydata$groupvariable))
        }
        message(sprintf("NOTE: A median split is being performed on your grouping variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmediangroup))
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
      
      summarydata$xvariable<-as.numeric(summarydata$xvariable)
      summarydata$yvariable<-as.numeric(summarydata$yvariable)
      summarydata$groupvariable<-as.factor(summarydata$groupvariable)
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
        if(showoutput==TRUE&&showsimpleslopesplot==FALSE){
          print(originallevels)
          print(newlevels)
        }
      }
      if(showoutput==TRUE&&showsimpleslopesplot==FALSE){
        print(summarydata)
      }
      
      if(is.null(showcoloredshading)&&showanimation==FALSE){
        if(length(levels(as.factor(summarydata$groupvariable)))<4){
          showcoloredshading<-TRUE
          message("Because you have three or fewer groups in your color variable, your scatterplot has been set to colored shading by default. To adjust this, use showcoloredshading=FALSE")
        }else{
          showcoloredshading<-FALSE
        }
      }
      
      if(!is.null(setcustomintercept)){
        if(is.null(setxaxisstart)&&min(summarydata$xvariable,na.rm=T)>-.001){
          setxaxisstart<-0
          setxaxisend<-round(max(summarydata$xvariable,na.rm=T),0)
          if(round(max(summarydata$xvariable,na.rm=T),0)>50){
            setxaxisspacing<-10
          }
          if(round(max(summarydata$xvariable,na.rm=T),0)<=50&&round(max(summarydata$xvariable,na.rm=T),0)>20){
            setxaxisspacing<-5
          }
          if(round(max(summarydata$xvariable,na.rm=T),0)<=20&&round(max(summarydata$xvariable,na.rm=T),0)>2){
            setxaxisspacing<-1
          }
          if(round(max(summarydata$xvariable,na.rm=T),0)<=2){
            setxaxisspacing<-.1
          }
        }
        if(is.null(setyaxisstart)&&min(summarydata$yvariable,na.rm=T)>-.001){
          setyaxisstart<-0
          setyaxisend<-round(max(summarydata$yvariable,na.rm=T),0)
          if(round(max(summarydata$yvariable,na.rm=T),0)>50){
            setyaxisspacing<-10
          }
          if(round(max(summarydata$yvariable,na.rm=T),0)<=50&&round(max(summarydata$yvariable,na.rm=T),0)>20){
            setyaxisspacing<-5
          }
          if(round(max(summarydata$yvariable,na.rm=T),0)<=20&&round(max(summarydata$yvariable,na.rm=T),0)>2){
            setyaxisspacing<-1
          }
          if(round(max(summarydata$yvariable,na.rm=T),0)<=2){
            setyaxisspacing<-.1
          }
        }
        if(showoutput==TRUE){
          print("Y-axis tick spacing set to:")
          print(setyaxisspacing)
          print("X-axis tick spacing set to:")
          print(setxaxisspacing)
        }
        message("You have set a custom slope and intercept. To adjust where the x and y-axis start and end, or to adjust the spacing of tickmarks, use setxaxisstart= , setxaxisend=, and setxaxisspacing = (or, for the y-axis, setyaxisstart =, setyaxisend=, etc.)")
      }
      
      #Adjusting the default jitter
      if(is.null(jitterheight)){
        if(showsimpleslopesplot==FALSE){
          jitterheight<-.4  
        }
        if(showsimpleslopesplot==TRUE){
          jiterheight<-.9
        }
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
        if(showsimpleslopesplot==FALSE){
          jitterwidth<-.4  
        }
        if(showsimpleslopesplot==TRUE){
          jiterwidth<-.9
        }
        if(showdata==TRUE){
          if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<3){
            jitterwidth<-.095
            message("\nNOTE: Given the narrow range of values on the x-axis, the default jitter width has been changed. You may adjust the jitter width using setjitterwidth= or adjust the jitter height using setjitterheight= .")  
          }
          if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<2){
            jitterwidth<-.05
          }
          if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<1){
            jitterwidth<-.01
          }
        }
      }
      
      if(showsimpleslopesplot==FALSE){
        if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
          graph<-ggplot(data=summarydata,aes(x = xvariable, y = yvariable, color = groupvariable,fill=groupvariable))+
            geom_point(shape=setdotshape,stroke=setdotoutlinethickness,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
            theme_bw()+ylab(colnames(graphvariables)[1])+
            xlab(colnames(graphvariables)[2])+
            theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))
        }else{
          graph<-ggplot(data=summarydata,aes(x = xvariable, y = yvariable, color = groupvariable,fill=groupvariable))+
            geom_point(shape=setdotshape,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
            theme_bw()+ylab(colnames(graphvariables)[1])+
            xlab(colnames(graphvariables)[2])+
            theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))  
        }
        if(showline==TRUE&&showanimation==FALSE&&is.null(setcustomintercept)&&is.null(setcustomslope)){ #If you're showing standard regression lines
          if(showdashedlines==TRUE){ #If they are dashed (color-blind friendly)
            if(setlinetransparency!=1){
              message("\n ERROR: Line transparency must be set to 1 to show color-blind friendly/dashed lines.\n")
              stop()
            }
            message("Color blind-friendly (dashed) lines are shown. To remove these, add showdashedlines=FALSE or showcolorblindgraph=FALSE")
            if(showcoloredshading==TRUE){ #If shading is colored based on group
              graph<-graph+
                geom_smooth(aes(linetype=groupvariable),method=setlinetype, level=setconfidencelevel,linewidth=linethickness,fullrange=TRUE,alpha=seterrorshadingtransparency)
            }
            
            if(showcoloredshading==FALSE){
              if(shading==TRUE){ #If you are showing error shading but it is not colored by group
                graph<-graph+
                  geom_smooth(aes(linetype=groupvariable),method=setlinetype, level=setconfidencelevel,linewidth=linethickness,fill=shadingcolor,fullrange=TRUE,alpha=seterrorshadingtransparency)
              }
            }
            
            if(shading==FALSE){ #If you are not showing error shading at all
              graph<-graph+
                geom_smooth(aes(linetype=groupvariable),method=setlinetype, level=setconfidencelevel,se=FALSE,linewidth=linethickness,fullrange=TRUE,alpha=seterrorshadingtransparency)
            }
          }
          if(showdashedlines==FALSE){ #If you're not showing dashed (color-blind friendly) lines
            if(showcoloredshading==TRUE){ #If you're showing colored error shading based on group
              if(shading==TRUE){
                if(setlinetransparency==1){
                  graph<-graph+
                    geom_smooth(method=setlinetype, level=setconfidencelevel,linewidth=linethickness,linetype="solid",fullrange=TRUE,alpha=seterrorshadingtransparency)
                }
                
                if(setlinetransparency!=1){
                  graph<-graph+
                    stat_smooth(geom="ribbon",linewidth=0,method=setlinetype, level=setconfidencelevel,fullrange=TRUE,alpha=seterrorshadingtransparency,show.legend = FALSE)+
                    stat_smooth(geom="line",linetype="solid",method=setlinetype, linewidth=linethickness,fullrange=TRUE,alpha=setlinetransparency)
                }
              }
            }
            
            if(showcoloredshading==FALSE){ #If you're not showing colored error shading based on group
              if(shading==TRUE){ #If you're showing gray shading
                if(setlinetransparency==1){
                  graph<-graph+
                    geom_smooth(method=setlinetype, level=setconfidencelevel,linewidth=linethickness,linetype="solid",fill=shadingcolor,fullrange=TRUE,alpha=seterrorshadingtransparency)
                }
                
                if(setlinetransparency!=1){
                  graph<-graph+
                    stat_smooth(geom="ribbon",linewidth=0,method=setlinetype, level=setconfidencelevel,fullrange=TRUE,alpha=seterrorshadingtransparency,fill=shadingcolor,show.legend = FALSE)+
                    stat_smooth(geom="line",linetype="solid",method=setlinetype, linewidth=linethickness,fullrange=TRUE,alpha=setlinetransparency)
                }
              }
            }
            
            if(shading==FALSE){ #If you're not showing shading at all
              if(setlinetransparency==1){
                graph<-graph+
                  geom_smooth(method=setlinetype, level=setconfidencelevel,se=FALSE,linewidth=linethickness,linetype="solid",fullrange=TRUE,alpha=seterrorshadingtransparency)
              }
              if(setlinetransparency!=1){
                graph<-graph+stat_smooth(geom="line",linetype="solid",method=setlinetype, linewidth=linethickness,fullrange=TRUE,alpha=setlinetransparency)
              }
            }
          }
        }
        if(showline==FALSE&&showanimation==FALSE){ #If you're not showing a standard regression line at all
          
          if(!is.null(setcustomintercept)&&!is.null(setcustomslope)){ #If you're instead adding a line with a custom intercept/slope and want
            #error shading
            if(showdashedlines==FALSE){ #If you're not using dashed (color-blind friendly) lines
              graph<-graph+geom_abline(intercept=c(setcustomintercept),slope=c(setcustomslope),linetype="solid",linewidth=linethickness,color=c(linecolor),alpha=seterrorshadingtransparency)
            }
            if(showdashedlines==TRUE){ #If you are
              graph<-graph+geom_abline(intercept=c(setcustomintercept),slope=c(setcustomslope),linewidth=linethickness,color=c(linecolor),alpha=seterrorshadingtransparency)
            }
            if(linethickness==1.2){
              message("Because you specified a custom line, the default line thickness has changed. To adjust this, use setlinethickness=")
            }
          }
        }
      }
      
      #### INTERACTION PLOTS ####
      if(showsimpleslopesplot==TRUE){
        if(setystandardize==TRUE||setxstandardize==TRUE){
          message("\nERROR: Data are automatically standardized when using simple-slope interaction plots.")
          stop()
        }
        if(!is.null(setlegendlevels)){
          message("\nERROR: You cannot manually adjust the legend levels in a simple-slopes interaction plot in the present version.")
          stop()
        }
        if(!is.null(setreverseorderlegend)){
          message("\nERROR: You cannot manually adjust the legend levels in a simple-slopes interaction plot in the present version.")
          stop()
        }
        if(!is.null(panelvariable)){
          message("\nERROR: You cannot use a panel (facet) variable when generating simple-slopes plots.")
          stop()
        }
        # if(!is.null(setreversecodegroup)){
        #   message("ERROR: You cannot manually adjust the legend levels in a simple-slopes interaction plot in the present version.")
        #   stop()
        # }
        
        #Duplicate the summarydata dataframe
        summarydata2<-summarydata
        
        #Scale the variables
        summarydata2$outcome_scale <- as.vector(scale(summarydata2$yvariable))
        summarydata2$predictor_scale <- as.vector(scale(summarydata2$xvariable))
        summarydata2$groupvariable<- as.numeric(summarydata2$groupvariable)
        summarydata2$factor_scale <- as.vector(scale(summarydata2$groupvariable))
        
        #Separate unscaled values into low, medium, and high groups
        mod_mean <- mean(summarydata2$groupvariable, na.rm=T)
        mod_sd <- sd(summarydata2$groupvariable, na.rm=T)
        
        #Cutoff by k sd
        k <- 1 #when k=1, one sd
        mod_cutoffs <- c(mod_mean-k*mod_sd, mod_mean+k*mod_sd)
        
        #Create group variable
        groupeddata<-summarydata2
        groupeddata$factor_group <- ifelse(groupeddata$groupvariable > mod_cutoffs[1] & groupeddata$groupvariable < mod_cutoffs[2], "Mean", "Other")
        
        #Set other groups
        groupeddata$factor_group[groupeddata$groupvariable < mod_cutoffs[1]] <- "1 SD Below"
        groupeddata$factor_group[groupeddata$groupvariable > mod_cutoffs[2]] <- "1 SD Above"
        groupeddata$factor_group <- as.factor(groupeddata$factor_group)
        group_vals <- unique(groupeddata$factor_group)[!is.na(unique(groupeddata$factor_group))]
        
        # Model Creation #
        interact_mod <- lm(outcome_scale ~ predictor_scale * factor_scale, data = groupeddata)
        
        # Prediction for Confidence Intervals ####
        pred_range <- range(groupeddata$predictor_scale, na.rm = TRUE)
        factor_labels <- c("1 SD Below", "Mean", "1 SD Above")
        scale_mean <- mean(groupeddata$factor_scale)
        scale_sd <- sd(groupeddata$factor_scale)
        factor_values <- c(scale_mean-k*scale_sd, scale_mean, scale_mean + scale_sd)
        
        #Create a grid of predictor values - 100 to match interaction package
        pred_seq <- seq(from = pred_range[1], to = pred_range[2], length.out = 100)
        
        #Build the prediction grid
        pred_grid <- expand.grid(
          predictor_scale = pred_seq,
          factor_scale = factor_values
        )
        
        #Predict confidence intervals w/ standard error
        predictions <- predict(interact_mod, 
                               newdata = pred_grid,
                               se.fit = TRUE)
        
        #Create data frame with predictions and factor labels
        pred_data <- data.frame(
          predictor_scale = pred_grid$predictor_scale,
          factor_scale = pred_grid$factor_scale,
          outcome_scale = predictions$fit,
          fit = predictions$fit,
          se = predictions$se.fit
        ) %>%
          dplyr::mutate(factor_group = factor(rep(factor_labels, each = length(pred_seq))),
                 # Calculate upper and lower bounds (approximately 95% CI)
                 lwr = fit - 1.96 * se,
                 upr = fit + 1.96 * se,
          )  %>%
          dplyr::arrange(factor_group)
        
        #Get pred_data summarizations
        pred_data_summarize <- pred_data %>% dplyr::group_by(factor_group) %>% 
          dplyr::summarize(xend = max(predictor_scale),
                           xstart = min(predictor_scale),
                           yend = max(fit),
                           ystart = min(fit)) %>%
          dplyr::mutate(factor_group = factor(factor_group)) %>%
          dplyr::arrange(factor_group)
        
        if(length(levels(groupeddata$factor_group))<3){
          message("ERROR: You cannot show an interaction plot unless subjects are represented across all three groups (at mean, 1 SD below, and 1 SD above). See the dataframe above for details.")
          stop()
        }
        
        #Set default colors
        if(is.null(colors)){
          interactioncolors<-c("red","blue","green")
        }
        #For custom colors
        if(!is.null(colors)){
          interactioncolors<-colors
        }
        
        xrange <- c(min(groupeddata$predictor_scale), max(groupeddata$predictor_scale))
        yrange <- c(min(groupeddata$outcome_scale), max(groupeddata$outcome_scale))
        group.colors <- c(`1 SD Below` = interactioncolors[1], `Mean`=interactioncolors[2], `1 SD Above` = interactioncolors[3])
        
        if(shading==TRUE){
          graph<-ggplot(data=groupeddata,aes(x = predictor_scale, y = outcome_scale)) + geom_point(aes(fill=factor_group),shape=setdotshape,stroke=setdotoutlinethickness,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize) +
            coord_cartesian(xlim = xrange) +
            scale_color_manual(values = group.colors) +
            scale_fill_manual(values = group.colors) +
            geom_segment(data=pred_data_summarize,
                         aes(x=xstart, y=ystart, xend=xend, yend=yend, color=factor_group),
                         inherit.aes = TRUE,
                         linewidth = linethickness,alpha=setlinetransparency) +
            geom_ribbon(data=pred_data, aes(x=predictor_scale, ymax=upr, ymin=lwr, fill=factor_group),
                        color=NA, show.legend = FALSE,alpha=seterrorshadingtransparency)
        }
        if(shading==FALSE){
          graph<-ggplot(data=groupeddata,aes(x = predictor_scale, y = outcome_scale)) + geom_point(aes(fill=factor_group),shape=setdotshape,stroke=setdotoutlinethickness,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize) +
            coord_cartesian(xlim = xrange) +
            scale_color_manual(values = group.colors) +
            scale_fill_manual(values = group.colors) +
            geom_segment(data=pred_data_summarize,
                         aes(x=xstart, y=ystart, xend=xend, yend=yend, color=factor_group),
                         inherit.aes = TRUE,
                         linewidth = linethickness,alpha=setlinetransparency) 
        }
        
        graph<-graph+theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))
        
        #graph<-graph+scale_fill_manual(values=c(interactioncolors))
        
        if (is.null(setxaxistitle)) {
          graph <- graph + xlab(colnames(graphvariables[2]))
        }
        
        if (is.null(setyaxistitle)) {
          graph <- graph + ylab(colnames(graphvariables[1]))
        }
        
        if(showoutput==TRUE){
          print(groupeddata)
        }
        
      }
      if(showanimation==TRUE){ #If you're creating an animated graph (animated lines not currently supported)
        if(showsimpleslopesplot==TRUE){
          message("ERROR: You cannot create animated simple-slope interaction plots at this time.")
          stop()
        }
        if(!is.null(setcustomintercept)||!is.null(setcustomslope)){
          message("Regression lines cannot be used with animations in the present version.")
        }
        if(showsimpleslopesplot==TRUE){
          message("ERROR: You cannot combine animations with simple-slope interaction plots.")
          stop()
        }
        if(setdotshape==21||setdotshape==22||setdotshape==23||setdotshape==24||setdotshape==25||setdotshape=="circle filled"||setdotshape=="diamond filled"||setdotshape=="square filled"||setdotshape=="triangle down filled"||setdotshape=="triangle filled"){
          graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
            geom_point(aes(group=idvariablespecifiedbyuser),shape=setdotshape,stroke=setdotoutlinethickness,fill=dotcolor,color=setdotoutlinecolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=transparency,size=dotsize)+
            theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
            ylab(colnames(graphvariables)[1])+
            xlab(colnames(graphvariables)[2])
        }else{
          graph<-ggplot(data=summarydata, aes(x=xvariable, y=yvariable)) +
            geom_point(aes(group=idvariablespecifiedbyuser),shape=setdotshape,color=dotcolor,position=position_jitter(width=jitterwidth,height=jitterheight),alpha=.5,size=dotsize)+
            theme_bw()+theme(plot.title=element_text(hjust=0.5,size=titlesize,face=settitleface),axis.title.y=element_text(size=setytitlesize),axis.title.x=element_text(size=setxtitlesize),axis.text.x=element_text(size=setxaxistextsize),axis.text.y=element_text(size=setyaxistextsize),legend.text=element_text(size=setlegendlevelsize),legend.title=element_text(size=setlegendtitlesize),plot.subtitle=element_text(hjust=0.5,size=setsubtitlesize,face=setsubtitleface))+
            ylab(colnames(graphvariables)[1])+
            xlab(colnames(graphvariables)[2])  
        }
      }
      
      if(linecolor[[1]]!=c("#1e7388")){
        message("NOTE: To set the color of your lines when showing multiple regression lines, use the command setcolors=c(). For example: setcolors=c('turquoise','purple'). When using this command, you may show colored shading using showcoloredshading=TRUE or use the default gray shading using showcoloredshading=FALSE")
      }
      if(!is.null(legendtitle)){
        graph<-graph + labs(linetype=legendtitle,color=legendtitle,fill=legendtitle)
      }
      
      if(is.null(legendtitle)){
        graph<-graph + labs(linetype=colnames(graphvariables)[[3]],color=colnames(graphvariables)[[3]],fill=colnames(graphvariables)[[3]])
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
      
      if (!is.null(title)) {
        graph <- graph + ggtitle(title)
      }
      
      if (!is.null(setxaxistitle)) {
        graph <- graph + xlab(setxaxistitle)
      }
      
      if (!is.null(setyaxistitle)) {
        graph <- graph + ylab(setyaxistitle)
      }
      
      if(is.null(setlegendlevels)){
        if(!is.null(colors)&&showsimpleslopesplot==FALSE){
          graph<-graph+scale_color_manual(values=c(colors))+scale_fill_manual(values=c(colors))
        }
      }
      
      if(!is.null(setlegendlevels)&&showsimpleslopesplot==FALSE){
        if(!is.null(colors)){
          graph<-graph+scale_color_manual(values=c(colors),labels=c(setlegendlevels))+scale_fill_manual(values=c(colors),labels=c(setlegendlevels))
        }
        
        if(is.null(colors)){
          graph <- graph + scale_color_discrete(labels=c(setlegendlevels))+scale_fill_discrete(labels=c(setlegendlevels))
        }
      }
      
      if(!is.null(setpositionhorizontalline)){
        graph<-graph+geom_hline(yintercept=setpositionhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness)
      }
      if(!is.null(setpositiondottedhorizontalline)){
        graph<-graph+geom_hline(yintercept=setpositiondottedhorizontalline,color=sethorizontallinecolor,size=sethorizontallinethickness,linetype="dotted")
      }
      
      if(!is.null(panelvariable)){
        graph<-graph+facet_wrap(~facetvariable)
      }
      if(ytickspecs>0&&ytickspecs<3){
        message("To adjust your y-axis, use setyaxisstart=, setyaxisend=, and setyaxisspacing=. All three parameters must be specified to adjust the y-axis.")
        stop()
      }
      if(xtickspecs>0&&xtickspecs<3){
        message("To adjust your x-axis, use setxaxisstart=, setxaxisend=, and setxaxisspacing=. All three parameters must be specified to adjust the x-axis.")
        stop()
      }
      if(!is.null(setxaxisspacing)){
        graph<-graph+scale_x_continuous(breaks=seq(setxaxisstart,setxaxisend,by = setxaxisspacing))
      }
      if(!is.null(setyaxisspacing)){
        graph<-graph+scale_y_continuous(breaks=seq(setyaxisstart,setyaxisend,by = setyaxisspacing))
      }
      
      if(showanimation==TRUE){
        require(gganimate)
        require(magick)
        graph<-graph+transition_states(groupvariable,transition_length=c(setanimationtransitionlength),state_length=c(setanimationlength))+
          labs(subtitle="{closest_state}",title=title)+theme(plot.title=element_text(size=titlesize,face=settitleface),
                                                             axis.text.x = element_text(size=setxaxistextsize),axis.text.y = element_text(size=setyaxistextsize), axis.title.x = element_text(size=setxtitlesize), axis.title.y = element_text(size=setytitlesize),
                                                             plot.subtitle = element_text(size=setanimationheadingsize,hjust=0.5))+exit_fade()+enter_fade()
        if(is.null(setanimationid)&&showdata==TRUE){
          message("NOTE: Although an ID variable may sometimes be correctly inferred, no explicit ID variable is set by default. If you are using a repeated-measures animation variable and want each datapoint to refer to the same subject across frames, it is best to add setanimationid= and specify an ID variable in your dataset.")
        }
        if(!is.null(colors)){
          message("To set the color of an animated scatterplot, use setcolor= or setdotcolor=")
        }
        if(setanimationheight<150||setanimationwidth<150){
          if("animationoverride" %!in% names(custom_args)){
            message("\nERROR: Your animation is too small. Height and width are set in pixels. The default commands are setanimationheight=500 and setanimationwidth=790. If you wish to generate an extremely small animation and override this message, use the command animationoverride=TRUE")
            stop()  
          }
        }
        return(animate(graph,renderer=magick_renderer(),height=setanimationheight,width=setanimationwidth,units="px"))
      }
      
      if(!is.null(panelvariable)){
        graph<-graph+facet_wrap(~facetvariable)+theme(strip.text=element_text(size=setpaneltitlesize))
      }
    }
    if(showblackandwhitegraph==TRUE){
      graph<-graph+scale_color_grey()+scale_fill_grey()
    }
    if(showdarkgraph==TRUE){
      graph<-graph+theme(
        plot.background = element_rect(fill = "black"),panel.background=element_rect(fill="black"),legend.background=element_rect(fill="black"), legend.key=element_rect(fill="black"),axis.text=element_text(color="white"),legend.text=element_text(color="white"),axis.title=element_text(color="white"),legend.title=element_text(color="white"))
    }
    if(showrotatedxlabels==TRUE){
      graph<-graph + theme(axis.text.x = element_text(angle = 90,vjust=0.5))
    }
    if(!is.null(setdottransparency)){
      if(setdottransparency>1&&setdottransparency<10.1){
        message("Note: Transparency is typically set on a scale from 0 (completely invisible) - 1 (not at all transparent). Because you specified a transparency value greater than 1, we divided this value by 10, effectively making a scale from 1.1 - 10. However, specifying a transparency of 1 will trigger the default 0 - 1 scale and remove all transparency. To avoid confusion, we recommend using the default 0 - 1 scale in the future.")
      }
    }
    if(showgridlines==FALSE){
      graph<-graph+theme_classic()
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
    if(!is.null(setyupperbound)){
      graph<-graph+ylim(setylowerbound,setyupperbound)
    }
    if(!is.null(setxupperbound)){
      graph<-graph+xlim(setxlowerbound,setxupperbound)
    }
    if(!is.null(showboldedaxistitles)){
      if(showboldedaxistitles==TRUE){
        if(is.null(showboldedlegendtitle)||showboldedlegendtitle==FALSE){
          message("NOTE: You are bolding your axis titles. To also bold the legend title, use showboldedlegendtitle = TRUE")
        }
      }
    }
    if(max(as.numeric(summarydata$xvariable))-min(as.numeric(summarydata$xvariable))<3){
      if(jitterwidth>.1){
        if(showdata==TRUE){
          message("WARNING: Because you have a relatively narrow range of values in your x-axis variable, jitter may make your datapoints appear to be in a different location than is likely to be appropriate. We recommend adjusting your jitter using the setjitterheight and setjitterwidth commands (or use the setjitter command to set both height and width simultaneously--for example, setjitter=.01).")  
        }
      }
    }
    if(max(as.numeric(summarydata$yvariable))-min(as.numeric(summarydata$yvariable))<3){
      if(jitterheight>.1){
        if(showdata==TRUE){
          message("WARNING: Because you have a relatively narrow range of values in your y-axis variable, jitter may make your datapoints appear to be in a different location than is likely to be appropriate. We recommend adjusting your jitter using the setjitterheight and setjitterwidth commands (or use the setjitter command to set both height and width simultaneously--for example, setjitter=.01).")  
        }
      }
    }
    if(setlinetransparency!=1){
      if(showsimpleslopesplot==FALSE){
        message("\n WARNING: Adjusting the line transparency to a value other than 1 with a color (legend) variable is stil in beta. Check to make sure that the error shading aligns with your line and that you do not have any duplicate lines. \n")
      }
    }
    if(showsimpleslopesplot==TRUE){
    message("You are generating a simple-slopes plot. To show a normal graph, remove the command showsimpleslopesplot=TRUE or set this command to FALSE.")
    }
    if("setxaxissize" %in% names(custom_args)){
      message("The command setxaxissize is no longer available. Use the command setxaxistextsize instead.")
    }
    if("setyaxissize" %in% names(custom_args)){
      message("The command setyaxissize is no longer available. Use the command setyaxistextsize instead.")
    }
    if("setaxissize" %in% names(custom_args)){
      message("The command setaxissize is no longer available. Use the command setaxistextsize instead.")
    }
    return(graph)
  }
}
