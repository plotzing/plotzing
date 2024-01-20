#### GENERATE DATASET ####
generate_dataset<-function(dv=NULL,iv1=NULL,iv2=NULL,panelvariable=NULL,setbaroutlinecolor="gray",setbarshorizontal=FALSE,jitterheight=.4,jitterwidth=.3,setjitterheight=NULL,setjitterwidth=NULL,setjitter=NULL,internalfunctionautorotation=FALSE,errorbars="default",seterrorbarthickness=NULL,splitx=FALSE,splitgroup=FALSE,splitpanel=FALSE,showdata=TRUE,bold=FALSE,data=df,setdata=NULL,transparency=NULL,dottransparency=NULL,dotsize=NULL,title=NULL,settitle=NULL,setxaxistitle=NULL,setyaxistitle=NULL,colors=NULL,color1=-1,color2=-1,color3=-1,color4=-1,color5=-1,color6=-1,color7=-1,color8=-1,color9=-1,color10=-1,color=NULL,setcolor=NULL,setlegendlevels=NULL,level1=NULL,level2=NULL,level3=NULL,level4=NULL,level5=NULL,level6=NULL,level7=NULL,level8=NULL,level9=NULL,level10=NULL,legendtitle=NULL,titlesize=NULL,settitlesize=NULL,setxtitlesize=NULL,setytitlesize=NULL,setxaxissize=NULL,setyaxissize=NULL,setaxistextsize=NULL,setaxistitlesize=NULL,setlegendtitlesize=NULL,setlegendlevelsize=NULL,setpaneltitlesize=NULL,settitleface="bold",seterrorbarwidth=.2,seterrorbartransparency=NULL,showrotatedxlabels=NULL,rotatexaxislabels=FALSE,setpanellevels=NULL,setxlevels=NULL,setystandardize=FALSE,split1=FALSE,split2=FALSE,split3=FALSE,setsplitx=NULL,setsplitgroup=NULL,setsplitpanel=NULL,seterrorbars=NULL,setcolors=NULL,showerrorbars=TRUE,setlegendtitle=NULL,setdotsize=NULL,setdottransparency=NULL,setbaroutlinethickness=NULL,showcolorederrorbars=FALSE,showspacebelowzero=TRUE,setbartransparency=0.9,dodgewidth=0.9,setdodgewidth=NULL,setdotoutlinethickness=0.5,showdots=NULL,setdotoutlinecolor=NULL,showdotoutline=TRUE,customdata=FALSE,means=NULL,datapoints=NULL,setpositionhorizontalline=NULL,setpositiondottedhorizontalline=NULL,sethorizontallinecolor="black",sethorizontallinethickness=1,showanimation=FALSE,setanimationid=NULL,setconfidencelevel=0.95,seterrorbarcolor="#7F7F7F",groupvariable=NULL,groupingvariable=NULL,showblackandwhitegraph=FALSE,showdarkgraph=FALSE,setreversecodex=FALSE,setreversecodey=FALSE,setreversecodegroup=FALSE,setreversecodepanel=FALSE,setreverseorderx=FALSE,setreverseordergroup=FALSE,setreverseorderpanel=FALSE,reverseorderx=NULL,reverseordergroup=NULL,reverseorderpanel=NULL,reversecodex=NULL,reversecodey=NULL,reversecodegroup=NULL,reversecodepanel=NULL,setxlevelorder=NULL,setgrouplevelorder=NULL,setpanellevelorder=NULL,setyaxisspacing=NULL,setyaxisend=NULL,setyaxisstart=NULL,showoutput=TRUE,showgridlines=TRUE){

  if(errorbars=="default"){
    errorbars<-"ci"
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

  if(!is.null(setsplitx)){
    splitx<-setsplitx
  }

  if(!is.null(setsplitgroup)){
    splitgroup<-setsplitgroup
  }

  if(!is.null(setsplitpanel)){
    splitpanel<-setsplitpanel
  }

  '%!in%' <- function(x,y)!('%in%'(x,y))
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
        summarydata$xvariable<-ifelse(summarydata$xvariable>median(summarydata$xvariable,na.rm=T),"High",ifelse(summarydata$xvariable<=median(summarydata$xvariable,na.rm=T),"Low",NA))
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c("Low","High"))
        message(sprintf("NOTE: A median split is being performed on your x-axis variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianx))
      }

      if(splitpanel==TRUE&&!is.null(panelvariable)){
        summarydata$facetvariable<-as.numeric(summarydata$facetvariable)
        recordedmedianfacet<-median(summarydata$facetvariable,na.rm=T)
        summarydata$facetvariable<-ifelse(summarydata$facetvariable<=median(summarydata$facetvariable,na.rm=T),"Low",ifelse(summarydata$facetvariable>median(summarydata$facetvariable,na.rm=T),"High",NA))
        summarydata$facetvariable<-factor(summarydata$facetvariable,levels=c("Low","High"))
        message(sprintf("NOTE: A median split is being performed on your panel variable. Points less than or equal to %s have been set to 'Low.' Points greater than this value have been set to 'High.'",recordedmedianfacet))
      }

      if(!is.null(setxlevels)){
        summarydata$xvariable<-as.factor(summarydata$xvariable)
        originallevels<-levels(summarydata$xvariable)
        levels(summarydata$xvariable)<- c(setxlevels)
        newlevels<-levels(summarydata$xvariable)
        message("Levels of x variable have been converted:")
      }

      summarydata$xvariable<-as.factor(summarydata$xvariable)
    }

  }

  #### part 2 ####
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
      if(splitx==TRUE){
        summarydata$xvariable<-as.numeric(summarydata$xvariable)
        recordedmedianx<-median(summarydata$xvariable,na.rm=T)
        summarydata$xvariable<-ifelse(summarydata$xvariable>median(summarydata$xvariable,na.rm=T),"High",ifelse(summarydata$xvariable<=median(summarydata$xvariable,na.rm=T),"Low",NA))
        summarydata$xvariable<-factor(summarydata$xvariable,levels=c("Low","High"))
        if(showoutput==TRUE){
          print(table(summarydata$facetvariable))
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

      if(splitgroup==TRUE){
        summarydata$groupvariable<-as.numeric(summarydata$groupvariable)
        recordedmediangroup<-median(summarydata$groupvariable,na.rm=T)
        summarydata$groupvariable<-ifelse(summarydata$groupvariable<=median(summarydata$groupvariable,na.rm=T),"Low",ifelse(summarydata$groupvariable>median(summarydata$groupvariable,na.rm=T),"High",NA))
        summarydata$groupvariable<-factor(summarydata$groupvariable,levels=c("Low","High"))
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
    }

  }
  return(summarydata)
}
