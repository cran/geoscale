geoscale.plot<-function(ages,data,units=c("Age","Epoch","Period"),scale="Period",boxes="Age",cex.age=0.3,cex.ts=0.4,cex.pt=1,xlim=NULL,ylim=NULL,nscale,ts.col=TRUE,height=0.3,label=NULL,...){ 

  if(any(units == "Other") && missing(nscale) == TRUE){
    print("You need to specify a file for the argument nscale, option for 'Other' is removed")
    units <-subset(units,units != "Other")
  }

  tscale_data<-matrix(ncol=3,nrow=6)
    colnames(tscale_data) <-c("srt","Depth","size")
    rownames(tscale_data) <-c("Eon","Era","Period","Epoch","Age","Other")
      tscale_data[,1] <- c(0,0,0,90,90,90)
      tscale_data[,2] <- c(0.5,0.5,0.5,1,1,1)
      tscale_data[,3] <- c(1,1,1,0.8,0.7,0.5)
  
  tscale<-timescale[order(timescale[,1],decreasing=T),]
    
  ## SORTING PERIODS
  	units<-rownames(tscale_data)[sort(match(units,rownames(tscale_data)),decreasing=T)] 

if(scale != "n"){
  if(scale=="myr"){

    scale1=1
    scale2=10
    ticks<-seq(0,4600,scale1)
    time<-seq(0,4600,scale2)
    lwd<-c(1,0.5,0.5,0.5,0.5,0.7,0.5,0.5,0.5,0.5)
    col<-c("black","grey","grey","grey","grey")}
  
  if(scale != "myr"){
    
    if(scale == "Other"){
      time <- nscale
    }else time<-subset(timescale,timescale[,"Type"] == scale & timescale[,"Source"] == "ICS")    
    
    time1<-time[,1]
    time2<-time[,2]
    time<-unique(c(time1,time2))
    ticks<-time
    lwd=1
    col="black"
  }
}  
  par(fig=c(0,1,0,(height)))
  par(mar=c(1,4,0,2))
  
  plot(ages,data,type="n",xaxt="n",yaxt="n",bty="n",ylab="",xlab="",xlim=xlim)
    
    vals<-tscale_data[units,"Depth"]
      vals<-c(vals,0.8)
      vals<-cumsum(vals/sum(vals))
          vals<-c(par()$usr[4],par()$usr[4]-(vals*(par()$usr[4]-par()$usr[3])))
      			val<-vals[length(vals)-1] - vals[length(vals)]

if(scale != "n"){  			
  text(time,(vals[length(vals)]+val*0.3),time, cex=cex.age,srt=90)
  	segments(ticks,(vals[length(vals)-1]),ticks,(vals[length(vals)]+val*0.75),lwd=lwd)
}  
   
 for(t in 1:length(units)){
 	
 	if(units[t] == "Other"){
     tscale_n<-nscale
 	} else tscale_n<-subset(tscale,tscale[,"Type"] == units[t])
 	
 	if(ts.col == TRUE & units[t] != "Other"){ rect(tscale_n[,"Start"],vals[t],tscale_n[,"End"],vals[t+1],col=rgb(tscale_n[,"Col_R"],tscale_n[,"Col_G"],tscale_n[,"Col_B"],maxColorValue=255))}
    else rect(tscale_n[,"Start"],vals[t],tscale_n[,"End"],vals[t+1],col="white")
          text(tscale_n[,"Midpoint"],(vals[t] + vals[t+1])/2,tscale_n[,"Period"],cex=cex.ts*tscale_data[match(units[t],rownames(tscale_data)),"size"],srt=tscale_data[match(units[t],rownames(tscale_data)),"srt"])  	
 	
 }
    
  par(fig=c(0,1,(height),1),new=T)
  par(mar=c(0,4,2,2))
  
  plot(ages,data,type="n",xaxt="n",yaxt="n",ann=T,bty="n",ylab=label,ylim=ylim,xlim=xlim)
  
    if (! missing(boxes) ){
      if(boxes == "Other"){
        tscale_x<-nscale
      } else    tscale_x<-subset(tscale,tscale[,"Type"] == boxes)
	     
	      rect(tscale_x[,"Start"],par()$usr[3],tscale_x[,"End"],par()$usr[4],col=c("grey90","white"),border=NA)}
 
  points(ages,data,ylab="Length (mm)",cex=cex.pt,...)
axis(2)
}
  