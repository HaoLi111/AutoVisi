
'%V%' <-function(a,b) Visi(a,b)
'%VR%'<-function(a,b) Visi(a,b,wrt=TRUE)#return code


Visi<-function(mydata,...) UseMethod('Visi')
Visi.data.frame<-function(mydata,aspect,Facet = NULL){
  if(length(aspect)==1){
    if(aspect=='::/'){
    filled.contour(mydata)
  }else if(aspect=='Cor'|aspect == "~"){

    mydata = mydata[,sapply(mydata[1,],is.numeric)]    #numerical value only
    m=cor(mydata)
    corrplot::corrplot(m,method = 'ellipse',type = "lower")
    title(paste('Linear correlation'))
    symnum(m)
  }else if(aspect=='2d'|aspect=='::'){
    mydata = mydata[,sapply(mydata[1,],is.numeric)]
    GGally::ggpairs(mydata)
  }else if(aspect=='Box'|aspect==':'){
    mydata =  mydata[,sapply(mydata[1,],is.numeric)]
    layout(matrix(1:ncol(mydata),1))
    for(i in 1: ncol(mydata)){
      boxplot(mydata[,i],ylab = colnames(mydata)[i],col = 'blue',
              main=paste('Boxplot of',colnames(mydata)[i]))
      try(rug(jitter(mydata[,i]),col='red',side = 2));abline(h=mean(mydata[,i],na.rm=T),lty=2,col = 'red')
      #title()
      #text(y = mean(data[,i],na.rm = T),paste('Mean:',mean(data[,i],na.rm = T)),
      #    side = 2)
    }
  }else if(aspect=='Hist'|aspect==':/'){
    mydata =  mydata[,sapply(mydata[1,],is.numeric)]
    layout(matrix(1:ncol(mydata),1))
    for(i in 1: ncol(mydata)){
    hist(mydata[,i],prob = T,breaks=29,xlab=colnames(mydata)[i],
         col = 'blue',main = paste('Histogram of',colnames(mydata)[i]))
    lines(density(mydata[,i],na.rm = T),col = 'red',lty=2)
    rug(jitter(mydata[,i]),col = 'red')
    }
    layout(1)
    #see also Data Mining with R by Luis Torgo
  }else{

    tt=ifelse(is.character(aspect),aspect,colnames(mydata)[aspect])
    layout(matrix(1:2,1))
    i = aspect
    hist(mydata[,i],prob = T,breaks=29,xlab=colnames(mydata)[i],
         col = 'blue',main ='Histogram')
    lines(density(mydata[,i],na.rm = T),col = 'red',lty=2)
    rug(jitter(mydata[,i]),col = 'red')
    #boxplot
    boxplot(mydata[,i],ylab = colnames(mydata)[i],col = 'blue',
            main='Boxplot')
    try(rug(jitter(mydata[,i]),col='red',side = 2));abline(h=mean(mydata[,i],na.rm=T),lty=2,col = 'red')
    layout(1)
    title(tt)
  }
    }else if(length(aspect)==2){
    name=ifelse(is.character(aspect),aspect,colnames(mydata)[aspect])
    mydata = mydata[,name]
    plot(mydata)
  }else if(length(aspect)==3){
    name=NULL
    for(k in 1:3){

      if(is.character(aspect[k])){
        name[k]=aspect[k]
      }else{
        name[k]=colnames(mydata)[aspect[k]]
      }
    }
    mydata = mydata[,name]
    plot3D::scatter3D(mydata[,1],mydata[,2],mydata[,3],xlab=name[1],
                                 ylab=name[2],zlab=name[3])#,highlight.3d = T)
  }else{
    #Higher D data

  }
}

Visi.matrix<-function(m,dir = '::/'){
  if(dir =='::/') plot3Drgl::persp3Drgl(z=m)
}
