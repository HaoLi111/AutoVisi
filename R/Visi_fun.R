Visi.function<-function(f,rge,
                        x=seq(from=-5,to=5,length.out = 30),y =seq(from=-5,to=5,length.out = 30),z=seq(from=-5,to=5,length.out = 30)){
  if(rge==':'){

    value=sapply(x,f)
    plot(x,value,xlab='x',ylab='y',type='l')#,main =paste(as.character(body(f))))
  }else if(rge=='::'){
    m=mesh(x,y)
    z=matrix(NA,length(x),length(y))


  }else if(rge=='::/'){
    MFVN::contour.function(f,x=x,y=y)
  }else if(rge==':::'){
    z= matrix(NA,length(x),length(y))
    for(i in x){
      for(j in y){
        z[i,j] = f(x[i],y[j])
      }
    }
    m = mesh(x,y)

    plot3D:: surf3D(m$x,m$y,z)
  }
}
