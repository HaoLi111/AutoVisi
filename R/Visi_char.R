#Visi.character
Visi.character =function(f,rge,
                                        x=seq(from=-3,to=3,length.out = 30),y =seq(from=-3,to=3,length.out = 30),z=seq(from=-5,to=5,length.out = 30)){
  if(rge==':'){

    value=eval(parse(text = f))
    plot(x,value,xlab='x',ylab='y',type='l',main = f)#,main =paste(as.character(body(f))))
  }else if(rge=='::'){
    m=mesh(x,y)
    z=matrix(NA,length(x),length(y))

  }else if(rge=='::/'){
    MFVN::contour.function(f,x=x,y=y)
  }else if(rge==':::'){
    m = mesh(x,y)
    i=x;j=y
    z= matrix(NA,length(x),length(y))
    for(x in i){
      for(y in j){
        z[x,y] = eval(parse(text =f))
      }
    }


    plot3D:: surf3D(m$x,m$y,z)
  }
}
