#visiMap

vmap = function(.x,.f){
  if(is.function(.f)){
    .f(.x)
  }else{
    for(i in seq_along(.f)) {
      #if(!.f[i]=='ugl') windows(...)#no new window for rgl based plot
      if(substr(.f[i],1,2)=='gg'){#print from html5 from gg objects
        print(do.call(.f[i],list(.x)))
      }else{#normal plots
        do.call(.f[i],list(.x))
      }

    }
  }

}

vmap_windows = function(.x,.f,...){
  if(is.function(.f)) .f = deparse(substitude(.f))

  for(i in seq_along(.f)) {
    if(!(substr(.f[i],1,3)=='ugl')) windows(...)#no new window for rgl based plot
    if(substr(.f[i],1,2)=='gg'){#print from html5 from gg objects
      print(do.call(.f[i],list(.x)))
    }else{#normal plots
    do.call(.f[i],list(.x))
    }
  }
}

vmap_save =function(.x,.f,where = getwd()){
  if(substr(.f[i],1,3)=='ugl'){
    tiff::writeTIFF(do.call(.f[i],list(.x)),
                    paste0(f[i],'.tif'))
  }else if(substr(.f[i],1,2)=='gg'){
    g=do.call(.f[i],list(.x))
    ggsave(filename = paste0(f[i],'.pdf'),g,path = where)
  }else if(substr(.f[i],1,5) =='image'){
    tiff::writeTIFF(do.call(.f[i],list(.x)),
                    paste0(f[i],'.tif'))
  }else{
    do.call(.f[i],list(.x))
    bmp(paste0())
  }

}

#vmap_windows(iris,c("ggpairs","ggparcoord","plot","ugl"))
#vmap(iris,c("ggpairs","plot","ugl"))
#vmap(iris,c("ggpairs","ugl"))
