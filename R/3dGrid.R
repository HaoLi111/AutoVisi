GL_R3<-function(){
  #surf3Drgl(m$x,m$y,z,border = 'black',box = "black")
  rgl::grid3d('x')
  rgl::grid3d('y')
  rgl::grid3d('z')
  rgl::axes3d('x')
  rgl::axes3d('y')
  rgl::axes3d('z')
}#Draw linear Cartesian (R^3) boxes
