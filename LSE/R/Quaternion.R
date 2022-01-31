Quaternion = function(a,b,c,d){
  Q = matrix(c(a,-b,d,-c,b,a,-c,-d,-d,c,a,-b,c,d,b,a),4,4,byrow = T)
  Q
}
