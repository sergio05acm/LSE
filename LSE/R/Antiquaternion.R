Antiquaternion = function(x){

  t2 = ncol(x)/4 # no de cuat en cols
  t1 = nrow(x)/4 # no de cuat en fil
  a=0
  b=0
  c=0
  d=0
  aq = matrix(0,t1,4*t2)

  for (l in 1:t2) {


    for (i in 1:t1) {
      a[i] = x[4*i-3,4*l-3]
      a
    }
    for (j in 1:t1) {
      b[j] = x[(4*j-2),4*l-3]
      b
    }
    for (j in 1:t1) {
      c[j] = x[(4*j-1),4*l-2]
      c
    }
    for (j in 1:t1) {
      d[j] = x[(4*j),4*l-2]
      d
    }

    for (j in 1:t1) {
      aq[j,(4*l-3):(4*l)] = cbind(a[j],b[j],c[j],d[j])
    }
  }
  colnames(aq) = c("1","i","j","k")
  aq
}
