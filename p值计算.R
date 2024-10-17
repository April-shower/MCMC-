g<-2.394679
l<--0.3297759
p<-function(x){
  (exp(g+l*x)/(1+exp(g+l*x)))
}
p(4)