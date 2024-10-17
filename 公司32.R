Y<-c(0,1,0,1,1,0,1,1,0,0,0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,0,0,1)
X1<-c(0.534503786,0.097173137,0.479738312,0.267310172,0.346014883,0.62615745,0.448985303,0.299743827,0.2352772
      ,0.666748669,0.585250605,0.294560024,0.653692543,0.745123646,0.179843241
      ,0.209004944,0.660941077,0.759702454,0.376895215,0.519455444,0.203629356,0.615732552,0.414607249,
      0.875188996	,0.510567448,0.496535899,0.588507185,0.205960082,0.742280081,
      0.685533878,0.509872552,0.050191288	)
X2<-c(1.534177486,	7.815731238	,1.977006339,	3.049766012,	1.820409645,
      0.978324166,	1.681433481,	1.509947433,	2.415259908,	1.231105892,
      0.350630819,	2.095815964,	2.48634545,	1.083223436,	4.623892443,
      3.265785699,	1.339135491,	1.41463018,	2.138851146,	1.696304618,
      3.460590282,	1.261196807,	1.19896294,	0.67442633,	1.436678273,
      0.902996806,	1.255644554,	1.202871355,	1.054003857,	1.521485979,
      1.647619837,	17.95946671)
X3<-c(0.064538125,	0.049437674,	0.086142,	0.071707237,	0.060869237,
      0.006408925,	0.102218901,	0.00245843,	0.040280125,	-0.078463299,
      0.03490298,	0.034121977,	0.008902044,	0.02866814,	0.294087272	,
      0.07184204,	0.001978627	,-0.000147674	,0.065154085,	0.047796913	,
      0.034849065,	0.002849899,	0.035492472,	0.006096851,	0.026644734,
      0.01989917,	-0.029191679,	0.009761452,	-0.029647245,	0.003778394	,
      -0.028729088,	0.019014264)
β0<-numeric(50000)
β1<-numeric(50000)
β2<-numeric(50000)
β3<-numeric(50000)
β0[1]<-1
β1[1]<-1
β2[1]<-1
β3[1]<-1
Calculate_expression<- function(g,l,h,c) {#先验
  exp(-(g^2 / 20000) - ((l)^2 /20000)-((h-10)^2 /8)-((c)^2 /20000))
}
Calculate_expression_2<-function(g,l,h,c){#样本
  m<-1
  for (i in 1:32 ) {
    m=m*((exp(g+l*X1[i]+h*X2[i]+c*X3[i])/(1+exp(g+l*X1[i]+h*X2[i]+c*X3[i])))^Y[i])*(1/(1+(exp(g+l*X1[i]+h*X2[i]+c*X3[i]))))^(1-Y[i])
  }
  return(m)
}
tao_0<-16
tao_1<-16
tao_2<-4
tao_3<-100
refuse_value0<-0
refuse_value1<-0
refuse_value2<-0
refuse_value3<-0#拒绝次数
for (w in 2:50000) {
  new_β0<-rnorm(1,β0[w-1],tao_0)
  new_β0_post<-Calculate_expression(new_β0,β1[w-1],β2[w-1],β3[w-1])*Calculate_expression_2(new_β0,β1[w-1],β2[w-1],β3[w-1])
  old_β0_post<-Calculate_expression(β0[w-1],β1[w-1],β2[w-1],β3[w-1])*Calculate_expression_2(β0[w-1],β1[w-1],β2[w-1],β3[w-1])
  ratio_β0<-new_β0_post/old_β0_post
  alpha_0<-min(1,ratio_β0)
  U_0<-runif(1,min = 0,max = 1)
  if(U_0<=alpha_0){
    β0[w]<-new_β0
  }
  else{
    β0[w]<-β0[w-1]
    refuse_value0<-refuse_value0+1
  }
  new_β1<-rnorm(1,β1[w-1],tao_1)
  new_β1_post<-Calculate_expression(β0[w],new_β1,β2[w-1],β3[w-1])*Calculate_expression_2(β0[w],new_β1,β2[w-1],β3[w-1])
  old_β1_post<-Calculate_expression(β0[w],β1[w-1],β2[w-1],β3[w-1])*Calculate_expression_2(β0[w],β1[w-1],β2[w-1],β3[w-1])
  ratio_β1<-new_β1_post/old_β1_post
  alpha_1<-min(1,ratio_β1)
  U_1<-runif(1,min = 0,max = 1)
  if(U_1<=alpha_1){
    β1[w]<-new_β1
  }
  else{
    β1[w]<-β1[w-1]
    refuse_value1<-refuse_value1+1
  }
  new_β2<-rnorm(1,β2[w-1],tao_2)
  new_β2_post<-Calculate_expression(β0[w],β1[w],new_β2,β3[w-1])*Calculate_expression_2(β0[w],β1[w],new_β2,β3[w-1])
  old_β2_post<-Calculate_expression(β0[w],β1[w],β2[w-1],β3[w-1])*Calculate_expression_2(β0[w],β1[w],β2[w-1],β3[w-1])
  ratio_β2<-new_β2_post/old_β2_post
  alpha_2<-min(1,ratio_β2)
  U_2<-runif(1,min = 0,max = 1)
  if(U_2<=alpha_2){
    β2[w]<-new_β2
  }
  else{
    β2[w]<-β2[w-1]
    refuse_value2<-refuse_value2+1
  }
  new_β3<-rnorm(1,β3[w-1],tao_3)
  new_β3_post<-Calculate_expression(β0[w],β1[w],β2[w],new_β3)*Calculate_expression_2(β0[w],β1[w],β2[w],new_β3)
  old_β3_post<-Calculate_expression(β0[w],β1[w],β2[w],β3[w-1])*Calculate_expression_2(β0[w],β1[w],β2[w],β3[w-1])
  ratio_β3<-new_β3_post/old_β3_post
  alpha_3<-min(1,ratio_β3)
  U_3<-runif(1,min = 0,max = 1)
  if(U_3<=alpha_3){
    β3[w]<-new_β3
  }
  else{
    β3[w]<-β3[w-1]
    refuse_value3<-refuse_value3+1
  }
}  
plot(β0,pch=16,cex=0.5,type = "b",col="orange")  
plot(β1,pch=16,cex=0.5,type = "b",col="orange")  
plot(β2,pch=16,cex=0.5,type = "b",col="orange")  
plot(β3,pch=16,cex=0.5,type = "b",col="orange")
mean_β0<-numeric(50000)
mean_β1<-numeric(50000)
mean_β2<-numeric(50000)
mean_β3<-numeric(50000)
for (z in 1:50000 ) {
  mean_β0[z]<-mean(β0[1:z])
  mean_β1[z]<-mean(β1[1:z])
  mean_β2[z]<-mean(β2[1:z])
  mean_β3[z]<-mean(β3[1:z])
}
plot(mean_β0,pch=16,cex=0.5,type = "b",col="orange")
plot(mean_β1,pch=16,cex=0.5,type = "b",col="orange")
plot(mean_β2,pch=16,cex=0.5,type = "b",col="orange")
plot(mean_β3,pch=16,cex=0.5,type = "b",col="orange")
mean(β0[20000:50000])
mean(β1[20000:50000])
mean(β2[20000:50000])
mean(β3[20000:50000])