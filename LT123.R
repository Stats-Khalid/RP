#Life Table 23
x= c(0,1,seq(from=5, to=85, by=5))
lx = c(100000,99562,99484,99440,99387,99277,99148,98981,98743,98386,97817,96923,95510,93393,90159,85201,77504,65457,48163)
dx=c() ; px=c() ; qx=c() ; Lx=c() ; Tx=c() ; e0_x=c()
length(x)
length(lx)
for (i in 1:length(x)){
  dx[i]=lx[i]-lx[i+1]
  px[i]=lx[i+1]/lx[i]
  qx[i]=1-px[i]
  Lx[i]=((x[i+1]-x[i])/2)*(lx[i]+lx[i+1])
}
for (i in 1:19){
  Tx[i]=sum(Lx[i:(length(x)-1)])
  e0_x[i]= Tx[i]/lx[i]
}
life_Table =data.frame(Age=x,lx,dx,px,qx,Lx,Tx,e0_x)
life_Table



#Life table 13
x0= 40 ; x1=41 ; l0=10653 ; l1=10543 ; d1=169 ; T0=193820
d0=l0-l1;d0
q0=d0/l0;q0
q1=d1/l1;q1
p0=1-q0;p0
p1=1-q1;p1
L0= l0-d0/2;L0
L1= l1-d1/2;L1
T1=T0-L0;T1
e0_0 = T0/l0;e0_0
e0_1=T1/l1;e0_1


#life table 16
x= c(4,5)
lx= c(95000,NA)
dx=c(500,400)
Tx=c(4850300,NA)
lx[2]=lx[1]-dx[1] ; lx
qx=dx/lx ; qx
px=1-qx ; px
Lx= lx-dx/2 ; Lx
Tx[2]=Tx[1]-Lx[1] ; Tx
ex_0=Tx/Lx ; ex_0
LT= data.frame(Age=x,lx=lx,dx=dx,px=round(px,4),qx=round(qx,4),Lx=Lx,Tx=Tx,ex0=ex_0)
LT


#15to calculate GFR, TFR...
age <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
women <- c(16000,16400,15800,15200,14800,15000,14500)
births <- c(260,2244,1894,1320,916,280,145)
#Compute GFR
total_births <- sum(births)
total_women <- sum(women)
GFR <- (total_births/total_women)*1000 ; GFR
#Compute ASFR
ASFR <- (births/women)*1000 ; ASFR
# Compute TFR
TFR <- (5/1000)*sum(ASFR) ; TFR
# Compute GRR
GRR <- TFR*0.462 ; GRR
# Compute NRR
px <- c(120,180,150,200,220,230,250)
lx <- 1-(px/1000)
NRR <- sum(ASFR * 5* 0.462*lx)/1000 
NRR
