coveragecomb=function(A,nrep=1000,a=NULL) {


Similitud <-function(x,n,p=0,a=c(-1,1)){

if(length(x)==1) {pv=p} else {
 pp=x/n; L=a%*%pp 
 if (L==p) {pv=pp} else  {
 Z=FindZ.MA(x,n,a=a,lambda=p)
 rest=L-p; b=1-2*x/n
 r2=n^2*rest^2+ a^2*Z^2 +2*n*a*b*rest*Z
 pv=(n*rest+a*Z-sign(rest)*sqrt(r2))/(2*a*Z)}}
 pv}



p0=Similitud(x=A$x, n=A$n, p=A$p, a=A$a) 

nx=length(A$x); xb=numeric(nx)
pvalor=NULL
for (irep in 1:nrep) {
  for (j in 1:nx)  xb[j]=rbinom(1, A$n[j], prob=p0[j])
  
if (missing(a)) 
{pvalor2=prop.comb(x=xb, n=A$n, p=A$p, alternative=A$alternative, conf.level=A$conf.level)$inference[,4]} else {pvalor2=prop.comb(x=xb, n=A$n, p=A$p,a=a, alternative=A$alternative, conf.level=A$conf.level)$inference[,4]}

pvalor=rbind(pvalor,pvalor2)
} # bucle repeticiones

pvalor2=apply(pvalor,2,"<",1-A$conf.level)
B=apply(pvalor2,2,mean,na.rm=TRUE)

A$inference=cbind(A$inference,1-B)
colnames(A$inference)[5]="coverage"
A
} # end





