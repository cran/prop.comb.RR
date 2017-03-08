

coverageRR=function(A,nrep=1000) {
p0=Pvero_ratio(A$x,A$n,A$rho)
p0=c(p0,A$rho*p0)
xb=numeric(2)
pvalor=NULL
for (irep in 1:nrep) {
  for (j in 1:2)  xb[j]=rbinom(1, A$n[j], prob=p0[j])
  pvalor=rbind(pvalor,
    prop.RR(x=xb, n=A$n, rho=A$rho,alternative=A$alternative,conf.level=A$conf.level)$inference[,4])
}

pvalor2=apply(pvalor,2,"<",1-A$conf.level)
B=apply(pvalor2,2,mean,na.rm=TRUE)

A$inference=cbind(A$inference,1-B)
colnames(A$inference)[5]="coverage"
A
}

