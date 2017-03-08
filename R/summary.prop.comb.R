summary.prop.comb <-
function(object, ...){

##############################################################

#               A PROPORTION

if (object$k==1) {    
    cat("", "\n")
    cat("      1-sample proportion test", "\n")
    cat("", "\n")
    cat ("number of successes =",object$x , "number of trials =" ,object$n,"\n" ) #,object$nn ,"\n") 

cat("sample estimates", "\n")
cat("p","\n")
cat(round(object$estimate,4),"\n")
cat("","\n")

borrar = switch(object$alternative, 
          two.sided = "is not equal to", 
          greater = "is greater than", 
          less = "is less than" )

cat("alternative hypothesis: true probability of success", borrar, object$p, "\n")
cat("               ",round(100*object$conf.level),"percent confidence interval", "\n")

print(round(object$inference[-6,],4))



}

# DIFFERENCE OF PROPORTIONS
else if(object$k==2 & object$a[1]==-1 & object$a[2]==1) {
 
    cat("", "\n")
    cat("      ","difference of proportions", "\n")
   
cat("","\n")

P=cbind(1:object$k,object$x,object$n,object$estimate)

colnames(P)=c("sample","x","n","prop")
rownames(P)=rep("",dim(P)[1])

cat("x: number of successes", "\n")
cat("n: number of trials", "\n")
cat("prop: proportion sample estimates", "\n")
cat("","\n")
print(round(P,4))

cat("","\n")

cat("difference of proportions: D=p2-p1","\n")
cat("estimated D:",round(object$difference,4), "\n")

cat("","\n")

borrar = switch(object$alternative, 
          two.sided = "is not equal to", 
          greater = "is greater than", 
          less = "is less than" )

cat("alternative hypothesis: true difference D", borrar, object$p, "\n")
cat("               ",round(100*object$conf.level),"percent confidence interval", "\n")
print(round(object$inference,4))


}



##############################################################

#               COMBINATION OF TWO OR MORE PROPORTIONS

else  {
 
    cat("", "\n")
    cat("      ",object$k,"linear combination of proportions", "\n")
   
cat("","\n")

P=cbind(1:object$k,object$x,object$n,object$a,object$estimate)

colnames(P)=c("sample","x","n","beta","prop")
rownames(P)=rep("",dim(P)[1])

cat("x: number of successes", "\n")
cat("n: number of trials", "\n")
cat("beta: coefficients of the combination", "\n")
cat("prop: proportion sample estimates", "\n")
cat("","\n")
print(round(P,4))

cat("","\n")


L=paste(round(object$a[1],4),"*p1",sep="")
for (i in 2:object$k) {
if (object$a[i]>=0) {L=paste(L,"+",round(object$a[i],4),"*p",i,sep="")} else 
{L=paste(L,round(object$a[i],4),"*p",i,sep="")}
}

cat("combination of interest: L=",L, "\n")
cat("estimated L:",round(object$L,4), "\n")

cat("","\n")

borrar = switch(object$alternative, 
          two.sided = "is not equal to", 
          greater = "is greater than", 
          less = "is less than" )

cat("alternative hypothesis: true combination L", borrar, object$p, "\n")
cat("               ",round(100*object$conf.level),"percent confidence interval", "\n")
print(round(object$inference,4))

}

cat("","\n")
cat("Recommendation:","\n")
cat( object$recommendation,"\n")

#cat("Power:","\n")
#print(object$power)
}