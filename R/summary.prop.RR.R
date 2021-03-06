summary.prop.RR <-
function(object, ...) {

 
    cat("", "\n")
    cat("      ","relative risk", "\n")
   
cat("","\n")

P=cbind(1:2,object$x,object$n,object$estimate)

colnames(P)=c("sample","x","n","prop")
rownames(P)=rep("",dim(P)[1])

cat("x: number of successes", "\n")
cat("n: number of trials", "\n")
cat("prop: proportion sample estimates", "\n")
cat("","\n")
print(round(P,4))

cat("","\n")


cat("realtive risk: RR=p2/p1","\n")
cat("estimated RR:",round(object$RR,4), "\n")

cat("","\n")

borrar = switch(object$alternative, 
          two.sided = "is not equal to", 
          greater = "is greater than", 
          less = "is less than" )

cat("alternative hypothesis: true relative risk R", borrar, object$rho, "\n")
cat("","\n")
cat("               ",round(100*object$conf.level),"percent confidence interval", "\n")
print(round(object$inference,4))
cat("","\n")
cat("Recommendation:","\n")
cat(object$recommendation,"\n")
# cat("Power:","\n")
# print(object$power)

}

