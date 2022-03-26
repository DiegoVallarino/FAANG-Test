#These are some of the exercises that the Tech companies put to evaluate the DS with +18 years of experience.
#In my opinion, valueless.

#Exercise ONE

x<-seq(1:10)
faangTest1 <- function (x) {
     if (x > 5) { print("poronga")}
     else { 
         if (x == 4) {return(x*1/2)}
         else {return(x**2)}}}
 
vec <- Vectorize(faangTest1, vectorize.args = "x")
vec(x)

#Exercise TWO

faangTest2 <- function(x){
  return(abs(x)/(2))
}
faangTest2(x)
