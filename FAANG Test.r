x<-seq(1:10)
faangTest <- function (x) {
     if (x > 5) { print("poronga")}
     else { 
         if (x == 4) {return(x*1/2)}
         else {return(x**2)}}}
		 
vec <- Vectorize(faangTest, vectorize.args = "x")
vec(x)