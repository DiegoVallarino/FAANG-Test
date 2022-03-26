#These are some of the exercises that the Tech companies put to evaluate the DS with +18 years of experience.
#In my opinion, the value of these roles goes elsewhere.

x<-seq(1:10)
faangTest <- function (x) {
     if (x > 5) { print("poronga")}
     else { 
         if (x == 4) {return(x*1/2)}
         else {return(x**2)}}}
		 
vec <- Vectorize(faangTest, vectorize.args = "x")
vec(x)