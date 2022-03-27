#These are some of the exercises that the FAANG companies put to evaluate the DS with +18 years of experience (my experience).
#In my opinion, valueless if they are looking for a Director/Principal Data Scientist to add value to a business.

#Exercise ONE (Example)
x<-seq(1:10)
faangTest1 <- function (x) {
     if (x > 5) { print("poronga")}
     else { 
         if (x == 4) {return(x*1/2)}
         else {return(x**2)}}}
 
vec <- Vectorize(faangTest1, vectorize.args = "x")
vec(x)

# Calculate the mean of each number in a vector (FAANG March 2022)
media <- function(x){
    return(abs(x)/(2))
}
media(x)

# Generate a random sample, show the repeated numbers and calculate the sum, the max, and the min. (FAANG March 2022)
set.seed(1234)
df<-sample(1:100, 100, replace = TRUE)
df
df[duplicated(df)]
dfd<-df[duplicated(df)]
dfnd<-df[!duplicated(df)]
max(dfd)
min(dfd)
table(dfd)
sort(table(dfd))

# What are the prime numbers that appear in a number X (FAANG Feb 2022)
FactoresPrimos <- function(num) {
    current <- num
    ret.vals <- vector()
    x <- 2
    while (x <= num - 1){
        while (current %% x == 0) {
            current <- current / x
            ret.vals <- c(ret.vals, x)
        }
        x <- x + 1
    }
    if (is.logical(ret.vals)) return(num) else return(ret.vals)
}

# Generate FIBONACCI numbers (FAANG Feb 2022)
Num_fibonacci <-function(x){
num <-c(0, 1)
i =2
for (i in i:(x-1)){
num[i+1] <-num[i]+num[i-1]
}
num <-num[1:length(num)]
return(num)
}
Num_fibonacci(x)



