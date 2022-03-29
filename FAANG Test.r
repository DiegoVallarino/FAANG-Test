# These are some of the exercises that the FAANG companies put to evaluate the DS with +18 years of experience (my experience).

#Exercise ONE (Example)
x<-seq(1:10)
faangTest1 <- function (x) {
     if (x > 5) { print("poronga")}
     else { 
         if (x == 4) {return(x*1/2)}
         else {return(x**2)}}}
vec <- Vectorize(faangTest1, vectorize.args = "x")
vec(x)

# Replace NAs with the mean or median in an array (FAANG Feb 2022)
NAfunc <- function(x) {
 na_index <- which(is.na(x))        
mean_x <- mean(x, na.rm=T)
x[na_index] <- mean_x
return(x)}
df_clean <- apply(df,2,NAfunc)

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
sum(dfd)
table(dfd)
sort(table(dfd))

# Generate the odd numbers from 1 to 100, calculate the square root of each one and add them. (FAANG March 2022)
x<-seq(1,100,by=2) #números impares
raiz <- function(x){
    return(abs(x)**(1/2))
}
raiz(x)
sum(raiz(x))

# Note: some questions were about SQL (more focused on queries than on data management) and I also did it with R. Just an example:
library(sqldf)
data_Male<-sqldf("SELECT * FROM data WHERE Sex like 'Male' ")

# SOME OTHERS EXAMPLES OF TECHNICAL QUESTIONS (from leetcode, solution in R)
#Given two integers a and b, return the sum of the two integers without using the operators + and - (leetcode - Medium)
x<-7
y<-3
sum(x,y)

# Find the Duplicate Number (leetcode - Medium)
df<-sample(1:50, 50, replace = TRUE)
dfd<-df[duplicated(df)]
table(dfd)
sort(table(dfd))

# Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays (leetcode - Hard)
A <- matrix(c(10, 8, 5, 12, 15, 19), ncol = 2, byrow = TRUE)
B <- matrix(c(5, 3, 15, 6, 10, 12), ncol = 2, byrow = TRUE)
X<-(A+B)/2
X ---- # Note: If the distribution is symmetric then the mean is equal to the median and the distribution will have zero skewness.

# Given two integers dividend and divisor, divide two integers without using multiplication, division, and mod operator. (leetcode - Hard)
dividend = 10
divisor = 3
 10%/%3

# Given a string n representing an integer, return the closest integer (not including itself), which is a palindrome. If there is a tie, return the smaller one. (leetcode - Hard)
 




