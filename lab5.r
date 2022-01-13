
#Factorial function

fact<-function(n)
{
    if (n==0)
    {
    return(1)
    }
    else
    {
    return(n*fact(n-1))
    }
}


#Next permutation function
nextpem<-function(x){
n = length(x) 
j = n-1
while (x[j] > x[j+1]) 
{ 
    j = j-1
}
k = n
while (x[j]>x[k]) 
{ 
    k = k-1
}
b = x[j]
x[j] = x[k] 
x[k] = b
r = n
s =  j+1 
while (r>s) {
    e = x[r] 
    x[r] = x[s] 
    x[s] = e
    r = r-1 
    s = s+1
}
return(x)
}

#Permutable function
pem<-function(a)
{
n = length(a) 
r = fact(n)
x <- array(dim = c(r,n)) 
for (i in 1:n) {
    x[1,i] = a[i]
}
for (i in 2:r) {
    x[i,] = nextpem(x[i-1,])
}
return(x)
}

#Set array A and B
A <- c(3,4,5)
B <- c(8,9,10,11)

cat("Permutable for array A : \n\n")
pem(A)
cat("\nPermutable for array B : \n\n")
pem(B)