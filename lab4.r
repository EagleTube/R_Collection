fact <- function(n)
{
    if(n==0)
    {
        return(1)
    }
    else
    {
        f <- n*fact(n-1)
        return(f)
    }
}

pwr <- function(a,n)
{
    if(n==0)
    {
        return(1)
    }
    else
    {
        f <- a*pwr(a,n-1)
        return(f)
    }
}

lns <- function(i,j,x,A)
{
    if(A[i]==x)
    {
        return(i)
    }
    else if(A[i]==j)
    {
        return(0)
    }
    else
    {
        f <- lns(i+1,j,x,A)
        return(f)
    }
}

bns <- function(i,j,x,B)
{
    m=(i+j)/2
    if(x==B[m])
    {
        return(floor(m))
    }
    else if(x<B[m] && i<m)
    {
        return(bns(i,m-1,x,B))
    }
    else if(x>B[m] && j>m)
    {
        return(bns(m+1,j,x,B))
    }
    else
    {
        return(0)
    }
    return(floor(bns))
}

A <- c(3,50,22,97,113,60,32,66,88,99)

cat("Factorial of number 3\n")
fact(3)
cat("\nFactorial of number 54\n")
fact(54)
cat("\nFactorial of number 10\n")
fact(27)

cat("\n2 power of 2\n")
pwr(2,2)
cat("\n4 power of 3\n")
pwr(4,3)

cat("\nLinear search of number 32 at\n")
lns(1,10,32,A)
cat("\nLinear search of number 88 at\n")
lns(1,10,88,A)

cat("\nBinary search of number 97 at\n")
bns(1,10,97,A)
cat("\nBinary search of number 113 at\n")
bns(1,10,113,A)