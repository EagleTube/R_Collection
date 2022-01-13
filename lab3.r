
linear<-function(x,A){
    n<-length(A)
    i<-1
    loc<-A[i]
    while(i<=n && x!=A[i])
    {
        i<-i+1
        if(i<=n)
        {
            loc<-i
        }
        else
        {
            loc<-0
        }
    }
    if(loc!=0)
    {
        return(cat("Number",x,"found!","\n"))
    }
    else
    {
        return(cat("Number",x,"not found!","\n"))
    }
}

B<-c(8,1,5,2,4,3,7,6,9)
sort<-function(A){
    n<-length(A)
    for(j in 1:(n-1)){
        for(i in 1:(n-j)){
            if(A[i]>A[i+1]){
                temp<-A[i]
                A[i]<-A[i+1]
                A[i+1]<-temp
            }
        }
    }
    return(A)
}

A<-c(1,2,3)
bsearch<-function(x,A)
{
  i = 1 
  j = length(A)
  while (i<j) {
    m = floor((i+j)/2)
    if (x>A[m])
      {
      i = (m+1)
    }
    else{
      j = m
    }
  }
    if(x == A[i])
    {
      return(cat("Number",x,"found!","\n"))
    }
    else {
      return(cat("Number",x,"not found!","\n"))
    }
}
bsearch(0,A)
bsearch(1,A)
bsearch(2,A)
bsearch(3,A)
bsearch(4,A)
