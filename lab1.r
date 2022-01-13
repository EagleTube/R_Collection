and <- function(x,y){
    z = F
    if (x==T){
        if (y==T){
            z=T
            }
    }
    return(z)
}


not <- function(p){
    z=F
    if(p==F)
    {
        z=T
    }
    return(z)
}
exc <- function(p,q){
    z=T
    if(p==q)
    {
        z=F
    }
    return(z)
}
or <- function(p,q){
    z=T
    if(p==F){
        if(q==F)
        {
            z=F
        }
    }
    return(z)
} 

imp<-function(p,q)
{
    z = or(not(p),q)
    return(z)
}

bicoin<-function(p,q){
    z = and(imp(p,q),imp(p,q))
    return(z)
}

question6 <- function(p,q,r){
    z = or(or(not(p),and(not(q),r)),or(and(r,q),and(r,p)))
}

question7 <- function(p,q,r,s){
    z = bicoin(or(p,imp(q,and(r,not(p)))),or(q,not(s)))
    return(z)
}

cat('question7(0,0,0,0) -> ',question7(0,0,0,0),'\n')
cat('question7(0,0,0,1) -> ',question7(0,0,0,1),'\n')
cat('question7(0,0,1,0) -> ',question7(0,0,1,0),'\n')
cat('question7(0,0,1,1) -> ',question7(0,0,1,1),'\n')
cat('question7(0,1,0,0) -> ',question7(0,1,0,0),'\n')
cat('question7(0,1,0,1) -> ',question7(0,1,0,1),'\n')
cat('question7(0,1,1,0) -> ',question7(0,1,1,0),'\n')
cat('question7(0,1,1,1) -> ',question7(0,1,1,1),'\n')
cat('question7(1,0,0,0) -> ',question7(1,0,0,0),'\n')
cat('question7(1,0,0,1) -> ',question7(1,0,0,1),'\n')
cat('question7(1,0,1,0) -> ',question7(1,0,1,0),'\n')
cat('question7(1,0,1,1) -> ',question7(1,0,1,1),'\n')
cat('question7(1,1,0,0) -> ',question7(1,1,0,0),'\n')
cat('question7(1,1,0,1) -> ',question7(1,1,0,1),'\n')
cat('question7(1,1,1,0) -> ',question7(1,1,1,0),'\n')
cat('question7(1,1,1,1) -> ',question7(1,1,1,1),'\n')
