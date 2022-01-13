A <- function(a,b,c,d,e,f,g,h,i,j,k,l){
    matrix(c(a,b,c,d,e,f,g,h,i,j,k,l),nrow=3,ncol=4)
}
#print(A(1,4,5,3,2,6,2,0,8,-4,7,-3))

B <- function(a,b,c,d,e,f,g,h){
    matrix(c(a,b,c,d,e,f,g,h),nrow=4,ncol=2)
}

#print(B(-1,12,3,8,6,3,7,4))

C <- function(a,b,c,d,e,f,g,h,i){
    matrix(c(a,b,c,d,e,f,g,h,i),nrow=3,ncol=3)
}

#print(C(5,-13,9,7,11,8,9,2,-7))
ADD <- function(m1,m2){
    m1c <- ncol(m1)
    m1r <- nrow(m1)

    m2c <- ncol(m2)
    m2r <- nrow(m2)
    if(m1r==m2r && m1c==m2c)
    {
        d<-matrix(0,nrow=m1r,ncol=m1c)
            for(i in 1:m1r){
                for(j in 1:m1c)
                {
                    d[i,j]=m1[i,j]+m2[i,j]
                }
            }
        return(d)
    }
    else
    {
        return('Error adding two matrices, maybe different count of cols/rows')
    }
}

MUL <- function(m1,m2){
    m1c <- ncol(m1)
    m1r <- nrow(m1)

    m2c <- ncol(m2)
    m2r <- nrow(m2)

    d<-matrix(0,nrow=m1r,ncol=m2c)
    for ( ii in 1:dim(m1)[1] ) {
        for ( jj in 1:dim(m2)[2] ) {
            for (kk in 1:dim(m1)[2] ) {
                d[ii,jj] = d[ii,jj] + m1[ii,kk]*m2[kk,jj]
            }
        }
    }
    return(d)
}

TRAN <- function(m)
{
    mc <- ncol(m)
    mr <- nrow(m)
    d<-matrix(0,nrow=mc,ncol=mr)
    for(i in 1:mr){
        for(j in 1:mc){
            d[j,i]=m[i,j]
        }
    }
    return(d)
}

print(MUL(TRAN(A(1,4,5,3,2,6,2,0,8,-4,7,-3)),A(1,4,5,3,2,6,2,0,8,-4,7,-3)))