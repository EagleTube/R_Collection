transpose<-function(x)
{
  r1 = nrow(x)
  c1 = ncol(x)
  e = matrix(0, nrow = c1,ncol = r1)

  for(i in 1:c1)
  {
    for(j in 1:r1)
    {
      e[i,j] = x[j,i]
    }
  }
  return(e)
}

symmetric<-function(a)
{
  r1 = ncol(a)
  c1 = nrow(a)

  b = transpose(a)
  r2 = ncol(b)
  c2 = nrow(b)

  if(r1 == r2)
  {
    if(c1 == c2)
    {
      print("Matrix is symmetry after transpose")
  }
  }else
  {
      print("Matrix is not symmetry after transpose")
  }
}

M=matrix(c(1,-7,4,-7,2,0,4,0,3),nrow = 3,ncol = 3, byrow = TRUE)
N=matrix(c(4,-7,-3,0,2,-7,3,0,4),nrow = 3,ncol = 3, byrow = TRUE)

cat("Matrix M\n")
print(M)
cat("Matrix N\n")
print(N)
cat("\n\nMatrix M transpose\n")
transpose(M)
cat("\nMatrix N transpose\n")
transpose(N)
cat("\n\nMatrix M symmetric\n")
symmetric(M)
cat("\nMatrix N symmetric\n")
symmetric(N)

