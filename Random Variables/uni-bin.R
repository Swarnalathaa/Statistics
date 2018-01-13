
"function to get bernoulli from uniform"
bern<-function(N,p)
{
  U<-runif(N)
  A=1*(U<=p)
}

"function to get binomial from bernoullie"
binom<-function(N,m,p)
{
  
  a=bern(N*m,p)
  M = matrix(data =a,nrow = N, ncol=m)
  binom<-apply(M,1,sum)
}
"function to get geometric from bernouille"
geom<-function(N,p)
{
  L=c()
  for (i in 1:N) 
    {
    a=0
    j=0
    while (a!=1)
      {
      a=bern(1,p)
      j=j+1
      
    }
    L=c(L,j)
  }
  geom<-L
}

"function to get exponential from uniform"
sexp<-function(N,lamda)
{
  U=runif(N)
  sexp<-(-1/lamda)*log(U)
}



