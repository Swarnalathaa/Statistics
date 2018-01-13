"Law of Large Numbers state that as number of independent and identically 
distributed random variable increases, their sample mean 
approches the theoritical mean.

We have illustrated law of large numbers with exponential distribution. 

n : number of samples.
lamda : exponential parameter
K : number of times the experiment to be repeated

When we increae the value of n we can notice that the sample mean converges to 
theoritical mean.
"


LLN <- function(n,lamda,k)

{
  M = matrix(data = 0,nrow = k, ncol=n)
  for (i in 1:k) 
  {
    A = rexp(n,lamda)
    B = cumsum(A)/1:n
    M[i,] = B
  }

ma=max(M)

for(i in 1:k)
{
  plot(1:n,M[i,],type = 'l',col=palette()[i],xlim = c(1,n),ylim=c(0,ma))
  par(new=TRUE)
}
plot(c(1,n),c(1/lamda,1/lamda),xlim = c(1,n),ylim=c(0,ma),type='l',lwd=2)
}

