Poisson <- function(n, lambda)
{
  x=0:(n-1)
  y=dpois(x, lambda)
  barplot(y, space=0, main='barplot', sub="ex 10", xlab="axa x", ylab="axa y")
}

Poisson(15, 3)
