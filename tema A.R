#Functie distributie Poisson
poisson_prob = function(lambda, k, m) {
  sapply(k:m, function(x) dpois(x, lambda))
}

#Functie distributie Geometrica
geometric_prob = function(p, k, m) {
  sapply(k:m, function(x) dgeom(x, p))
}

#Functie distributie Binomiala
binomial_prob = function(n, p, k, m) {
  sapply(k:m, function(x) dbinom(x, n, p))
}



#(b) Reprezentarea grafica 
plot_probabilities = function(lambda, p, n, k, m) {
  # Calculam probabilitatile
  poisson_probs = poisson_prob(lambda, k, m)
  geometric_probs = geometric_prob(p, k, m)
  binomial_probs = binomial_prob(n, p, k, m)
  
  # Vector pentru valorile de la k la m
  values = k:m 
  
  # Plot
  plot(values, poisson_probs, type="h", lwd=2, col="blue", 
       xlab = "Values", ylab="Probability", main = "Probability Mass Functions")
  lines(values, geometric_probs, type="h", lwd=2, col="red")
  lines(values, binomial_probs, type="h", lwd=2, col="green")
  
  legend("topright", legend=c("Poisson", "Geometric", "Binomial"), 
         col=c("blue", "red", "green"), lwd=2, lty=1)
}


# Exemplu de apelare a funcției
plot_probabilities(lambda=3, p=0.5, n=10, k=0, m=10)



#c) Cel mai mic k0 pt care P(Y<=k0)>1−10^(-6)
find_k0_poisson = function(lambda, limita=1 - 1e-6) {
  k0 = 0
  while (ppois(k0, lambda) <= limita) {
    k0 = k0 + 1
  }
  return(k0)
}

# Exemplu de apelare a funcției
lambda <- 3
k0 <- find_k0_poisson(lambda)
print(k0)

#ex2
ex2_a=function(){
  ob=read.csv("note_PS.csv",header=T,sep=',');
  x=ob[["P"]];
  y=ob[["S"]];
  freq_x = as.vector(table(x));
  freq_y = as.vector(table(y));
  rel_freq_x = freq_x / length(x);
  rel_freq_y = freq_y / length(y);
  exp_x = mean(x);
  exp_y = mean(y);
  print("freq x");
  print(freq_x);
  print("freq y");
  print(freq_y);
  print("rel freq x");
  print(rel_freq_x);
  print("rel freq y");
  print(rel_freq_y);
  print("expected x");
  print(exp_x);
  print("expected y");
  print(exp_y);
}
print("ex2_a")
probabilities =ex2_a();
print(probabilities);

ex2_b=function(file_name){
  
  x=scan(file_name)
  print(x);
  z=as.vector(summary(x));
  m=z[3];
  q1=z[2];
  q3=z[4];
  iqr=q3-q1;
  y=vector();
  k=1;
  for(i in 1:length(x))
  {
    if(x[i]>q1-1.5*iqr & x[i]<q3+1.5*iqr)
    {
      y[k]=x[i];
      k=k+1;
    }
  }
  intervals= seq(0, 10, 1);
  freq_y= table(cut(y, breaks = intervals));
  barplot(freq_y);
  print(y);
  
}
print("ex2_b")
probabilities =ex2_b("sample1.txt");
print(probabilities)

