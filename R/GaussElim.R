GaussElim <- function (A, b)
{
  # Vérification des dimensions des deux matrices A et b
  if(nrow(A)<1) return("La matrice A n'a aucune ligne")
  if(nrow(A)!=ncol(A)) return("La matrice A doit être carrée")
  if(nrow(A)!=nrow(b)) return("Le nombre de ligne de b doit être égal à celui de A")
  if(ncol(b)>1) return("b doit avoir une seule colonne") 
  
  N<-nrow(A)
  if(N == 1) {
    if (A[1,1] != 0) {
      return(eval(b[1,1] / A[1,1]))
    }
    else {
      if (b[1,1] != 0)
        return("Pas de solution")
      else
        return("La solution est l'ensemble R")
    }
  }
  
  if(N>1) {
    
    for(i in 1:(N-1)) {
      #Choix du plus grand pivot
      l <- i
      m <- abs(A[i,i])
      for(j in (i+1):N) {
        s <- abs(A[j,i])
        if(m<s) {
          l <- j
          m <- s
        }
      }
      if(l!=i) {
        for(j in i:N) {
          temp <- A[i,j]
          A[i,j] <- A[l,j]
          A[l,j] <- temp
        }
        temp <- b[i,1]
        b[i,1] <- b[l,1]
        b[l,1] <- temp
      }
      ########################################
      
      p <- 1/A[i,i]
      for(j in i:N) {
        A[i,j] <- p*A[i,j]
      }
      b[i,1] <- p*b[i,1]
      for(k in (i+1):N) {
        for(j in (i+1):N) {
          A[k,j] <- A[k,j] - A[k,i]*A[i,j]
        }
        b[k,1] <- b[k,1] - A[k,i]*b[i,1]
        A[k,i] <- 0
      }
    }
    
  }
  p <- 1/A[N,N]
  A[N,N] <- p*A[N,N]
  b[N,1] <- p*b[N,1]
  
  #Solution : x
  x<-matrix(nrow=N)
  x[N,1] <- b[N,1]
  for(i in (N-1):1) {
    x[i,1] <- b[i,1]
    for(j in N:(i+1)) {
      x[i,1] <- x[i,1] - A[i,j]*x[j,1]
    }
  }
  return(list("A"=A,"b"=b,"x"=x))
}
