#Helper function for multiplying  line i and column j for two matrices until# an index
MultLC <- function(A, B, i, j, k) {        
  #Vérifications    
  if(class(A)!="matrix") return("A n'est pas une matrice")    
  if(class(B)!="matrix") return("B n'est pas une matrice")    
  if(nrow(B)!=ncol(A)) return("La matrice B doit avoir le même nombre de colonnes que A!")    
  if(i<1 || i > nrow(A)) return("i doit être entre 1 et lignes A !")    
  if(j<1 || j > ncol(B)) return("j doit être entre 1 et colonnes B  !")    
  if(k<1 || k > ncol(A)) return("k doit être entre 1 et colonnes A  !")        

  sum=0    for(l in 1:k) {        
   sum = sum + A[i,l]*B[l,j]    
  }        
  return(sum)
}

#LU Decomposition
LUdec <- function (A)
{
 # Cet algorithme nÃ©cessite que toutes les sous-matrices principales soit rÃ©guliÃ¨res
 # On doit donc vÃ©rifier cela en premier.
  if(class(A)!="matrix") return("A n'est pas une matrice")
  if(nrow(A)!=ncol(A)) return("La matrice A doit Ãªtre carrÃ©e !")
　
  if(nrow(A)<=1) return("La matrice A doit avoir au moins deux lignes")
　
　
　
  N<-nrow(A)
  if(N<2) {
    return("La matrice A doit Ãªtre au moins 2*2")
  }
  if(A[1,1]==0) {
    return(cat("La premiÃ¨re matrice principale n'est pas inversible ! (A[1,1]=0"))
  }
　
  for(i in 2:N) {
      B<-A[1:i,1:i]
      C<-InverseMat(B)
      if(!is.finite(C[1,1])) { return(cat("La ",i,"-Ã¨me matrice principale n'est pas inversible !"))}
  }
  cat("Toutes les matrices principale de A sont inversibles.\n")
  
  for(i in 2:N) {
      A[1,i] = A[1,i]/A[1,1]
  }
  
 
  
  if((N-1)<2) {
      A[N,N] = A[N,N] -MultLC(A,A,N,N,N-1)
      return(A)
  }
  
　
  
  for(k in 2:(N-1)) {
      A[k,k] = A[k,k]-MultLC(A,A,k,k,k-1)
      if((k+1)<=N) {
          for(i in (k+1):N) {
              A[i,k] = A[i,k] - MultLC(A,A,i,k,k-1)
              if(A[k,k]==0) {
                  return(cat("Division par zéro !"))
              }
              A[k,i] = (1/A[k,k])*(A[k,i]- MultLC(A,A,k,i,k-1))
          }
      }
  }
  
  A[N,N] = A[N,N] -MultLC(A,A,N,N,N-1)
  return(A)
　
}

