LUdec <- function (A)
{
 # Cet algorithme nécessite que toutes les sous-matrices principales soit régulières
 # On doit donc vérifier cela en premier.
  if(class(A)!="matrix") return("A n'est pas une matrice")
  if(nrow(A)!=ncol(A)) return("La matrice A doit être carrée !")

  if(nrow(A)<=1) return("La matrice A doit avoir au moins deux lignes")



  N<-nrow(A)
  if(N<2) {
    return("La matrice A doit être au moins 2*2")
  }
  if(A[1,1]==0) {
    return(cat("La première matrice principale n'est pas inversible ! (A[1,1]=0"))
  }

  for(i in 2:N) {
      B<-A[1:i,1:i]
      C<-InverseMat(B)
      if(!is.finite(C[1,1])) { return(cat("La ",i,"-ème matrice principale n'est pas inversible !"))}
  }
  cat("Toutes les matrices principale de A sont inversibles .")

}
