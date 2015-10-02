InverseMat <- function (M)
{
  if(class(M)!="matrix") return("M n'est pas une matrice")
  if(nrow(M)!=ncol(M)) return("La matrice M doit être carrée")
  return(GaussJordan(M,diag(nrow(M) ))$A2)

}
