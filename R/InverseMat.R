InverseMat <- function (M)
{

  if(nrow(M)!=ncol(M)) return("La matrice M doit être carrée")
  return(GaussJordan(M,diag(nrow(M) ))$A2)

}
