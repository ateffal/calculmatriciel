GaussJordan <- function (A1, A2=matrix())
{
  #

  if(nrow(A1)<1) return("La matrice A1 n'a aucune ligne")
  if(nrow(A1)!=ncol(A1)) return("La matrice A1 doit être carrée")
  if(!is.na(A2[1,1])) {
    if(nrow(A1)!=nrow(A2)) return("Le nombre de ligne de A2 doit être égal à celui de A1")
  }

  if(is.na(A2[1,1])) A2 <- A1

  N <- nrow(A1)
  M <- ncol(A2)

  if(N>1) {

    for(i in 1:(N)) {
      #Choix du plus grand pivot
      l <- i
      m <- abs(A1[i,i])

      for(j in i:N) {
        if(j!=i) {
          s <- abs(A1[j,i])
          if(m<s) {
            l <- j
            m <- s
          }
        }
      }
      if(l!=i) {
        #Echange des lignes i et l de A1
        for(j in 1:N) {
          temp <- A1[i,j]
          A1[i,j] <- A1[l,j]
          A1[l,j] <- temp
        }

        #Echange des lignes de A2
        for(j in 1:M) {
          temp <- A2[i,j]
          A2[i,j] <- A2[l,j]
          A2[l,j] <- temp
        }
      }

      ########################################

      p <- 1/A1[i,i]
      #Division de la ligne par le pivot
      for(j in 1:N) {
        A1[i,j] <- p*A1[i,j]
      }

      for(j in 1:M) {
        A2[i,j] <- p*A2[i,j]
      }

      #Génération des zéros
      for(k in 1:N) {
        if(k!=i) {
          temp2 <- A1[k,i]
          for(j in 1:N) {
            if(j!=i) {
              A1[k,j] <- A1[k,j] - temp2*A1[i,j]
            }
          }

          for(j in 1:M) {
            A2[k,j] <- A2[k,j] - temp2*A2[i,j]
          }
          A1[k,i] <- 0
        }
      }
    }

#     p <- 1/A1[N,N]
#     A1[N,N] <- p*A1[N,N]
#     for(j in 1:M) {
#       A2[N,j] <- p*A2[N,j]
#     }


    return(list("A1"=A1,"A2"=A2))
  }

}
