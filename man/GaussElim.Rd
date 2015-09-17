\name{GaussElim}
\alias{GaussElim}
\title{
Resolving linear equation system : A.x = b
}
\description{
The fonction gives solutions of equation system A.x = b using the Gauss elimination algorithm
in which A is a N*N matrix, x the unknown N*1 vector and b is a N*1 vector.
}
\usage{
GaussElim(A,b)
}
\arguments{
  \item{A}{
The N*N matrix in A.x = b
}
\item{b}{
The (N*1) vector in A.x = b
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A (N*1) matrix containing the solutions of A.x = b
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Amine Teffal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
