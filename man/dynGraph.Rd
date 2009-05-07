\name{dynGraph}
\alias{dynGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interactive visualization of dataframes and factorial planes}
\description{
dynGraph is a java application that allows to interact with the representation of scatterplots from a dataframe or 
from a multivariate analysis performed with the FactoMineR package
}
\usage{
dynGraph(res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{\code{res} can be either a dataframe or results provided by a PCA, CA, MCA, MFA with the FactoMineR package}
}
\author{Julien Durand and Sebastien Le }

\examples{
library(FactoMineR)

data(wine)
dynGraph(wine)

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
dynGraph(res.pca)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
