\name{calculate.overlap}
\alias{calculate.overlap}
\title{Calculate Overlap}
\description{Determine the groupings of values as they would be presented in the venn diagram.}
\usage{
calculate.overlap(x)
}
\arguments{
  \item{x}{A list of vectors (e.g., integers, chars), with each component corresponding 
  to a separate circle in the Venn diagram}
  }
\details{
This function mostly complements the venn.diagram() function for the case where users
want to know what values are grouped into the particular areas of the venn diagram.
}
\value{
Returns a list of lists which contain the values assigned to each of the areas of a venn diagram, where the list names a1, a2, ..., a7 are defined as:
For an element i in the set of all values contained in the vectors a, b and c:
a1 contains all i ϵ (a & !b & !c), i.e. all elements found only in a
a2 contains all i ϵ (a & b & !c), i.e. all elements found only in a and b
a3 contains all i ϵ (b & !a & !c), i.e. all elements found only in b
a4 contains all i ϵ (a & c & !b), i.e. all elements found only in a and c
a5 contains all i ϵ (a & b & c), i.e. all elements found in a and b and c
a6 contains all i ϵ (b & c & !a), i.e. all elements found only in b and c
a7 contains all i ϵ (c & !a & !b), i.e. all elements found only in c
}
\author{Christopher Lalansingh}
\examples{
# A simple single-set diagram
cardiome <- letters[1:10]
superset <- letters[8:24]
overlap <- calculate.overlap(
	x = list(
		"Cardiome" = cardiome,
		"SuperSet" = superset
		)
	);
}
\keyword{hplot}
