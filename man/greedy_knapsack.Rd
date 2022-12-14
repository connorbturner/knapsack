% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy_knapsack.R
\name{greedy_knapsack}
\alias{greedy_knapsack}
\title{A Greedy Heuristic Model for the Knapsack Problem}
\usage{
greedy_knapsack(x, W)
}
\arguments{
\item{x}{A data.frame with two columns: w (the weights of the objects) and
v (the values of the objects)}

\item{W}{The maximum weight the knapsack can hold}
}
\value{
A named list with two elements: value (which displays the highest
value that can be held in the knapsack) and elements (a vector listing the
item numbers in said knapsack)
}
\description{
A greedy heuristic approximation model of the Knapsack Problem.
}
\details{
This model approximates a solution to the knapsack problem with a
value of at least m/2 (or 1/2 the maximum value that could be found to fit).
It does this by sorting each item in descending order by value per unit of
weight and fitting as many of these value-dense items into the knapsack as
possible and then comparing the value with the value of the first item that
is not able to fit. All items that cannot fit (w > W) are skipped over. For
more information regarding this method, please see the following description:
\url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
}
\examples{
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

}
