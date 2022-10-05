#' @title A Greedy Heuristic Model for the Knapsack Problem
#'
#' @description A greedy heuristic approximation model of the Knapsack Problem.
#'
#' @param x A data.frame with two columns: w (the weights of the objects) and
#' v (the values of the objects)
#' @param W The maximum weight the knapsack can hold
#'
#' @return A named list with two elements: value (which displays the highest
#' value that can be held in the knapsack) and elements (a vector listing the
#' item numbers in said knapsack)
#'
#' @details This model approximates a solution to the knapsack problem with a
#' value of at least m/2 (or 1/2 the maximum value that could be found to fit).
#' It does this by sorting each item in descending order by value per unit of
#' weight and fitting as many of these value-dense items into the knapsack as
#' possible and then comparing the value with the value of the first item that
#' is not able to fit. All items that cannot fit (w > W) are skipped over. For
#' more information regarding this method, please see the following description:
#' <https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm>
#'
#' @examples greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#'
#' @export
#'

greedy_knapsack <- function(x, W){

  # Before we start, we must ensure that the inputs are in the correct format:

  # Checks if W is positive
  if (W <= 0){
    stop("Error: 'W' cannot be less than or equal to 0")
  }

  # Checks if all entries in x are positive
  has.neg <- apply(x, 2, function(row) any(row < 0))
  if (length(which(has.neg)) != 0){
    stop("Error: 'x' contains negative entries")
  }

  # Checks if x is a data.frame
  if (!is.data.frame(x)){
    stop("Error: 'x' is not a data.frame")
  }

  # Checks if x contains only two columns
  if (ncol(x) != 2){
    stop("Error: data.frame 'x' should contain only 2 columns, 'w' and 'v'")
  }

  # Checks if x has proper column names
  if (colnames(x)[1] != "w" || colnames(x)[2] != "v"){
    stop("Error: 'x' contains invalid column names. colnames(x) should be w and v.")
  }

  # First, we create a new variable for unit value and use it to rank each
  # object in x from highest value density to lowest value density:
  x$unitval = x$v / x$w
  x <- x[order(x$unitval, decreasing = TRUE),]

  # Next, we define local variables for total weight, row indexing, total
  # value, and a vector of all elements in the knapsack:
  weight = W
  index = 1
  value = 0
  elements = c()

  # Then, we start at the top of the list and put the most value-dense items
  # into the knapsack until the knapsack can't fit anything more or we run
  # out of items to put into it:
  repeat{

    # Checks if item weighs more than the knapsack can handle (w > W)
    if (x$w[index] > W){
      index = index + 1
    }

    # Subtracts weight of item from the current avaialble weight limit
    weight = weight - x$w[index]

    # Breaks the loop if this item puts us over the weight limit
    if (weight < 0){
      break
    }

    # If the item fits, we add the value of the item to the total value and
    # place the element number in the elements vector
    value = value + x$v[index]
    elements = append(elements,
                      as.numeric(rownames(x[index,])))

    # Breaks the loop if we run out of items to put in the knapsack
    index = index + 1
    if (index > nrow(x)){
      break
    }

  }

  # If there are items that did not fit, we must compare our value to the
  # value of the first item that did not fit, which we will call s2:

  # Gives us the row number for s2
  repeat{

    # Checks if we are out of items to check
    if (index > nrow(x)){
      break
    }

    # Checks if item weighs more than the knapsack can handle (w > W)
    if (x$w[index] > W){
      index = index + 1
    } else {
      break
    }

  }

  # If we have an s2, we record the value and compare it to our current value.
  # If the value of s2 is greater than our current value, we replace our
  # current value with the value of s2 and list s2 as the only element in the
  # knapsack:
  if (index <= nrow(x)){

    s2 = as.numeric(rownames(x[index,]))
    s2val = x$v[index]

    if (s2val > value){
      value = s2val
      elements = c(s2)
    }

  }

  # Finally, we place the value and element vector in a named list and return:
  returnlist <- list("value" = round(value, 0), "elements" = elements)

  return(returnlist)
}
