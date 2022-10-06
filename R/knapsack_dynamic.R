#' @title A Dynamic Programming Model for the Knapsack Problem
#'
#' @param x A data.frame with two columns: w (the weights of the objects) and
#' v (the values of the objects)
#' @param W The maximum weight the knapsack can hold
#'
#' @return A named list with two elements: value (which displays the highest
#' value that can be held in the knapsack) and elements (a vector listing the
#' item numbers in said knapsack)
#'
#' @examples knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#'
#' @export
#'

knapsack_dynamic <- function(x, W){

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
    stop("Error: 'x' contains invalid column names. colnames(x) should be 'w' and 'v'.")
  }

  n <- nrow(x)
  mat <- matrix(0,nrow =n+1, ncol = W+1)
  rownames(mat) <- c(0:n)
  colnames(mat) <- c(0:W)
  
  for (i in 1:n){
    for (j in 1:W){
      if (x$w[i]>j){
        mat[as.character(i),as.character(j)] <-  mat[as.character(i-1),as.character(j)]
      }
      else{
        mat[as.character(i),as.character(j)] <- 
          max(mat[as.character(i-1),as.character(j)],
              mat[as.character(i-1),as.character(j-x$w[i])]+x$v[i])
      }
    }
  }
  el <- NULL
  max_v = mat[as.character(n),as.character(W)]
  max_value = max_v
  for (i in n:1){
    if(max_v %in% mat[as.character(i),]){
      if(max_v %in% mat[as.character(i-1),]){
        next
      }
    }
    el <- append(el,i)
    max_v <- max_v - x$v[i]
  }
  result = c(value = max_value, elements = list(el))
  return(result)
  
}