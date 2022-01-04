#' Rotates the elements of a vector to the left
#'
#' @param v Vector to be rotated
#' @param rot Number of positions to rotate
#'
#' @return Rotated vector
#' @export
#'
#' @examples
#' leftrot(1:10,2)
#' leftrot(letters,3)
leftrot <- function(v,rot){
  l <- length(v)
  v[(0:(l-1)+rot)%%l+1]
}
