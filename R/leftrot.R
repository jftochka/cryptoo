leftrot <- function(v,rot){
  l <- length(v)
  v[(0:(l-1)+rot)%%l+1]
}
