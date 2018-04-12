#' Creating cube size NxNxN
#'
#' Creates a cube object with empty moves and color scheme information
#'
#' @param N integer - size of cube. Default value is 3, and maximum is 20. More than maximum (20) is possible, however parser will ignore moving layers with number greater than 10 - you will not be able to get full varity of those cubes.
#' @param mode string "normal" (default) or "octa" or "void".
#' @param scheme string vector - colour scheme for plotting cube. Name of colours should be given in specified order: front, top, right, bottom, left, back.
#'
#' Default value is c("orange","yellow","blue","white","green","red")
#' @return Cube class object
#'
#' @examples
#' # Create 3x3x3 cube with original color scheme:
#' cube <- createCube()
#' # Create 14x14x14 cube with original color scheme:
#' cube <- createCube(N = 14)
#' # Create 3x3x3 cube with "japanese" color scheme:
#' cube <- createCube(scheme = c("green","white","red","blue","orange","yellow"))
#'
#' @export
createCube <- function(N = 3, mode = "normal", scheme = c("orange","yellow","blue","white","green","red")) {
  k0 <- matrix(rep(0,times = N*N),ncol = N)
  if(mode == "octa") {
    m = round(N/2)
    n = N-m
  k211 <- matrix(rep(5,times = n*n),ncol = n)
  k212 <- matrix(rep(6,times = n*m),ncol = m)
  k221 <- matrix(rep(1,times = m*n),ncol = n)
  k222 <- matrix(rep(2,times = m*m),ncol = m)
  k2 <- rbind(cbind(k211,k212),cbind(k221,k222))

  k111 <- matrix(rep(1,times = m*n),ncol = n)
  k112 <- matrix(rep(2,times = m*m),ncol = m)
  k121 <- matrix(rep(3,times = n*n),ncol = n)
  k122 <- matrix(rep(4,times = m*n),ncol = m)
  k1 <- rbind(cbind(k111,k112),cbind(k121,k122))
  k311 <- matrix(rep(2,times = m*m),ncol = m)
  k312 <- matrix(rep(6,times = n*m),ncol = n)
  k321 <- matrix(rep(4,times = m*n),ncol = m)
  k322 <- matrix(rep(7,times = n*n),ncol = n)
  k3 <- rbind(cbind(k311,k312),cbind(k321,k322))
  k411 <- matrix(rep(3,times = m*n),ncol = n)
  k412 <- matrix(rep(4,times = m*m),ncol = m)
  k421 <- matrix(rep(8,times = n*n),ncol = n)
  k422 <- matrix(rep(7,times = m*n),ncol = m)
  k4 <- rbind(cbind(k411,k412),cbind(k421,k422))
  k511 <- matrix(rep(5,times = m*n),ncol = n)
  k512 <- matrix(rep(1,times = m*m),ncol = m)
  k521 <- matrix(rep(8,times = n*n),ncol = n)
  k522 <- matrix(rep(3,times = m*n),ncol = m)
  k5 <- rbind(cbind(k511,k512),cbind(k521,k522))
  k611 <- matrix(rep(6,times = m*m),ncol = m)
  k612 <- matrix(rep(5,times = m*n),ncol = n)
  k621 <- matrix(rep(7,times = m*n),ncol = m)
  k622 <- matrix(rep(8,times = n*n),ncol = n)
  k6 <- rbind(cbind(k611,k612),cbind(k621,k622))
  }
  if(mode %in% c("normal","void")) {
  k1 <- matrix(rep(1,times = N*N),ncol = N)
  k2 <- matrix(rep(2,times = N*N),ncol = N)
  k3 <- matrix(rep(3,times = N*N),ncol = N)
  k4 <- matrix(rep(4,times = N*N),ncol = N)
  k5 <- matrix(rep(5,times = N*N),ncol = N)
  k6 <- matrix(rep(6,times = N*N),ncol = N)}
  if(mode == "void"){
    l <- floor(N/3)+1
    p <- N-l+1
    k1[l:p,l:p] <- 7
    k2[l:p,l:p] <- 7
    k3[l:p,l:p] <- 7
    k4[l:p,l:p] <- 7
    k5[l:p,l:p] <- 7
    k6[l:p,l:p] <- 7
scheme <- c(scheme,"black")
  }
  w1 <- cbind(k0,k2,k0,k0)
  w2 <- cbind(k5,k1,k3,k6)
  w3 <- cbind(k0,k4,k0,k0)
  kostka <- rbind(w1,w2,w3)
  res <- list()
  res$cube <- kostka
  res$moves <- ""
  res$scheme <- scheme
  res$size <- N
  #res$scheme <- c("white","red","green","orange","blue","yellow")
  if(mode == "octa") res$scheme <- c("grey","red","green","orange","blue","yellow","violet","white")
  class(res) <- "cube"
  return(res)
}
