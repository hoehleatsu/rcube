#' Twist cube
#'
#' Twist the cube by given string of moves and number of times.
#' @param cube - cube object
#' @param moves - string parameter
#' Syntax: The main QTM clockwise movements are the same as in the Singmasters notation: "U", "D", "F", "B", "R", "L". However moves from HTM such as U2 is not move of upper layer by 180 degrees (it will be explained further).
#' Counter clockwise moves are denoted by lowercase letters: "u", "d", "f", "b", "r", "l".
#' Rotations of the cube are denoted by "O" (rotate cube horizontally, "o" means rotation horizontally in different direction); and "P" (rotate cube vertically, "p" means rotation vertically in different direction).
#' Repetitions of the moves: there are several ways to repeat given sequence of moves. The simplest way is to copy commands. The most effective way to do this is using parameter times. However, in some cases it is useful to repeat only parts of sequence of moves - then we could use bracketing terms and operator times "x".
#' @param times - integer (default is 1). Number of repetitions of moves.
#' @return cube - cube object
#'
#' @examples
#' cube <- createCube()
#' # check moves LL FF RR BB
#' cube2 <- twistCube(cube,"LLFFRRBB")
#' # check if LFRB repeated 316 times is cycle:
#' cube3 <- twistCube(cube,"(LFRB)x316")
#' is.solved(cube3)
#' # TRUE
#' @export
twistCube <- function(cube,moves = "", times = 1){
  cube$cube <- kostka.obrot(cube$cube,moves,times)
  cube$moves <- paste(cube$moves,moves)
  return(cube)
}
