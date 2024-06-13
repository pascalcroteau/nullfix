
#' Logical And Operator That Works With `NULL`
#'
#' This infix function returns `FALSE` when either `x` or `y` is `NULL`. It
#' otherwise works like the operator `&&`.
#'
#' No checks are performed; they are left to the `&&` operator.
#'
#' @param x,y    Any types that `&&` accepts, or `NULL`.
#'
#' @returns
#' A length-one logical vector, including if either `x` or `y` is `NULL`. In the
#' latter case, `FALSE` is returned.
#'
#' @name op-and-null-lgl
#' @seealso [op-and-null-nil]
#'
#' @examples
#' TRUE %.&&.% FALSE # FALSE
#' NULL %.&&.% FALSE # FALSE
#' TRUE %.&&.% NULL # FALSE
#' NULL %.&&.% NULL # FALSE
#'
#' @export
`%.&&.%` <- function(x, y)
{
  if (is.null(x) || is.null(y)) FALSE else x && y
}



#' Logical And Operator That Works With `NULL`
#'
#' This infix function returns `NULL` when either `x` or `y` is `NULL`. It
#' otherwise works like the operator `&&`.
#'
#' No checks are performed; they are left to the `&&` operator.
#'
#' @param x,y    Any types that `&&` accepts, or `NULL`.
#'
#' @returns
#' A length-one logical vector, except if either `x` or `y` is `NULL`. In the
#' latter case, `NULL` is returned.
#'
#' @name op-and-null-nil
#' @seealso [op-and-null-lgl]
#'
#' @examples
#' TRUE %.AND.% FALSE # FALSE
#' NULL %.AND.% FALSE # NULL
#' TRUE %.AND.% NULL # NULL
#' NULL %.AND.% NULL # NULL
#'
#' @export
`%.AND.%` <- function(x, y)
{
  if (is.null(x) || is.null(y)) NULL else x && y
}




`%.&.%` <- function(x, y)
{
  nx <- length(x)
  ny <- length(y)
  if (nx != ny) {
    i.n <- which.min(c(nx, ny))
    if (i.n == 1L) {
      warning()
      y <- y[1:nx]
    } else {
      warning()
      x <- x[1:ny]
      nx <- ny
    }
  }
  not_null <- !is.null(x) & !is.null(y)

  lgl_out <- logical(nx)
  if (any(not_null)) lgl_out[not_null] <- x[not_null] & y[not_null]

  lgl_out
}





`%.IF.%` <- function(x, test)
{
  if (test) x else NULL
}



`%.IN.%` <- function(x, table)
{
  if (is.null(x)) return(FALSE)
  `%in%`(x, table)
}


#' @importFrom magrittr ` %>% `
`%.>.%` <- function(lhs, rhs)
{
  if (is.null(lhs)) return(NULL)

  lhs %>% rhs
}






