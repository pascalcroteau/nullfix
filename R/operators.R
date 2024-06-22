
#' Logical And Operator That Works With `NULL`
#'
#' This infix function returns `FALSE` when `x` is falsey (`NULL` or `FALSE`).
#' It will also returns `FALSE` if `x` is truthy and `y` is `NULL`.
#' It otherwise works like the operator `&&`. It is inspired by the way the
#' operator `&&` works in Ruby.
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
  if (is.null(x)) FALSE else if (is.null(y)) FALSE else x && y
}



#' Logical And Operator That Works With `NULL`
#'
#' This infix function returns `x` when `x` is falsey (`FALSE` or `NULL`). So it
#' would return `NULL` if `x` is `NULL`. It otherwise works like the operator
#' `&&`. It is inspired by the way the operator `and` works in Ruby.
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
  if (is.null(x)) NULL else if (is.null(y)) NULL else x && y
}



#
# `%.&.%` <- function(x, y)
# {
#   nx <- length(x)
#   ny <- length(y)
#   if (nx != ny) {
#     i.n <- which.min(c(nx, ny))
#     if (i.n == 1L) {
#       warning("foo")
#       y <- y[1:nx]
#     } else {
#       warning("bar")
#       x <- x[1:ny]
#       nx <- ny
#     }
#   }
#   not_null <- !is.null(x) & !is.null(y)
#
#   lgl_out <- logical(nx)
#   if (any(not_null)) lgl_out[not_null] <- x[not_null] & y[not_null]
#
#   lgl_out
# }




#' If Modifier
#'
#' This infix operator is inspired by Ruby's `if` modifier.
#'
#' With the `%.IF.%` operator, the `x` value is returned if the `condition`
#' evaluates to `TRUE`, and `NULL` otherwise.
#'
#' No checks on `condition` are performed; they are left to the `if` statement.
#'
#' @note
#' The `%.IF.%` has low precedence value, so you may have to use parenthesis to
#' get the behavior you are looking for. See examples.
#'
#' @param x            A R object to return if `condition` evaluates to `TRUE`.
#' @param condition    A length-1 `logical` vector.
#'
#' @returns
#' If `condition` evaluates to `TRUE`, returns `x`; `NULL` otherwise.
#'
#' @examples
#' number <- 5
#' value <- 10
#' (value %.IF.% TRUE) # 10
#'
#' # This is likely to give surprising results, as you may expect that number > 20
#' # is FALSE and thus NULL should be returned.
#' # But in reality, because of operator precedence, only `number` is passed to
#' # `%.IF.%`, which evaluates to `TRUE` (since it is greater than 0).
#' # The order of operation is
#' # 1. value %.IF.% number, which returns `value`.
#' # 2. ((value %.IF.% number) > 20), which is `value` > 20, which is `FALSE`
#' (value %.IF.% number > 20) # FALSE
#'
#' # This is more likely what was intended
#' (value %.IF.% (number > 20)) # NULL
#' (value %.IF.% (number > 1)) # 10
#'
#' @export
`%.IF.%` <- function(x, condition)
{
  if (condition) x else NULL
}





`%.IN.%` <- function(x, table)
{
  if (is.null(x)) return(FALSE)
  `%in%`(x, table)
}


#' @importFrom magrittr `%>%`
`%.>.%` <- function(lhs, rhs)
{
  if (is.null(lhs)) return(NULL)

  lhs %>% rhs
}






