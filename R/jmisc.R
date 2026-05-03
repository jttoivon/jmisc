
#' A function to check whether a numeric vector contains no decimals.
#'
#' @description
#' Check whether the numeric vector can be converted to type integer
#' without loosing information.
#'
#' @param x A numeric vector
#' @param tol The tolerance allowed
#'
#' @returns A single logical value
#' @seealso [is.integer()] which only checks the type
#' @export
#'
#' @examples
#' is_whole_number(c(0.0, 1, NA))
is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
  all(abs(x - round(x)) < tol, na.rm = TRUE)
}

# Unite levels so that count of each level is at least 'min'


#' A function to unite factor levels so that no level has less than 5 occurrences
#'
#' @description
#' Combines levels with small count so that no level has less than 5 occurrences.
#'
#' @param object A factor
#' @param min Minimum number of occurrences for a level
#' @param other_level Name of the combination level
#' @param max_levels Maximum number of levels in the result, including the lumped level
#'
#' @returns A factor
#' @seealso [forcats::fct_lump_min()] lumps levels that appear fewer than min times.
#' @importFrom forcats fct_other
#' @export
#'
#' @examples
#' v <- rep(letters[1:4], c(6,5,2,1)) |> as.factor()
#' v
#' fct_min_count(v)
fct_min_count <- function(object, min = 5, other_level = "Other", max_levels = NA) {
  nas <- is.na(object)
  ll <- levels(object)
  na_count <- sum(nas)
  if (0 < na_count && na_count < min) stop("Total count of NA values is too low")
  #if (na_count > 0) max_levels <- max_levels - 1

  tbl <- table(object)    # Count the elements
  tt <- c(tbl)            # Convert to named vector
  names(tt) <- dimnames(tbl)[[1L]]  # Is this really necessary?
  o <- sort.list(tt, decreasing = TRUE)   # sort.list returns the indices, not the values

  # Unite small bins into 'other' bin
  #if (min(tt) >= min && (is.na(max_levels))) return(object)
  all_levels_are_large <- min(tt) >= min
  if (all_levels_are_large && (is.na(max_levels) || length(ll) <= max_levels)) return(object)

  #print(o)
  i <- match(TRUE, tt[o] < min)  # Index of the first element that is less than min_count
  if (is.na(i)) i <- length(ll) + 1
  #print(i)

  # Guarantee that the resulting factor has at most
  # max_levels levels
  first_to_drop <- if (is.na(max_levels)) i else min(i, max_levels)

  last_to_drop <- match(TRUE, tt[o] == 0) - 1 # Don't drop the non-occurring levels
  if (is.na(last_to_drop)) last_to_drop <- length(ll) # If all levels occur, drop till end

  # Nothing to drop
  if (last_to_drop < first_to_drop) return(object)


  if (sum(tt[o[first_to_drop:last_to_drop]]) < min) {
    if (first_to_drop == 1) {
      stop("Total count of non-NA values is too low")
    } else {
      first_to_drop <- first_to_drop - 1
    }
  }

  drop <- first_to_drop:last_to_drop
  levels_to_unite <- names(tt)[o[drop]]
  #print(levels_to_unite)
  forcats::fct_other(object, drop = levels_to_unite, other_level = other_level)
}
