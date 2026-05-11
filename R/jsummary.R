# TODO Handle levels with zero occurrences
# DONE This does not handle the case when the sum of "other" elements is less than min_count
# DONE What if there are less than min_count NAs?
# TODO For binary vectors, check if there are less than min_count FALSE or TRUE values
# DONE Calendar times and dates don't work
# DONE The "(Other)" level is not last in the summaries
# DONE Option to not show quantiles
# DONE difftime does not work
# How to print standard deviation for dates/time/timediffs?

#' Privacy aware object summaries
#'
#' @param object An object
#' @param ... Additional arguments
#' @param digits Passed to signif()
#' @param quantile.type Integer
#' @param show_quantiles A logical value
#' @param sd A logical value. Whether to show standard deviation
#'
#' @returns Depends on the class of the argument
#' @importFrom stats sd
#' @export
#'
#' @examples
#' jsummary(1:10)
jsummary.default <- function(object, ..., digits, quantile.type = 7, show_quantiles = TRUE, sd = TRUE)
{
  #show_quantiles <- TRUE
  #show_quantiles <- FALSE
  if (is.factor(object))
    return(jsummary.factor(object, show_quantiles = show_quantiles, ...))
  else if (is.matrix(object)) {
    if (missing(digits))
      return(summary.matrix(object, quantile.type = quantile.type,
                            ...))
    else return(summary.matrix(object, digits = digits, quantile.type = quantile.type,
                               ...))
  }
  value <- if (is.logical(object))
    c(Mode = "logical", {
      tb <- table(object, exclude = NULL, useNA = "ifany")
      if (!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n))) dimnames(tb)[[1L]][iN] <- "NA's"
      tb
    })
  else if (is.numeric(object)) {
    nas <- is.na(object)
    object <- object[!nas]
    if (show_quantiles) {
      qq <- stats::quantile(object, names = FALSE, type = quantile.type)
      qq <- c(qq[1L:3L], mean(object), qq[4L:5L])
    } else {
      qq <- c(mean(object))
    }
    if (sd) qq <- c(qq, sd(object))
    if (!missing(digits))
      qq <- signif(qq, digits)
    if (show_quantiles) {
      new_names <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                     "Max.")
    } else {
      new_names <- c("Mean")
    }
    if (sd) new_names <- c(new_names, "Sd")
    names(qq) <- new_names
    if (any(nas))
      c(qq, `NA's` = sum(nas))
    else qq
  }
  else if (is.recursive(object) && !is.language(object) &&
           (n <- length(object))) {
    sumry <- array("", c(n, 3L), list(names(object), c("Length",
                                                       "Class", "Mode")))
    ll <- numeric(n)
    for (i in 1L:n) {
      ii <- object[[i]]
      ll[i] <- length(ii)
      cls <- oldClass(ii)
      sumry[i, 2L] <- if (length(cls))
        cls[1L]
      else "-none-"
      sumry[i, 3L] <- mode(ii)
    }
    sumry[, 1L] <- format(as.integer(ll))
    sumry
  }
  else c(Length = length(object), Class = class(object), Mode = mode(object))
  class(value) <- c("summaryDefault", "table")
  value
}


#' Title
#'
#' @param object A factor
#' @param maxsum Maximum number of levels to show
#' @param ... Additional arguments
#'
#' @returns A named vector
#' @export
#'
#' @examples
#' v <- factor(rep(c("a", "b"), c(3, 2)), levels=c("a", "b", "c"))
#' jsummary(v)
jsummary.factor <- function(object, maxsum = 100L, ...)
{
  min_count <- 5
  other_level <- "(Other)"
  nas <- is.na(object)
  ll <- levels(object)
  if (ana <- any(nas))
    maxsum <- maxsum - 1L
  object <- fct_min_count(object, min = min_count, other_level = other_level, max_levels = maxsum)
  tbl <- table(object)    # Count the elements
  tt <- c(tbl)            # Convert to named vector
  names(tt) <- dimnames(tbl)[[1L]]
  o <- sort.list(tt, decreasing = TRUE)   # sort.list returns the indices, not the values
  # Unite small bins into 'other' bin
  # if (length(ll) > maxsum || min(tt) < min_count) {
  #   #print(o)
  #   i <- match(TRUE, tt[o] < min_count)  # Index of the first element that is less than min_count
  #   #print(i)
  #   first_to_drop <- maxsum
  #   if (! is.na(i) && i < first_to_drop) first_to_drop <- i
  #   if (sum(tt[o[first_to_drop:length(ll)]]) < min_count) {
  #     if (first_to_drop == 1) stop("Total count of non-NA values is too low") else first_to_drop <- first_to_drop - 1
  #   }
  #   drop <- first_to_drop:length(ll)
  #   tt <- c(tt[o[-drop]], `(Other)` = sum(tt[o[drop]]))
  # } else {
  tt <- tt[o]
  if (other_level %in% names(tt)) {
    tt <- c(tt[names(tt) != other_level], tt[other_level])
  }
  #}
  if (ana)
    c(tt, `NA's` = sum(nas))
  else tt
}

# Changed the default value of maxsum from 7 to 8 to match the added std. dev.
# That is, for factors and numerics total of 8 rows are shown at maximum


#' Title
#'
#' @param object A dataframe
#' @param maxsum Maximum number of levels shown
#' @param digits Passed to signif()
#' @param show_quantiles A logical value
#' @param sd A logical value. Whether to show standard deviation
#' @param ... Additional arguments
#'
#' @returns A table
#' @export
#'
#' @examples
#' jsummary(attenu)
jsummary.data.frame <- function (object, maxsum = 8L, digits = max(3L, getOption("digits") -
                                                                     3L),
                                 show_quantiles = TRUE, sd = TRUE, ...)
{
  ncw <- function(x) {
    z <- nchar(x, type = "w", allowNA = TRUE)
    if (any(na <- is.na(z))) {
      z[na] <- nchar(encodeString(z[na]), "b")
    }
    z
  }
  z <- lapply(X = as.list(object), FUN = jsummary, maxsum = maxsum,
              digits = 12L, show_quantiles = show_quantiles, sd = sd, ...)
  nv <- length(object)
  nm <- names(object)
  lw <- numeric(nv)
  nr <- if (nv)
    max(vapply(z, function(x) NROW(x) + !is.null(attr(x,
                                                      "NAs")), integer(1)))
  else 0
  for (i in seq_len(nv)) {
    sms <- z[[i]]
    if (is.matrix(sms)) {
      cn <- paste(nm[i], gsub("^ +", "", colnames(sms),
                              useBytes = TRUE), sep = ".")
      tmp <- format(sms)
      if (nrow(sms) < nr)
        tmp <- rbind(tmp, matrix("", nr - nrow(sms),
                                 ncol(sms)))
      sms <- apply(tmp, 1L, function(x) paste(x, collapse = "  "))
      wid <- sapply(tmp[1L, ], nchar, type = "w")
      blanks <- paste(character(max(wid)), collapse = " ")
      wcn <- ncw(cn)
      pad0 <- floor((wid - wcn)/2)
      pad1 <- wid - wcn - pad0
      cn <- paste0(substring(blanks, 1L, pad0), cn, substring(blanks,
                                                              1L, pad1))
      nm[i] <- paste(cn, collapse = "  ")
    }
    else {
      sms <- format(sms, digits = digits)
      lbs <- format(names(sms))
      sms <- paste0(lbs, ":", sms, "  ")
      lw[i] <- ncw(lbs[1L])
      length(sms) <- nr
    }
    z[[i]] <- sms
  }
  if (nv) {
    z <- unlist(z, use.names = TRUE)
    dim(z) <- c(nr, nv)
    if (anyNA(lw))
      warning("probably wrong encoding in names(.) of column ",
              paste(which(is.na(lw)), collapse = ", "))
    blanks <- paste(character(max(lw, na.rm = TRUE) + 2L),
                    collapse = " ")
    pad <- floor(lw - ncw(nm)/2)
    nm <- paste0(substring(blanks, 1, pad), nm)
    dimnames(z) <- list(rep.int("", nr), nm)
  }
  else {
    z <- character()
    dim(z) <- c(nr, nv)
  }
  attr(z, "class") <- c("table")
  z
}

# jsummary.POSIXlt <- function (object, digits = 15, ...) {
#   summary(as.POSIXct(object), digits = digits, ...)
# }

#' Title
#'
#' @param object A calendar date
#' @param digits Number of digits for fractional seconds
#' @param show_quantiles A logical value
#' @param sd A logical value. Whether to show standard deviation
#' @param ... Additional arguments
#'
#' @returns A summaryDefault object
#' @export
#'
#' @examples
#' jsummary(as.POSIXct(0:10))
jsummary.POSIXct <- function (object, digits = 15L, show_quantiles = TRUE, sd = FALSE, ...)
{
  x <- jsummary.default(unclass(object), digits = digits, show_quantiles = show_quantiles, sd = FALSE, ...)
  if (m <- match("NAs", names(x), 0L)) {
    NAs <- as.integer(x[m])
    x <- x[-m]
    attr(x, "NAs") <- NAs
  }
  .POSIXct(x, tz = attr(object, "tzone"), cl = c("summaryDefault",
                                                 oldClass(object)))
}


#' Title
#'
#' @param object A Date object
#' @param digits Number of significant digits for computations
#' @param show_quantiles A logical value
#' @param sd A logical value. Whether to show standard deviation
#' @param ... Additional arguments
#'
#' @returns A summaryDefault object
#' @export
#'
#' @examples
#' jsummary(as.Date(0:10))
jsummary.Date <- function (object, digits = 12L, show_quantiles = TRUE, sd = FALSE, ...)
{
  x <- jsummary.default(unclass(object), digits = digits, show_quantiles = show_quantiles, sd = FALSE, ...)
  if (m <- match("NAs", names(x), 0L)) {
    NAs <- as.integer(x[m])
    x <- x[-m]
    attr(x, "NAs") <- NAs
  }
  .Date(x, c("summaryDefault", oldClass(object)))
}


#' Title
#'
#' @param object A difftime object
#' @param digits Number of significant digits for computations
#' @param show_quantiles A logical value
#' @param sd A logical value. Whether to show standard deviation
#' @param ... Additional arguments
#'
#' @returns A summaryDefault object
#' @export
#'
#' @examples
#' jsummary(Sys.Date() - as.Date("2026-01-01"))
jsummary.difftime <- function (object, digits = getOption("digits"), show_quantiles = TRUE, sd = FALSE, ...)
{
  x <- jsummary.default(unclass(object), digits = digits, show_quantiles = show_quantiles, sd = FALSE, ...)
  if (m <- match("NAs", names(x), 0L)) {
    NAs <- as.integer(x[m])
    x <- x[-m]
    attr(x, "NAs") <- NAs
  }
  .difftime(x, attr(object, "units"), c("summaryDefault", oldClass(object)))
}

#' Generic function to produce privacy aware object summaries
#'
#' @param object An object to summarise
#' @param ... Additional arguments
#'
#' @returns Depends on the class of the argument
#' @export
#'
#' @examples
#' jsummary(1:10)
jsummary <- function(object, ...)
{
  UseMethod("jsummary")
}
