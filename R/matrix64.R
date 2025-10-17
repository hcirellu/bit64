#' Working with integer64 arrays and matrices
#'
#' These functions and methods facilitate working with integer64
#'   objects stored in matrices. As ever, the primary motivation
#'   for having tailor-made functions here is that R's methods
#'   often receive input from bit64 and treat the vectors as doubles,
#'   leading to unexpected and/or incorrect results.
#'
#' As of now, the `colSums()` and `rowSums()` methods are implemented
#'   as wrappers around equivalent `apply()` approaches, because
#'   re-using the default routine (and then applying integer64 to the
#'   result) does not work for objects with missing elements. Ideally
#'   this would eventually get its own dedicated C routine mimicking
#'   that of `colSums()` for integers; feature requests and PRs welcome.
#'
#' `aperm()` is required for `apply()` to work, in general, otherwise
#'    `FUN` gets applied to a class-stripped version of the input.
#'
#' @param x An array of integer64 numbers.
#' @param na.rm,dims Same interpretation as in [colSums()].
#' @param ... Passed on to subsequent methods.
#' @examples
#' A = as.integer64(1:6)
#' dim(A) = 3:2
#'
#' colSums(A)
#' rowSums(A)
#' aperm(A, 2:1)
#' @name matrix64
NULL


#' @rdname matrix64
#' @export
matrix = function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL) UseMethod("matrix")
#' @s3method matrix default
matrix.default = base::matrix

#' @s3method matrix integer64
matrix.integer64 = function(data=NA_integer64_, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL) {
  # input validation
  nrowVal = as.integer(nrow)[1L]
  ncolVal = as.integer(ncol)[1L]
  if (!is.finite(nrowVal) || nrowVal <= 0L) stop("invalid 'nrow' value")
  if (!is.finite(ncolVal) || ncolVal <= 0L) stop("invalid 'ncol' value")
  if (!length(data)) data = NA_integer64_
  
  # determine nrow or ncol if one or both are missing
  if (missing(nrow) && missing(ncol)) {
    nrowVal = length(data)
  } else if (missing(nrow)) {
    nrowVal = ceiling(length(data) / ncolVal)
  } else if (missing(ncol)) {
    ncolVal = ceiling(length(data) / nrowVal)
  }

  # shorten or extend to correct size
  if (length(data) %% nrowVal != 0L && nrowVal %% length(data) != 0L) {
    warning("data length [", length(data), "] is not a sub-multiple or multiple of the number of rows [", nrowVal, "]")
  } else if (length(data) %% ncolVal != 0L && ncolVal %% length(data) != 0L) {
    warning("data length [", length(data), "] is not a sub-multiple or multiple of the number of columns [", ncolVal, "]")
  }
  data = rep_len(data, nrowVal * ncolVal)
  
  if (isTRUE(byrow)) {
    dim(data) = c(ncolVal, nrowVal)
    ret = t(data)
  } else {
    dim(data) = c(nrowVal, ncolVal)
    ret = data
  }
  if (!is.null(dimnames)) dimnames(ret) = dimnames
  ret
}


#' @export
array = function(data=NA, dim=length(data), dimnames=NULL) UseMethod("array")
#' @s3method array default
array.default = base::array

#' @s3method array integer64
array.integer64 = function(data=NA_integer64_, dim=length(data), dimnames=NULL) {
  # input validation
  dim = as.integer(dim)
  if (!length(data)) data = NA_integer64_

  if (!length(dim)) 
      stop("'dim' cannot be of length 0")
  vl = prod(dim)
  if (vl < 0)
    stop("negative length vectors are not allowed")
  if (length(data) != vl) {
      data = rep_len(data, vl)
  }
  if (length(dim)) 
      dim(data) = dim
  if (is.list(dimnames) && length(dimnames)) 
      dimnames(data) = dimnames
  data
}


#' @rdname matrix64
#' @export
colSums = function(x, na.rm=FALSE, dims=1L) UseMethod("colSums")
#' @rdname matrix64
#' @export
colSums.default = function(x, na.rm=FALSE, dims=1L) base::colSums(x, na.rm, dims)

#' @rdname matrix64
#' @export
colSums.integer64 = function(x, na.rm=FALSE, dims=1L) {
  n_dim = length(dim(x))
  stopifnot(
    `dims= should be a length-1 integer between 1 and length(dim(x))-1L` =
      length(dims) == 1L && dims > 0L && dims < n_dim
  )
  MARGIN = tail(seq_len(n_dim), -dims)
  ret = apply(x, MARGIN, sum, na.rm = na.rm)
  class(ret) = "integer64"
  ret
}


#' @rdname matrix64
#' @export
rowSums = function(x, na.rm=FALSE, dims=1L) UseMethod("rowSums")
#' @rdname matrix64
#' @export
rowSums.default = function(x, na.rm=FALSE, dims=1L) base::rowSums(x, na.rm, dims)

#' @rdname matrix64
#' @export
rowSums.integer64 = function(x, na.rm=FALSE, dims=1L) {
  n_dim = length(dim(x))
  stopifnot(
    `dims= should be a length-1 integer between 1 and length(dim(x))-1L` =
      length(dims) == 1L && dims > 0L && dims < n_dim
  )
  MARGIN = seq_len(dims)
  ret = apply(x, MARGIN, sum, na.rm = na.rm)
  class(ret) = "integer64"
  ret
}


#' @rdname matrix64
#' @param a,perm Passed on to [aperm()].
#' @export
aperm.integer64 = function(a, perm, ...) {
  class(a) = minusclass(class(a), "integer64")
  ret = aperm(a, perm, ...)
  class(ret) = plusclass(class(a), "integer64")
  ret
}
