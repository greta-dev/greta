# nolint start
#' @name overloaded
#'
#' @title Functions overloaded by greta
#'
#' @description greta provides a wide range of methods to apply common R
#'   functions and operations to `greta_array` objects. A few of these
#'   functions and operators are not associated with a class system, so they are
#'   overloaded here. This should not affect normal use of these functions, but
#'   they need to be documented to satisfy CRAN's check.
#'
#' @param
#'   x,y,size,LINPACK,V,na.rm,dims,MARGIN,STATS,FUN,check.margin,\dots,r,k,upper.tri,transpose,l,X,INDEX,symmetric,only.values,EISPACK,x1,x2,compact,along,rev.along,new.names,force.array,make.names,use.anon.names,use.first.dimnames,hier.names,use.dnns,nrow,ncol
#'    arguments as in original documentation
#'
#' @details
#'   Note that, as in R, the LINPACK argument is defunct and silently ignored.
#'   The argument is only included for compatibility with the base functions
#'   that call it.
#'
NULL
# nolint end
