enlist = function(...) {
  result = list(...)
  if ((nargs() == 1) & is.character(n <- result[[1]])) {
    result = as.list(seq(n))
    names(result) = n
    for (i in n) result[[i]] = get(i)
  }
  else {
    n = sys.call()
    n = as.character(n)[-1]
    if (!is.null(n2 <- names(result))) {
      which = n2 != ""
      n[which] = n2[which]
    }
    names(result) = n
  }
  return(result)
}

# Setup function (see setup_xyd in quantgen package).
setup_xy <- function(x,y, weights = NULL){
  n = nrow(x)
  p = ncol(x)
  
  good_inds = rowSums(is.na(x)) == 0 & 
    !is.na(y) & 
    rowSums(is.infinite(x)) == 0 &
    !is.infinite(y)
  
  x = x[good_inds,]
  y = y[good_inds]
  n = sum(good_inds)
  
  # Configure weights
  if (is.null(weights)) weights = rep(1,n)
  else weights = weights[good_inds]
  
  return(enlist(x, y, weights))
}