four_tuples <- function(X) {
  temp0 <- combn(1:ncol(X),4)
  apply(do.call("rbind",strsplit(unlist(lapply(1:nrow(X), function(g) {
    t(sapply(1:ncol(temp0), function(k) {
      paste0(X[g,temp0[,k]],collapse="")
    }))
  })),"")),2,as.numeric)
}