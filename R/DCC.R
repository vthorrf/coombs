DCC <- function(PDC, TbTt, Pairs, Rows, Cols) {
  DC <- vector("logical", length=ncol(TbTt)+{length(unlist(Pairs,recursive=F))*4})
  # All 3x3 test results
  for(i in 1:ncol(TbTt)) {
    midpoint1 <- sum(PDC[,TbTt[1,i]] > PDC[,TbTt[5,i]],na.rm=T)
    midpoint2 <- sum(PDC[,TbTt[2,i]] > PDC[,TbTt[4,i]],na.rm=T)
    midpoint3 <- sum(PDC[,TbTt[2,i]] > PDC[,TbTt[6,i]],na.rm=T)
    midpoint4 <- sum(PDC[,TbTt[3,i]] > PDC[,TbTt[5,i]],na.rm=T)
    test0 <- {midpoint1 >= midpoint2} == {midpoint3 >= midpoint4}
    if(!test0) {
      DC[i] <- TRUE
    } else {
      midpoint5 <- sum(PDC[,TbTt[1,i]] > PDC[,TbTt[6,i]],na.rm=T)
      midpoint6 <- sum(PDC[,TbTt[3,i]] > PDC[,TbTt[4,i]],na.rm=T)
      DC[i] <- {midpoint1 >= midpoint2} == {midpoint5 >= midpoint6}
    }
  }
  # All 4x4 test results
  DC[{1+ncol(TbTt)}:length(DC)] <- unlist(lapply(seq_along(Pairs), function(g) {
    tests <- matrix(NA, nrow=length(Pairs[[g]]), ncol=4)
    for(i in seq_along(Pairs[[g]])) {
      # First test
      midpoint1 <- sum(PDC[,Rows[1,Pairs[[g]][i]]] > PDC[,Cols[2,Pairs[[g]][i]]],na.rm=T)
      midpoint2 <- sum(PDC[,Rows[2,Pairs[[g]][i]]] > PDC[,Cols[1,Pairs[[g]][i]]],na.rm=T)
      midpoint3 <- sum(PDC[,Rows[3,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
      midpoint4 <- sum(PDC[,Rows[4,Pairs[[g]][i]]] > PDC[,Cols[3,Pairs[[g]][i]]],na.rm=T)
      test1 <- {midpoint1 >= midpoint2} == {midpoint3 >= midpoint4}
      if(!test1) {
        tests[i,1] <- TRUE
      } else {
        midpoint5 <- sum(PDC[,Rows[1,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
        midpoint6 <- sum(PDC[,Rows[2,Pairs[[g]][i]]] > PDC[,Cols[3,Pairs[[g]][i]]],na.rm=T)
        tests[i,1] <- {midpoint1 >= midpoint2} == {midpoint5 >= midpoint6}
      }
      # Second test
      midpoint1 <- sum(PDC[,Rows[1,Pairs[[g]][i]]] > PDC[,Cols[3,Pairs[[g]][i]]],na.rm=T)
      midpoint3 <- sum(PDC[,Rows[2,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
      test2 <- {midpoint1 >= midpoint2} == {midpoint3 >= midpoint4}
      if(!test2) {
        tests[i,2] <- TRUE
      } else {
        midpoint5 <- sum(PDC[,Rows[1,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
        midpoint6 <- sum(PDC[,Rows[3,Pairs[[g]][i]]] > PDC[,Cols[2,Pairs[[g]][i]]],na.rm=T)
        tests[i,2] <- {midpoint1 >= midpoint2} == {midpoint5 >= midpoint6}
      }
      # Third test
      midpoint3 <- sum(PDC[,Rows[3,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
      test3 <- {midpoint1 >= midpoint2} == {midpoint3 >= midpoint4}
      if(!test3) {
        tests[i,3] <- TRUE
      } else {
        midpoint5 <- sum(PDC[,Rows[1,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
        midpoint6 <- sum(PDC[,Rows[2,Pairs[[g]][i]]] > PDC[,Cols[2,Pairs[[g]][i]]],na.rm=T)
        tests[i,3] <- {midpoint1 >= midpoint2} == {midpoint5 >= midpoint6}
      }
      # First test
      midpoint1 <- sum(PDC[,Rows[1,Pairs[[g]][i]]] > PDC[,Cols[2,Pairs[[g]][i]]],na.rm=T)
      midpoint3 <- sum(PDC[,Rows[2,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
      test4 <- {midpoint1 >= midpoint2} == {midpoint3 >= midpoint4}
      if(!test1) {
        tests[i,4] <- TRUE
      } else {
        midpoint5 <- sum(PDC[,Rows[1,Pairs[[g]][i]]] > PDC[,Cols[4,Pairs[[g]][i]]],na.rm=T)
        midpoint6 <- sum(PDC[,Rows[3,Pairs[[g]][i]]] > PDC[,Cols[3,Pairs[[g]][i]]],na.rm=T)
        tests[i,4] <- {midpoint1 >= midpoint2} == {midpoint5 >= midpoint6}
      }
    }
    return(tests)
  }),recursive=F)
  return(DC)
}
