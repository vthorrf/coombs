### Auxiliary functions====
rotate <- function(x) t(apply(x, 2, rev))
four_tuples <- function(X) {
  temp0 <- combn(1:ncol(X),4)
  apply(do.call("rbind",strsplit(unlist(lapply(1:nrow(X), function(g) {
    t(sapply(1:ncol(temp0), function(k) {
      paste0(X[g,temp0[,k]],collapse="")
    }))
  })),"")),2,as.numeric)
}
`%!in%` <- function(x,y) !{x %in% y}
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
Rcpp::cppFunction('NumericVector pascalTriangle(int h) {
  NumericVector s(h);
  for (int j = 0; j < h; j++) {
    s[j] = Rf_choose(h - 1, j);
  }
  return s;
}')

### Coombs System Scaling====
CS_ranking <- function(data, ndim=2, type="mspline", circle="column", ...) {
  if(ncol(data) < 6) {
    stop("Oh-oh! It seems like you have less than 6 stimuli. However, you need at least 6 to test the structure :/")
  }
  ### Basic statistics
  # Number of stimuli
  n <- ncol(data)
  # Number of respondents
  N <- nrow(data)
  # Total number of possible response patterns
  TPRP <- factorial(ncol(data))
  # Observed response patterns
  all_ORP <- apply(data, 1, paste0, collapse="")
  ORP <- unique(all_ORP)
  # Preference patterns
  Prefs <- t(sapply(1:ncol(data), function(g) table(factor(data[,g],levels=1:ncol(data)))))
  rownames(Prefs) <- paste0("Pos_",1:ncol(data))
  # Number of observed response patterns
  NORP <- length(ORP)
  # Folding condition number of possible response patterns
  FPRP <- 2 ^ {ncol(data)-1}
  # Single path number of possible response patterns
  SPPRP <- {.5 * ncol(data) * {ncol(data)-1}} + 1
  
  ### Scaling as criterion
  ## All possible response patterns
  all_rp <- gtools::permutations(n, n, repeats.allowed = FALSE) # All PRP
  string_rp <- sort(apply(all_rp, 1, paste0, collapse="")) # String of all PRP
  ## Folding condition
  n_pos_resp <- rep(1:n, times=pascalTriangle(n)) # Number of PRP per alternative
  folding_rp <- vector("list") # Get possible response patterns due to folding condition
  for(i in 1:n) {
    if(i == 1) {
      folding_rp[[i]] <- string_rp[1]
    } else if(i == n) {
      folding_rp[[i]] <- string_rp[length(string_rp)]
    } else if(i == 2){
      temp0 <- all_rp[all_rp[,1] == i,-1]
      temp1 <- string_rp[all_rp[,1] == i]
      temp2 <- temp0
      temp2[temp2 < i] <- NA
      monotonic1 <- apply(temp2, 1, cor, y=c(1:{n-1}), use="pairwise", method="s")
      crit1 <- sort(unique(monotonic1), decreasing=T)[2]
      folding_rp[[i]] <- temp1[monotonic1 > crit1]
    } else if(i == {n-1}) {
      temp0 <- all_rp[all_rp[,1] == i,-1]
      temp1 <- string_rp[all_rp[,1] == i]
      temp3 <- temp0
      temp3[temp3 > i] <- NA
      monotonic2 <- apply(temp3, 1, cor, y=c(1:{n-1}), use="pairwise", method="s")
      crit2 <- sort(unique(monotonic2), decreasing=T)[length(unique(monotonic2))-1]
      folding_rp[[i]] <- temp1[monotonic2 < crit2]
    } else {
      temp0 <- all_rp[all_rp[,1] == i,-1]
      temp1 <- string_rp[all_rp[,1] == i]
      temp2 <- temp3 <- temp0
      temp2[temp2 < i] <- NA
      temp3[temp3 > i] <- NA
      monotonic1 <- apply(temp2, 1, cor, y=c(1:{n-1}), use="pairwise", method="s")
      monotonic2 <- apply(temp3, 1, cor, y=c(1:{n-1}), use="pairwise", method="s")
      crit1 <- sort(unique(monotonic1), decreasing=T)[2]
      crit2 <- sort(unique(monotonic2), decreasing=T)[length(unique(monotonic2))-1]
      folding_rp[[i]] <- temp1[{monotonic1 > crit1} & {monotonic2 < crit2}]
    }
  }
  folding_rp <- unlist(folding_rp, recursive=F)
  FC <- mean(all_ORP %!in% folding_rp) # Proportion of response patterns not on folding condition
  
  ## Fit: Hamming distance from Folding Condition
  HD_RP_FC <- sapply(folding_rp, function(c) sapply(all_ORP, function(r) mapply(adist,r,c)))
  HD_FC <- apply(HD_RP_FC, 1, min) - 1
  HD_FC[HD_FC == -1] <- 0
  NHD_FC <- apply(HD_RP_FC, 1, min)/{n-1}
  names(HD_FC) <- names(NHD_FC) <- rownames(data)
  
  ### Scaling as technique
  ordData <- t(apply(data,1,order))
  colnames(ordData) <- paste0("S",sprintf(paste0("%0",nchar(ncol(data)),"d"),1:ncol(data)))
  ## Locations of the items
  unfolding <- smacof::unfolding(ordData, ndim=ndim, type=type, circle=circle, ...)
  delta_hat <- unfolding$conf.col
  ## Locations of the individuals
  theta_hat <- unfolding$conf.row
  
  ## Single Path
  # Interstimulus Midpoints
  m <- outer(delta_hat[,1], delta_hat[,1], function(r,c) {r + c}/2)
  rownames(m) <- colnames(m) <- 1:ncol(m) 
  MP <- data.frame(row=rownames(m)[row(m)[upper.tri(m)]], 
                   col=colnames(m)[col(m)[upper.tri(m)]], 
                   mid=m[upper.tri(m)])
  # Ordered Midpoints
  direction <- sign(cor(cbind(delta_hat[,1],1:n), method="s")[1,2]) == -1
  ordered_MP <- apply(MP[,c(1,2)],1,paste0,collapse="")[order(MP[,3],decreasing=direction)]
  rev_ordered_MP <- apply(MP[order(MP[,3],decreasing=direction),c(2,1)],2,as.numeric)
  # Dominant path
  Dominant_Path <- matrix(NA,nrow=SPPRP,ncol=n)
  for(i in 1:SPPRP) {
    if(i == 1) {
      Dominant_Path[i,] <- order(delta_hat[,1],decreasing=direction)
    } else {
      temp0 <- rev_ordered_MP[i-1,1]
      temp1 <- rev_ordered_MP[i-1,2]
      Dominant_Path[i,] <- Dominant_Path[i-1,]
      Dominant_Path[i,which(Dominant_Path[i-1,] == temp0)] <- Dominant_Path[i-1,which(Dominant_Path[i-1,] == temp1)]
      Dominant_Path[i,which(Dominant_Path[i-1,] == temp1)] <- Dominant_Path[i-1,which(Dominant_Path[i-1,] == temp0)]
    }
  }
  Dominant_Path_Mat <- Dominant_Path
  Dominant_Path <- apply(Dominant_Path, 1, paste0, collapse="")
  SP <- mean(all_ORP %!in% Dominant_Path) # Proportion of response patterns not on dominant path
  
  ## Fit: Hamming distance from Dominant Single Path
  HD_RP_SP <- sapply(Dominant_Path, function(c) sapply(all_ORP, function(r) mapply(adist,r,c)))
  HD_SP <- apply(HD_RP_SP, 1, min) - 1
  HD_SP[HD_SP == -1] <- 0
  NHD_SP <- apply(HD_RP_SP, 1, min)/{n-1}
  names(HD_SP) <- names(NHD_SP) <- rownames(data)
  
  ## Standard sequence
  SS <- seq(-1,1,len=n)
  PS <- seq(-1,1,len={n-1}*2)
  RPSS <- apply(sapply(PS, function(g) order(abs(g - SS))),2,paste0,collapse="")
  NSS <- mean(all_ORP %!in% RPSS) # Proportion of response patterns not on standard sequence
  
  ## Fit: Hamming distance from Standard Sequence
  HD_RP_SS <- sapply(RPSS, function(c) sapply(all_ORP, function(r) mapply(adist,r,c)))
  HD_SS <- apply(HD_RP_SS, 1, min) - 1
  HD_SS[HD_SS == -1] <- 0
  NHD_SS <- apply(HD_RP_SS, 1, min)/{n-1}
  names(HD_SS) <- names(NHD_SS) <- rownames(data)
  
  ## Double cancellation
  # All 3 X 3 tests
  TbTt <- combn(1:ncol(data), 6)
  # All 4 X 4 tests
  Base <- combn(1:ncol(data), 4)
  Rows <- as.matrix(Base[,apply(Base[c(3,4),],2,paste0,collapse="") %in%
                          apply(Base[c(1,2),],2,paste0,collapse="")])
  Cols <- as.matrix(Base[,apply(Base[c(1,2),],2,paste0,collapse="") %in%
                          apply(Base[c(3,4),],2,paste0,collapse="")])
  Pairs <- lapply(1:ncol(Rows), function(g) {
    which(paste0(Rows[c(3,4),g],collapse="") == apply(as.matrix(Cols[c(1,2),]),2,paste0,collapse=""))
  })
  DC <- 1 - rowMeans(t(sapply(1:nrow(data), function(g) {
    PDC <- t(apply(four_tuples(data[g,]),1,match,x=1:ncol(data)))
    DCC(PDC,TbTt=TbTt, Pairs=Pairs, Rows=Rows, Cols=Cols)
  })))
  
  ### Final results
  Results <- list(ObservedRP=data.frame(ORP,FC=ORP %in% folding_rp,
                                        RP=ORP %in% Dominant_Path), # Unique Observed response patterns
                  PF=folding_rp, # The possible foldings on the latent dimension
                  DP=Dominant_Path, # Dominant path
                  SS=RPSS, # The response patterns of the possible standard sequence
                  RP_NTotal=TPRP, # Total possible response patterns
                  RP_NFolding=FPRP, # Number of possible response patterns if folding conditions holds
                  RP_NSingle=SPPRP, # Number of possible response patterns if single path holds
                  RP_NStandard={n-1}*2, # Number of possible response patterns if standard sequence
                  RP_NObs=NORP, # Number of observed response patterns
                  FC=FC, # Percentage of observed response patterns not following folding condition
                  SP=SP, # Percentage of observed response patterns not following single path
                  NS=NSS, # Percentage of observed response patterns not following standard sequence
                  HD_FC=HD_FC, # The Hamming distance for each respondent according to the possible
                               # response patterns if the folding condition is true
                  HD_SP=HD_SP, # The Hamming distance for each respondent according to the possible
                               # response patterns if the single path condition is true
                  HD_SS=HD_SS, # The Hamming distance for each respondent according to the possible
                               # response patterns if the latent position is a standard sequence
                  NHD_FC=NHD_FC, # The normalized Hamming distance for each respondent according to
                                 # the possible response patterns if the folding condition is true
                  NHD_SP=NHD_SP, # The normalized Hamming distance for each respondent according to
                                 # the possible response patterns if the single path condition is true
                  NHD_SS=NHD_SS, # The normalized Hamming distance for each respondent according to the
                                 # possible response patterns if the latent position is a standard sequence
                  DC=DC, # Percentage of responses not following double cancellation
                  PP=t(Prefs), # Preference patterns
                  MDU=unfolding, # MUD complete results
                  delta_hat=delta_hat, # Latent locations of the items
                  theta_hat=theta_hat) # Latent locations of the individuals
  class(Results) <- "CS_ranking"
  return(Results)
}
print.CS_ranking <- function(x, ...) {
  cat("Number of Total Unique Possible Response Patterns:", x$RP_NTotal,"\n")
  cat("Number of Unique Possible Response Patterns with Folding Condition:", x$RP_NFolding,"\n")
  cat("Number of Unique Possible Response Patterns with Single Path Condition:", x$RP_NSingle,"\n")
  cat("Number of Unique Possible Response Patterns with Standard Sequence:", x$RP_NStandard,"\n")
  cat("Number of Unique Observed Response Patterns:", x$RP_NObs,"\n")
  cat("Proportion of Observed Response Patterns Not Following Folding Condition:", round(x$FC,3),"\n")
  cat("Proportion of Observed Response Patterns Not Following Single Path:", round(x$SP,3),"\n")
  cat("Proportion of Observed Response Patterns Not Following Standard Sequence:", round(x$NS,3),"\n")
  cat("Average Hamming Distance on Folding Condition:", round(mean(x$HD_FC),3),"\n")
  cat("Average Hamming Distance on Single Path:", round(mean(x$HD_SP),3),"\n")
  cat("Average Hamming Distance on Standard Sequence:", round(mean(x$HD_SS),3),"\n")
  cat("Average normalized Hamming Distance on Folding Condition:", round(mean(x$NHD_FC),3),"\n")
  cat("Average normalized Hamming Distance on Single Path:", round(mean(x$NHD_SP),3),"\n")
  cat("Average normalized Hamming Distance on Standard Sequence:", round(mean(x$NHD_SS),3),"\n")
  cat("Proportion of violations of the double cancellation axiom:", round(mean(x$DC),3),"\n")
}
plot.CS_ranking <- function(x, ...) {
  dens <- density(x$theta_hat[,1])
  plot(dens, xlim=c(-1,1), main="Distribution of scores in the main dimension",
       xlab="Latent positions (vertical lines are the positions of the items)")
  abline(v=x$delta_hat[,1], lty=2, col="dark gray")
}
