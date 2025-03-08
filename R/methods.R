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
summary.CS_ranking <- function(object, ...) {
  cat("===== Scaling as a criterion:\n")
  cat("Proportion of Observed Response Patterns Not Following Folding Condition:", round(object$FC,3),"\n")
  cat("Proportion of Observed Response Patterns Not Following Single Path:", round(object$SP,3),"\n")
  cat("Proportion of Observed Response Patterns Not Following Standard Sequence:", round(object$NS,3),"\n")
  cat("Average Hamming Distance on Folding Condition:", round(mean(object$HD_FC),3),"\n")
  cat("Average Hamming Distance on Single Path:", round(mean(object$HD_SP),3),"\n")
  cat("Average Hamming Distance on Standard Sequence:", round(mean(object$HD_SS),3),"\n")
  cat("Proportion of violations of the double cancellation axiom:", round(mean(object$DC),3),"\n\n")
  cat("===== Scaling as a technique:\n")
  print(object$MDU)
  cat("Latent positions of stimuli on the underlying dimension:","\n")
  print(object$delta_hat)
}
plot.CS_ranking <- function(x, ...) {
  dens <- density(x$theta_hat[,1])
  plot(dens, xlim=c(-1,1), main="Distribution of scores in the main dimension",
       xlab="Latent positions (vertical lines are the positions of the items)", ...)
  abline(v=x$delta_hat[,1], lty=2, col="dark gray")
}
`%!in%` <- function(x,y) !{x %in% y}