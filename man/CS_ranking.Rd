\name{CS_ranking}
\alias{CS_ranking}
\title{Coombs scaling of Rank Order Data}
\description{
This function performs both scaling as technique and as criterion of rank order data. The reference for the scaling as criterion is Michell (1990). For the scaling as technique, check De Leeuw and Mair (2009). For doing scaling as criterion it is necessary that there at least six stimuli. Therefore, if you have rank order data, but with less than six stimuli, this code will not run.
}
\usage{
CS_ranking(data, ndim=2, type="mspline", circle="column", ...)
}
\arguments{
   \item{data}{A data frame or matrix with the rank order data. The rows are the participants. The columns should represent the ordering of the stimuli; e.g., the first column represents the most liked/more representative/best/whatnot stimulus. The second column is the second most and so forth. The stimuli should be numerically coded, with "1" representing the stimulus that should be the "left-most" in the latent dimension. And, e.g., if there are six stimuli, the "right-most" item in the latent dimension should be coded as "6". This means that there should be a priori expectation of the latent ordering of the stimuli.}
   \item{ndim}{Number of dimensions of latent dimensions. Defaults to 2.}
   \item{type}{The type of multidimensional scaling. It can be: "interval"; "ratio"; "ordinal"; or "mspline". Because the operations within CS_ranking assumes the data to be ordinal, we suggest the "ordinal" or "mspline" should be used, as these are the types adequate for ordinal data. Defaults to "mspline".}
   \item{circle}{An argument specifying what type of configural restriction should (if any) be applied to the scaling as technique solution. Defaults to "column", which applies a restriction in regards to the stimuli. We strongly advice that the default should always be used. The other possible values are "row" and "none".}
   \item{...}{Further arguments to be passed to the `unfolding` function of the smacof package, used internally for performing scaling as a technique.}
}
\value{
A large list with the following objects:
* ObservedRP: Unique Observed response patterns;
* PF: The possible foldings on the latent dimension;
* DP: The dominant path;
* SS: The response patterns of the possible standard sequence;
* RP_NTotal: Total possible response patterns;
* RP_NFolding: Number of possible response patterns if folding conditions holds;
* RP_NSingle: Number of possible response patterns if single path holds;
* RP_NStandard: Number of possible response patterns if standard sequence;
* RP_NObs: Number of observed response patterns;
* FC: Percentage of observed response patterns not following folding condition;
* SP: Percentage of observed response patterns not following single path;
* NS: Percentage of observed response patterns not included in the standard sequence;
* HD_FC: The Hamming distance for each respondent according to the possible response patterns if the folding condition is true;
* HD_SP: The Hamming distance for each respondent according to the possible response patterns if the single path condition is true;
* HD_SS: The Hamming distance for each respondent according to the possible response patterns if the latent position is a standard sequence;
* NHD_FC: The normalized Hamming distance for each respondent according to the possible response patterns if the folding condition is true;
* NHD_SP: The normalized Hamming distance for each respondent according to the possible response patterns if the single path condition is true;
* NHD_SS: The normalized Hamming distance for each respondent according to the possible response patterns if the latent position is a standard sequence;
* DC: Percentage of responses not following double cancellation;
* PP: Preference patterns;
* MDU: Multidimensional unfolding model estimated with scaling as technique complete results;
* delta_hat: Latent locations of the items;
* theta_hat: Latent locations of the individuals.
}
\references{
De Leeuw, J., & Mair, P. (2009). Multidimensional scaling using majorization: SMACOF in R. Journal of statistical software, 31, 1-30.
Michell, J. (1990). An introduction to the logic of psychological measurement. Psychology Press.
}
\examples{##### Multiple Linear Regression with 5 Predictors
### Packages and functions====
require(coombs)

### Data====
## Complete responses
data("CoombsGPT")
## Only the response patterns of the ranking order
RP <- CoombsGPT[,grep("P_",colnames(CoombsGPT))]
head(RP)

### Load the package and run the analysis====
## Run the analysis
res <- CS_ranking(RP)
## Short summary
res
## Complete summary
summary(res)

### Plots====
## Bidimensional Distribution of the Items and Individuals
plot(res$delta_hat, ylim=c(-1,1), xlim=c(-1,1),
     main="Bidimensional Distribution of the Items and Individuals")
abline(v=0, lty=2)
abline(h=0, lty=2)
points(res$theta_hat, col="gray")
## Distribution of scores in the main dimension
plot(res)
}
