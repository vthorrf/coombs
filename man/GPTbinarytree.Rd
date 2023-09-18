\name{GPTbinarytree}
\alias{GPTbinarytree}
\title{AIG of binary tree structures with ChatGPT}
\description{
This function is used to automatically create items with a binary tree structure (Michell, 1994) using ChatGPT. The items are of the structure "X is Y" where X is the object of belief and Y are predicates based on the kernell concept. For more details see Franco and Carvalho (2023).
}
\usage{
GPTbinarytree(objbel=NULL, kercon=NULL, n.items=6, OPENAI_API_KEY=NULL, lang="English")
}
\arguments{
   \item{objbel}{The object of belief (i.e., the subject).}
   \item{kercon}{The kernel concept (i.e., the predicate).}
   \item{n.items}{The number of items to be generated. Defaults to 6.}
   \item{OPENAI_API_KEY}{Your ChatGPT API key.}
   \item{lang}{The language in which the items must be written. Defaults to "English".}
}
\details{
To use this function one must setup their ChatGPT API key in R. For doing that, first you will need to obtain your ChatGPT API key. You can create an API key by accessing OpenAI API page (https://platform.openai.com/account/api-keys). Don’t miss their article about Best Practices for API Key Safety: https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety
}
\value{
A vector with the generated items.
}
\references{
Franco, V. R., & de Francisco Carvalho, L. (2023). A Tutorial on How to Use ChatGPT to Generate Items Following a Binary Tree Structure. https://doi.org/10.31234/osf.io/5hnkz
Michell, J. (1994). Measuring dimensions of belief by unidimensional unfolding. Journal of Mathematical Psychology, 38(2), 244-273.
}
\examples{##### Multiple Linear Regression with 5 Predictors
### Packages and functions====
require(coombs)

### Create items====
## Create 8 items in Englishe where the object of belief is "mathematics" and the
## kernell concept is "relevance to my education"
OPENAI_API_KEY = "Insert your OPENAI_API_KEY as a string in here"
items <- GPTbinarytree(objbel="mathematics", kercon="relevance to my education", n.items=8,
                       OPENAI_API_KEY=OPENAI_API_KEY, lang="English")

}
