GPTbinarytree <- function(objbel=NULL, kercon=NULL, n.items=6, OPENAI_API_KEY=NULL, lang="English") {
  ### Arguments
  # objbel: The object of belief (i.e., the subject)
  # kercon: The kernel concept (i.e., the predicate)
  # n.items: The number of items to be generated. Defaults to 6.
  # OPENAI_API_KEY: Your ChatGPT API key
  # lang: The language in which the items must be written. Defaults to "English"
  if(is.null(objbel)) stop("You must provide an object of belief!")
  if(is.null(kercon)) stop("You must provide a kernel concept!")
  if(is.null(OPENAI_API_KEY)) stop("Please provide your ChatGPT API key.")
  
  ### Setup your ChatGPT API key in R.
  ## First you will need to obtain your ChatGPT API key. You can create an API key by accessing
  ## OpenAI API page (https://platform.openai.com/account/api-keys). Don’t miss their article about
  ## Best Practices for API Key Safety: https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety
  ## Then you have to assign your API key for usage in R, this can be done just for the actual
  ## session, by doing:
  Sys.setenv(OPENAI_API_KEY = OPENAI_API_KEY)
  
  ### Generate the prompt
  prompt <- paste0("Hi Assistant! A binary tree structure is a procedure used",
                   " to create psychometric items that are ordered in a single latent dimension.",
                   " The binary tree structure works like this:\n",
                   "Define that the items are a set of statements of the form \"X is Y\", ",
                   "where \"X\" is the object of belief, and \"Y\" is a predicate that ",
                   "expresses a kernel concept;\n",
                   "Because the set of statements is unidimensional, the statements will vary only ",
                   "in respect to their predicates;\n",
                   "Then, the statements are created in an iterative manner, with the ",
                   "initial statement being binarily divided into predicates which are ",
                   "(a) fairly general, (b) mutually contrary, and (c) ordered relative ",
                   "to the kernel concept;\n",
                   "Each predicate may then be subjected to further binary divisions ",
                   "according to the same principle, with the additional requirement that ",
                   "any predicate within the tree must be logically included within the ",
                   "semantic scope of its immediately preceding predicate. ",
                   "This procedure is stopped when the desired number of items is achieved;\n",
                   "The first and the last items should contain hyperboles to assure that ",
                   "they represent extremes in the kernel concept.\n",
                   "Using the binary tree structure, please provide me with ", n.items,
                   " statements in ", lang, " where the object of belief is ",
                   objbel, " and the kernel concept is ", kercon,". Use the structure \"X is Y\", where X is the object of belief.",
                   " Please return the statements ordered according to the kernel concept.")
  response <- invisible(ask_chatgpt(prompt))
  Results <- list("Full"=response,
                  "items"=strsplit(response,"\n")[[1]][grep("^[[:digit:]]+",strsplit(response,"\n")[[1]])])
  return(Results)
}