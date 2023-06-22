coombs: Methods for fitting and assessing unfolding-based models
=============

The unfolding theory is a psychometric theory based on psychological distances. This theory assumes that individuals rank a set of objects or stimuli based on some underlying psychological dimension. However, the rankings obtained may not directly reflect the underlying dimension due to response biases or individual differences. The Coombs unfolding theory aims to uncover and model these underlying dimensions based on the observed rankings.

There are two general approaches for the psychometric scaling of unfolding models. The approach of scaling as a technique is the most common and it is implemented in other packages (e.g., smacof). This approach envolves estimating latent positions for items and individuals in a pre-established number of latent dimensions. The approach of scaling as a criterion, on the other hand, is (to our knowledge) not implemented in other packages. This approach is centered on testing the assumptions of the quantitative properties of the latent dimensions of the models from the scaling as a technique approach.

There is also a function called `GPTbinarytree` that can be used to access ChatGPT and automatically generate items with it, using a function from the chatgpt package. These items are created using the binary tree structure, I procedure that guarantees that the items are ordered in the relevant dimension of assessment. Before you can use this function, you will need to obtain your ChatGPT API key. You can create an API key by accessing OpenAI API page (https://platform.openai.com/account/api-keys). More details can be found on OpenAI's article about best practices for API key safety (https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety) and on the documentation of the chatgpt package.

# Installation #

Using the 'remotes' package:

    install.packages("remotes")
    remotes::install_github("vthorrf/coombs")
