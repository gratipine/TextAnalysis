# Latent Dirichlet allocation -------------
# - every document is a mixture of topics
# - every topic is a mixture of words

library(topicmodels)

data("AssociatedPress")
AssociatedPress

# set a seed so that the output of the model is predictable
# Long wait time
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

# Word-topic probabilities
