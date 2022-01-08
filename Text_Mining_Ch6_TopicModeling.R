### Text Mining with R: A Tidy Approach
## Chapter 6: Topic Modeling

library(topicmodels)

data("AssociatedPress")

AssociatedPress

#We can use the LDA() function from the topicmodels package, setting k = 2, to create a two-topic LDA model

# Set a seed so that the output of the model is predictable. k set the number of topics.

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

ap_lda

# 6.1.1 Word-topic probabilities

# 'beta': per-topic-per-word probabilities

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_topics

# Can use dplyr's slice_max() to find the 10 terms that are most common within each topic
# Can then use ggplot2, since it will be a tidy data frame

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# graphical representation of the terms most common to each topic
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Consider the terms with the greatest difference in beta
library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))


slice_max(beta_wide, order_by = log_ratio, n = 10) 
slice_max(beta_wide, order_by = -log_ratio, n = 10) 

## 6.1.2 Document-topic probabilities


  
