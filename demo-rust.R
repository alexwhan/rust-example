library(tidyverse)
vals <- c(1:9, ";", "+")
wt <- c(rep(1, 9), 0.1, 0.1)
list_vals <- function(n) {
  purrr::map_chr(vector("character", n),
                        ~paste0(sample(vals, 
                                       size = sample(1:3, 1, prob = c(1, 0.25, 0.05)),
                                       prob = wt),
                                collapse = ","))}
demo <- tibble(id = 1:100) %>% 
  mutate(rep1 = list_vals(100),
         rep2 = list_vals(100),
         rep3 = list_vals(100))

#rules:
# ; should 0
# + should be ignored


# first get reps gathered
longer1 <- demo %>% 
  pivot_longer(2:4,
               names_to = "rep",
               values_to = "value") %>% 
  mutate(rep = str_replace(rep, "rep", ""))

#separate multiple scores
# (not always necessary)

#find max number of scores
max(str_count(longer1$value, ",")) #max 2 commas, three scores
sep1 <- longer1 %>% 
  separate(value, c("score1", "score2", "score3"), sep = ",")

#now gather multiple scores
longer2 <- sep1 %>% 
  pivot_longer(3:5,
               names_to = "score_num",
               values_to = "score") %>% 
  mutate(score_num = str_replace(score_num, "score", "")) %>% 
  filter(!is.na(score))

#now can apply the conversion rules
pheno <- longer2 %>% 
  filter(score != "+") %>% 
  mutate(score = as.numeric(case_when(
    score == ";" ~ "0",
    TRUE ~ score
  )))