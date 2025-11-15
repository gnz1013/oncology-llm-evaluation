library(readxl)
library(dplyr)
library(tidyr)
library(irr)
library(irrCAC)

dat <- read_xlsx("main-dat.xlsx")

hall_mat <- dat %>%
  select(Question, LLM, Reviewer, Hallucination) %>%
  mutate(Reviewer = as.factor(Reviewer)) %>%
  pivot_wider(
    id_cols   = c(Question, LLM),
    names_from  = Reviewer,
    values_from = Hallucination
  ) %>%
  select(-Question, -LLM) %>%
  as.matrix()

ready_mat <- dat %>%
  select(Question, LLM, Reviewer, `Ready to use`) %>%
  mutate(Reviewer = as.factor(Reviewer)) %>%
  pivot_wider(
    id_cols   = c(Question, LLM),
    names_from  = Reviewer,
    values_from = `Ready to use`
  ) %>%
  select(-Question, -LLM) %>%
  as.matrix()

ac1_hall  <- gwet.ac1.raw(hall_mat)
ac1_ready <- gwet.ac1.raw(ready_mat)

ac1_hall$est   # Gwet's AC1 for hallucination
ac1_ready$est  # Gwet's AC1 for ready-to-use


set.seed(123)
B <- 5000  
ac1_fun <- function(mat) {
  est_tab <- irrCAC::gwet.ac1.raw(mat)$est  
  est_tab$coeff.val[1]                      
}

set.seed(123)
ac1_boot_hall <- replicate(B, {
  idx <- sample(seq_len(nrow(hall_mat)), replace = TRUE)
  ac1_fun(hall_mat[idx, , drop = FALSE])
})

ac1_boot_ready <- replicate(B, {
  idx <- sample(seq_len(nrow(ready_mat)), replace = TRUE)
  ac1_fun(ready_mat[idx, , drop = FALSE])
})

ac1_hall$est
ac1_ready$est



