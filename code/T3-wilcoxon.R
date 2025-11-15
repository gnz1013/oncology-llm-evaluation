library(readxl)
library(dplyr)
library(tidyr)

d <- read_excel("main-dat.xlsx")
colnames(d)

rank_wide <- d %>%
  group_by(Question, LLM) %>%
  summarise(rank = mean(rank), .groups = "drop") %>%  
  pivot_wider(names_from = LLM, values_from = rank)

head(rank_wide)


rank_mat <- as.data.frame(rank_wide[ , -1])

friedman_test <- friedman.test(as.matrix(rank_mat))
friedman_test


models <- colnames(rank_mat)
pairs_mat <- combn(models, 2)
res_list <- vector("list", ncol(pairs_mat))

for (i in seq_len(ncol(pairs_mat))) {
  m1 <- pairs_mat[1, i]
  m2 <- pairs_mat[2, i]
  
  x <- rank_mat[[m1]]
  y <- rank_mat[[m2]]
  
  keep <- !(is.na(x) | is.na(y))
  
  wt <- wilcox.test(x[keep], y[keep],
                    paired = TRUE,
                    exact = FALSE)  
  
  res_list[[i]] <- data.frame(
    Model_1      = m1,
    Model_2      = m2,
    W_stat       = unname(wt$statistic),
    p_raw        = wt$p.value,
    stringsAsFactors = FALSE
  )
}

pairwise_df <- bind_rows(res_list)


pairwise_df$p_adj_Holm <- p.adjust(pairwise_df$p_raw, method = "holm")
pairwise_df$Significant_0.05 <- pairwise_df$p_adj_Holm < 0.05


pairwise_df <- pairwise_df %>%
  arrange(p_adj_Holm) %>%
  mutate(across(c(W_stat, p_raw, p_adj_Holm), ~ round(.x, 4)))

pairwise_df
