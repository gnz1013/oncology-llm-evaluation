
# ------------------------------------------------------------
# Read data
# ------------------------------------------------------------
df <- read_excel("main-dat.xlsx", sheet = "combined_ratings")

## ==============================
## Packages
## ==============================
library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(lme4)
library(writexl)

names(df) <- trimws(names(df))
df <- df %>% mutate(Reviewer = as.numeric(as.factor(Reviewer)))

metrics <- c("Accuracy","Relevance","Comprehensive")
models <- unique(df$LLM)

icc_list <- list()

## ==============================
## calculate ICC(2,1) / ICC(2,3)
## ==============================
for (m in models) {
  for (metric in metrics) {
    sub <- df %>%
      filter(LLM == m) %>%
      select(Question, Reviewer, all_of(metric)) %>%
      drop_na()
    
    if (nrow(sub) == 0) next

    wide <- sub %>%
      distinct(Question, Reviewer, .keep_all = TRUE) %>%   
      pivot_wider(names_from = Reviewer, values_from = all_of(metric)) %>%
      select(-Question)
    
    if (ncol(wide) < 2) next
    icc_res <- psych::ICC(as.matrix(wide))
    
    icc2  <- icc_res$results[icc_res$results$type == "ICC2",  c("ICC", "lower bound", "upper bound")]
    icc2k <- icc_res$results[icc_res$results$type == "ICC2k", c("ICC", "lower bound", "upper bound")]

    model <- lmer(as.formula(paste(metric, "~ 1 + (1|Question) + (1|Reviewer)")), 
                  data = sub, REML = TRUE)
    vc <- as.data.frame(VarCorr(model))
    total <- sum(vc$vcov)
    vc_pct <- vc %>% mutate(Percent = round(vcov / total * 100, 1))
    
    q_pct <- vc_pct %>% filter(grp=="Question") %>% pull(Percent)
    r_pct <- vc_pct %>% filter(grp=="Reviewer") %>% pull(Percent)
    e_pct <- vc_pct %>% filter(grp=="Residual") %>% pull(Percent)
    
    icc_list[[length(icc_list)+1]] <- data.frame(
      Model = m,
      Metric = metric,
      ICC_2_1 = round(icc2["ICC"],3),
      CI_2_1  = sprintf("(%.2f–%.2f)",icc2["lower bound"],icc2["upper bound"]),
      ICC_2_3 = round(icc2k["ICC"],3),
      CI_2_3  = sprintf("(%.2f–%.2f)",icc2k["lower bound"],icc2k["upper bound"]),
      Var_Question_pct = q_pct,
      Var_Reviewer_pct = r_pct,
      Var_Residual_pct = e_pct
    )
  }
}

icc_table <- do.call(rbind, icc_list)
write_xlsx(icc_table, "ICC_summary_fixed.xlsx")
print(icc_table)
