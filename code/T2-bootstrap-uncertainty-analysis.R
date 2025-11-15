

# ------------------------------------------------------------
# Load packages
# ------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(boot)
library(reshape2)
library(DescTools)  

# ------------------------------------------------------------
# Read data
# ------------------------------------------------------------
df <- read_excel("main-dat.xlsx", sheet = "combined_ratings")
# ------------------------------------------------------------
# Helper: cluster-bootstrap 95% CI over questions
# ------------------------------------------------------------
bootstrap_ci_over_questions <- function(df_metric, metric_col, B = 5000, ci = 0.95, seed = 42) {
  set.seed(seed)
  
  # Step 1 : average over reviewers within each Question × LLM
  q_agg <- df_metric %>%
    group_by(Question, LLM) %>%
    summarise(val = mean(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
  
  questions <- unique(q_agg$Question)
  llms <- unique(q_agg$LLM)
  nQ <- length(questions)
  
  observed <- q_agg %>% group_by(LLM) %>%
    summarise(Mean = mean(val), .groups = "drop")
  
  # Step 2 : bootstrap resampling at question level
  boot_stats <- replicate(B, {
    sampled_q <- sample(questions, nQ, replace = TRUE)
    res <- q_agg %>% filter(Question %in% sampled_q)
    # Weight by frequency of sampling
    w <- table(sampled_q)
    res <- res %>% mutate(w = as.numeric(w[as.character(Question)]))
    res %>% group_by(LLM) %>%
      summarise(wmean = weighted.mean(val, w), .groups = "drop") %>%
      pull(wmean)
  })
  boot_stats <- t(boot_stats)
  colnames(boot_stats) <- llms
  
  alpha <- (1 - ci) / 2
  ci_table <- apply(boot_stats, 2, function(x)
    quantile(x, probs = c(alpha, 1 - alpha), na.rm = TRUE))
  ci_table <- as.data.frame(t(ci_table))
  names(ci_table) <- c("Lower 95% CI", "Upper 95% CI")
  
  out <- observed %>%
    left_join(ci_table %>% tibble::rownames_to_column("LLM"), by = "LLM") %>%
    arrange(desc(Mean))
  return(out)
}

# ------------------------------------------------------------
# Compute bootstrap CIs for all metrics
# ------------------------------------------------------------
acc_ci  <- bootstrap_ci_over_questions(df, "Accuracy")
rel_ci  <- bootstrap_ci_over_questions(df, "Relevance")
comp_ci <- bootstrap_ci_over_questions(df, "Comprehensive")
hall_ci <- bootstrap_ci_over_questions(df, "Hallucination")
rdy_ci  <- bootstrap_ci_over_questions(df, "Ready to use")

print(acc_ci)
print(rel_ci)
print(comp_ci)
print(hall_ci)
print(rdy_ci)

# ------------------------------------------------------------
# Friedman tests (paired across Question × Reviewer)
# ------------------------------------------------------------
pivot_metric <- function(metric) {
  df %>%
    select(Question, Reviewer, LLM, !!sym(metric)) %>%
    pivot_wider(names_from = LLM, values_from = !!sym(metric))
}

acc_wide  <- pivot_metric("Accuracy")
rel_wide  <- pivot_metric("Relevance")
comp_wide <- pivot_metric("Comprehensive")

friedman_acc  <- friedman.test(as.matrix(acc_wide[,-c(1,2)]))
friedman_rel  <- friedman.test(as.matrix(rel_wide[,-c(1,2)]))
friedman_comp <- friedman.test(as.matrix(comp_wide[,-c(1,2)]))

cat("\nFriedman Accuracy:\n");  print(friedman_acc)
cat("\nFriedman Relevance:\n"); print(friedman_rel)
cat("\nFriedman Comprehensiveness:\n"); print(friedman_comp)

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Accuracy_CI");          writeData(wb, "Accuracy_CI", acc_ci)
addWorksheet(wb, "Relevance_CI");         writeData(wb, "Relevance_CI", rel_ci)
addWorksheet(wb, "Comprehensive_CI");     writeData(wb, "Comprehensive_CI", comp_ci)
addWorksheet(wb, "Hallucination_CI");     writeData(wb, "Hallucination_CI", hall_ci)
addWorksheet(wb, "ReadyToUse_CI");        writeData(wb, "ReadyToUse_CI", rdy_ci)
addWorksheet(wb, "Friedman_Tests")
writeData(wb, "Friedman_Tests",
          data.frame(Metric=c("Accuracy","Relevance","Comprehensiveness"),
                     Chi.square=c(friedman_acc$statistic,
                                  friedman_rel$statistic,
                                  friedman_comp$statistic),
                     df=c(4,4,4),
                     p.value=c(friedman_acc$p.value,
                               friedman_rel$p.value,
                               friedman_comp$p.value)))
saveWorkbook(wb, "llm_bootstrap_results_R.xlsx", overwrite = TRUE)
