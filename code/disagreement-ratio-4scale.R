library(readxl)
library(dplyr)


dat <- read_excel("main-dat.xlsx")

library(dplyr)
calc_severe_disagreement <- function(data, score_col) {
  data %>%
    group_by(LLM, Question) %>%
    summarise(
      max_diff = max(.data[[score_col]], na.rm = TRUE) -
        min(.data[[score_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      severe_disagreement = max_diff >= 2
    ) %>%
    group_by(LLM) %>%
    summarise(
      metric               = score_col,
      n_questions          = n(),                          
      n_severe_disagree    = sum(severe_disagreement),
      prop_severe_disagree = n_severe_disagree / n_questions,
      .groups = "drop"
    )
}

dis_acc  <- calc_severe_disagreement(dat, "Accuracy")
dis_rel  <- calc_severe_disagreement(dat, "Relevance")
dis_comp <- calc_severe_disagreement(dat, "Comprehensive")


dis_all <- bind_rows(dis_acc, dis_rel, dis_comp)
dis_all

