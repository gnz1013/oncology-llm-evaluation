
library(dplyr)
library(readxl)


df <- read_excel("main-dat.xlsx", sheet = "combined_ratings")

calc_disagreement <- function(data, var){
  data %>%
    group_by(LLM, Question) %>%
    summarise(disagree = length(unique(.data[[var]])) > 1, .groups = "drop") %>%
    group_by(LLM) %>%
    summarise(
      Disagreement_Rate = mean(disagree),
      Agreement_Rate = 1 - Disagreement_Rate,
      .groups = "drop"
    ) %>%
    mutate(Dimension = var)
}

res_hall <- calc_disagreement(df, "Hallucination")
res_ready <- calc_disagreement(df, "Ready to use")


agreement_df <- bind_rows(res_hall, res_ready)

agreement_df %>%
  mutate(
    Agreement_Rate = round(Agreement_Rate, 3),
    Disagreement_Rate = round(Disagreement_Rate, 3)
  ) %>%
  select(Dimension, LLM, Agreement_Rate, Disagreement_Rate)
