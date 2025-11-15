library(readxl)
library(dplyr)
library(tidyr)

df <- read_excel("main-dat.xlsx", sheet = "combined_ratings")

df$Question <- as.integer(df$Question)

domain_map <- c(
  "1"="DA","2"="C2","3"="DA","4"="C1","5"="B1","6"="C1","7"="C2","8"="D1","9"="C1","10"="B2",
  "11"="D3","12"="D3","13"="C2","14"="B2","15"="C1","16"="B2","17"="B1","18"="NT/SP","19"="D2",
  "20"="NT/SP","21"="B2","22"="D2","23"="D2","24"="B2","25"="B1","26"="C2","27"="C2","28"="C1",
  "29"="C2","30"="C2","31"="C2","32"="C2","33"="C2","34"="C1","35"="C2","36"="C2","37"="C2",
  "38"="C1","39"="C1","40"="NT/SP","41"="NT/SP","42"="D2","43"="NT/SP","44"="B1","45"="B1",
  "46"="B2","47"="B1","48"="B2","49"="B2","50"="B2"
)

broad_map <- c(
  "DA"   = "Diagnostic",
  "B1"   = "NDMM",
  "B2"   = "NDMM",
  "C1"   = "RRMM",
  "C2"   = "RRMM",
  "D1"   = "Special",
  "D2"   = "Special",
  "D3"   = "Special",
  "NT/SP"= "Novel/SpecialPop"
)

df <- df %>%
  mutate(
    Domain = domain_map[as.character(Question)],
    Group  = broad_map[Domain]
  )


df %>%
  distinct(Question, Group) %>%
  count(Group, name = "n_questions")


rank_q_llm <- df %>%
  group_by(Group, Question, LLM) %>%
  summarise(
    mean_rank = mean(rank, na.rm = TRUE),
    .groups   = "drop"
  )


domain_summary <- rank_q_llm %>%
  group_by(Group, LLM) %>%
  summarise(
    n_questions = n_distinct(Question),
    mean_rank   = mean(mean_rank, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(Group, mean_rank)

domain_summary

domain_table <- domain_summary %>%
  select(Group, LLM, mean_rank) %>%
  pivot_wider(names_from = LLM, values_from = mean_rank)

domain_table


domain_n <- domain_summary %>%
  distinct(Group, n_questions)

domain_table_with_n <- domain_n %>%
  left_join(domain_table, by = "Group")

domain_table_with_n

write.csv(domain_table_with_n,
          "domain_level_mean_ranks_accuracy.csv",
          row.names = FALSE)

ref_model <- "HopeAI"

domain_diff_vs_ref <- domain_summary %>%
  group_by(Group) %>%
  mutate(
    ref_rank    = mean_rank[LLM == ref_model],
    diff_vs_ref = mean_rank - ref_rank
  ) %>%
  ungroup()

domain_diff_vs_ref
