
# install.packages(c("readxl","dplyr","tidyr","PMCMRplus"))
library(readxl); library(dplyr); library(tidyr); library(PMCMRplus)

dat <- read_excel("/Users/guannanzhai/Desktop/combined_mapping_dat.xlsx", sheet = "combined_ratings")
dat$Question <- as.integer(dat$Question)


domain_map <- c(
  "1"="DA","2"="C2","3"="DA","4"="C1","5"="B1","6"="C1","7"="C2","8"="D1","9"="C1","10"="B2",
  "11"="D3","12"="D3","13"="C2","14"="B2","15"="C1","16"="B2","17"="B1","18"="NT/SP","19"="D2",
  "20"="NT/SP","21"="B2","22"="D2","23"="D2","24"="B2","25"="B1","26"="C2","27"="C2","28"="C1",
  "29"="C2","30"="C2","31"="C2","32"="C2","33"="C2","34"="C1","35"="C2","36"="C2","37"="C2",
  "38"="C1","39"="C1","40"="NT/SP","41"="NT/SP","42"="D2","43"="NT/SP","44"="B1","45"="B1",
  "46"="B2","47"="B1","48"="B2","49"="B2","50"="B2"
)
broad_map <- c("DA"="Diagnostic","B1"="NDMM","B2"="NDMM","C1"="RRMM","C2"="RRMM",
               "D1"="Special","D2"="Special","D3"="Special","NT/SP"="Novel/SpecialPop")
dat$Domain <- domain_map[as.character(dat$Question)]
dat$Group  <- broad_map[dat$Domain]

models <- c("ChatGPT o1-preview","Claude 3.5 Sonnet","Gemini 1.5 Pro","HopeAI","Myelo")


agg <- dat |>
  group_by(Group, Question, LLM) |>
  summarise(Accuracy = mean(Accuracy, na.rm = TRUE), .groups = "drop") |>
  filter(LLM %in% models)

library(dplyr); library(tidyr)


models <- c("ChatGPT o1-preview","Claude 3.5 Sonnet","Gemini 1.5 Pro","HopeAI","Myelo")

run_friedman <- function(g) {
 
  block <- agg %>%
    filter(Group == g) %>%
    select(Question, LLM, Accuracy) %>%
    mutate(LLM = factor(LLM, levels = models)) %>%          
    tidyr::pivot_wider(names_from = LLM, values_from = Accuracy) %>%
    select(Question, all_of(models)) %>%                     
    arrange(Question)

  keep <- stats::complete.cases(block[, models, drop = FALSE])
  block <- block[keep, , drop = FALSE]
  if (nrow(block) == 0) return(NULL)
  
  X <- as.matrix(block[, models, drop = FALSE])
  mode(X) <- "numeric"
  
  fr <- stats::friedman.test(X)   
  
  ranks <- t(apply(X, 1, function(x) rank(-x, ties.method = "average")))
  avg_r <- colMeans(ranks)
  ord   <- order(avg_r) 
  
  k <- ncol(X); n <- nrow(X)
  q <- 2.728
  CD <- q * sqrt(k * (k + 1) / (6 * n))
  
  pairs <- t(combn(colnames(X), 2))
  diffs <- abs(avg_r[pairs[,1]] - avg_r[pairs[,2]])
  sig_pairs <- data.frame(model_i = pairs[diffs > CD, 1],
                          model_j = pairs[diffs > CD, 2],
                          row.names = NULL)
  
  list(
    n_scen    = n,
    friedman  = fr,
    CD        = CD,
    avg_ranks = data.frame(model = names(avg_r)[ord],
                           avg_rank = as.numeric(avg_r[ord]),
                           row.names = NULL),
    sig_pairs = sig_pairs
  )
}


res_NDMM <- run_friedman("NDMM")
res_RRMM <- run_friedman("RRMM")

res_NDMM$friedman
res_NDMM$avg_ranks
res_RRMM$friedman
res_RRMM$avg_ranks

write.csv(res_NDMM$avg_ranks, "Table_S2_NDMM_avg_ranks.csv", row.names = FALSE)
write.csv(res_RRMM$avg_ranks, "Table_S2_RRMM_avg_ranks.csv", row.names = FALSE)

