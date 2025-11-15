# package load
library(irr)      
library(tidyverse)

calculate_weighted_kappa <- function(data) {
  pairs <- list(
    c("Reviewer1", "Reviewer2"),
    c("Reviewer1", "Reviewer3"),
    c("Reviewer2", "Reviewer3")
  )
  
  results <- list()
  summary_data <- data.frame(
    Pair = character(),
    Kappa = numeric(),
    z = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(pair in pairs) {
    pair_name <- paste(pair[1], "vs", pair[2])
    
    pair_data <- data[, pair]
    
    # weighted kappa (could change weights here)
    k <- kappa2(pair_data, c(0, 0, 0.2, 0.6))
    
    results[[pair_name]] <- k
    summary_data <- rbind(summary_data, data.frame(
      Pair = pair_name,
      Kappa = k$value,
      z = k$statistic,
      p_value = k$p.value
    ))
  }
  
  summary_data$Interpretation <- case_when(
    summary_data$Kappa < 0 ~ "Poor",
    summary_data$Kappa < 0.20 ~ "Slight",
    summary_data$Kappa < 0.40 ~ "Fair",
    summary_data$Kappa < 0.60 ~ "Moderate",
    summary_data$Kappa < 0.80 ~ "Substantial",
    TRUE ~ "Almost Perfect"
  )
  
  return(list(
    detailed_results = results,
    summary = summary_data
  ))
}

analyze_disagreements <- function(data) {
  disagreements <- data.frame(
    Case = 1:nrow(data),
    Reviewer1 = data$Reviewer1,
    Reviewer2 = data$Reviewer2,
    Reviewer3 = data$Reviewer3
  )
  
  disagreements$Max_Diff <- apply(disagreements[,c("Reviewer1","Reviewer2","Reviewer3")], 1, function(x) {
    ratings <- as.numeric(x)
    max(ratings) - min(ratings)
  })
  
  # if Max_Diff > 1 report (also can change)
  major_disagreements <- disagreements[disagreements$Max_Diff > 1,]
  return(major_disagreements)
}

# report the analysis resulys
generate_kappa_report <- function(data) {
  # kappa
  kappa_results <- calculate_weighted_kappa(data)
  
  # disagreement cases report
  disagreements <- analyze_disagreements(data)
  

  cat("\nWeighted Kappa Analysis Report\n")
  cat("============================\n\n")
  
  cat("1. Pairwise Agreement Results:\n")
  print(kappa_results$summary)
  
  cat("\n2. Cases with Major Disagreements (>1 level difference):\n")
  if(nrow(disagreements) > 0) {
    print(disagreements)
    cat(sprintf("\nTotal cases with major disagreements: %d (%.1f%%)\n",
                nrow(disagreements),
                nrow(disagreements)/nrow(data)*100))
  } else {
    cat("No major disagreements found.\n")
  }
  
  return(list(
    kappa_results = kappa_results,
    disagreements = disagreements
  ))
}

