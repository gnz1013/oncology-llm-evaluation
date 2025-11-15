
getDatInfo = function(dat, models, dimensions){
  # Initialize list to store all tables
  rating_tables <- list()
  # Function to extract ratings for a specific model and dimension
  extract_ratings <- function(data, model, dimension) {
   # Filter data for specific model and dimension
    subset <- data[data$LLM == model, c("Question", "Reviewer", dimension)]
    
    # Create matrix with 50 rows (questions) and 3 columns (reviewers)
    result <- matrix(nrow = 50, ncol = 3)
    
    # Fill matrix with ratings
    for(q in 1:50) {
      for(r in 1:3) {
        rating <- subset[subset$Question == q & subset$Reviewer == r, dimension]
        result[q, r] <- if(length(rating) > 0) rating else NA
      }
    }
    
    # Convert to dataframe with appropriate column names
    result <- as.data.frame(result)
    result <- data.frame(lapply(result, function(x) {factor(x, ordered = TRUE)}))
    colnames(result) <- paste0("Reviewer", 1:3)
    rownames(result) <- paste0("Q", 1:50)
    
    return(result)
  }
  
  # Generate all tables
  for(model in models) {
    for(dimension in dimensions) {
      table_name <- paste(model, dimension, sep="_")
      rating_tables[[table_name]] <- extract_ratings(data, model, dimension)
    }
  }
  
  return(rating_tables)
}

