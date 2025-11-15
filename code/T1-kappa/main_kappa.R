#### main report and results ####

################################################
############## GET DATA ########################
################################################

  # Read data and create master dataframe
  data <- read.csv("combined_ratings.csv")
  # Define models and dimensions
  models <- c("ChatGPT.o1_preview", "Gemini.1.5.Pro", "Claude.3.5.Sonnet", "Myelo", "HopeAI")
  dimensions <- c("Accuracy", "Comprehensive", "Relevance", "Hallucination", "Ready.to.use", "Rank")
  # get the data 
  getDatList = getDatInfo(data, models, dimensions)
  

################################################
############## Get Agreement Report ############
################################################
  
# Prepare the data into the factor type 
  
  # Step 1: Extact the required dataframe from List getDatList
  acc_Myelo_agree = getDatList$Myelo_Accuracy
  # Step 2: Use generate_kappa_report() function to get the corresponding agreement report
  generate_kappa_report(acc_Myelo_agree)
  
