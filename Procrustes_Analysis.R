library(vegan)
library(writexl)
library(readxl)
library("TOSTER")

PA <- function(a) {
  setwd("C:/Users/ LOCATION OF DATA FILES")
  mp <- read_excel("MediaPipeData.xlsx")
  tc <- read_excel("COMPARISON DATA.xlsx")
  
  # List of names of your video files
  column_suffixes <- c("1","2","5")
  
  # Empty list to store Procrustes results
  procrustes_results <- list()
  
  # Iterate through column suffixes
  for (suffix in column_suffixes) {
    # Construct column names for mp and tc data frames
    mp_col_x <- paste0(a,'naming convention', suffix)
    mp_col_y <- paste0(a,'naming convention', suffix)
    tc_col_x <- paste0(a,'naming convention',suffix)
    tc_col_y <- paste0(a,'naming convention', suffix)
    
    # Extract the columns of interest from each data frame
    mp_data <- data.frame(x = mp[, mp_col_x], y = mp[, mp_col_y])
    tc_data <- data.frame(x = tc[, tc_col_x], y = tc[, tc_col_y])
    
    # Run Procrustes analysis on the extracted data
    procrustes_result <- procrustes(mp_data,tc_data, scale=TRUE, translation=TRUE,dilation=TRUE, reflection = TRUE)
    
    # Store the result in the list
    procrustes_results[[suffix]] <- procrustes_result
  }
  
  # Access and examine the procrustes results for each column
  for (suffix in column_suffixes) {
    result <- procrustes_results[[suffix]]
    print(paste("Procrustes results for", suffix))
    print(result)
  }
  
  ss_values <- vector("list", length(column_suffixes))
  for (i in 1:length(column_suffixes)) {
    ss_values[[i]] <- procrustes_results[[column_suffixes[i]]]$ss
  }
  final_vector <- sapply(ss_values, function(x) sqrt(x/1500))
  print(final_vector)
  print(ss_values)
  return (final_vector)
}


#Equivalence testing
final_vector <- PA()
compare <- final_vector
mean(final_vector)
sd(final_vector)
res4 = t_TOST(x = final_vector,
              hypothesis = "EQU",
              eqb = c(0,0.3),
              smd_ci = "goulet")
res4
describe(res4)
plot(res4, type = "cd") 