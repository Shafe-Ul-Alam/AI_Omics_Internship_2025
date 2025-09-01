# ===================================================================
# Assignment 2: Classification of Differentially Expressed Genes (DEGs)
# ===================================================================

# -------------------------------------------------
# Step 1: Define the function classify_gene()
# -------------------------------------------------
classify_gene <- function(logFC, padj) {
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}

# -------------------------------------------------
# Step 2: Setup input and output directories
# -------------------------------------------------
input_dir <- "Raw_Data"       # folder jekhane CSV file ache
output_dir <- "Results"       # output folder banabo

# create output folder if not exist
if(!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Files to process
files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")

# Empty list to store processed results
result_list <- list()

# -------------------------------------------------
# Step 3: Loop through each file
# -------------------------------------------------
for (file_name in files_to_process) {
  cat("\nProcessing:", file_name, "\n")
  
  # Input file path
  input_file_path <- file.path(input_dir, file_name)
  
  # Import dataset
  data <- read.csv(input_file_path, header = TRUE)
  cat("File imported. Checking for missing values...\n")
  
  # Replace missing padj with 1
  data$padj[is.na(data$padj)] <- 1
  
  # Add new column 'status' using classify_gene()
  data$status <- mapply(classify_gene, data$logFC, data$padj)
  cat("Status column added successfully.\n")
  
  # Save results in R list
  result_list[[file_name]] <- data
  
  # Save processed results into Results folder
  output_file_path <- file.path(output_dir, paste0("Processed_", file_name))
  write.csv(data, output_file_path, row.names = FALSE)
  cat("Results saved to:", output_file_path, "\n")
  
  # -------------------------------------------------
  # Step 4: Print summary using table()
  # -------------------------------------------------
  cat("Summary of gene classification:\n")
  print(table(data$status))
}

# -------------------------------------------------
# Step 5: Access results from result_list
# -------------------------------------------------
results_1 <- result_list[["DEGs_Data_1.csv"]]
results_2 <- result_list[["DEGs_Data_2.csv"]]

save.image("Md.Shafe_Ul_Alam_Class_2_Assignment.RData")

