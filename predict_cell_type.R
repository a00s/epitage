library(EpiDISH)
data(centEpiFibIC.m)

# Define the root directory
root_dir <- "/home/maia/arquivos_antigos"

# List all folders starting with "GSM"
folders <- list.dirs(path = root_dir, full.names = TRUE, recursive = FALSE)

# Assuming each BloodFrac.m result has the same length, we'll initialize the matrix after processing the first folder
initialized <- FALSE

# Loop through each folder
for(folder in folders) {
  # Construct the path to betas.txt in the current folder
  betas_path <- file.path(folder, "betas.txt")
  
  # Check if betas.txt exists in the current folder
  if(file.exists(betas_path)) {
    # Read the betas.txt file
    betas <- read.table(betas_path, quote="\"", comment.char="", header=FALSE)
    rownames(betas) <- betas$V1
    betas <- betas[-1]
    betas_matrix <- as.matrix(betas)
    
    # Process the data
    BloodFrac.m <- epidish(beta.m = betas_matrix, ref.m = centDHSbloodDMC.m, method = "RPC")$estF
    
    # Initialize the matrix if not already done
    if(!initialized) {
      num_cols <- length(BloodFrac.m)
      all_results <- matrix(nrow = length(folders), ncol = num_cols)
      rownames(all_results) <- basename(folders)
      colnames(all_results) <- paste0("BloodFrac.m_", 1:num_cols)
      initialized <- TRUE
    }
    
    # Store the result in the matrix
    all_results[basename(folder), ] <- BloodFrac.m
  }
}

# Save the compiled results in a single file
write.table(all_results, file = file.path(root_dir, "compiled_results.txt"), quote = FALSE, row.names = TRUE)

