convert_csv_to_tsv_fun <- function(csv_file) {
  # Read the .csv file
  data <- read_csv(csv_file)
  
  # Create the .tsv file name
  tsv_file <- sub("\\.csv$", ".tsv", csv_file)
  
  # Write the data to a .tsv file
  write_tsv(data, tsv_file)
  file.remove(csv_file)
}