#' Clean and Process Microbiome Data
#'
#' Processes microbiome and metadata files (e.g., 16S rRNA sequencing data) to produce an analysis-ready dataset.
#' Supports CSV, TXT, and 'Excel' file formats. This function validates file formats, reads the data,
#' and merges the datasets by the common column 'SampleID'. If a 'Taxonomy' column exists, the data
#' are filtered to include only rows matching the provided taxonomic level.
#'
#' @param microbiome_data A string specifying the path to the microbiome data file.
#' @param metadata A string specifying the path to the metadata file.
#' @param level A string indicating the taxonomic level for filtering the data (e.g., "genus").
#'
#' @return A data frame containing the cleaned and merged dataset.
#'
#' @examples
#' \dontrun{
#'   mb  <- system.file("extdata", "microbiome.csv", package = "mbX")
#'   md  <- system.file("extdata", "metadata.csv",   package = "mbX")
#'   if (nzchar(mb) && nzchar(md)) {
#'     cleaned_data <- ezclean(mb, md, "g")
#'     head(cleaned_data)
#'   } else {
#'     message("Sample data files not found.")
#'   }
#' }
#'
#' @importFrom stats aggregate
#' @importFrom utils read.delim read.table
#' @importFrom openxlsx write.xlsx
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#' @import dplyr
#' @import tidyr
#' @export
ezclean <- function(microbiome_data, metadata, level = "d") {
  # Check the file extension for microbiome_data
  microbiome_ext <- tools::file_ext(microbiome_data)
  if (!microbiome_ext %in% c("csv", "xls", "xlsx")) {
    return("The file is not csv, xls, or xlsx format. Please check the file type for microbiome data.")
  }
  
  # Check the file extension for metadata
  metadata_ext <- tools::file_ext(metadata)
  if (metadata_ext == "txt") {
    metadata_df <- utils::read.delim(metadata, sep = "\t", header = TRUE, check.names = FALSE)
  } else if (metadata_ext == "csv") {
    metadata_df <- read.csv(metadata, header = TRUE, check.names = FALSE)
  } else if (metadata_ext %in% c("xls", "xlsx")) {
    metadata_df <- readxl::read_excel(metadata, col_names = TRUE)
    # Optionally, convert to data.frame if needed:
    metadata_df <- as.data.frame(metadata_df, check.names = FALSE)
  } else {
    return("Please check the file format of metadata.")
  }
  
  
  
  
  # Checking the first header of metadata
  valid_headers <- c("id", "sampleid", "sample id", "sample-id", "featureid", "feature id", "feature-id")
  # Ensure that the header is trimmed of any leading or trailing whitespace and converted to lower case
  if (!(tolower(trimws(names(metadata_df)[1])) %in% valid_headers)) {
    return("Please check the first header of the metadata for file format correction.")
  }
  
  # — sanitize metadata column names & values —
  # replace spaces in column names
  colnames(metadata_df) <- gsub("\\s+", "_", colnames(metadata_df))
  
  # for any character columns, replace spaces in the values
  char_cols <- sapply(metadata_df, is.character)
  metadata_df[ , char_cols] <- lapply(
    metadata_df[ , char_cols, drop = FALSE],
    function(x) gsub("\\s+", "_", x)
  )
  
  # for any factor columns, replace spaces in the levels
  factor_cols <- sapply(metadata_df, is.factor)
  metadata_df[ , factor_cols] <- lapply(
    metadata_df[ , factor_cols, drop = FALSE],
    function(x) factor(gsub("\\s+", "_", as.character(x)))
  )
  
  
  
  
  
  
  # Read microbiome data
  if (microbiome_ext == "txt") {
    microbiome_df <- read.delim(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext == "csv") {
    microbiome_df <- read.csv(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext %in% c("xls", "xlsx")) {
    # Convert tibble to data.frame to preserve names as-is.
    microbiome_df <- as.data.frame(readxl::read_excel(microbiome_data, skip = 1), check.names = FALSE)
  } else {
    return("The microbiome file is not in a supported format. Please use txt, csv, xls, or xlsx.")
  }
  
  
  # — sanitize first column values — 
  # replace spaces with underscores in whatever the first column is  
  first_col <- names(microbiome_df)[1]  
  microbiome_df[[first_col]] <- gsub("\\s+", "_", as.character(microbiome_df[[first_col]]))
  
  
  # grab just the basename (no path, no extension)
  microbiome_base <- tools::file_path_sans_ext(basename(microbiome_data))
  
  # Define the levels map with all variations
  levels_map <- c(
    "domain" = "d__", "Domain" = "d__", "DOMAIN" = "d__", "D" = "d__", "d" = "d__",
    "kingdom" = "d__", "Kingdom" = "d__", "KINGDOM" = "d__", "K" = "d__", "k" = "d__",
    "phylum" = "p__", "Phylum" = "p__", "PHYLUM" = "p__", "P" = "p__", "p" = "p__",
    "class" = "c__", "Class" = "c__", "CLASS" = "c__", "C" = "c__", "c" = "c__",
    "order" = "o__", "Order" = "o__", "ORDER" = "o__", "O" = "o__", "o" = "o__",
    "family" = "f__", "Family" = "f__", "FAMILY" = "f__", "F" = "f__", "f" = "f__",
    "genera" = "g__", "genus" = "g__", "Genera" = "g__", "GENERA" = "g__", "G" = "g__", "g" = "g__",
    "species" = "s__", "Species" = "s__", "SPECIES" = "s__", "S" = "s__", "s" = "s__"
  )
  
  # Convert input level parameter to the correct taxonomy prefix
  level_key <- levels_map[tolower(level)]
  if (is.null(level_key)) {
    return(sprintf("The level value should be one of the following: domain, phylum, class, order, family, genera, species or their abbreviations."))
  }
  level_value <- level_key
  
  
  # Remove columns in microbiome data that are not in metadata headers (common columns)
  common_cols <- intersect(colnames(microbiome_df), colnames(metadata_df))
  just_microbiome_df <- microbiome_df[ , !(colnames(microbiome_df) %in% common_cols), drop = FALSE]
  just_metadata_df <- metadata_df[ , (colnames(metadata_df) %in% common_cols), drop = FALSE]
  
  
  # Save the files
  write.xlsx(just_microbiome_df, "just_microbiome.xlsx")
  write.xlsx(just_metadata_df, "just_metadata.xlsx")
  
  # Transpose just_microbiome_df and save as microbiome_ezy-1.xlsx
  # Transpose just_microbiome_df exactly as in Excel's paste special transpose
  # ---- Updated Transposition Block Start ----
  # Combine the header row with the data to mimic Excel's full-range transpose
  df_to_transpose <- rbind(names(just_microbiome_df), just_microbiome_df)
  # Convert all values to character (so that Excel sees exactly what you expect)
  df_to_transpose <- data.frame(lapply(df_to_transpose, as.character), stringsAsFactors = FALSE)
  # Transpose the entire dataset (including the header row)
  transposed_microbiome <- t(df_to_transpose)
  # Convert the transposed matrix back to a data frame
  transposed_microbiome_df <- as.data.frame(transposed_microbiome, stringsAsFactors = FALSE)
  # Write the transposed data to Excel without row names and without the auto-generated column names
  write.xlsx(transposed_microbiome_df, "microbiome_ezy_1.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Updated Transposition Block End ----
  
  
  
  
  #CONVERTING THE ENTIRE TEXT INTO NUMERICAL AND TEXT, AS MEZY_1 EXCEL IS ALL TEXT VALUED
  # ---- Additional Conversion Block Start ----
  # Read the previously generated Excel file (all values as text)
  df_ezy1 <- openxlsx::read.xlsx("microbiome_ezy_1.xlsx", colNames = FALSE)
  
  # Separate header row (first row) and data rows (remaining rows)
  header_row <- df_ezy1[1, ]
  data_rows  <- df_ezy1[-1, ]
  
  # Convert all columns except the first in data_rows to numeric
  data_rows[, -1] <- lapply(data_rows[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  
  # Write the header row as text.
  # Convert header_row (a one-row data frame) to a vector and transpose it
  header_vec <- as.vector(unlist(header_row))
  openxlsx::writeData(wb, sheet = "Sheet1", x = t(header_vec),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  # The first column remains text while columns 2:end are numeric
  openxlsx::writeData(wb, sheet = "Sheet1", x = data_rows,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as microbiome_ezy_2.xlsx
  openxlsx::saveWorkbook(wb, "microbiome_ezy_2.xlsx", overwrite = TRUE)
  # ---- Additional Conversion Block End ----
  
  ####ADD A NEW BLANK COLUMN AS TAXA AS THE SECOND COLUMN
  
  # ---- Additional Blank Column Block Start ----
  # Read the file microbiome_ezy_2.xlsx (all values are as saved)
  df_ezy2 <- openxlsx::read.xlsx("microbiome_ezy_2.xlsx", colNames = FALSE)
  
  # Convert the data frame to a matrix for easy column insertion
  m_ezy2 <- as.matrix(df_ezy2)
  
  # Create a new matrix by binding:
  # - The first column of m_ezy2,
  # - A new column (with a blank value for each row, except the header),
  # - The remaining columns of m_ezy2.
  new_matrix <- cbind(m_ezy2[, 1, drop = FALSE],
                      rep("", nrow(m_ezy2)),
                      m_ezy2[, -1, drop = FALSE])
  
  # Set the header (first row, second column) to "Taxa"
  new_matrix[1, 2] <- "Taxa"
  
  # Write the new matrix as microbiome_ezy_3.xlsx
  openxlsx::write.xlsx(as.data.frame(new_matrix, stringsAsFactors = FALSE),
                       "microbiome_ezy_3.xlsx",
                       rowNames = FALSE, colNames = FALSE)
  # ---- Additional Blank Column Block End ----
  
  
  
  
  
  
  
  ####REPLACING THE LEVEL VALUE WITH THE @
  
  # Read the microbiome_ezy_3.xlsx file without any modifications
  df_ezy3 <- openxlsx::read.xlsx("microbiome_ezy_3.xlsx", colNames = FALSE)
  
  # Save the data as microbiome_ezy_4.xlsx
  openxlsx::write.xlsx(df_ezy3, "microbiome_ezy_4.xlsx", rowNames = FALSE, colNames = FALSE)
  
  
  ####GRABBING THE TAXA NAME FROM THE FIRST COLUMN AND PUTTING IT INTO SECOND COL "TAXA" 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ---- Additional Extraction and Paste Block Start ----
  # Read the microbiome_ezy_4.xlsx file (all data, including header)
  df_ezy4 <- openxlsx::read.xlsx("microbiome_ezy_4.xlsx", colNames = FALSE)
  
  # Define the taxonomic level order (highest to lowest)
  levels_order <- c("d__", "p__", "c__", "o__", "f__", "g__", "s__")
  
  # Define a mapping from abbreviated level markers to full names
  level_mapping <- c("d__" = "domain", "p__" = "phylum", "c__" = "class", 
                     "o__" = "order", "f__" = "family", "g__" = "genus", "s__" = "species")
  
  # Assume level_value is defined (e.g., "s__") and is one of the above.
  # For example:
  # level_value <- "s__"
  
  blank_counter <- 0  # to count how many blanks (unidentified) occur
  
  # Process only if there is more than just the header row
  if (nrow(df_ezy4) > 1) {
    # For each data row (excluding the header)
    extracted_text <- sapply(df_ezy4[-1, 1], function(x) {
      # Only attempt extraction if the target marker exists in the string
      if (grepl(level_value, x)) {
        pattern_target <- paste0(".*", level_value, "\\s*([^;]*)(;.*)?$")
        candidate <- trimws(sub(pattern_target, "\\1", x))
      } else {
        candidate <- ""
      }
      
      if (candidate != "") {
        # If extraction for level_value is nonblank, return it
        return(candidate)
      } else {
        # If blank, search for a previous (higher) level in the defined order
        target_idx <- match(level_value, levels_order)
        found_value <- ""
        found_marker <- ""
        if (!is.na(target_idx) && target_idx > 1) {
          # Look upward from the immediate preceding marker down to "d__"
          for (i in seq(from = target_idx - 1, to = 1)) {
            if (grepl(levels_order[i], x)) {
              pattern_prev <- paste0(".*", levels_order[i], "\\s*([^;]*)(;.*)?$")
              candidate_prev <- trimws(sub(pattern_prev, "\\1", x))
              if (candidate_prev != "") {
                found_value <- candidate_prev
                found_marker <- levels_order[i]
                break  # stop at the first nonblank found
              }
            }
          }
        }
        if (found_value != "") {
          blank_counter <<- blank_counter + 1
          return(paste0("unidentified_", level_mapping[level_value], "_", blank_counter,
                        "_from_", found_value, "_", level_mapping[found_marker]))
        } else {
          return("")
        }
      }
    })
    
    # Paste the extracted (or constructed) text into the second column for data rows
    df_ezy4[-1, 2] <- extracted_text
  }
  
  # Save the updated data as microbiome_ezy_5.xlsx without extra row or column names
  openxlsx::write.xlsx(df_ezy4, "microbiome_ezy_5.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Additional Extraction and Paste Block End ----
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #### DELETE THE FIRST COLUMN THE "INDEX" COLUMN WITH THE LONG NAME OF THE TAXA
  
  # ---- Additional Delete First Column Block Start ----
  # Read the microbiome_ezy_5.xlsx file (all data, including header)
  df_ezy5 <- openxlsx::read.xlsx("microbiome_ezy_5.xlsx", colNames = FALSE)
  
  # Delete the first column entirely
  df_ezy5 <- df_ezy5[, -1, drop = FALSE]
  
  # Save the updated data as microbiome_ezy_6.xlsx without extra row or column names
  openxlsx::write.xlsx(df_ezy5, "microbiome_ezy_6.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Additional Delete First Column Block End ----
  
  
  
  ####AGAIN CONVERTING THE THINGS INTO NUMERICALS
  #####
  # ---- Additional Numeric Conversion Block for ezy_7 Start ----
  # Read the microbiome_ezy_6.xlsx file (all values as text)
  df_ezy6 <- openxlsx::read.xlsx("microbiome_ezy_6.xlsx", colNames = FALSE)
  
  # Separate header row (first row) and data rows (remaining rows)
  header_row_6 <- df_ezy6[1, ]
  data_rows_6  <- df_ezy6[-1, ]
  
  # Convert all columns except the first in data_rows_6 to numeric
  data_rows_6[, -1] <- lapply(data_rows_6[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb2 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb2, "Sheet1")
  
  # Write the header row as text.
  header_vec_6 <- as.vector(unlist(header_row_6))
  openxlsx::writeData(wb2, sheet = "Sheet1", x = t(header_vec_6),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  openxlsx::writeData(wb2, sheet = "Sheet1", x = data_rows_6,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as microbiome_ezy_7.xlsx
  openxlsx::saveWorkbook(wb2, "microbiome_ezy_7.xlsx", overwrite = TRUE)
  # ---- Additional Numeric Conversion Block for ezy_7 End ----
  
  ###HANDLING THE SAME TAXA NAME AND THE BLANKS 
  
  # ---- Additional Aggregation Block Start ----
  # Read the microbiome_ezy_7.xlsx file (all data, including the header row)
  df_ezy7 <- openxlsx::read.xlsx("microbiome_ezy_7.xlsx", colNames = FALSE)
  
  # Separate the header row (first row) and the data rows (remaining rows)
  header_row_7 <- df_ezy7[1, ]
  data_rows_7  <- df_ezy7[-1, ]
  
  # Ensure the first column is character and all other columns are numeric
  data_rows_7[, 1] <- as.character(data_rows_7[, 1])
  data_rows_7[, -1] <- lapply(data_rows_7[, -1, drop = FALSE], function(x) as.numeric(x))
  
  # Define replacement names based on the level_value
  other_names <- list(
    "d__" = "Other_domains",
    "p__" = "Other_phyla",
    "c__" = "Other_classes",
    "o__" = "Other_orders",
    "f__" = "Other_families",
    "g__" = "Other_genera",
    "s__" = "Other_species"
  )
  
  # Determine the appropriate replacement name using the level_value
  replacement_name <- other_names[[level_value]]
  
  # Replace rows with missing taxa information (NA, blank, or "#VALUE") with the determined replacement_name
  data_rows_7[, 1] <- ifelse(is.na(data_rows_7[, 1]) | data_rows_7[, 1] == "" | data_rows_7[, 1] == "#VALUE",
                             replacement_name, data_rows_7[, 1])
  
  # Aggregate the data by the taxa (first) column:
  # For rows with the exact same taxa value, sum the numeric columns column-wise.
  aggregated_df <- aggregate(data_rows_7[, -1],
                             by = list(Taxa = data_rows_7[, 1]),
                             FUN = sum, na.rm = TRUE)
  
  
  
  
  
  # Set the column names of the aggregated data using the original header row.
  # Convert the header row to a vector.
  new_header <- as.character(unlist(header_row_7))
  # Ensure the first header is "Taxa" (or any appropriate label)
  new_header[1] <- "Taxa"
  colnames(aggregated_df) <- new_header
  
  # Create a new workbook and add a worksheet
  wb3 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb3, "Sheet1")
  
  # Write the aggregated data (including header) starting at row 1
  openxlsx::writeData(wb3, sheet = "Sheet1", x = aggregated_df,
                      startRow = 1, startCol = 1, colNames = TRUE)
  
  # Save the new workbook as microbiome_ezy_8.xlsx
  openxlsx::saveWorkbook(wb3, "microbiome_ezy_8.xlsx", overwrite = TRUE)
  # ---- Additional Aggregation Block End ----
  
  
  
  
  
  
  ####NOW CONVERTING THE NUMBERS TO PERCENTAGES 
  # ---- Additional Column-wise Percentage Block Start ----
  # Read the microbiome_ezy_8.xlsx file with header information
  microbiome_ezy8 <- openxlsx::read.xlsx("microbiome_ezy_8.xlsx", colNames = TRUE)
  microbiome_ezy8 <- as.data.frame(microbiome_ezy8)
  
  # Extract the taxa column (first column) and numeric columns (columns 2 onward)
  taxa_column <- microbiome_ezy8[, 1]
  numeric_columns <- microbiome_ezy8[, -1, drop = FALSE]
  
  # Ensure all numeric columns are properly converted to numeric
  numeric_columns[] <- lapply(numeric_columns, as.numeric)
  
  # Calculate the column-wise percentages:
  # Divide each element by the column sum (ignoring NA's) and multiply by 100
  percentage_columns <- sweep(numeric_columns, 2, colSums(numeric_columns, na.rm = TRUE), "/") * 100
  
  # Combine the taxa column with the calculated percentage columns
  microbiome_ezy9 <- cbind(taxa_column, percentage_columns)
  
  # Save the resulting data frame as microbiome_ezy_9.xlsx without row names
  openxlsx::write.xlsx(microbiome_ezy9, "microbiome_ezy_9.xlsx", rowNames = FALSE)
  # ---- Additional Column-wise Percentage Block End ----
  
  
  
  
  
  ####TRANSPOSING THE DATA 
  
  # ---- Additional Transposition for ezy_9 to ezy_10 Block Start ----
  # Read the microbiome_ezy_9.xlsx file with header information
  df_ezy9 <- openxlsx::read.xlsx("microbiome_ezy_9.xlsx", colNames = TRUE)
  
  # Combine the header row with the data to mimic Excel's full-range transpose
  df_to_transpose_9 <- rbind(names(df_ezy9), df_ezy9)
  
  # Convert all values to character (so that Excel sees exactly what you expect)
  df_to_transpose_9 <- data.frame(lapply(df_to_transpose_9, as.character), stringsAsFactors = FALSE)
  
  # Transpose the entire dataset (including the header row)
  transposed_ezy9 <- t(df_to_transpose_9)
  
  # Convert the transposed matrix back to a data frame
  transposed_ezy9_df <- as.data.frame(transposed_ezy9, stringsAsFactors = FALSE)
  
  # Write the transposed data to Excel without row names and without auto-generated column names
  openxlsx::write.xlsx(transposed_ezy9_df, "microbiome_ezy_10.xlsx", rowNames = FALSE, colNames = FALSE)
  # ---- Additional Transposition for ezy_9 to ezy_10 Block End ----
  
  
  
  
  
  #####GETTING THE ORIGINAL HEADER FOR THE FIRST COLUMN LIKE SAMPLE-ID
  
  # ---- Additional Header Replacement Block Start ----
  # Extract the header of the first column from the metadata file
  metadata_first_header <- names(metadata_df)[1]
  
  # Read the microbiome_ezy_10.xlsx file; since it was written without column names,
  # the first row actually contains the header information.
  df_ezy10 <- openxlsx::read.xlsx("microbiome_ezy_10.xlsx", colNames = FALSE)
  
  # Extract the first row as the current header and convert to a character vector
  new_header <- as.character(unlist(df_ezy10[1, ]))
  
  # Replace the first element with the metadata header
  new_header[1] <- metadata_first_header
  
  # Remove the first row (which contained the original header) from the data
  df_ezy10_data <- df_ezy10[-1, ]
  
  # Assign the new header to the data
  colnames(df_ezy10_data) <- new_header
  
  # Save the updated data as microbiome_ezy_11.xlsx with proper column names
  openxlsx::write.xlsx(df_ezy10_data, "microbiome_ezy_11.xlsx", rowNames = FALSE, colNames = TRUE)
  # ---- Additional Header Replacement Block End ----
  
  
  
  
  
  
  #### SETTING THE NUMERIC AS ALL VALUES ARE TEXT AGAIN
  # ---- Additional Numeric Conversion Block for ezy_12 Start ----
  # Read the microbiome_ezy_11.xlsx file (all values as text)
  df_ezy11 <- openxlsx::read.xlsx("microbiome_ezy_11.xlsx", colNames = FALSE)
  
  # Separate header row (first row) and data rows (remaining rows)
  header_row_11 <- df_ezy11[1, ]
  data_rows_11  <- df_ezy11[-1, ]
  
  # Convert all columns except the first in data_rows_11 to numeric
  data_rows_11[, -1] <- lapply(data_rows_11[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb12 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb12, "Sheet1")
  
  # Write the header row as text.
  header_vec_11 <- as.vector(unlist(header_row_11))
  openxlsx::writeData(wb12, sheet = "Sheet1", x = t(header_vec_11),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  openxlsx::writeData(wb12, sheet = "Sheet1", x = data_rows_11,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as microbiome_ezy_12.xlsx
  openxlsx::saveWorkbook(wb12, "microbiome_ezy_12.xlsx", overwrite = TRUE)
  # ---- Additional Numeric Conversion Block for ezy_12 End ----
  
  
  
  
  
  
  
  
  # ---- Revised Metadata Merge Block Start ----
  # Read the microbiome_ezy_12.xlsx file (with header)
  df_ezy12 <- openxlsx::read.xlsx("microbiome_ezy_12.xlsx", colNames = TRUE)
  
  # Extract only the first column (sample-ID) and the rest of the microbiome columns
  first_col   <- df_ezy12[, 1, drop = FALSE]
  micro_cols  <- df_ezy12[, -1, drop = FALSE]
  
  # Prepare metadata exactly as before:
  meta_remaining <- metadata_df[, -1, drop = FALSE]
  meta_key       <- metadata_df[[1]]
  df_key         <- df_ezy12[[1]]
  appended_metadata <- meta_remaining[match(df_key, meta_key), , drop = FALSE]
  
  # Now bind them in the desired order:
  #  (1) sample-ID, (2) metadata columns, (3) microbiome columns
  df_ezy13 <- cbind(first_col, appended_metadata, micro_cols)
  
  # Save the combined data as microbiome_ezy_13.xlsx with headers
  openxlsx::write.xlsx(df_ezy13, "microbiome_ezy_13.xlsx", rowNames = FALSE)
  # ---- Revised Metadata Merge Block End ----
  
  
  
  
  
  #I WILL DELETE ALL THE INTERMEDIATE FILES, KEEPING THEM FOR NOW FOR THE DEBUGGING PURPOSE, FOR THE FINAL EZCLEAN THERE WILL ONLY BE MICROBIOME_EZY_13.XLSX
  # THE FINAL FILE NAME WILL BE NAMED AS MICROBIOME_CLEANED_DATA.XLSX
  
  
  # Return the modified level value for further steps
  #return(list(just_microbiome_path = "just_microbiome.xlsx",
  #just_metadata_path = "just_metadata.xlsx",
  #just_microbiome_transposed = "microbiome_ezy_1.xlsx",
  #df_ezy2= "microbiome_ezy_2.xlsx",
  #df_ezy3= "microbiome_ezy_3.xlsx",
  #df_ezy4= "microbiome_ezy_4.xlsx",
  #df_ezy5= "microbiome_ezy_5.xlsx",
  #df_ezy6= "microbiome_ezy_6.xlsx",
  #df_ezy7= "microbiome_ezy_7.xlsx",
  #df_ezy8= "microbiome_ezy_8.xlsx",
  #df_ezy9= "microbiome_ezy_9.xlsx",
  #df_ezy10= "microbiome_ezy_10.xlsx",
  #df_ezy11= "microbiome_ezy_11.xlsx",
  #df_ezy12= "microbiome_ezy_12.xlsx",
  #df_ezy13= "microbiome_ezy_13.xlsx",
  #level = level_value))
  
  
  
  
  #####REMOVING THE INTERMEDIATE FILES IF ANY WRITTEN IN THE CURRENT DIRECTORY
  
  # List of intermediate files to be removed (excluding the final file "microbiome_ezy_13.xlsx")
  intermediate_files <- c(
    "just_microbiome.xlsx",
    "just_metadata.xlsx",
    "microbiome_ezy_1.xlsx",
    "microbiome_ezy_2.xlsx",
    "microbiome_ezy_3.xlsx",
    "microbiome_ezy_4.xlsx",
    "microbiome_ezy_5.xlsx",
    "microbiome_ezy_6.xlsx",
    "microbiome_ezy_7.xlsx",
    "microbiome_ezy_8.xlsx",
    "microbiome_ezy_9.xlsx",
    "microbiome_ezy_10.xlsx",
    "microbiome_ezy_11.xlsx",
    "microbiome_ezy_12.xlsx"
  )
  
  # Remove each intermediate file if it exists
  sapply(intermediate_files, function(f) {
    if (file.exists(f)) file.remove(f)
  })
  #############
  
  
  
  
  
  
  
  
  ####FINAL NAME RETURN
  
  # ---- Final Return Block ----
  final_names <- list(
    "d__" = paste0("mbX_cleaned_domains_or_kingdoms_", microbiome_base, ".xlsx"),
    "p__" = paste0("mbX_cleaned_phyla_",                     microbiome_base, ".xlsx"),
    "c__" = paste0("mbX_cleaned_classes_",                    microbiome_base, ".xlsx"),
    "o__" = paste0("mbX_cleaned_orders_",                     microbiome_base, ".xlsx"),
    "f__" = paste0("mbX_cleaned_families_",                   microbiome_base, ".xlsx"),
    "g__" = paste0("mbX_cleaned_genera_",                     microbiome_base, ".xlsx"),
    "s__" = paste0("mbX_cleaned_species_",                    microbiome_base, ".xlsx")
  )
  
  
  
  
  
  final_file_name <- final_names[[level_value]]
  #file.rename("microbiome_ezy_13.xlsx", final_file_name)
  
  dir_name_ezclean <- tools::file_path_sans_ext(final_file_name)
  
  # my new dir ccreate folder if needed:
  if (!dir.exists(dir_name_ezclean)) {
    dir.create(dir_name_ezclean)
  }
  
  # 4) Move (rename) the file into that folder:
  new_path <- file.path(dir_name_ezclean, final_file_name)
  file.rename("microbiome_ezy_13.xlsx", new_path)
  
  # 5) Return the full path of the moved file:
  cat("Done with the cleaning, your cleaned file is here: ")
  return(new_path)
  
  
  message('Please cite us: "Lamichhane, U., & Lourenco, J. (2025). mbX: An R Package for Streamlined Microbiome Analysis. Stats, 8(2), 44."')
  message("DOI to paper: https://doi.org/10.3390/stats8020044")
  
}



#ezclean("level-7.csv", "metadata.txt", "f")
#ezclean("level-7.csv", "metadata.txt", "g")
#ezclean("level-7.csv", "metadata.txt", "s")





#' Visualize Microbiome Data
#'
#' Generates publication-ready visualizations for microbiome data. This function first processes
#' the microbiome and metadata files using ezclean(), then creates a bar plot using ggplot2.
#' Supported file formats are CSV, TXT, and 'Excel'. Note: Only one of the parameters top_taxa or threshold
#' should be provided.
#'
#' @param microbiome_data A string specifying the path to the microbiome data file.
#' @param metadata A string specifying the path to the metadata file.
#' @param level A string indicating the taxonomic level for filtering the data (e.g., "genus").
#' @param selected_metadata A string specifying the metadata column used for grouping.
#' @param top_taxa An optional numeric value indicating the number of top taxa to keep. Use this OR
#'        threshold, but not both.
#' @param threshold An optional numeric value indicating the minimum threshold value; taxa below this
#'        threshold will be grouped into an "Other" category.
#' @param flip Logical. If `TRUE`, the order of the stacks is reversed.
#'
#' @return A ggplot object containing the visualization.
#'
#' @examples
#' \donttest{
#' mb  <- system.file("extdata", "microbiome.csv", package = "mbX")
#' md  <- system.file("extdata", "metadata.csv",   package = "mbX")
#' plot_obj <- ezviz(
#'   microbiome_data = mb,
#'   metadata        = md,
#'   level           = "genus",
#'   selected_metadata    = "sample_type",
#'   top_taxa        = 20,
#'   flip            = FALSE
#' )
#' print(plot_obj)
#' }

#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @export
ezviz <- function(microbiome_data, metadata, level, selected_metadata,
                  top_taxa = NULL, threshold = NULL,
                  flip = FALSE) {
  # Check the file extension for microbiome_data
  microbiome_ext <- tools::file_ext(microbiome_data)
  if (!microbiome_ext %in% c("csv", "xls", "xlsx")) {
    return("The file is not csv, xls, or xlsx format. Please check the file type for microbiome data.")
  }
  #memory.limit(size = 50)
  # Check the file extension for metadata
  metadata_ext <- tools::file_ext(metadata)
  if (metadata_ext == "txt") {
    metadata_df <- read.delim(metadata, sep = "\t", header = TRUE, check.names = FALSE)
  } else if (metadata_ext == "csv") {
    metadata_df <- read.csv(metadata, header = TRUE, check.names = FALSE)
  } else if (metadata_ext %in% c("xls", "xlsx")) {
    metadata_df <- readxl::read_excel(metadata, col_names = TRUE)
    # Optionally, convert to data.frame if needed:
    metadata_df <- as.data.frame(metadata_df, check.names = FALSE)
  } else {
    return("Please check the file format of metadata.")
  }
  
  
  
  
  
  ##removING WARNING 
  #old_warn <- getOption("warn")
  #options(warn = -1)
  #on.exit(options(warn = old_warn))
  
  
  
  
  # Check the first header of metadata
  valid_headers <- c("id", "sampleid", "sample id", "sample-id", 
                     "featureid", "feature id", "feature-id")
  if (!(tolower(trimws(names(metadata_df)[1])) %in% valid_headers)) {
    return("Please check the first header of the metadata for file format correction.")
  }
  
  # Check if the selected_metadata is a valid categorical column in metadata_df
  if (!(selected_metadata %in% colnames(metadata_df)) ||
      !(is.factor(metadata_df[[selected_metadata]]) || is.character(metadata_df[[selected_metadata]]))) {
    return("The selected metadata is either not in the metadata or not a categorical value")
  }
  
  
  # — sanitize metadata column names & values —
  # replace spaces in column names
  colnames(metadata_df) <- gsub("\\s+", "_", colnames(metadata_df))
  
  # for any character columns, replace spaces in the values
  char_cols <- sapply(metadata_df, is.character)
  metadata_df[ , char_cols] <- lapply(
    metadata_df[ , char_cols, drop = FALSE],
    function(x) gsub("\\s+", "_", x)
  )
  
  # for any factor columns, replace spaces in the levels
  factor_cols <- sapply(metadata_df, is.factor)
  metadata_df[ , factor_cols] <- lapply(
    metadata_df[ , factor_cols, drop = FALSE],
    function(x) factor(gsub("\\s+", "_", as.character(x)))
  )
  
  
  
  # Read microbiome data
  # Read microbiome data based on file extension
  if (microbiome_ext == "txt") {
    microbiome_df <- read.delim(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext == "csv") {
    microbiome_df <- read.csv(microbiome_data, header = TRUE, check.names = FALSE)
  } else if (microbiome_ext %in% c("xls", "xlsx")) {
    # Convert tibble to data.frame to preserve names as-is.
    microbiome_df <- as.data.frame(readxl::read_excel(microbiome_data, skip = 1), check.names = FALSE)
  } else {
    return("The microbiome file is not in a supported format. Please use txt, csv, xls, or xlsx.")
  }
  
  # — sanitize first column values — 
  # replace spaces with underscores in whatever the first column is  
  first_col <- names(microbiome_df)[1]  
  microbiome_df[[first_col]] <- gsub("\\s+", "_", as.character(microbiome_df[[first_col]]))
  
  
  microbiome_base <- tools::file_path_sans_ext(basename(microbiome_data))
  
  
  # Define the levels map with variations
  levels_map <- c(
    "domain"  = "d__", "Domain"  = "d__", "DOMAIN"  = "d__", "D" = "d__", "d" = "d__",
    "kingdom" = "d__", "Kingdom" = "d__", "KINGDOM" = "d__", "K" = "d__", "k" = "d__",
    "phylum"  = "p__", "Phylum"  = "p__", "PHYLUM"  = "p__", "P" = "p__", "p" = "p__",
    "class"   = "c__", "Class"   = "c__", "CLASS"   = "c__", "C" = "c__", "c" = "c__",
    "order"   = "o__", "Order"   = "o__", "ORDER"   = "o__", "O" = "o__", "o" = "o__",
    "family"  = "f__", "Family"  = "f__", "FAMILY"  = "f__", "F" = "f__", "f" = "f__",
    "genera"  = "g__", "Genera"  = "g__", "GENERA"  = "g__", "G" = "g__", "g" = "g__",
    "species" = "s__", "Species" = "s__", "SPECIES" = "s__", "S" = "s__", "s" = "s__"
  )
  
  # Get the corresponding level code using the lower-case version of level
  level_code <- levels_map[tolower(level)]
  if (is.na(level_code)) {
    return("Invalid taxonomic level provided.")
  }
  
  # Ensure that only one of top_taxa or threshold is provided
  if (!is.null(top_taxa) && !is.null(threshold)) {
    return("Only one of the parameter can be selected between top_taxa and threshold")
  }
  
  # Determine the threshold value based on the provided parameter
  if (!is.null(threshold)) {
    threshold_value <- threshold
  } else if (!is.null(top_taxa)) {
    threshold_value <- top_taxa
  } else {
    threshold_value <- NULL
  }
  
  ##### File Handling: Run ezclean and use its output #####
  
  # Run the ezclean function using the first three parameters.
  # It should return one of the following fixed file names, e.g. "mbX_cleaned_families.xlsx"
  cleaned_file_ezviz <- ezclean(microbiome_data, metadata, level)
  message("ezclean returned:", cleaned_file_ezviz, "\n")
  
  # 1a) Copy into working dir:
  cleaned_basename <- basename(cleaned_file_ezviz)
  file.copy(cleaned_file_ezviz, cleaned_basename, overwrite = TRUE)
  
  # 1b) Point the variable at the local copy:
  cleaned_file_ezviz <- cleaned_basename
  
  if (!file.exists(cleaned_file_ezviz)) {
    stop("The file returned by ezclean does not exist: ", cleaned_file_ezviz)
  }
  cleaned_data <- openxlsx::read.xlsx(cleaned_file_ezviz, colNames = TRUE, check.names = FALSE)
  
  
  # Option 1: Prevent conversion by ensuring check.names = FALSE everywhere (preferred)
  
  # OR, Option 2: Conditionally replace dots with spaces:
  correct_names <- names(metadata_df)
  for(i in seq_along(names(cleaned_data))) {
    orig_name <- names(cleaned_data)[i]
    if (!(orig_name %in% correct_names)) {
      candidate <- gsub("\\.", " ", orig_name)
      if (candidate %in% correct_names) {
        names(cleaned_data)[i] <- candidate
      }
    }
  }
  
  # Debug: Print out the names of the columns in cleaned_data
  #cat("Columns in cleaned_data:\n", paste(names(cleaned_data), collapse = ", "), "\n")
  
  # Identify metadata columns in cleaned_data that are also present in metadata_df and remove
  # Identify the metadata columns that exist in the cleaned data:
  metadata_cols_in_cleaned <- intersect(names(metadata_df), names(cleaned_data))
  
  # Check that the selected_metadata is indeed present in the cleaned data:
  if (!(selected_metadata %in% metadata_cols_in_cleaned)) {
    stop("The selected metadata column '", selected_metadata, "' was not found in the cleaned data.")
  }
  
  # Determine which metadata columns (that are present) should be removed
  metadata_to_remove <- setdiff(metadata_cols_in_cleaned, selected_metadata)
  
  # Subset the cleaned_data to keep only non-metadata columns plus the selected_metadata column
  subset_data <- cleaned_data[, !(names(cleaned_data) %in% metadata_to_remove), drop = FALSE]
  
  
  # Write the resulting subset_data to "mbX_cleaning_1.xlsx" using openxlsx
  openxlsx::write.xlsx(subset_data, file = "mbX_cleaning_1.xlsx", rowNames = FALSE, colNames = TRUE)
  #cat("Output file 'mbX_cleaning_1.xlsx' has been created in the working directory.\n")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######NOW I WILL take average along the colUMN SEGREGATED BY THE CATEGORIES IN THE SELECTED_METADATA
  
  
  
  # Read in the "mbX_cleaning_1.xlsx" file
  # Read in the "mbX_cleaning_1.xlsx" file
  data_cleaning_mbX_1 <- openxlsx::read.xlsx("mbX_cleaning_1.xlsx", colNames = TRUE)
  
  # Filter out rows where the selected_metadata column is empty, NA, "Na", "#VALUE!", or "#NAME?"
  data_cleaning_mbX_1 <- data_cleaning_mbX_1 %>%
    filter(!(is.na(.data[[selected_metadata]]) |
               .data[[selected_metadata]] %in% c("", "Na", "NA", "#VALUE!", "#NAME?")))
  
  # Group the data by the selected_metadata column and calculate the mean for each numeric column
  data_summary_mbX_2 <- data_cleaning_mbX_1 %>%
    group_by(across(all_of(selected_metadata))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # Write the summarized data to "mbX_cleaning_2.xlsx" in the working directory
  openxlsx::write.xlsx(data_summary_mbX_2, file = "mbX_cleaning_2.xlsx", rowNames = FALSE, colNames = TRUE)
  
  #cat("Output file 'mbX_cleaning_2.xlsx' has been created in the working directory.\n")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###now the thiNGS ARE AVERAGED
  ######## I WILL TRANSPOSE THEM to get the OTHER_TAXA THING BEYOND THE THRESHOLD OR THE TOP TAXA
  
  
  
  
  # Read in the "mbX_cleaning_2.xlsx" file with headers
  data_cleaning_mbX_2 <- openxlsx::read.xlsx("mbX_cleaning_2.xlsx", colNames = TRUE)
  
  # Convert the data frame to a matrix and transpose it
  data_matrix_mbX_2 <- as.matrix(data_cleaning_mbX_2)
  transposed_matrix_mbX_3 <- t(data_matrix_mbX_2)
  
  # Convert the transposed matrix back to a data frame.
  # Note: We set rowNames = FALSE when writing to avoid an extra index column.
  data_transposed_mbX_3 <- as.data.frame(transposed_matrix_mbX_3, stringsAsFactors = FALSE, check.names = FALSE )
  
  # Write the transposed data to "mbX_cleaning_3.xlsx" in the working directory without row names.
  openxlsx::write.xlsx(data_transposed_mbX_3, file = "mbX_cleaning_3.xlsx", rowNames = TRUE)
  
  #cat("Output file 'mbX_cleaning_3.xlsx' has been created in the working directory.\n")
  
  
  
  
  
  
  
  ### NOW THE CLEANING_3 IS THE TRANSPOSED BUT HAS ONE EXTRA ROW WITH THE index
  ### so here we delete THE FIRST ROW 
  
  # Read in the "mbX_cleaning_3.xlsx" file without using a header
  raw_data_mbX_3 <- openxlsx::read.xlsx("mbX_cleaning_3.xlsx", colNames = FALSE)
  
  # Debug: show dimensions of the raw data
  #cat("Dimensions of raw data:", dim(raw_data_mbX_3), "\n")
  
  # Remove the very first row (assumed to be the unwanted index row)
  raw_data_no_index <- raw_data_mbX_3[-1, ]
  
  # Now, the first row of raw_data_no_index is the actual header.
  header_row <- raw_data_no_index[1, ]
  data_without_index <- raw_data_no_index[-1, ]
  
  # Assign the header row as column names
  colnames(data_without_index) <- header_row
  
  # Write the cleaned data to "mbX_cleaning_4.xlsx" in the working directory
  openxlsx::write.xlsx(data_without_index, file = "mbX_cleaning_4.xlsx", rowNames = FALSE)
  #cat("Output file 'mbX_cleaning_4.xlsx' has been created in the working directory.\n")
  #######33333333DONE TILL THIS
  
  
  
  
  
  
  
  
  
  
  
  #######NOW THE ISSUE IS EVERYTHING IS TEXT 
  
  # Read in the "mbX_cleaning_4.xlsx" file without treating any row as header
  data_cleaning_mbX_4 <- openxlsx::read.xlsx("mbX_cleaning_4.xlsx", colNames = FALSE)
  
  # Separate the header row (first row) and the data rows (remaining rows)
  header_row_mbX_4 <- data_cleaning_mbX_4[1, ]
  data_rows_mbX_4  <- data_cleaning_mbX_4[-1, ]
  
  # Convert all columns except the first in data_rows_mbX_4 to numeric
  data_rows_mbX_4[, -1] <- lapply(data_rows_mbX_4[, -1, drop = FALSE], function(col) as.numeric(col))
  
  # Create a new workbook and add a worksheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  
  # Write the header row as text.
  # We first convert the header row to a vector and then transpose it so that it is written as a row.
  header_vec_mbX_4 <- as.vector(unlist(header_row_mbX_4))
  openxlsx::writeData(wb, sheet = "Sheet1", x = t(header_vec_mbX_4),
                      startRow = 1, startCol = 1, colNames = FALSE)
  
  # Write the data rows starting at row 2.
  openxlsx::writeData(wb, sheet = "Sheet1", x = data_rows_mbX_4,
                      startRow = 2, startCol = 1, colNames = FALSE)
  
  # Save the new workbook as "mbX_cleaning_5.xlsx"
  openxlsx::saveWorkbook(wb, "mbX_cleaning_5.xlsx", overwrite = TRUE)
  
  #cat("Output file 'mbX_cleaning_5.xlsx' has been created in the working directory.\n")
  
  
  
  #####NOW THE BIG PART GETTING THAT OTHER_TAXA EITHER FROM THE THRESHOLD OR THE TOP_TAXA 
  
  
  # Read in "mbX_cleaning_5.xlsx"
  df_clean5 <- openxlsx::read.xlsx("mbX_cleaning_5.xlsx", colNames = TRUE)
  
  # Assume the first column contains taxon names and the remaining columns are numeric.
  # Compute the row averages (ignoring the first column)
  numeric_matrix <- as.matrix(df_clean5[, -1])
  row_avg <- rowMeans(numeric_matrix, na.rm = TRUE)
  
  # Attach the computed averages as a new column (for selection/sorting only)
  df_clean5$RowAvg <- row_avg
  
  # Sort the data frame in descending order by the row average
  df_sorted <- df_clean5[order(-df_clean5$RowAvg), ]
  
  # We'll remove the RowAvg column later.
  # Define the mapping for the aggregated "Other" row names
  other_name_map <- list(
    "d__" = "Other_domains",
    "p__" = "Other_phyla",
    "c__" = "Other_classes",
    "o__" = "Other_orders",
    "f__" = "Other_families",
    "g__" = "Other_genera",
    "s__" = "Other_species"
  )
  
  # Retrieve the proper "Other" name based on level_code
  other_row_name <- other_name_map[[ level_code ]]
  
  # Initialize final data frame variable
  final_df <- NULL
  
  if (!is.null(threshold)) {
    # --- THRESHOLD LOGIC ---
    # Keep rows with average >= threshold
    rows_keep <- df_sorted[df_sorted$RowAvg >= threshold, ]
    # Rows with average below threshold will be aggregated
    rows_agg <- df_sorted[df_sorted$RowAvg < threshold, ]
    
    if (nrow(rows_agg) > 0) {
      # Sum the numeric values (columns 2 to ncol-1, since last column is RowAvg)
      agg_values <- colSums(as.matrix(rows_agg[, 2:(ncol(rows_agg)-1)]), na.rm = TRUE)
      # Create a new row: first column is the Other row name; numeric columns are the aggregated sums.
      agg_row <- data.frame(matrix(ncol = ncol(df_sorted), nrow = 1), stringsAsFactors = FALSE)
      colnames(agg_row) <- colnames(df_sorted)
      agg_row[1, 1] <- other_row_name
      # Fill aggregated numeric values into columns 2 to (ncol-1)
      agg_row[1, 2:(ncol(df_sorted)-1)] <- agg_values
      # For RowAvg column, we can leave as NA (or compute a new average if desired)
      agg_row[1, ncol(df_sorted)] <- NA
      # Combine the kept rows with the aggregated row.
      final_df <- rbind(rows_keep, agg_row)
    } else {
      final_df <- df_sorted
    }
    
  } else if (!is.null(top_taxa)) {
    # --- TOP_TAXA LOGIC ---
    # Select top 'top_taxa' rows based on average
    if (nrow(df_sorted) > top_taxa) {
      rows_keep <- df_sorted[1:top_taxa, ]
      rows_agg <- df_sorted[(top_taxa + 1):nrow(df_sorted), ]
      
      if (nrow(rows_agg) > 0) {
        agg_values <- colSums(as.matrix(rows_agg[, 2:(ncol(rows_agg)-1)]), na.rm = TRUE)
        agg_row <- data.frame(matrix(ncol = ncol(df_sorted), nrow = 1), stringsAsFactors = FALSE)
        colnames(agg_row) <- colnames(df_sorted)
        agg_row[1, 1] <- other_row_name
        agg_row[1, 2:(ncol(df_sorted)-1)] <- agg_values
        agg_row[1, ncol(df_sorted)] <- NA
        final_df <- rbind(rows_keep, agg_row)
      } else {
        final_df <- df_sorted
      }
    } else {
      final_df <- df_sorted
    }
  }
  
  # Remove the helper RowAvg column before writing the output
  final_df$RowAvg <- NULL
  
  # Write the final data frame to "mbX_cleaning_6.xlsx"
  # Define a mapping for visualization data file names based on level_code
  # Define a mapping for the visualization data file names based on level_code
  
  
  vizDataNames <- list(
    "d__" = paste0("mbX_vizualization_data_domains_",  "_", microbiome_base,  "_","by", "_", selected_metadata , ".xlsx"),
    "p__" = paste0("mbX_vizualization_data_phyla_", microbiome_base,  "_","by", "_", selected_metadata , ".xlsx"),
    "c__" = paste0("mbX_vizualization_data_classes_", microbiome_base,  "_","by", "_", selected_metadata , ".xlsx"),
    "o__" = paste0("mbX_vizualization_data_orders_", microbiome_base,  "_","by", "_", selected_metadata , ".xlsx"),
    "f__" = paste0("mbX_vizualization_data_families_",microbiome_base,  "_","by", "_", selected_metadata , ".xlsx"),
    "g__" = paste0("mbX_vizualization_data_genera_", microbiome_base,  "_","by", "_", selected_metadata , ".xlsx"),
    "s__" = paste0("mbX_vizualization_data_species_", microbiome_base,  "_","by", "_", selected_metadata , ".xlsx")
  )
  
  # Determine the output file name based on level_code
  outputVizFile <- vizDataNames[[ level_code ]]
  
  # Write the final data frame to the dynamic file name
  openxlsx::write.xlsx(final_df, file = outputVizFile, rowNames = FALSE)
  #cat("Visualization data file written to: ", file.path(getwd(), outputVizFile), "\n")
  
  # --- Later, when reading the file for plotting ---
  
  # Check if the file exists before reading
  if (!file.exists(outputVizFile)) {
    stop("The file ", outputVizFile, " does not exist in the working directory: ", getwd())
  }
  df_clean6 <- openxlsx::read.xlsx(outputVizFile, colNames = TRUE)
  
  
  
  
  ###LEN FOR THE ROTATION OF THE TEXT IN THE X AXIS
  # Compute maximum header length (excluding the first column header)
  headers_to_check <- names(df_clean6)[-1]
  headers_clean <- gsub("\\s+", "", headers_to_check)  # Remove all whitespace
  max_header_length <- max(nchar(headers_clean), na.rm = TRUE)
  #cat("Maximum header length (excluding the first header) is:", max_header_length, "\n")
  
  # Set axis text parameters based on max_header_length
  if (max_header_length > 9) {
    axis_x_angle <- 65
    axis_x_hjust <- 1.0
  } else {
    axis_x_angle <- 0
    axis_x_hjust <- 0.5
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## THE PLOTTIIINNNGNGNGNGNNGNG
  
  
  
  # Load required packages
  
  
  # --- Dynamic Plotting Code Block using custom colors ---
  
  # Read in the final cleaned data
  
  # Define the mapping for taxonomic levels
  level_names <- list(
    "d__" = "Domains",
    "p__" = "Phyla",
    "c__" = "Classes",
    "o__" = "Orders",
    "f__" = "Families",
    "g__" = "Genera",
    "s__" = "Species"
  )
  
  rm(data)
  gc()
  
  # Assume that the variable 'level_code' is already defined (e.g., "f__" or "g__")
  # Retrieve the proper descriptive name for the level from the mapping
  taxon_descriptor <- level_names[[ level_code ]]
  if (is.null(taxon_descriptor)) {
    stop("Invalid level_code provided.")
  }
  
  # Build dynamic titles for the plot and legend
  plot_title   <- paste("Relative Abundance of Microbial", taxon_descriptor)
  legend_title <- paste("Microorganism", taxon_descriptor)
  
  # Reshape the data from wide to long format
  # The first column is taxon names; all remaining columns represent samples.
  df_long <- df_clean6 %>%
    tidyr::pivot_longer(
      cols = -1,
      names_to = "Sample",
      values_to = "Abundance"
    )
  
  # 2) Build the factor on the taxon column in the same order as they appear in df_clean6[,1]:
  taxa_col       <- names(df_clean6)[1]
  # ensure unique taxa so we don't get duplicate factor‐level errors
  original_levels <- unique(as.character(df_clean6[[taxa_col]]))
  # (this is a vector of taxa in the order they were in the cleaned table)
  
  df_long[[taxa_col]] <- factor(
    df_long[[taxa_col]],
    levels = original_levels
  )
  
  # 3) If flip = TRUE, *reverse* those factor levels on df_long[[taxa_col]]:
  
  flip_flag <- FALSE
  if (is.logical(flip) && flip) {
    flip_flag <- TRUE
  } else if (is.character(flip) && tolower(flip) == "true") {
    flip_flag <- TRUE
  }
  
  # Reverse the stacking order if requested:
  if (flip_flag) {
    df_long[[taxa_col]] <- factor(
      df_long[[taxa_col]],
      levels = rev(levels(df_long[[taxa_col]]))
    )
  }
  
  
  
  
  # 4) Now df_long[[taxa_col]] has either original_levels (flip = FALSE) or reversed (flip = TRUE).
  #    Next you can define your palettes and do ggplot on df_long as before:
  
  tab10 <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  set3 <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
            "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd")
  
  tab10_rep <- rep(tab10, length.out = 100)
  set3_rep  <- rep(set3, length.out = 100)
  custom_colors <- as.vector(rbind(tab10_rep, set3_rep))
  
  n_colors <- length(unique(df_long[[taxa_col]]))
  if(n_colors > length(custom_colors)){
    warning("Number of taxa exceeds custom color length; colors will be recycled.")
    palette_colors <- rep(custom_colors, length.out = n_colors)
  } else {
    palette_colors <- custom_colors[1:n_colors]
  }
  
  p <- ggplot(df_long, aes(x = Sample, y = Abundance, fill = .data[[taxa_col]])) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = palette_colors) +
    guides(fill = guide_legend(ncol = 1)) +
    labs(
      title = plot_title,
      x = selected_metadata,
      y = "Relative Abundance (%)",
      fill = legend_title
    ) +
    theme_bw() +
    theme(
      plot.title  = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.text.x = element_text(angle = axis_x_angle, hjust = axis_x_hjust, size = 12),
      legend.position = "right",
      legend.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 16),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  
  
  # Define output filename dynamically
  
  
  final_names_viz <- list(
    "d__" = paste0("mbX_viz_domains_or_kingdoms_", microbiome_base, "_","by", "_", selected_metadata),
    "p__" = paste0("mbX_viz_phyla_",                     microbiome_base, "_","by", "_", selected_metadata, ".pdf"),
    "c__" = paste0("mbX_viz_classes_",                    microbiome_base, "_","by", "_", selected_metadata, ".pdf"),
    "o__" = paste0("mbX_viz_orders_",                     microbiome_base, "_","by", "_", selected_metadata, ".pdf"),
    "f__" = paste0("mbX_viz_families_",                   microbiome_base, "_","by", "_", selected_metadata, ".pdf"),
    "g__" = paste0("mbX_viz_genera_",                     microbiome_base, "_","by", "_", selected_metadata, ".pdf"),
    "s__" = paste0("mbX_viz_species_",                    microbiome_base, "_","by", "_", selected_metadata, ".pdf")
  )
  output_plot_filename <- paste0(final_names_viz[[ level_code ]])
  
  
  # Save the plot with DPI = 1800 and dimensions adjusted to avoid cropping
  # Calculate the dimensions of df_clean6
  df_clean6_dim <- dim(df_clean6)
  df_clean6_row <- df_clean6_dim[1]
  df_clean6_column <- df_clean6_dim[2]
  
  # Compute plot width:
  # Minimum width is 12; if there are more than 4 columns, add 0.7 for each additional column.
  if (df_clean6_column > 4) {
    plot_width <- 12 + 0.7 * (df_clean6_column - 4)
  } else {
    plot_width <- 12
  }
  
  # Compute plot height:
  # Minimum height is 15; if there are more than 55 rows, add 0.35 for each additional row.
  if (df_clean6_row > 55) {
    plot_height <- 15 + 0.35 * (df_clean6_row - 55)
  } else {
    plot_height <- 15
  }
  
  #cat("Calculated plot width:", plot_width, "and height:", plot_height, "\n")
  
  # Save the plot with dynamic dimensions
  ggsave(
    filename = output_plot_filename,
    plot     = p,
    width    = plot_width,
    height   = plot_height,
    dpi      = 1200
  )
  
  #cat("Output plot '", output_plot_filename, "' has been created.\n")
  
  # Delete temporary cleaning files before closing the function
  ####DELETE TEMP FILE FOR DEBUGGING HERE
  temp_files <- c("mbX_cleaning_1.xlsx", "mbX_cleaning_2.xlsx", 
                  "mbX_cleaning_3.xlsx", "mbX_cleaning_4.xlsx", 
                  "mbX_cleaning_5.xlsx")
  for(file in temp_files) {
    if(file.exists(file)) {
      file.remove(file)
      #cat("Deleted file:", file, "\n")
    } else {
      #cat("File not found (already deleted or never created):", file, "\n")
    }
  }
  
  
  
  
  final_names_viz_dir <- list(
    "d__" = paste0("mbX_viz_domains_or_kingdoms_", microbiome_base),
    "p__" = paste0("mbX_viz_phyla_",                     microbiome_base),
    "c__" = paste0("mbX_viz_classes_",                    microbiome_base),
    "o__" = paste0("mbX_viz_orders_",                     microbiome_base),
    "f__" = paste0("mbX_viz_families_",                   microbiome_base),
    "g__" = paste0("mbX_viz_genera_",                     microbiome_base),
    "s__" = paste0("mbX_viz_species_",                    microbiome_base)
  )
  output_plot_filename_dir <- paste0(final_names_viz_dir[[ level_code ]], ".pdf")
  
  
  viz_folder <- tools::file_path_sans_ext(output_plot_filename_dir)
  if (!dir.exists(viz_folder)) {
    dir.create(viz_folder)
  }
  
  # Move both the Excel and the PDF into that new folder
  #    (outputVizFile is the XLSX, output_plot_filename is the PDF)
  if (file.exists(outputVizFile)) {
    file.rename(outputVizFile, file.path(viz_folder, outputVizFile))
  }
  if (file.exists(output_plot_filename)) {
    file.rename(output_plot_filename, file.path(viz_folder, output_plot_filename))
  }
  
  # ── REMOVE THE EZCLEAN OUTPUT FROM WORKING DIRECTORY ──
  if (exists("cleaned_basename") && file.exists(cleaned_basename)) {
   file.remove(cleaned_basename)
}
  
  message("ezviz outputs have been saved to: ", viz_folder)
  message("Done with the vizualization, cite us!")
  message('Please cite us: "Lamichhane, U., & Lourenco, J. (2025). mbX: An R Package for Streamlined Microbiome Analysis. Stats, 8(2), 44."')
  message("DOI to paper: https://doi.org/10.3390/stats8020044")
  
  message("*****PLEASE IGNORE THE FOLLOWING WARNING MESSAGE - In rm(data) : object 'data' not found, warning message!*****")
  
}
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("SampleID", "Taxonomy", "Value", "data", "Sample", "Abundance"))
}


#ezviz("level-7.csv", "metadata.txt", "f", "sample_type", top_taxa = 20, threshold = 11) 
#ezviz("level-7.csv", "metadata.txt", "s", "sample_type2", 30) 
#ezviz("level-7.csv", "metadata.txt", "f", "sample_type2", threshold = 1) 


#' Statistical Analysis and Visualization of Microbiome Data
#'
#' Performs Kruskal_Wallis tests, post_hoc Dunn comparisons, Compact Letter Display (CLD)
#' summaries, and generates boxplots annotated with CLD letters for taxa abundances
#' grouped by a chosen metadata variable.
#'
#' @param microbiome_data Character; path to the microbiome abundance table (CSV, TSV, XLS, or XLSX).
#' @param metadata        Character; path to the sample metadata file (CSV, TXT, XLS, or XLSX).
#' @param level           Character; taxonomic rank to aggregate at (e.g. "genus", "g").
#' @param selected_metadata Character; name of the categorical metadata column to group by.
#'
#' @return Invisibly returns the \code{data.frame} of cleaned sample_taxa abundances used for all analyses.
#'
#' @details
#' This function first calls ezclean to produce a cleaned, merged table of sample IDs,
#' metadata, and taxa abundances at the requested taxonomic level.  It then:
#' \enumerate{
#'   \item Runs Kruskal_Wallis tests on each taxon and writes results with FDR_correction.
#'   \item Performs Dunns pairwise post_hoc tests (BH_adjusted) for taxa with KW p less than or equal to 0.05.
#'   \item Computes CLD letters for significantly different groups and writes a summary Excel.
#'   \item Generates high-resolution (900 dpi) boxplots annotated with CLD letters.
#' }
#'
#' @examples
#' \dontrun{
#'   mb  <- system.file("extdata", "microbiome.csv", package = "mbX")
#'   md  <- system.file("extdata", "metadata.csv",   package = "mbX")
#'   if (nzchar(mb) && nzchar(md)) {
#'     ezstat(mb, md, "genus", "Group")
#'   }
#' }
#'
#' @importFrom stats kruskal.test p.adjust
#' @importFrom utils read.delim read.csv
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom readxl read_excel
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom stats median
#' @importFrom tibble deframe
#' @import dplyr
#' @import rstatix
#' @import FSA
#' @import tibble
#' @import multcompView
#' @import ggplot2
#' @export
ezstat <- function(microbiome_data, metadata, level, selected_metadata) {
  #
  #
  levels_map <- c(
    "domain"  = "d__", "Domain"  = "d__", "DOMAIN"  = "d__", "D" = "d__", "d" = "d__",
    "kingdom" = "d__", "Kingdom" = "d__", "KINGDOM" = "d__", "K" = "d__", "k" = "d__",
    "phylum"  = "p__", "Phylum"  = "p__", "PHYLUM"  = "p__", "P" = "p__", "p" = "p__",
    "class"   = "c__", "Class"   = "c__", "CLASS"   = "c__", "C" = "c__", "c" = "c__",
    "order"   = "o__", "Order"   = "o__", "ORDER"   = "o__", "O" = "o__", "o" = "o__",
    "family"  = "f__", "Family"  = "f__", "FAMILY"  = "f__", "F" = "f__", "f" = "f__",
    "genera"  = "g__", "genus"   = "g__", "Genera"  = "g__", "GENERA" = "g__", "G" = "g__", "g" = "g__",
    "species" = "s__", "Species" = "s__", "SPECIES" = "s__", "S" = "s__", "s" = "s__"
  )
  tax_code <- levels_map[tolower(level)]
  if (is.na(tax_code)) {
    stop("Invalid taxonomic level.  Please choose one of: domain, phylum, class, order, family, genus, or species (or their abbreviations).")
  }
  code_to_name <- c(
    "d__" = "domain",
    "p__" = "phylum",
    "c__" = "class",
    "o__" = "order",
    "f__" = "family",
    "g__" = "genus",
    "s__" = "species"
  )
  canonical_level <- code_to_name[[tax_code]]
  
  #
  #  2) Run ezclean() and read its output **in place** (no moving).
  #
  cleaned_path <- ezclean(microbiome_data, metadata, level)
  if (!file.exists(cleaned_path)) {
    stop("ezclean() did not return a valid file path.  Got: ", cleaned_path)
  }
  
  cleaned_df <- read.xlsx(cleaned_path, colNames = TRUE, check.names = FALSE)
  
  #
  #  3) Load the original metadata (to check selected_metadata is valid & categorical).
  #
  meta_ext <- tools::file_ext(metadata)
  if (meta_ext == "txt") {
    metadata_df <- utils::read.delim(metadata, sep = "\t", header = TRUE, check.names = FALSE)
  } else if (meta_ext == "csv") {
    metadata_df <- read.csv(metadata, header = TRUE, check.names = FALSE)
  } else if (meta_ext %in% c("xls", "xlsx")) {
    metadata_df <- readxl::read_excel(metadata, col_names = TRUE) |> as.data.frame(check.names = FALSE)
  } else {
    stop("Please check the file format of 'metadata'.")
  }
  
  if (!(selected_metadata %in% colnames(metadata_df))) {
    stop("'", selected_metadata, "' is not a column of the metadata.")
  }
  if (!(is.factor(metadata_df[[selected_metadata]]) || is.character(metadata_df[[selected_metadata]]))) {
    stop("The selected metadata ('", selected_metadata, "') is not categorical.")
  }
  
  
  
  # — sanitize metadata column names & values —
  # replace spaces in column names
  colnames(metadata_df) <- gsub("\\s+", "_", colnames(metadata_df))
  
  # for any character columns, replace spaces in the values
  char_cols <- sapply(metadata_df, is.character)
  metadata_df[ , char_cols] <- lapply(
    metadata_df[ , char_cols, drop = FALSE],
    function(x) gsub("\\s+", "_", x)
  )
  
  # for any factor columns, replace spaces in the levels
  factor_cols <- sapply(metadata_df, is.factor)
  metadata_df[ , factor_cols] <- lapply(
    metadata_df[ , factor_cols, drop = FALSE],
    function(x) factor(gsub("\\s+", "_", as.character(x)))
  )
  
  
  
  
  
  
  
  
  
  
  #
  #  4) Identify which metadata columns survived in cleaned_df; build subset:
  #
  meta_cols_in_cleaned <- intersect(colnames(metadata_df), colnames(cleaned_df))
  if (!selected_metadata %in% meta_cols_in_cleaned) {
    stop("After cleaning, '", selected_metadata, "' was not found in the cleaned file.")
  }
  
  #########new line added jul12
  
  cleaned_df[[selected_metadata]] <- gsub("-", "_", cleaned_df[[selected_metadata]])
  
  #########new line added jul12
  
  sample_id_col <- colnames(cleaned_df)[1]
  taxa_cols     <- setdiff(colnames(cleaned_df), meta_cols_in_cleaned)
  cols_to_keep  <- c(sample_id_col, selected_metadata, taxa_cols)
  stat_df       <- cleaned_df[, cols_to_keep, drop = FALSE]
  
  #
  #  5) Create output directory "ezstat_outputs_<canonical_level>_<microbiome_base>"
  #
  microbiome_base <- tools::file_path_sans_ext(basename(microbiome_data))
  dir_name        <- paste0("mbX_stats_", canonical_level, "_", microbiome_base)
  if (!dir.exists(dir_name)) dir.create(dir_name)
  
  #
  #  6) Write "mbX_ezstat_1.xlsx" 
  #
  out1 <- file.path(dir_name, "mbX_ezstat_1.xlsx")
  openxlsx::write.xlsx(stat_df, file = out1, rowNames = FALSE)
  message("ezstat wrote: ", out1)
  
  #
  #  7) Kruskal_Wallis on taxa columns (3rd onward) grouped by selected_metadata
  #
  
  df_stat <- stat_df
  if (is.character(df_stat[[selected_metadata]])) {
    df_stat[[selected_metadata]] <- factor(df_stat[[selected_metadata]])
  }
  
  taxa_names <- names(df_stat)[3:ncol(df_stat)]
  test_stats <- numeric(length(taxa_names))
  p_values   <- numeric(length(taxa_names))
  
  for (i in seq_along(taxa_names)) {
    this_taxon <- taxa_names[i]
    vals  <- df_stat[[this_taxon]]
    grps  <- df_stat[[selected_metadata]]
    if (length(unique(grps[!is.na(vals)])) < 2) {
      test_stats[i] <- NA
      p_values[i]   <- NA
    } else {
      kt            <- kruskal.test(vals ~ grps)
      test_stats[i] <- as.numeric(kt$statistic)
      p_values[i]   <- kt$p.value
    }
  }
  FDR_pvals <- p.adjust(p_values, method = "fdr")
  
    results_kw <- data.frame(
    taxa           = taxa_names,
    teststat_value = test_stats,
    P_value        = p_values,
    FDR_P_value    = FDR_pvals,
    stringsAsFactors = FALSE
  )

  # ── Drop the FDR_P_value column *only* from the file you write ──
  results_kw_to_save <- results_kw[, c("taxa","teststat_value","P_value")]

  out_kw <- file.path(
    dir_name,
    paste0("ezstat_KW_tests_", canonical_level, "_","by", "_", selected_metadata, ".xlsx")
  )
  openxlsx::write.xlsx(results_kw_to_save, file = out_kw, rowNames = FALSE)
  message(" ezstat wrote: ", out_kw)

  
  #
  #  9) Pairwise Dunns comparisons for any taxon with KW P_value less than or equals to 0.05
  #
  
  kw_df    <- openxlsx::read.xlsx(out_kw, colNames = TRUE, check.names = FALSE)
  sig_taxa <- kw_df %>%
    filter(!is.na(P_value) & P_value <= 0.05) %>%
    pull(taxa)
  
  dunn_list <- list()
  for (tax in sig_taxa) {
    temp <- df_stat %>%
      select(!!sym(selected_metadata), !!sym(tax)) %>%
      rename(Group = !!sym(selected_metadata), Value = !!sym(tax)) %>%
      mutate(Value = as.numeric(Value)) %>%
      filter(!is.na(Value))
    
    if (n_distinct(temp$Group) < 2) next
    
    dunn_res <- temp %>%
      dunn_test(Value ~ Group, p.adjust.method = "BH") %>%
      rename(
        Comparison1  = group1,
        Comparison2  = group2,
        statistic    = statistic,
        P_value      = p,
        FDR_P_value  = p.adj
      ) %>%
      mutate(
        Comparison = paste0(Comparison1, "_vs_", Comparison2),
        taxa       = tax
      ) %>%
      select(taxa, Comparison, statistic, P_value, FDR_P_value)
    
    dunn_list[[tax]] <- dunn_res
  }
  
  pairwise_df <- bind_rows(dunn_list)
  out_pw <- file.path(
    dir_name,
    paste0("ezstat_pairwise_", canonical_level, "_","by", "_", selected_metadata, ".xlsx")
  )
  openxlsx::write.xlsx(pairwise_df, file = out_pw, rowNames = FALSE)
  message("ezstat wrote: ", out_pw)
  
  #
  # 10) CLD_Summary: for each taxon in sig_taxa, run FSA::dunnTest( ) to build a p vector
  #
  
  
  cld_list <- list()
  for (tax in sig_taxa) {
    temp_df <- df_stat %>%
      select(!!sym(selected_metadata), !!sym(tax)) %>%
      rename(Group = !!sym(selected_metadata), Value = !!sym(tax)) %>%
      mutate(Value = as.numeric(Value)) %>%
      filter(!is.na(Value))
    
    if (n_distinct(temp_df$Group) < 2) next
    
    # 10.1) Compute per_Group median & mean
    summary_stats <- temp_df %>%
      group_by(Group) %>%
      summarise(
        Median = median(Value, na.rm = TRUE),
        Mean   = mean(Value,   na.rm = TRUE),
        .groups = "drop"
      )
    
        # 10.2) Read in your pairwise-Dunn results and extract the BH‐adjusted p‐values
    pw_file <- file.path(
      dir_name,
      paste0("ezstat_pairwise_", canonical_level, "_","by", "_", selected_metadata, ".xlsx")
    )
    pw_df <- openxlsx::read.xlsx(pw_file, colNames = TRUE, check.names = FALSE) %>%
             mutate(Comparison = gsub("\\s+", "", Comparison))

    # For this taxon, grab its comparisons and FDR_P_value
    tax_pw <- pw_df %>% filter(taxa == tax)
	pval_vec <- setNames(tax_pw$FDR_P_value, gsub("_vs_", "-", tax_pw$Comparison))

    groups_present <- sort(unique(temp_df$Group))
    n_groups       <- length(groups_present)

    if (n_groups == 2) {
      # two‐group special case: only one adjusted p‐value in pval_vec
      padj    <- unname(pval_vec)[1]
      letters <- if (padj <= 0.05) c("a","b") else c("a","a")
      cld_df <- data.frame(
        Group = groups_present,
        CLD   = letters,
        stringsAsFactors = FALSE
      )
    } else {
      needed <- choose(n_groups, 2)
      if (length(pval_vec) == needed) {
        cld_out <- multcompLetters(pval_vec)
        cld_df   <- data.frame(
          Group = names(cld_out$Letters),
          CLD   = unname(cld_out$Letters),
          stringsAsFactors = FALSE
        )
      } else {
        # fallback if your pairwise file is missing any comparisons
        cld_df <- data.frame(
          Group = groups_present,
          CLD   = rep("a", n_groups),
          stringsAsFactors = FALSE
        )
      }
    }

    
    
    
    # 10.5) Merge summary_stats and CLD letters, then reorder/rename
    merged_cld <- summary_stats %>%
      left_join(cld_df, by = "Group") %>%
      mutate(
        taxa                 = tax,
        !!selected_metadata  := Group
      ) %>%
      select(
        taxa,
        !!selected_metadata,
        Median,
        Mean,
        Group = CLD
      )
    
    cld_list[[tax]] <- merged_cld
  }
  
  cld_summary_df <- bind_rows(cld_list)
  out_cld <- file.path(
    dir_name,
    paste0("ezstat_CLD_Summary_", canonical_level, "_", "by", "_", selected_metadata, ".xlsx")
  )
  openxlsx::write.xlsx(cld_summary_df, file = out_cld, rowNames = FALSE)
  message("ezstat wrote: ", out_cld)
  
  #
  # 11) Generate Boxplots for each taxon with KW P_value less than or equals to  0.05, annotated with CLD letters
  #
  
  # Create a subdirectory "Boxplots_<selected_metadata>" inside our ezstat_outputs folder
  box_dir <- file.path(dir_name, paste0("Boxplots_", selected_metadata))
  if (!dir.exists(box_dir)) dir.create(box_dir)
  
  # Re_read the Kruskal_Wallis results to identify significant taxa
  kw_df    <- openxlsx::read.xlsx(out_kw, colNames = TRUE, check.names = FALSE)
  sig_taxa <- kw_df %>%
    filter(!is.na(P_value) & P_value <= 0.05) %>%
    pull(taxa)
  
  # Re_read the CLD summary table so we know which letter goes to which group
  cld_summary_df <- openxlsx::read.xlsx(out_cld, colNames = TRUE, check.names = FALSE)
  # cld_summary_df has columns: taxa, <selected_metadata>, Median, Mean, Group (where Group is the CLD letter)
  
  for (tax in sig_taxa) {
    # 11.1) Build a small data.frame of (Group, Value) for this taxon
    temp_df <- df_stat %>%
      select(!!sym(selected_metadata), !!sym(tax)) %>%
      rename(Group = !!sym(selected_metadata), Value = !!sym(tax)) %>%
      mutate(Value = as.numeric(Value)) %>%
      filter(!is.na(Value))
    
    # Skip if fewer than two distinct groups remain
    if (n_distinct(temp_df$Group) < 2) next
    
    # 11.2) Extract the CLD letters for this taxon
    letter_df <- cld_summary_df %>%
      filter(taxa == tax) %>%
      select(!!sym(selected_metadata), Group) %>%
      rename(Group = !!sym(selected_metadata), Letter = Group)
    # Now `letter_df` has columns: Group (the category) and Letter (the CLD letter)
    
    # 11.3) Compute a reasonable y_position for each group's letter (a bit above the boxplot)
    y_pos_df <- temp_df %>%
      group_by(Group) %>%
      summarise(y_max = max(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(y = ifelse(y_max == 0, 0.001, y_max + 0.05 * y_max))
    # If the groups max is 0, we nudge it to 0.001 so the letter is visible
    
    # 11.4) Combine letter positions
    letter_pos <- letter_df %>%
      left_join(y_pos_df %>% select(Group, y), by = "Group")
    
    # 11.5) Build the boxplot
    p <- ggplot(temp_df, aes(x = Group, y = Value)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.5, size = 0.8) +
      geom_text(
        data = letter_pos,
        aes(x = Group, y = y, label = Letter),
        vjust = 0,
        size = 5,
        fontface = "bold"
      ) +
      labs(
        title = paste0("Abundance of ", tax, " by ", selected_metadata),
        x = selected_metadata,
        y = "Relative Abundance"
      ) +
      theme_bw() +
      theme(
        plot.title  = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face = "bold")
      )
    
    # 11.6) Save as a 900 DPI PNG file
    out_png <- file.path(box_dir, paste0(tax, ".png"))
    ggsave(
      filename = out_png,
      plot     = p,
      dpi      = 900,
      width    = 6,
      height   = 4,
      units    = "in"
    )
  }
  
  message("ezstat wrote boxplots to: ", box_dir)
  message("All ezstat outputs are in: ", dir_name)
  
  message('Please cite us: "Lamichhane, U., & Lourenco, J. (2025). mbX: An R Package for Streamlined Microbiome Analysis. Stats, 8(2), 44."')
  message("DOI to paper: https://doi.org/10.3390/stats8020044")
  
  
  #if (getRversion() >= "2.15.1") {
    #utils::globalVariables(c(
     # "P_value", "taxa", "group1", "group2", "statistic",
      #"p.adj", "Comparison1", "Comparison2", "Comparison",
      #"FDR_P_value", "Group", "P.adj", "Median", "Mean",
      #"CLD", "y_max", "y", "Letter", ":="
    #))
  #}
  
  #remove that file
  if (file.exists(out1)) file.remove(out1)
  
  return(invisible(stat_df))
}




#ezstat("microbiome10.csv", "metadata10.csv", "g", "check")
#ezviz("microbiome10.csv", "metadata10.csv", "g", "check", top_taxa =10)
#ezclean("microbiome10.csv", "metadata10.csv", "g")



