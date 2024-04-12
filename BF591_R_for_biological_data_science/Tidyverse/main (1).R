library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
# library(purrr)


# ----------------------- Helper Functions to Implement ------------------------

#' Read the expression data "csv" file.
#'
#' Function to read microarray expression data stored in a csv file. The
#' function should return a sample x gene tibble, with an extra column named
#' "subject_id" that contains the geo accession ids for each subject.
#'
#' @param filename (str): the file to read.
#'
#' @return
#' @export
#'
#' @examples expr_mat <- read_expression_table('example_intensity_data_subset.csv')
read_expression_table <- function(filename) { 

  # read the csv file
expr_data <- read.csv("data/example_intensity_data_subset.csv", header=TRUE, sep=",", row.names = 1)
  
expr_tdata <-   t(expr_data) %>%
    as_tibble(rownames = "subject_id")

}



#' Load Metadata from Specified CSV File
#'
#' This function reads the provided CSV file into a dataframe.
#'
#' @param filepath (character) The path to the CSV file.(data/proj_metadata.csv)
#'
#' @return A dataframe containing the loaded metadata.
#'

metadata <- function(filepath) { read_csv("data/proj_metadata.csv")
  # TODO: Use appropriate function to read in the CSV file
  #metadata <-   
    
    # Return the loaded metadata
    return(metadata)
}






#' Replaces all '.' in a string with '_'
#'
#' @param str String to operate upon.
#'
#' @return reformatted string.
#' @export
#'
#' @examples
#' period_to_underscore("foo.bar")
#' "foo_bar"
period_to_underscore <- function(str) { 
  

sub <- gsub(".", "_", str, fixed = TRUE)
 
return(sub)
 
}


# rename variables:
# Age_at_diagnosis to Age
# SixSubtypesClassification to Subtype
# normalizationcombatbatch to Batch

#' Rename and select specified columns.
#'
#' Function to rename Age_at_diagnosis, SixSubtypesClassification, and
#' normalizationcombatbatch columns to Age, Subtype, and Batch, respectively. A
#' subset of the data should be returned, containing only the Sex, Age, TNM_Stage,
#' Tumor_Location, geo_accession, KRAS_Mutation, Subtype, and Batch columns.
#'
#' @param data (tibble) metadata information for each sample
#'
#' @return (tibble) renamed and subsetted tibble
#' @export
#'
#' @examples rename_and_select(metadata)
#' 
#' 
rename_and_select <- function(data) {
  
  output <- data %>%
    rename(
      Age = Age_at_diagnosis,
      Subtype = SixSubtypesClassification,
      Batch = normalizationcombatbatch
    ) %>%
    select(
      Sex, Age, TNM_Stage, Tumor_Location, geo_accession,
      KRAS_Mutation, Subtype, Batch
    )
  select_tibble <- as_tibble(output)
  return(select_tibble)

}


#' Create new "Stage" column containing "stage " prefix.
#'
#' Creates a new column "Stage" with elements following a "stage x" format, where
#' `x` is the cancer stage data held in the existing TNM_Stage column. Stage
#' should have a factor data type.
#'
#' @param data  (tibble) metadata information for each sample
#'
#' @return (tibble) updated metadata with "Stage" column
#' @export
#'
#' @examples metadata <- stage_as_factor(metadata)
stage_as_factor <- function(data) {
 
  data <- data %>%
    mutate(Stage = factor(paste0("stage ", TNM_Stage)))
  
  return(data)
}
  
  

  
  
  
  
  



#' Calculate age of samples from a specified sex.
#'
#' @param data (tibble) metadata information for each sample
#' @param sex (str) which sex to calculate mean age. Possible values are "M"
#' and "F"
#'
#' @return (float) mean age of specified samples
#' @export
#'
#' @examples mean_age_by_sex(metadata, "F")
mean_age_by_sex <- function(data, sex) {
  filtered_data <- dplyr::filter(data, Sex == sex)
  
# Calculate the mean age for the filtered samples
  mean_age <- mean(filtered_data$Age)
# need to convert into a numeric value.
  return(as.numeric(mean_age))  
}




#' Calculate average age of samples within each cancer stage. Stages should be
#' from the newly created "Stage" column.
#'
#' @param data (tibble) metadata information for each sample
#'
#' @return (tibble) summarized tibble containing average age for all samples from
#' each stage. Name the newly created column containing the average, 'mean_avg'
#' @export
#'
#' @examples age_by_stage(data)
age_by_stage <- function(data) {
  age_by_stage_output <- data %>%
    group_by(Stage) %>%
    summarise(mean_avg = mean(Age, na.rm = TRUE, digits =1))
  
 return(age_by_stage_output)
  
}

#' Create a cross tabulated table for Subtype and Stage using dplyr methods.
#'
#' @param data (tibble) metadata information for each sample
#'
#' @return (tibble) table where rows are the cancer stage of each sample, and the
#' columns are each cancer subtype. Elements represent the number of samples from
#' the corresponding stage and subtype. If no instances of a specific pair are
#' observed, a zero entry is expected.
#' @export
#'
#' @examples cross_tab <- dplyr_cross_tab(metadata)
subtype_stage_cross_tab <- function(data) {
  
  # using summarise and group by to create a cross tab, 
  subtype_cross_tab <- data %>%
    group_by(Stage, Subtype) %>%
    summarise(Count = n()) %>%
    pivot_wider(names_from = Subtype, values_from = Count, values_fill = 0)
  
  return(subtype_cross_tab)
}

#' Summarize average expression and probe variability over expression matrix.
#'
#' @param exprs An (n x p) expression matrix, where n is the number of samples,
#' and p is the number of probes.
#'
#' @return A summarized tibble containing `main_exp`, `variance`, and `probe`
#' columns documenting average expression, probe variability, and probe ids,
#' respectively.
summarize_expression <- function(exprs) {

  # calculating the mean expression and only grabbing the names of numeric columns which would be 
  mean_exp <- colMeans(exprs[sapply(exprs, is.numeric)])
  
 
 # putting it into a tibble the lazy way
  tibbled <- tibble( 
    mean_exp = mean_exp,
    probe = colnames(exprs[sapply(exprs,is.numeric)])# grabbing column names and only grabbing the columns that have numerics.
  )
  # variance was extra as labeled in the report.
  return(tibbled)
  
  
}

