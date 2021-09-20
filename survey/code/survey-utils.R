library(readr)
library(purrr)
library(dplyr)

#' Fetch all survey data in a chosen directory.
#'
#' This function should be used on the monthly rollup CSVs available to data
#' users, with names like YYYY-MM.csv.gz.
#'
#' This function extracts the date from each file and produces a single data
#' frame representing the most recent data available for each day. It can read
#' gzip-compressed CSV files, such as those on the SFTP site, using
#' `readr::read_csv`.
#'
#' @param directory Directory in which to look for survey CSV files, relative to
#'   the current working directory.
#' @param pattern Regular expression indicating which files in that directory to
#'   open. By default, selects all `.csv.gz` files, such as those downloaded
#'   from the SFTP site.
#' @return A single data frame containing all survey responses. Note that this
#'   data frame may have millions of rows and use gigabytes of memory, if this
#'   function is run on *all* survey responses.
get_survey_df <- function(directory, pattern = "*.csv.gz$") {
  files <- list.files(directory, pattern = pattern)

  big_df <- map_dfr(
    files,
    function(f) {
      # stop readr from thinking commas = thousand separators,
      # and from inferring column types incorrectly
      read_csv(file.path(directory, f),
               locale = locale(grouping_mark = ""),
               col_types = cols(
                 A2b = col_number(),
                 A3 = col_character(),
                 B2 = col_character(),
                 B2_14_TEXT = col_character(),
                 B2c = col_character(),
                 B2c_14_TEXT = col_character(),
                 B7 = col_character(),
                 B10b = col_character(),
                 B12a = col_character(),
                 V5a = col_character(),
                 V5b = col_character(),
                 V5c = col_character(),
                 V5d = col_character(),
                 V6 = col_character(),
                 C1 = col_character(),
                 C13 = col_character(),
                 C13a = col_character(),
		 C13b = col_character(),
                 D1_4_TEXT = col_character(),
                 D7 = col_character(),
                 fips = col_character(),
                 UserLanguage = col_character(),
                 StartDatetime = col_character(),
                 EndDatetime = col_character(),
                 .default = col_number()))
    }
  )
  return(big_df)
}

#' Split multiselect options into codable form
#'
#' Multiselect options are coded by Qualtrics as a comma-separated string of
#' selected options, like "1,14", or the empty string if no options are
#' selected. Split these into vectors of selected options.
#'
#' @param column vector of selections, like c("1,4", "5", ...)
#' @return list of same length, each entry of which is a vector of selected
#'   options
split_options <- function(column) {
  return(strsplit(column, ",", fixed = TRUE))
}

#' Test if a specific selection is selected
#'
#' @param vec A list whose entries are character vectors, such as c("14", "15").
#' @param selection one string, such as "14"
#' @return a logical vector; for each list entry, whether selection is contained
#'   in the character vector.
is_selected <- function(vec, selection) {
  selections <- sapply(
    vec,
    function(resp) {
      if (length(resp) == 0 || all(is.na(resp))) {
        # All our selection items include "None of the above" or similar, so
        # treat no selection the same as missingness.
        NA
      } else {
        selection %in% resp
      }
    })

  return(selections)
}
