#' Prepare Survey/Questionnaire Data for Likert Plotting
#'
#' This function reshapes a survey/questionnaire dataframe (questions in columns and responses in rows)
#' into the format required by `HH::likert()` for creating Likert-type plots.
#' It handles missing values according to the specified options and allows customization of response level ordering.
#'
#' @param data A dataframe containing the questionnaire data.
#' @param question_cols A vector of column names representing the questionnaire items to transform for plotting.
#' @param na_action A character string to specify how to handle missing values (`NA`s). Use "omit" to exclude `NA`s,
#'   or "as_category" to treat them as a separate category.
#' @param response_levels_in_order A character vector specifying the ordered levels of responses, ensuring the desired order in the plot.
#' @param na_category An optional character string to label `NA` values when `na_action` is set to "as_category". Default is "No response".
#'
#' @return A dataframe(tibble) reshaped for compatibility with `HH::likert()`, with response levels and missing values managed as specified.
#' @import dplyr
#' @import tidyr
#' @examples
#' # Example usage:
#' # data <- your_dataframe
#' # result <- prepare_likert_data(data, question_cols = c("Q1", "Q2"), na_action = "as_category",
#' #                                response_levels_in_order = c("Strongly disagree", "Disagree", "No response",
#' #                                                             "Agree", "Strongly agree"),
#' #                                na_category = "No response")
#' @export
prepare_likert_data <- function(data, question_cols, na_action = "omit",
                                      response_levels_in_order, na_category = NULL) {

  library(dplyr)
  library(tidyr)
  library(tidyverse)

  ## Error handling

  # Check if response_levels_in_order is provided, otherwise, stop execution
  if (missing(response_levels_in_order) || is.null(response_levels_in_order)) {
    stop("Error: 'response_levels_in_order' must be provided with a valid order of response levels.")
  }

  # Verify that na_action is either "omit" or "as_category"
  if (!na_action %in% c("omit", "as_category")) {
    stop("Invalid na_action. Choose 'omit' or 'as_category'.")
  }

  # Ensure na_category is NULL if na_action is set to "omit"
  if (na_action == "omit" && !is.null(na_category)) {
    stop("'na_category' must be NULL when 'na_action' is 'omit'. Use 'na_action = \"as_category\"' to retain NA values.")
  }

  ## Prepare unique response values for validation

  # Extract unique values from question columns, excluding NAs if na_action = "omit"
  unique_responses <- data |>
    dplyr::select(all_of(question_cols)) |>
    pivot_longer(cols = everything(), values_drop_na = (na_action == "omit")) |>
    distinct(value) |>          # Get distinct responses only
    pull(value)                 # Pull values in their original type

  # Add na_category to unique responses if na_action = "as_category" and na_category is not NULL
  if (na_action == "as_category" && !is.null(na_category)) {
    unique_responses <- c(as.character(unique_responses), na_category)
    unique_responses <- unique_responses[!is.na(unique_responses)]
  }

  # Ensure that unique responses in the data match the specified response_levels_in_order
  if (length(setdiff(unique_responses, response_levels_in_order)) > 0 ||
      length(setdiff(response_levels_in_order, unique_responses)) > 0) {
    stop("Error: Your 'response_levels_in_order' does not match the data.")
  }

  ## Data Transformation

  # Convert specified question columns to factors, then pivot to long format
  long_data <- data |>
    mutate(across(all_of(question_cols), ~ as.factor(.))) |>
    pivot_longer(cols = all_of(question_cols),
                 names_to = "Question",
                 values_to = "Response")

  # Handle NA values based on na_action argument
  if (na_action == "omit") {
    # Remove rows with NA in the Response column
    long_data <- long_data |>
      filter(!is.na(Response))
  } else if (na_action == "as_category") {
    # Assign a default label to NA values if na_category is not specified
    if (is.null(na_category)) {
      na_category <- "No response"
    }
    # Replace NA with na_category label in Response
    long_data <- long_data |>
      mutate(Response = fct_explicit_na(Response, na_level = as.character(na_category)))
  }

  # Convert Response column to a factor with the specified levels in response_levels_in_order
  long_data <- long_data |>
    mutate(Response = factor(Response, levels = response_levels_in_order))

  # Summarize counts by Question and Response, and calculate percentage within each Question
  summarized_data <- long_data |>
    group_by(Question, Response) |>
    summarise(Count = n(), .groups = "drop") |>
    group_by(Question) |>
    mutate(Percentage = Count / sum(Count) * 100) |>
    dplyr::select(Question, Response, Percentage)

  # Pivot data to wide format for compatibility with HH::likert
  final_data <- summarized_data |>
    pivot_wider(names_from = Response, values_from = Percentage, values_fill = 0)

  return(final_data)
}

