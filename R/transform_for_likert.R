#' Transform data for Likert Plot
#'
#' This function transforms a questionnaire dataframe into a format compatible with `HH::likert()` for Likert-type plots.
#'
#' @param data A dataframe containing the questionnaire data.
#' @param question_cols A vector of column names to transform.
#' @param na_action Character string to specify NA handling, either "omit" to remove NAs or "as_category" to treat NA as a category.
#' @param response_levels_in_order A character vector with response levels in the desired order.
#' @param na_category A character string specifying the label to replace NA values when `na_action` is "as_category".
#'
#' @return A transformed dataframe compatible with `HH::likert()`.
#' @import dplyr
#' @import tidyr
#' @examples
#' # Example usage:
#' # data <- your_dataframe
#' # result <- transform_for_likert(data, question_cols = c("Q1", "Q2"), na_action = "as_category", response_levels_in_order = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))
#' @export
transform_for_likert <- function(data, question_cols, na_action = "omit",
                                 response_levels_in_order, na_category = NULL) {

  # Check if na_action is valid
  if (!na_action %in% c("omit", "as_category")) {
    stop("Invalid na_action. Choose 'omit' or 'as_category'.")
  }

  # Pivot longer to get data into long format
  long_data <- data |>
    pivot_longer(cols = all_of(question_cols),
                 names_to = "Question",
                 values_to = "Response")
  cat("Checkpoint 1:")
  print(long_data)

  # Handle missing values
  if (na_action == "omit") {
    long_data <- long_data |>
      filter(!is.na(Response))
  } else if (na_action == "as_category") {
    # Set a default value for na_category if none is provided
    if (is.null(na_category)) {
      na_category <- "No response"
    }
    long_data <- long_data |>
      mutate(Response = fct_explicit_na(Response, na_level = na_category))
  }
  cat("Checkpoint 2:")
  print(long_data)

  long_data <- long_data |>
    mutate(Response = factor(Response, levels = response_levels_in_order))

  cat("Checkpoint 3:")
  print(long_data)

  # Group by question and response, then summarize to get counts
  summarized_data <- long_data |>
    group_by(Question, Response) |>
    summarise(Count = n(), .groups = "drop") |>
    group_by(Question) |>
    mutate(Percentage = Count / sum(Count) * 100) |>
    dplyr::select(Question, Response, Percentage)

  cat("Checkpoint 4:")
  print(summarized_data)

  # Pivot wider to get the data into the required format for HH::likert
  final_data <- summarized_data |>
    pivot_wider(names_from = Response, values_from = Percentage, values_fill = 0)

  cat("Checkpoint final (pre-rename):")
  print(final_data)

  return(final_data)
}
