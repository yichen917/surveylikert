#' Generate Likert Plot Directly from Survey/Questionnaire Data
#'
#' This function transforms questionnaire data to a format compatible with `HH::likert()`
#' and then creates a Likert plot for visualizing responses.
#'
#' @param data A dataframe containing the questionnaire data.
#' @param question_cols A vector of column names representing the questionnaire items to transform and plot.
#' @param response_levels_in_order A character vector specifying the ordered levels of responses for consistent plotting.
#' @param na_action A character string to specify how to handle missing values (`NA`s): "omit" to exclude them,
#'   or "as_category" to treat them as a separate category.
#' @param na_category An optional character string specifying a label for `NA` values when `na_action` is set to "as_category". Default is "No response".
#' @param main A character string for the main title of the plot.
#' @param xlab A character string for the x-axis label of the plot. Default is "Percentage".
#' @param ylab A character string for the y-axis label of the plot. Default is NULL.
#' @param positive.order Logical. If TRUE, orders questions by the percentage of positive responses (e.g., "Agree" or "Strongly Agree").
#' @return A Likert plot created with `HH::likert()`.
#' @import dplyr
#' @import tidyr
#' @importFrom HH likert
#' @examples
#' # Example usage:
#' # plot_likert(data = your_dataframe, question_cols = c("Q1", "Q2"),
#' #             response_levels_in_order = c("Strongly disagree", "Disagree",
#' #                                          "No response", "Agree", "Strongly agree"),
#' #             na_action = "as_category", na_category = "No response",
#' #             main = "Likert Plot of Responses", xlab = "Percentage", positive.order = TRUE)
#' @export
plot_likert <- function(data, question_cols, response_levels_in_order, na_action = "omit",
                        na_category = NULL, main = "Likert Plot of Responses", xlab = "Percentage",
                        ylab = NULL, positive.order = TRUE) {

  library(HH)

  # Transform the data using prepare_likert_data()
  transformed_data <- prepare_likert_data(
    data = data,
    question_cols = question_cols,
    na_action = na_action,
    response_levels_in_order = response_levels_in_order,
    na_category = na_category
  )

  # Create Likert plot using HH::likert()
  plot <- HH::likert(Question ~ ., data = transformed_data,
                     main = main, xlab = xlab, ylab = ylab,
                     positive.order = positive.order)

  # Return the plot
  return(plot)
}
