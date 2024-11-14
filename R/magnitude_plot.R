utils::globalVariables(c(
  "value", "rei_name", "font", "xmin", "xmax", "ymin",
  "ymax", "fill", ".x", "exp_format"
))

#' Create a Magnitude Plot
#'
#' @param data A data frame containing the data to be plotted.
#' @param value_col The name of the column containing the magnitude values.
#' @param name_col The name of the column containing the labels for each data point.
#' @param font_col The name of the column containing the font style (optional).
#' @param is_log10 Logical, indicating whether the values are already in log10 scale.
#' @param min_value The minimum value for the x-axis (default is 10).
#' @param max_value The maximum value for the x-axis (default is 1e12).
#' @param omit_bar Logical, whether to omit vertical bars (default is FALSE).
#' @param only_bar Logical, whether to show only vertical bars without labels (default is FALSE).
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A ggplot object representing the magnitude plot.
#' @export
magnitude_plot <- function(
    data, value_col, name_col, font_col = NULL,
    background_data = NULL, # New parameter for background bars
    background_value_col = NULL, # Column name for background values
    background_name_col = NULL, # Column name for background names
    is_log10 = TRUE,
    min_value = 10, max_value = 10^12, exp_format = TRUE,
    axis.title.x.size = 10,
    axis.text.x.size = 10,
    ...) {
  # Create base plot
  p <- ggplot() +
    create_magnitude_scale(min_value, max_value, exp_format)

  # Add background bars if provided
  if (!is.null(background_data) && !is.null(background_value_col)) {
    background_prepared <- prepare_data(
      background_data, background_value_col, background_name_col,
      font_col, is_log10
    )
    for (i in 1:nrow(background_prepared)) {
      p <- p + add_risk_line_and_label(
        background_prepared[i, ], i, nrow(background_prepared),
        plot_width, plot_height,
        only_bar = TRUE
      )
    }
  }

  # Add main data with labels
  prepared_data <- prepare_data(data, value_col, name_col, font_col, is_log10)
  for (i in 1:nrow(prepared_data)) {
    p <- p + add_risk_line_and_label(
      prepared_data[i, ], i, nrow(prepared_data), plot_width, plot_height,
      ...
    )
  }

  # Finalize plot
  p <- p + theme(axis.text.x = element_text(size = axis.text.x.size))
  p
}
