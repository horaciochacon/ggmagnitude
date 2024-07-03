#' Create a Magnitude Plot
#'
#' This function creates a magnitude plot in log10 scale to depict cumulative disability adjusted life years (DALYs) lost due to different risks.
#'
#' @param data A data frame containing the risk data.
#' @param value_col The name of the column containing the values (either in log10 scale or not).
#' @param name_col The name of the column containing the risk names.
#' @param font_col The name of the column containing the font style (optional).
#' @param is_log10 Logical, indicating whether the values are already in log10 scale.
#' @param min_value The minimum value for the x-axis (default is 1e8).
#' @param max_value The maximum value for the x-axis (default is 1e12).
#'
#' @return A ggplot object representing the magnitude plot.
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#' @import ggrepel
magnitude_plot <- function(
    data, value_col, name_col, font_col = NULL, is_log10 = TRUE,
    min_value = 10, max_value = 10^12, ...
    ) {
  # Prepare data
  prepared_data <- prepare_data(data, value_col, name_col, font_col, is_log10)

  # Calculate plot dimensions (in log10 units for width)
  plot_width <- log10(max_value) - log10(min_value)
  plot_height <- 1.5 + 0.3 * nrow(prepared_data) + 1  # Add 1 for some padding

  # Create base plot
  p <- ggplot() +
    create_magnitude_scale(min_value, max_value)

  # Add risk lines and labels
  for (i in 1:nrow(prepared_data)) {
    p <- p + add_risk_line_and_label(
      prepared_data[i, ], i, nrow(prepared_data), plot_width, plot_height,
      ...
      )
  }

  # Finalize plot
  p + theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.text.x = element_text(size = 10),
      plot.margin = unit(c(1,1,1,1), 'cm')
    ) +
    coord_cartesian(ylim = c(1, plot_height), clip = "off")

  # Return the final plot
  p
}
