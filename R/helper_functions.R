#' @importFrom dplyr filter mutate arrange case_when n
#' @importFrom rlang .data
#' @importFrom utils head tail
NULL

utils::globalVariables(c("value", "rei_name", "font", "xmin", "xmax", "ymin",
                         "ymax", "fill", ".x"))

create_magnitude_scale <- function(min_value, max_value, exp_format = TRUE) {
  # Define the values
  values <- 10^(0:12)

  # Create the color palette from yellow to red
  color_palette <- grDevices::colorRampPalette(c("darkgreen", "yellow", "darkred"))(12)

  # Create a data frame for geom_rect
  df <- data.frame(
    xmin = head(values, -1),
    xmax = tail(values, -1),
    ymin = 0,
    ymax = 1,
    fill = color_palette
  )

  df <- df %>%
    dplyr::filter(xmin >= min_value, xmax <= max_value)

  # Change the order of elements to ensure proper layering
  list(
    # First set the theme to ensure proper positioning
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(1,1,1,1), 'cm')
    ),
    # Then set the scales
    scale_y_continuous(expand = c(0, 0)),
    scale_x_continuous(
      trans = 'log10',
      limits = c(min_value, max_value),
      breaks = values,
      expand = c(0, 0),
      labels = ifelse(
        exp_format,
        scales::trans_format("log10", scales::math_format(10^.x)),
        scales::trans_format("log10", scales::math_format(.x))
      )
    ),
    # Add the rectangles for the color scale
    ggplot2::geom_rect(
      data = df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "white",
      linewidth = 0.5
    ),
    ggplot2::scale_fill_identity(),
    # Finally add the axis line on top
    theme(
      axis.line.x = element_line(color = "black", linewidth = 0.5),
      axis.ticks.x = element_line(color = "black"),
      axis.text.x = element_text(size = 10)
    )
  )
}

#' Calculate Label Nudge
#'
#' This function calculates the nudge values for positioning labels in the magnitude plot.
#'
#' @param x_pos X position of the data point
#' @param y_pos Y position of the data point
#' @param name_length Length of the label name
#' @param position Position of the label ("left", "right", or "center")
#' @param total_risks Total number of risks in the plot
#' @param plot_width Width of the plot
#' @param plot_height Height of the plot
#' @param name_value Adjustment factor for name length (default is 0)
#' @param x_nudge Base nudge in x direction (default is 0.75)
#' @param y_nudge Base nudge in y direction (default is 0.4)
#'
#' @return A list with x and y nudge values
#'
#' @importFrom dplyr case_when
#' @keywords internal

calculate_nudge <- function(
    x_pos, y_pos, name_length, position, total_risks, plot_width, plot_height,
    name_value = 0, x_nudge = 0.75, y_nudge = 0.4
    ) {
  # Calculate base nudge in x direction (in log10 units)
  base_nudge_x <- x_nudge  # Adjust this value to control the diagonal angle

  # Adjust x nudge based on name length (longer names need more space)
  name_factor <- (name_length * name_value)  # Normalize by average name length
  adjusted_nudge_x <- base_nudge_x * (1 + name_factor)

  # Calculate y nudge (in plot coordinate units)
  base_nudge_y <- y_nudge

  # Adjust y nudge based on position (higher positions need more space)
  position_factor <- (y_pos - 1.5) / (0.3 * total_risks)
  adjusted_nudge_y <- base_nudge_y * (1 + position_factor)

  # Set nudge direction based on position
  nudge_x <- case_when(
    position == "left" ~ -adjusted_nudge_x,
    position == "right" ~ adjusted_nudge_x,
    position == "center" ~ 0
  )

  nudge_y <- adjusted_nudge_y

  return(list(x = nudge_x, y = nudge_y))
}


#' Add Risk Line and Label
#'
#' This function adds a line and label for a single risk to the magnitude plot.
#'
#' @param risk_data Data for a single risk
#' @param index Index of the current risk
#' @param total_risks Total number of risks in the plot
#' @param plot_width Width of the plot
#' @param plot_height Height of the plot
#' @param size Size of the label text (default is 3)
#' @param omit_bar Logical, whether to omit the vertical bar (default is TRUE)
#' @param only_bar Logical, whether to show only the vertical bar without label (default is FALSE)
#' @param vertical_spacing Vertical spacing between risks (default is 0.3)
#' @param ... Additional arguments passed to calculate_nudge()
#'
#' @return A list of ggplot2 objects for the risk line and label
#'
#' @importFrom ggplot2 geom_segment aes
#' @importFrom ggrepel geom_text_repel
#' @keywords internal

add_risk_line_and_label <- function(
    risk_data, index, total_risks, plot_width, plot_height, size = 3,
    omit_bar = TRUE, only_bar = FALSE, vertical_spacing = 0.3,
    ...
) {
  y_pos <- 1 + risk_data$loc * vertical_spacing
  x_pos <- risk_data$value
  name_length <- nchar(risk_data$rei_name)

  nudge <- calculate_nudge(
    x_pos, y_pos, name_length, risk_data$position, total_risks,
    plot_width, plot_height, ...
  )

  # Initialize empty list for plot elements
  plot_elements <- list()

  # Add vertical bar if not omitted
  if (!omit_bar || only_bar) {
    if (only_bar) {
      # If only_bar is TRUE, draw just the vertical bar to y = 1
      plot_elements[[1]] <- ggplot2::geom_segment(
        data = risk_data,
        ggplot2::aes(x = value, xend = value, y = 0, yend = 1),
        size = 0.5,
        color = "black"
      )
    } else {
      # Regular vertical bar
      plot_elements[[1]] <- ggplot2::geom_segment(
        data = risk_data,
        ggplot2::aes(x = value, xend = value, y = 0, yend = y_pos),
        size = 0.5,
        color = "black"
      )
    }
  }

  # Add label and connecting line if not only showing bar
  if (!only_bar) {
    plot_elements[[length(plot_elements) + 1]] <- ggrepel::geom_text_repel(
      data = risk_data,
      ggplot2::aes(x = x_pos, y = y_pos, label = rei_name, fontface = font),
      size = size,
      nudge_x = nudge$x,
      nudge_y = nudge$y,
      direction = "both",
      hjust = dplyr::case_when(
        risk_data$position == "left" ~ 1,
        risk_data$position == "right" ~ 0,
        risk_data$position == "center" ~ 0.5
      ),
      vjust = 0.5,
      segment.size = 0.5,
      segment.color = "black",
      force = 1
    )
  }

  return(plot_elements)
}


