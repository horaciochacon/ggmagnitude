create_magnitude_scale <- function(min_value, max_value) {
  # Define the number of colors in the palette
  num_colors <- 12

  # Create the color palette from green to yellow to red
  color_palette <- colorRampPalette(c("darkgreen", "yellow", "darkred"))(num_colors)

  # Define the values from 10 to 10^12 on a log scale
  values <- c(10, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12)
  val <- c(30, 3e2, 3e3, 3e4, 3e5, 3e6, 3e7, 3e8, 3e9, 3e10, 3e11, 3e12)

  # Create a data frame that maps the values to colors
  df <- data.frame(Values = val, Colors = color_palette)

  list(
    geom_tile(
      aes(
        width = diff(c(0, log10(values))),
        x = Values, y = 1, fill = Colors
      ), color = "white", linewidth = 0.5, data = df
    ),
    scale_fill_identity(),
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x.bottom = element_text(size = 14),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black")
    ),
    labs(x = "", y = ""),
    scale_y_continuous(expand = c(0, 0)),
    scale_x_continuous(
      trans = 'log10',
      limits = c(min_value, max_value),
      breaks = values,
      expand = c(0, 0),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )
  )
}

calculate_nudge <- function(
    x_pos, y_pos, name_length, position, total_risks, plot_width, plot_height,
    name_value = 0
    ) {
  # Calculate base nudge in x direction (in log10 units)
  base_nudge_x <- 0.75  # Adjust this value to control the diagonal angle

  # Adjust x nudge based on name length (longer names need more space)
  name_factor <- (name_length * name_value)  # Normalize by average name length
  adjusted_nudge_x <- base_nudge_x * (1 + name_factor)

  # Calculate y nudge (in plot coordinate units)
  base_nudge_y <- 0.4  # Adjust this value to control vertical spacing

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

add_risk_line_and_label <- function(
    risk_data, index, total_risks, plot_width, plot_height, size = 3
    ) {
  y_pos <- 1.5 + risk_data$loc * 0.3
  x_pos <- risk_data$value
  name_length <- nchar(risk_data$rei_name)

  nudge <- calculate_nudge(
    x_pos, y_pos, name_length, risk_data$position, total_risks,
    plot_width, plot_height
    )

  list(
    geom_segment(
      data = risk_data,
      aes(x = value, xend = value, y = 1.5, yend = y_pos),
      size = 0.5,
      color = "black"
    ),
    geom_text_repel(
      data = risk_data,
      aes(x = x_pos, y = y_pos, label = rei_name, fontface = font),
      size = size,
      nudge_x = nudge$x,
      nudge_y = nudge$y,
      direction = "both",
      hjust = case_when(
        risk_data$position == "left" ~ 1,
        risk_data$position == "right" ~ 0,
        risk_data$position == "center" ~ 0.5
      ),
      vjust = 0.5,
      segment.size = 0.5,
      segment.color = "gray50",
      force = 1,
      max.iterations = 5000
    )
  )
}


