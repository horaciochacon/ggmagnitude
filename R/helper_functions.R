create_magnitude_scale <- function(min_value, max_value) {
  # Define the values
  values <- 10^(0:12)

  # Create the color palette from yellow to red
  color_palette <- colorRampPalette(c("darkgreen", "yellow", "darkred"))(12)

  # Create a data frame for geom_rect
  df <- data.frame(
    xmin = head(values, -1),
    xmax = tail(values, -1),
    ymin = 0,
    ymax = 1,
    fill = color_palette
  )

  df <- df %>% filter(xmin >=  min_value, xmax <= max_value)

  list(
    geom_rect(
      data = df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "white",
      linewidth = 0.5
    ),
    scale_fill_identity(),
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.text.x = element_text(size = 10),
      plot.margin = unit(c(1,1,1,1), 'cm')
    ),
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

add_risk_line_and_label <- function(
    risk_data, index, total_risks, plot_width, plot_height, size = 3,
    omit_bar = TRUE, vertical_spacing = 0.3,
    ...
    ) {
  y_pos <- 1 + risk_data$loc * vertical_spacing
  x_pos <- risk_data$value
  name_length <- nchar(risk_data$rei_name)

  nudge <- calculate_nudge(
    x_pos, y_pos, name_length, risk_data$position, total_risks,
    plot_width, plot_height, ...
    )

  list(
    geom_segment(
      data = risk_data,
      aes(x = value, xend = value, y = as.numeric(omit_bar), yend = y_pos),
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
      segment.color = "black",
      force = 1
    )
  )
}


