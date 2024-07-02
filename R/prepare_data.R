# R/prepare_data.R

prepare_data <- function(data, value_col, name_col, font_col, is_log10) {
  if (!is_log10) {
    data[[value_col]] <- log10(data[[value_col]])
  }

  data <- data %>%
    arrange(!!sym(value_col)) %>%
    mutate(
      value = 10^(!!sym(value_col)),
      loc = calculate_location(n()),
      order = row_number()
    )

  # Calculate position (left, center, right)
  data <- data %>%
    mutate(position = calculate_position(n(), order))

  if (!is.null(font_col)) {
    data <- data %>% rename(font = !!sym(font_col))
  } else {
    data$font <- "plain"
  }

  data %>% rename(rei_name = !!sym(name_col))
}

calculate_location <- function(n) {
  if (n %% 2 == 0) {  # Even number of risks
    mid <- n / 2
    c(1:mid, mid:1)
  } else {  # Odd number of risks
    mid <- ceiling(n / 2)
    c(1:mid, (mid-1):1)
  }
}

calculate_position <- function(n, order) {
  if (n %% 2 == 0) {
    # If even number of risks, split into left and right
    ifelse(order <= n/2, "left", "right")
  } else {
    # If odd number of risks, middle one is center
    ifelse(order < ceiling(n/2), "left",
           ifelse(order == ceiling(n/2), "center", "right"))
  }
}
