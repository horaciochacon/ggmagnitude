#' Prepare Data for Magnitude Plot
#'
#' This function prepares the input data for use in the magnitude plot.
#'
#' @param data A data frame containing the risk data.
#' @param value_col The name of the column containing the values.
#' @param name_col The name of the column containing the risk names.
#' @param font_col The name of the column containing the font style (optional).
#' @param is_log10 Logical, indicating whether the values are already in log10 scale.
#'
#' @return A data frame with additional columns needed for plotting.
#'
#' @importFrom dplyr arrange mutate rename
#' @keywords internal
prepare_data <- function(data, value_col, name_col, font_col = NULL, is_log10) {
  if (!is_log10) {
    data[[value_col]] <- log10(data[[value_col]])
  }

  data <- data %>%
    dplyr::arrange(!!rlang::sym(value_col)) %>%
    dplyr::mutate(
      value = 10^(!!rlang::sym(value_col)),
      loc = calculate_location(dplyr::n()),
      order = dplyr::row_number()
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
  if (n %% 2 == 0) { # Even number of risks
    mid <- n / 2
    c(1:mid, mid:1)
  } else { # Odd number of risks
    mid <- ceiling(n / 2)
    c(1:mid, (mid - 1):1)
  }
}

calculate_position <- function(n, order) {
  if (n %% 2 == 0) {
    # If even number of risks, split into left and right
    ifelse(order <= n / 2, "left", "right")
  } else {
    # If odd number of risks, middle one is center
    ifelse(order < ceiling(n / 2), "left",
      ifelse(order == ceiling(n / 2), "center", "right")
    )
  }
}
