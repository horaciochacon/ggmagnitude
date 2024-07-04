# ggmagnitude: Create Visual Magnitude Plots on a Colored Scale

<!-- badges: start -->
[![R-CMD-check](https://github.com/horaciochacon/ggmagnitude/workflows/R-CMD-check/badge.svg)](https://github.com/horaciochacon/ggmagnitude/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/ggmagnitude)](https://CRAN.R-project.org/package=ggmagnitude)
<!-- badges: end -->

The `ggmagnitude` package provides tools to create magnitude plots, primarily using a logarithmic scale, for visual representation of data across wide-ranging orders of magnitude. These plots offer an intuitive way to compare and understand the relative scale of various data points using color gradients and positioned labels.

## Installation

You can install the development version of ggmagnitude from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("horaciochacon/ggmagnitude")

## Usage

Here's a basic example of how to use the `ggmagnitude` package:

```r
library(ggmagnitude)

# Create sample data
data <- data.frame(
  label = c("Value A", "Value B", "Value C", "Value D", "Value E"),
  magnitude = c(1e2, 5e4, 2e6, 7e8, 3e10)
)

# Create the magnitude plot
magnitude_plot(data, value_col = "magnitude", name_col = "label")
```

This will create a magnitude plot showing the relative impact of different risks based on their DALY values.

## Features

- Create visually appealing magnitude plots in log10 scale
- Automatically handle positioning and labeling of data points
- Customizable color scale to represent magnitude
- Flexible input options for data in various formats

## Documentation

For more detailed information about the package functions and their usage, please refer to the package documentation:

```r
?magnitude_plot
```

You can also check out the package vignette for a more in-depth tutorial:

```r
vignette("ggmagnitude")
```

## Contributing

Contributions to `ggmagnitude` are welcome! Please refer to the [contribution guidelines](CONTRIBUTING.md) for more information.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Citation

If you use `ggmagnitude` in your research, please consider citing it:

```
Chacon Torrico, H. (2023). ggmagnitude: Create Magnitude Plots for DALYs. R package version 0.1.0.
https://github.com/horaciochacon/ggmagnitude
```

## Contact

For any questions or feedback, please [open an issue](https://github.com/horaciochacon/ggmagnitude/issues) on GitHub or contact the package maintainer at hchacont@uw.edu.
