---
title: "How to create a Bland-Altman plot using ggplot2"
subtitle: "Visualize how difference between two measurements changes with the average"
description: "Consider a repeatability/reproducibility analysis for four biomarkers"
date: "1-1-2025"
categories: [R, tidyverse, ggplot2, statistics]
twitter:
  card: summary_large_image # Choose 'summary', 'summary_large_image', 'app', or 'player'
  site: "@YourTwitterHandle" # Your website's Twitter handle (optional)
  creator: "@YourTwitterHandle" # Your personal or organization Twitter handle
  image: "path/to/your-image.jpg" # Image URL or path for the Twitter Card
execute:
  warning: false
  eval: true
  cache: false
---



## Introduction

Bland-Altman analysis, also known as a Tukey mean-difference plot, is a method to assess the agreement between two measurement techniques. 
The plot visualizes how the differences between the two methods vary with the average of the measurements.

I demonstrate how to create a Bland-Altman plot in R using the ggplot2 package from the tidyverse. We'll use a simulated dataset to illustrate the steps.


## Simulating Data

Let's create a sample dataset with two measurement techniques.



::: {.cell}

```{.r .cell-code}
# Load tidyverse
library(tidyverse)

# Simulate data
set.seed(123)
data <- tibble(
  method1 = rnorm(100, mean = 50, sd = 10),
  method2 = rnorm(100, mean = 50, sd = 10)
)
```
:::



## Calculating Bland-Altman Statistics

To create a Bland-Altman plot, we need to calculate the mean and difference of the two methods for each observation. We'll also compute the mean difference (bias) and limits of agreement (LoA = bias $±$ 1.96 * SD).



::: {.cell}

```{.r .cell-code}
# Calculate Bland-Altman statistics
data <- data %>%
  mutate(
    mean_value = (method1 + method2) / 2,
    difference = method1 - method2
  )

# Calculate bias and limits of agreement
bias <- mean(data$difference)
loa_upper <- bias + 1.96 * sd(data$difference)
loa_lower <- bias - 1.96 * sd(data$difference)

# Display results
list(bias = bias, loa_upper = loa_upper, loa_lower = loa_lower)
```

::: {.cell-output .cell-output-stdout}

```
$bias
[1] 1.979527

$loa_upper
[1] 28.67972

$loa_lower
[1] -24.72067
```


:::
:::



## Creating the Bland-Altman Plot

Now, let's create the Bland-Altman plot using ggplot2:



::: {.cell}

```{.r .cell-code}
# Load ggplot2
library(ggplot2)

# Create the plot
ba_plot <- data %>%
  ggplot(aes(x = mean_value, y = difference)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = bias, color = "blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(loa_upper, loa_lower), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = min(data$mean_value), y = bias, label = "Bias", hjust = 0, vjust = -1, color = "blue") +
  annotate("text", x = min(data$mean_value), y = loa_upper, label = "Upper LoA", hjust = 0, vjust = -1, color = "red") +
  annotate("text", x = min(data$mean_value), y = loa_lower, label = "Lower LoA", hjust = 0, vjust = -1, color = "red") +
  labs(
    title = "Bland-Altman Plot",
    x = "Mean of Two Methods",
    y = "Difference between Methods"
  ) +
  theme_minimal()

# Display the plot
print(ba_plot)
```

::: {.cell-output-display}
![](how_to_bland_altman_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::



## Interpreting the Plot

1. **Bias (blue dashed line):** Represents the average difference between the two methods. If it's close to zero, the methods are generally in agreement.
2. **Limits of Agreement (red dashed lines):** Define the range within which most differences lie (typically 95%). Smaller ranges indicate better agreement.
3. **Scatter of Points:** Points should ideally be randomly distributed around the bias line without any patterns.

## Conclusion

With ggplot2 and the tidyverse, creating a Bland-Altman plot is straightforward and customizable. This approach can be easily adapted to your own data to assess agreement between measurement techniques.

