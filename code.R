


library(hexSticker)
library(ggplot2)
library(survival)
library(ggsurvfit)

df <- colon |>
  filter(rx != "Lev") |>
  group_by(id) |>
  arrange(time) |>
  slice(1)

p <- survfit2(Surv(time, status) ~ rx, data = df) %>%
  ggsurvfit() +
  scale_x_continuous("Years", limits = c(0, 3000), breaks = seq(0, 3000, by = 365.25),
                     labels = 1:9) +
  scale_y_continuous(NULL, limits = c(0, 1)) +
  scale_color_manual(values = c("#C5050C", "#0479A8")) +
  theme_classic() +
  theme(
    legend.position = "none", # Position at upper-right corner
    # legend.justification = c(1, 1),  # Align legend to the upper-right
    # legend.title = element_blank(),        # Adjust title size
    # legend.text = element_text(size = 8),         # Adjust text size
    # legend.spacing.x = unit(0, "cm"),
    # legend.key.size = unit(0.2, "cm"),            # Adjust key size
    # legend.margin = margin(-1, -5, -1, -1),            # Adjust overall legend margin
    axis.title.x = element_text(margin = margin(t = -2)), # Reduce top margin
    panel.background = element_rect(fill = "transparent", color = NA), # Transparent panel
    plot.background = element_rect(fill = "transparent", color = NA)  # Transparent
  )   +
  guides(
    color = guide_legend(
      byrow = TRUE                               # Place legend items in rows           # Adjust point sizes in legend
    )
  )
  # add_risktable()
p


# ?legend.y.spacing

sticker(p, package="Survival analysis", p_size=15,
        s_x=0.9, s_y=0.80, s_width=1.6, s_height=0.9,
        h_fill="white", h_color="darkred", p_color="#C5050C",
        url = "https://lmaowisc.github.io/BMI741", u_size = 3,
        filename="images/surv_hex.png")



# ?sticker


# Longitudinal analysis course hexsticker ---------------------------------

library(tidyverse)

x <- seq(0, 5, by = 0.5)

m <- length(x)

b0_1 <- 0
b1_1 <- 1

b0_2 <- -2
b1_2 <- 0.3

y1t <- b0_1 + b1_1 * x
y1 <- y1t + 0.4 * rnorm(m)
y2t <- b0_2 + b1_2 * x
y2 <- y2t + 0.4 * rnorm(m)

ytall <- (y1t + y2t) / 2

df <- tibble(
  id = rep(c("A", "B"), each = m),
  x = rep(x, 2),
  y = c(y1, y2),
  yt = c(y1t, y2t),
  ytall = rep(ytall, 2)
)

df_labels <- df |>
  group_by(id) |>
  slice_max(x)




longit_lme <- df |>
  ggplot(aes(x = x, y = y, group = id)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = yt, linetype = "0"), linewidth = 0.2) +
  geom_line(aes(y = ytall, linetype = "1") , linewidth = 0.2) +
  geom_text(data = df_labels, aes(x = x + 0.1, y = yt, label = id)) +
  scale_x_continuous(NULL, expand = c(0, 0.1)) +
  scale_y_continuous(NULL) +
  scale_linetype_manual(values = c(2, 1)) +
  theme_classic() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", color = NA), # Transparent panel
    plot.background = element_rect(fill = "transparent", color = NA)  # Transparent
  )


longit_lme

sticker(longit_lme, package="Longitudinal analysis",
        p_size=12,
        s_x=0.9, s_y=.8, s_width=1.5, s_height=1.0,
        # h_color="#C5050C",
        h_fill="#0479A8",
        filename="images/longit_hex.png")
