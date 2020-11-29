library(tidyverse)

pe_wide <- read_csv("data/pos_peru_hist.csv") %>%
  pivot_wider(
    id_cols = fecha_resultado,
    names_from = corte,
    names_prefix = "d_",
    values_from = n
  ) %>%
  janitor::clean_names() %>%
  mutate(
    r_2020_08_01 = d_2020_08_01 / d_2020_11_23,
    r_2020_08_14 = d_2020_08_14 / d_2020_11_23,
    r_2020_08_31 = d_2020_08_31 / d_2020_11_23,
    r_2020_09_15 = d_2020_09_15 / d_2020_11_23,
    r_2020_09_30 = d_2020_09_30 / d_2020_11_23,
    r_2020_10_19 = d_2020_10_19 / d_2020_11_23,
    r_2020_10_29 = d_2020_10_29 / d_2020_11_23,
    r_2020_11_09 = d_2020_11_09 / d_2020_11_23,
    r_2020_11_16 = d_2020_11_16 / d_2020_11_23
  )

pe_df <- pe_wide %>%
  select(fecha_resultado, starts_with("r_")) %>%
  pivot_longer(
    cols = c(starts_with("r_")),
    names_to = "corte",
    values_to = "tasa"
  ) %>%
  mutate(
    corte = str_remove(corte, "r_") %>%
      str_replace_all("_", "-") %>%
      lubridate::ymd(),
    grp = as.factor(corte)
  )

ggplot(
  pe_df,
  aes(x = fecha_resultado, y = tasa,
      group = grp, color = grp)
) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~grp) +
  labs(
    x = "Fecha de resultado",
    y = "Tasa con respecto al corte del 2020-11-23",
    title = "Comparaciones de valores con respecto a los del 2020-11-23, a nivel del Perú"
  ) +
  scale_x_date(
    date_breaks = "4 weeks",
    date_minor_breaks = "1 week"
  ) +
  theme_bw(14) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

