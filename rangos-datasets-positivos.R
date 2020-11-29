library(tidyverse)

pe <- read_csv("data/pos_peru_hist.csv")

pe_summ <- pe %>%
  group_by(corte) %>%
  summarise(
    min_fecha = min(fecha_resultado, na.rm = TRUE),
    max_fecha = max(fecha_resultado, na.rm = TRUE)
  ) %>%
  mutate(
    grp = as.factor(corte)
  )

ggplot(pe_summ) +
  geom_errorbarh(
    aes(xmin = min_fecha,
        xmax = max_fecha,
        y = grp,
        group = grp,
        color = grp),
    size = 2,
    height = .5,
    show.legend = FALSE
  ) +
  labs(
    title = "Rangos de fechas de los datasets libres de positivos COVID-19",
    y = "Fecha de corte",
    x = "Rango de fechas"
  ) +
  scale_x_date(
    date_breaks = "4 weeks",
    date_minor_breaks = "1 week"
  ) +
  theme_bw(16)
