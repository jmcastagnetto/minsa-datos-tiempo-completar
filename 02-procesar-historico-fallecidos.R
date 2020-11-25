library(tidyverse)
library(fs)

csv_files <- dir_ls(
  path = "data",
  type = "file",
  recurse = TRUE
)

fac_files <- csv_files[str_detect(csv_files, "fallec")]

fac_peru_hist <- tibble()
fac_dpto_hist <- tibble()
fac_prov_hist <- tibble()

for (fn in fac_files) {
  fecha = lubridate::ymd(path_split(fn)[[1]][2])
  csv <- read_csv(fn)
  # nivel nacional
  pe_df <- csv %>%
    group_by(fecha_fallecimiento) %>%
    tally() %>%
    filter(!is.na(fecha_fallecimiento)) %>%
    mutate(
      corte = fecha
    )
  fac_peru_hist <- bind_rows(
    fac_peru_hist,
    pe_df
  )
  # nivel departamento
  dpt_df <- csv %>%
    group_by(fecha_fallecimiento, departamento) %>%
    tally() %>%
    filter(!is.na(fecha_fallecimiento)) %>%
    mutate(
      corte = fecha
    )
  fac_dpto_hist <- bind_rows(
    fac_dpto_hist,
    dpt_df
  )
  #nivel provincia
  prv_df <- csv %>%
  group_by(fecha_fallecimiento, departamento, provincia) %>%
    tally() %>%
    filter(!is.na(fecha_fallecimiento)) %>%
    mutate(
      corte = fecha
    )
  fac_prov_hist <- bind_rows(
    fac_prov_hist,
    prv_df
  )
}

write_csv(
  fac_peru_hist,
  file = "data/fac_peru_hist.csv"
)

write_csv(
  fac_dpto_hist,
  file = "data/fac_dpto_hist.csv"
)

write_csv(
  fac_prov_hist,
  file = "data/fac_prov_hist.csv"
)

# quick plot, national level
ggplot(
  fac_peru_hist,
  aes(x = fecha_fallecimiento, y = n,
      group = as.factor(corte),
      color = as.factor(corte))
) +
  geom_line() +
  theme(
    legend.position = "top"
  )

# quick plot, subnational level
# ggplot(
#   fac_dpto_hist,
#   aes(x = fecha_fallecimiento, y = n,
#       group = as.factor(corte),
#       color = as.factor(corte))
# ) +
#   geom_line() +
#   theme(
#     legend.position = "top"
#   ) +
#   facet_wrap(~departamento, scales = "free_y")
