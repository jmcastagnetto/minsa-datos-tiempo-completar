library(tidyverse)
library(fs)

csv_files <- dir_ls(
  path = "data",
  type = "file",
  recurse = TRUE
)

fal_files <- csv_files[str_detect(csv_files, "fallec")]

fal_peru_hist <- tibble()
fal_dpto_hist <- tibble()
fal_prov_hist <- tibble()

for (fn in fal_files) {
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
  fal_peru_hist <- bind_rows(
    fal_peru_hist,
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
  fal_dpto_hist <- bind_rows(
    fal_dpto_hist,
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
  fal_prov_hist <- bind_rows(
    fal_prov_hist,
    prv_df
  )
}

write_csv(
  fal_peru_hist,
  file = "data/fal_peru_hist.csv"
)

write_csv(
  fal_dpto_hist,
  file = "data/fal_dpto_hist.csv"
)

write_csv(
  fal_prov_hist,
  file = "data/fal_prov_hist.csv"
)

# quick plot, national level
ggplot(
  fal_peru_hist,
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
#   fal_dpto_hist,
#   aes(x = fecha_fallecimiento, y = n,
#       group = as.factor(corte),
#       color = as.factor(corte))
# ) +
#   geom_line() +
#   theme(
#     legend.position = "top"
#   ) +
#   facet_wrap(~departamento, scales = "free_y")
