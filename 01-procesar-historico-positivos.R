library(tidyverse)
library(fs)

csv_files <- dir_ls(
  path = "data",
  type = "file",
  recurse = TRUE
)
pos_files <- csv_files[str_detect(csv_files, "positiv")]

pos_peru_hist <- tibble()
pos_dpto_hist <- tibble()
pos_prov_hist <- tibble()

for (fn in pos_files) {
  fecha = lubridate::ymd(path_split(fn)[[1]][2])
  csv <- read_csv(fn)
  # nivel nacional
  pe_df <- csv %>%
    group_by(fecha_resultado) %>%
    tally() %>%
    filter(!is.na(fecha_resultado)) %>%
    mutate(
      corte = fecha
    )
  pos_peru_hist <- bind_rows(
    pos_peru_hist,
    pe_df
  )
  # nivel departamento
  dpt_df <- csv %>%
    group_by(fecha_resultado, departamento) %>%
    tally() %>%
    filter(!is.na(fecha_resultado)) %>%
    mutate(
      corte = fecha
    )
  pos_dpto_hist <- bind_rows(
    pos_dpto_hist,
    dpt_df
  )
  #nivel provincia
  prv_df <- csv %>%
  group_by(fecha_resultado, departamento, provincia) %>%
    tally() %>%
    filter(!is.na(fecha_resultado)) %>%
    mutate(
      corte = fecha
    )
  pos_prov_hist <- bind_rows(
    pos_prov_hist,
    prv_df
  )
}

write_csv(
  pos_peru_hist,
  file = "data/pos_peru_hist.csv"
)

write_csv(
  pos_dpto_hist,
  file = "data/pos_dpto_hist.csv"
)

write_csv(
  pos_prov_hist,
  file = "data/pos_prov_hist.csv"
)

# quick plot, national level
# ggplot(
#   pos_peru_hist,
#   aes(x = fecha_resultado, y = n,
#       group = as.factor(corte),
#       color = as.factor(corte))
# ) +
#   geom_line() +
#   theme(
#     legend.position = "top"
#   )

# quick plot, subnational level
# ggplot(
#   pos_dpto_hist,
#   aes(x = fecha_resultado, y = n,
#       group = as.factor(corte),
#       color = as.factor(corte))
# ) +
#   geom_line() +
#   theme(
#     legend.position = "top"
#   ) +
#   facet_wrap(~departamento, scales = "free_y")
