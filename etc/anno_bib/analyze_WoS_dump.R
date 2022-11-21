library(ggplot2)


View(d)

d <- readxl::read_xls("all_res.xls")
# View(d)
d <- readr::read_tsv("~/Downloads/savedrecs(1).txt")

min_max <- range(d$PY, na.rm = TRUE)


p <- d |>
  dplyr::count(PY) |>
  dplyr::filter(!is.na(PY)) |>
  tidyr::complete(PY = min_max[1]:min_max[2], fill = list(n = 0)) |>
  ggplot(aes(PY, n)) +
  geom_line() +
  ggpattern::geom_rect_pattern(aes(xmin = min_max[2] - 1, xmax = min_max[2] + 1, ymin = -Inf, ymax = Inf),
    pattern = "stripe", fill = "grey", pattern_fill = "white",
    colour = "black", alpha = 0.1
  ) +
  geom_line() +
  annotate("text", x = min_max[2] - 1, y = 10, label = "Ongoing year", angle = 90, vjust = -0.5) +
  ylab("# results from WoS") +
  xlab("year") +
  ggtitle("Time series results from WoS Query for the\n 'challenges and motivations of learning to\n program in traditionally non digital fields'") +
  # labs(caption = glue::glue('
  #                 **WoS QUERY**:
  # (ALL=("digital humanities" OR "cultural analytic$" OR "computational social science$") AND
  #  ALL=("policy" OR "learning" OR "motivation" OR "curriculum" OR "training" OR "education"))
  #                               OR
  #  (ALL=("open source" OR "free software" OR "FOSS" OR "FLOSS") AND
  #   ALL="motivation" AND ALL=("gender" OR "diversity"))')) +
  theme_bw() +
  theme(axis.text = element_text(size=15), title = element_text(size=16), axis.title = element_text(size=16))

ggsave("fig1.png", height = 7, width = 8)

library(tidyverse)

ds <- readxl::read_xls("interesting_subset_0_600.xls")

ds |>
  select(`Article Title`, Authors, `Source Title`) |> 
  View()
# dplyr::filter(stringr::str_detect(`Article Title`, "Ecology")) |> 
# dplyr::pull(`Article Title`)

d |> add_count(Authors) |> filter(n > 1) |> select(Authors, `Article Title`) |> View()