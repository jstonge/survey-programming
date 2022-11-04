library(ggplot2)

d <- readxl::read_xls("all_res.xls")
View(d)
min_max <- range(d$`Publication Year`, na.rm = TRUE)

d |>
  janitor::clean_names() |>
  dplyr::count(publication_year) |>
  dplyr::filter(!is.na(publication_year)) |>
  tidyr::complete(publication_year = min_max[1]:min_max[2], fill = list(n = 0)) |>
  ggplot(aes(publication_year, n)) +
  geom_line() +
  ggpattern::geom_rect_pattern(aes(xmin = min_max[2] - 1, xmax = min_max[2] + 1, ymin = -Inf, ymax = Inf),
    pattern = "stripe", fill = "grey", pattern_fill = "white",
    colour = "black", alpha = 0.1
  ) +
  geom_line() +
  annotate("text", x = min_max[2] - 1, y = 10, label = "Ongoing year", angle = 90, vjust = -0.5) +
  ylab("# results from WoS") +
  xlab("year") +
  ggtitle("Time series results from WoS Query for the 'challenges and motivations of\n learning to program in traditionally non digital fields'") +
  labs(caption = glue::glue('
                  **WoS QUERY**:
  (ALL=("digital humanities" OR "cultural analytic$" OR "computational social science$") AND
   ALL=("policy" OR "learning" OR "motivation" OR "curriculum" OR "training" OR "education")) 
                                OR 
   (ALL=("open source" OR "free software" OR "FOSS" OR "FLOSS") AND
    ALL="motivation" AND ALL=("gender" OR "diversity"))')) +
  theme_bw()


library(tidyverse)

ds <- readxl::read_xls("interesting_subset_0_600.xls")

ds |>
  select(`Article Title`, Authors, `Source Title`) |> 
  View()
# dplyr::filter(stringr::str_detect(`Article Title`, "Ecology")) |> 
# dplyr::pull(`Article Title`)

d |> add_count(Authors) |> filter(n > 1) |> select(Authors, `Article Title`) |> View()