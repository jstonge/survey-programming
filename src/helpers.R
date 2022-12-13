library(gt)
library(tidyverse)
library(glue)
library(janitor)

# plots -------------------------------------------------------------------


plot_bar_chart <- function(tbl, y, x, xlab) {
  vals <- pull(tbl, {{ x }} )
  if ("male" %in% vals) {
    col_vals <- c("darkred", "midnightblue")
  } else {
    col_vals <- c("darkgreen", "orange")
  }
  
  tbl |> 
    count({{ x }}, {{ y }}) |>
    drop_na() |>
    complete({{ x }}, {{ y }}, fill = list(n=0)) |>
    filter({{ y }} != 999) |>
    mutate(pct = n / sum(n)) |>
    ggplot(aes({{ y }}, pct )) + 
    geom_col(aes(fill = {{ x }}), width = 0.5, 
             alpha = 0.6, color = "black", position = "dodge") +
    scale_fill_manual(values = col_vals) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ylab("") +
    xlab(xlab) +
    theme(axis.text = element_text(size = 18), 
          axis.title = element_text(size=20), 
          legend.text = element_text(size=16),
          legend.title = element_text(size=20), title = element_text(size=20)) 
}

plot_simple <- function(tbl, x, xlab) {
  tbl |>
    count({{x}}) |>
    drop_na() |>
    filter({{ x }} != 999) |>
    mutate(pct = n / sum(n)) |>
    ggplot(aes({{x}}, pct)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_col() +
    theme_bw() +
    ylab("") + xlab(xlab) +
    theme(axis.text = element_text(size = 18), 
          axis.title = element_text(size=20), 
          legend.text = element_text(size=16),
          legend.title = element_text(size=20), title = element_text(size=20))
}


# table -----------------------------------------------------------------

apa_style <- function(tab) {
  tab %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.vlines.width = 0,
      table_body.vlines.width = 0,
      summary_row.border.width = 0,
      heading.border.bottom.width = 0,
      grand_summary_row.border.width = 0,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white",
      row_group.border.bottom.color = "white",
      row_group.border.top.color = "white",
      table_body.hlines.color = "white",
      stub.border.width = 0
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    )
}

summarize_tbl <- function(tbl, x, y) {
  tbl |> 
      tabyl(.data[[y]], .data[[x]], show_na = FALSE) |>
      adorn_totals("col") |>
      rename(dep_var = .data[[y]]) |>
      mutate(
        `%` = round(Total / sum(Total), 3) * 100,
        across(everything(), as.character)
        )
}

myunique <- function(tbl, x) {
  unique(tbl[x]) |> na.omit() |> pull() |> as.character()
}

make_apa_table <- function(tbl, x, y, title=NULL, footnote=NULL) {
  
  tbl_table <- map_dfr(y, ~summarize_tbl(df, x, .x))
  vars <- map(y, ~myunique(df, .x))
  
  mytab <- tbl_table %>% 
    gt(rowname_col = "dep_var") %>% 
    apa_style() %>% 
    opt_align_table_header(align = "left") %>%
    tab_options(
      column_labels.padding = px(10),
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black"
    ) 
      
  for (i in 1:length(y)) {
    mytab <- mytab %>% 
      tab_row_group(label = str_to_title(str_replace_all(y[i], "_", " ")), rows = vars[[i]]) %>%
      tab_stub_indent(rows = vars[[i]], indent = 4)
  } 
  
  if (is.null(title) == FALSE) {
    mytab <- mytab |> tab_header(title=title)
  }
  
  if (is.null(footnote) == FALSE) {
    mytab <- mytab %>% tab_foot(footnote)
  }
  
  return(mytab)
}
