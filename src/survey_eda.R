library(here)
library(tidyverse)
library(patchwork)
library(janitor)
library(gt) 
library(gtExtras)

source("src/helpers.R")

df <- readr::read_csv(here("output", "data_clean.csv"))

coder <- subset(df, is_coder == "coder")


# valuing open source -----------------------------------------------------


# all on the same scale
raw_oss_enthuasiast_cols <- c("value_oss_license_ord", "value_coc_ord", "value_contrib_guide_ord", "value_active_ord", "value_cla_ord",
                              "value_responsive_maintainers_ord", "value_welcoming_community_ord", "value_widespread_use_ord")

raw_open_sci_cols <- c("value_share_code_ord", "value_accessibility_paper_code_ord", "value_paper_code_citability_ord")

count_val <- coder |> 
  select(all_of(c(raw_oss_enthuasiast_cols, raw_open_sci_cols))) |> 
  mutate(id = row_number()) |>   
  pivot_longer(-id) |> 
  count(name, value) |>
  mutate(
    name = str_remove(name, "value"),
    name = str_remove(name, "ord"),
    name = str_replace_all(name, "_", " "), 
    name = str_to_title(name),
    value = factor(value)
  ) |>
  filter(!is.na(value))

max_val <- max(count_val$n)

# plot 1
ggplot(count_val, aes(x=value, y=n)) + 
  geom_col(aes(fill=name), show.legend = FALSE) + 
  facet_wrap(~name) +
  geom_hline(yintercept = max_val, color = "black", linetype="dotted") + 
  hrbrthemes::theme_ipsum(grid="Y") +
  ylim(0, 35) +
  scale_fill_brewer(palette = "Paired") +
  labs(title="People valueing open source and open science", y="# respondents", x="← less valued • more valued→") +
  theme(
    axis.title.x = element_text(size=20, hjust = 0.4, vjust = -1.5),
    axis.title.y = element_text(size=20, hjust = 0.6, vjust = 2.5)
) 

# Plot2

coder_long <- coder |> 
  select(all_of(c(raw_oss_enthuasiast_cols, raw_open_sci_cols,"experienced_coder", "gender_binary"))) |>
  mutate(id = row_number()) |>   
  pivot_longer(-c(id, gender_binary, experienced_coder), names_to = "project_component", values_to = "ordinal_value") |>  
  filter(!is.na(ordinal_value), !is.na(gender_binary)) |>
  mutate(
    project_component = str_remove(project_component, "value"),
    project_component = str_remove(project_component, "ord"),
    project_component = str_replace_all(project_component, "_", " "), 
    project_component = str_to_title(project_component),
    project_component = str_replace(project_component, "Active", "Active Development"))

coder_long_agg <- coder_long |>
  group_by(project_component, gender_binary, experienced_coder) |>
  summarize(
    value_mean = mean(ordinal_value, na.rm = TRUE),
    value_sd = sd(ordinal_value, na.rm = TRUE) 
    ) |>
  ungroup() 


# png("value_open_source.png", width=3000, height=2200, res=300) 
coder_long_agg |> 
filter(is.na(experienced_coder)) |>
ggplot(aes(
      x=value_mean, y = reorder(project_component, value_mean), 
      xmin = value_mean - value_sd, xmax = value_mean + value_sd,
      fill = gender_binary
      )) + 
    # geom_point(data=coder_long_agg, mapping = aes(
    #   x=value_mean, y = reorder(project_component, value_mean), 
    #   fill = gender_binary
    #   ), 
    # shape=21, alpha=0) +
    # geom_jitter(data = coder_long, aes(
    #   x = ordinal_value, y = project_component, fill = gender_binary
    #   ), shape = 21, height = 0.1, color="black")  +
    geom_pointrange(aes(shape=experienced_coder), 
      size=1.3, position = position_dodge(width=0.25) ) +     
    hrbrthemes::theme_ipsum() +
    scale_fill_manual(values = c("pink", "lightblue")) +
    labs(title="Respondents valueing dimensions of open source projects", y="", x="← less valued • more valued→") +
    theme(
      axis.title.x = element_text(size=20, hjust = 0.4, vjust = -1.5),
      axis.title.y = element_text(size=20, hjust = 0.6, vjust = 2.5),
      axis.text.y = element_text(size=18),
      legend.text = element_text(size=13),
      plot.title = element_text(size=18)
  ) 
# dev.off()

# Plot 3
ggplot(count_val) + 
  geom_col(aes(x=reorder(name, -n), y=n, fill=value), position = "stack") + 
  scale_fill_brewer(palette = "Greens") +
  labs(title="How early-career scientists who code value open source and open science", y="# respondents", x="") +
  hrbrthemes::theme_ipsum(grid="Y") + 
  theme(
    axis.title.y = element_text(size=20, hjust = 0.5, vjust = 2.5),
    axis.text =  element_text(angle=45, hjust=0.9)
  ) 

# Plot 4
png("value_code_field_coder_non_coder.png", width=2600, height=2000, res=300)
df |>
  mutate(
    is_stem = case_when(
      is_stem == 1 ~ "Stem", 
      is_stem == 0 ~ "Not Stem", 
      TRUE ~ "Unknown"
      )) |>
  count(value_learn_code_in_field, is_coder, gender_binary, is_stem) |>
  filter(!is.na(gender_binary), !is.na(value_learn_code_in_field)) |>
  mutate(value_learn_code_in_field = factor(
    value_learn_code_in_field, 
    levels=c("Not at all in important", "Slightly important", "Moderately important", "Very important", "Extremely important"), 
    ordered=TRUE
    )) |>
  ggplot(aes(x=factor(is_coder), y=n, fill=value_learn_code_in_field )) +
  geom_col(position = "stack", linewidth = 0, width = 0.5) +
  hrbrthemes::theme_ipsum() + 
  scale_fill_brewer(palette = "Greens") +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  labs(title="Coder versus Non coder", subtitle = "Overall, how important do you think it is to learn programming in your academic field today?", y="# respondents", x="") +
  hrbrthemes::theme_ipsum(grid="Y") + 
  facet_grid(gender_binary~is_stem) +
  theme(
    legend.position =  "bottom",
    legend.title = element_blank(),
    plot.title = element_text(size=22),
    plot.subtitle = element_text(size=16),
    legend.text = element_text(size=13),
    strip.text = element_text(hjust = 0.5, size=18),
    axis.text.y = element_text(size=15),
    axis.title.y = element_text(size=20, hjust = 0.5, vjust = 2.5),
    axis.text.x =  element_text(angle=45, hjust=0.9, size=15)
  ) 

dev.off()


# Plot 5
png("years_coding.png", width=2600, height=2000, res=300)

coder |> 
  mutate(
      is_stem = case_when(
        is_stem == 1 ~ "Stem", 
        is_stem == 0 ~ "Not Stem", 
        TRUE ~ "Unknown"
        )) |>
  count(years_coding, years_coding_c, pref_pronouns, is_stem, academia_status) |>
  filter(!is.na(pref_pronouns), years_coding != "Prefer not to say") |>
  mutate(`Preferred pronouns` = factor(
    pref_pronouns, 
    levels = c("he/him", "they/them", "she/they", "she/her", "Prefer not to say"),
    ordered = TRUE)) |>
  ggplot(aes(x = reorder(years_coding, years_coding_c), y = n )) +
  geom_col(aes(fill = `Preferred pronouns`), linewidth = 0) +
  scale_fill_manual(values = c("lightblue", "#cdade6", "#F9AEBB", "pink", "#e7e7e7cb")) +
  hrbrthemes::theme_ipsum(grid="Y") +
  labs(title="Years coding", subtitle = "Amongst people who code", y="# respondents", x="") +
  theme(
    plot.title = element_text(size=22), legend.text = element_text(size=18),
    plot.subtitle = element_text(size=20),
    axis.title.y = element_text(size=20, hjust = 0.5, vjust = 2.5),
    axis.text.y = element_text(size=15), 
    axis.text.x =  element_text(angle=45, hjust=0.9, size=14)
  ) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  facet_grid(~is_stem)

dev.off()

# Plot 6

get_corr <- function() {
  coder_p6 <- coder |> 
    # filter(is_male == gender) |>
    select(all_of(c(raw_oss_enthuasiast_cols, raw_open_sci_cols))) |>
    drop_na()

  colnames(coder_p6) <- colnames(coder_p6) |>
    str_replace_all("_", " ") |> 
    str_remove_all("(value|ord) ?") |> 
    str_to_title()

  colnames(coder_p6) <- map_chr(colnames(coder_p6), ~gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', .x,perl = TRUE))

  colnames(coder_p6)[5] <- "CLA"

  coder_p6 |>
    corrr::correlate() |>
    # corrr::rearrange(absolute = FALSE) |>
    corrr::shave() |>
    corrr::stretch(na.rm=TRUE) |>
    mutate(r = round(r,2))

}

coder_p6M<-get_corr(1)
coder_p6F<-get_corr(0)

coder_p6 <- coder_p6M |> 
  left_join(coder_p6F, by = c("x", "y"), suffix=c("M", "F")) |> 
  mutate(rM = str_remove(rM, "^0(?!$)"), rF = str_remove(rF, "^0(?!$)"), 
         rM = str_replace_all(rM, "^?-0", "-"), rF = str_replace_all(rF, "^?-0", "-")) |>
  unite(r, rM:rF, sep="(") |>
  mutate(r = paste(r, ")", sep=""))

footnote <- html("<br><i>Note.</i> Correlation values are `Male(Female)`. OL=Oss License; C=Code of Conduct; CG=Contrib Guide; A=Active;\nCLA=Contributor License Agreement;RM=Responsive Maintainer;\nWC=Welcoming Community;WU=Widespread Use; SC=Share Code; APC=Accessibility Paper Code; PCC=Paper Code Citability")

coder_p6 |> pivot_wider(names_from = x, values_from=r) |> 
  corrr::fashion() |>
  gt::gt() |>
  apa_style() |>
  opt_align_table_header(align = "left") |>
  tab_options(table.font.size = 24) |>
  tab_footnote(
    footnote = footnote
  ) 
  gt::gtsave("table_corr2.png")
  # corrr::stretch(na.rm = TRUE) |>
  # tidygraph::as_tbl_graph(directed=FALSE) |>
  # ggraph() +
  # geom_edge_fan(aes(label = round(r,2), size = r, filter = r > 0.2, alpha = after_stat(index), colour = r), 
  #               angle_calc = 'along',
  #               label_dodge = unit(2.5, 'mm'), show.legend = FALSE) +
  # geom_edge_fan(aes(alpha = after_stat(index), colour = r),show.legend = FALSE) +
  # geom_node_label(aes(label=str_to_title(str_replace_all(name, "_", " "))), repel=TRUE) +
  # geom_node_point(size=5, shape=21, fill="#b1aaa4", color="black") +
  # scale_edge_colour_gradient2(low="red", high="#35357a", midpoint=0) +
  # theme_graph()


# Summary DV ~ IV plot ----------------------------------------------------
min_cor = 0
p1.1 <- plot_simple(df, gender_binary, "Gender")
p1.2 <- plot_simple(df, dept_students_binary, "Department")
p1.3 <- plot_simple(df, first_line_code_before_18, "First line of code\nbefore 18 years old")
p1.4 <- plot_simple(df, year_born, "Year born") +  theme(axis.text.x = element_text(angle=45, vjust=.6))

p2.1 <- plot_bar_chart(coder, self_id_as_coder, gender_binary, "Self Id as coder")
p2.2 <- plot_bar_chart(coder, value_comp_skills_wrt_domain_ord, dept_students_binary, "Feeling computational\nskills valued")
p2.3 <- plot_bar_chart(coder, pct_social_contacts_coding_ord, gender_binary, "Pct Social Contacts\nprogramming")

p1.1 + p1.2 + p1.3 + p1.4 + p2.1 + p2.2 + p2.3 + 
  plot_layout(guides = 'collect', nrow=1) + 
  plot_annotation(title = "Demographics and descriptive statistics. ", 
                  caption = "Note. Feeling Computational skills valued (1=Less valued than my research capacity; 4=More valued than my research ability); Pct Social Contacts programming (1=Isolated; 3=Surrounded)",
                  theme = theme(plot.title = element_text(size = 22), plot.caption = element_text(size=16))) +
  ggsave("fig2.png", width = 32, height=8)




