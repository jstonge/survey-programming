library(gt)
library(tidyverse)
library(glue)
library(here)
source("src/helpers.R")

df <- readr::read_csv(here("output", "data_clean.csv"))
coder <- subset(df, is_coder == "coder")


# ---------------------------------- table 1 --------------------------------- #


relevant_dv_cols <- c("gender_binary", "first_line_code_before_18", "dept_students_binary")
relevant_iv_cols <- c("self_id_as_coder", "pct_social_contacts_coding_ord", "value_comp_skills_wrt_domain_ord", "value_oss_avg", "value_open_sci_avg")

footnote <- html("<br><i>Note.</i> <i>N</i> = 57 (<i>n =</i> 44 respondents are programmers). Respondents ")
title <- html("<b> Table 1 </b><br><br><i> Sociodemographic Characteristics of Respondents </i><br><br>")
my_dvs <- c("value_comp_skills_wrt_domain", "first_line_code", "self_id_as_coder", "dept_students_binary")

make_apa_table(df, "gender_binary", my_dvs, title) |>
opt_align_table_header(align = "left") |>
 tab_options(table.font.size = 24, heading.title.font.size = 24) |>
 tab_footnote(
    footnote = footnote
  ) |>
  gt::gtsave("table1.png")


# ---------------------------------- table 2 --------------------------------- #


make_apa_table(
    coder, 
    "first_line_code_before_18", 
    "pct_social_contacts_coding"
  )
