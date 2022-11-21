# Plan:
# 0. Data Wrangling
# 1. code: distinguishing label and question, providing the type (scale, ordinal, nominal),
#          which values are missing data.
# 2. recode: do we want to reduce likert scales in fewer categos?
#            Do we want to reduce buckets into categos (e.g. 1, 2 or >3)?
# 3. computing/mutating: calculating new vars based on original ones, checks using tables.
# 4. Adding cross-tabulation (IV as cols x DV as rows) + filter first, then cross-tabulation.

library(tidyverse)
library(here)
library(assertthat)

source(here("src", "lookups.R")) # import names to label lookup


#' combine_cols
#'
#' Combine the value in names(x) in column x, grab the question name
#' up to question mark (getting rid of the junk), then
#' throws out x and names(x)
#'
#'
#' @param tbl: a tibble of the survey
#' @param x: named list(old=new)
#' @param pattern: string pattern to match the 'Other' values
#'
#' @return tbl
combine_cols <- function(tbl, x, pattern) {
    col2repl <- names(x)
    if (is.list(x)) x <- x[[1]]
    assert_that(has_name(tbl, c(x,col2repl)))
    tbl %>%
        mutate(tmp_c = if_else(.data[[x]] == pattern, .data[[col2repl]], .data[[x]])) %>%
        select(-x, -col2repl) %>%
        rename_with(~str_extract(x, "^.+?\\?"), tmp_c) # rename to shorter version
}

#' safely_rename
#'
#' @param tbl
#' @param x: named list(old value = new value)
#'
#' @return
safely_rename <- function(tbl, x) {
    assert_that(has_name(tbl, names(x)))
    rename_with(tbl, ~str_extract(x, as.character(x)), names(x))
}

#' update_other_in_main_col
#'
#' In qualtrics, when you add a box saying 'Other' it creates 2 columns.
#' This function combine the columns ending with 'Other' or 'Fill in the blank'
#' with the corresponding columns ending with 'Selected choice'
#' Note that the read_csv function add ...\\d{1,2} when it finds columns with
#' identical name.
#'
#' @param tbl: a tibble of the survey
#'
#' @return tbl
update_other_in_main_col <- function(tbl) {
    cols <- colnames(tbl)

    # In the case of `Fill in the blank` columns,
    # we want these columns and not the `selected choice` ones.
    fill_in_cols <- cols[str_detect(cols, " - Fill in the blank: - Text(...\\d{1,2})?")]
    fill_in_cols_qs <- str_extract(fill_in_cols, "^.+\\?")

    selected_choice_cols <- cols[str_detect(cols, " - Selected Choice(...\\d{1,2})?")]
    selected_choice_cols_qs <- str_extract(selected_choice_cols, "^.+\\?")

    fill_in_old_cols <- selected_choice_cols[selected_choice_cols_qs %in% fill_in_cols_qs]
    selected_choice_cols <- selected_choice_cols[which(!selected_choice_cols_qs %in% fill_in_cols_qs)]

    lookup_sel_other <- c(
        fill_in_cols,
        selected_choice_cols,
        "How many advisors do you have?...23",
        "What is your second advisor's main department?...58"
    )

    other_cols <- cols[str_detect(cols, " - Other - Text(...\\d{1,2})?")]    

    names(lookup_sel_other) <- c(
        fill_in_old_cols,
        other_cols,
         "How many advisors do you have?...20",
         "What is your second advisor's main department?...62"
    )

    for (i in seq_along(lookup_sel_other)) {
        if (str_detect(names(lookup_sel_other[i]), "Fill in the blank")) {
            tbl <- combine_cols(tbl, lookup_sel_other[i], pattern = "Other")
        } else {
            tbl <- combine_cols(tbl, lookup_sel_other[i], pattern = "Other")
        }
    }

    return(tbl)
}

#' clean_time_expectation_cols
#'
#' This function takes care of combining the many columns that have to do
#' with advisors main department and expectation.
#'
#' @param tbl: a tibble of the survey
#'
#' @return tbl
clean_time_expectation_cols <- function(tbl) {
    # tbl <-df
    tbl <- rename_with(tbl, ~str_remove(.x, pattern="...59"), `For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?...59`)

    # list(old=new)
    lookup_time_exp <- list(
        "What is your advisor's main department?" = "What is your first advisor's main department?",
        "What is your advisor's first main department?" = "What is your first advisor's main department?",
        "For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?...57" = "For any of your current projects,, do you think you spend more time than your supervisor expect on programming?",
        "For any of your current projects,, do you think you spend more time than your first supervisor expect on programming?...61" = "For any of your current projects,, do you think you spend more time than your supervisor expect on programming?",
        "For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?...63" = "For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?"
    )

    for (i in seq_along(lookup_time_exp)) {
        tbl <- combine_cols(tbl, lookup_time_exp[i], pattern="")
    }

    lookup2 <- c(
        "What is your third advisor's main department?" = "third_adv_dept",
        "For any of your current projects,, do you think you spend more time than your third supervisor expect on programming?" = "third_adv_expect_time_coding",
        "What is your second advisor's main department?" = "second_adv_dept",
        "What is your first advisor's main department?" = "first_adv_dept",
        "For any of your current projects,, do you think you spend more time than your supervisor expect on programming?" = "first_adv_expect_time_coding",
        "For any of your current projects,, do you think you spend more time than your second supervisor expect on programming?" = "second_adv_expect_time_coding"
        )

    for (i in seq_along(lookup2)) {
        tbl <- safely_rename(tbl, lookup2[i])
    }

    # reorder cols to put lookup columns at the end
    tbl <- bind_cols(select(tbl, -lookup2), tbl[lookup2])

    return(tbl)
}


# 0. Wrangling ---------------------------------------------

# Some questions are asked many times because of the logic of our survey.
# For example, the question about 'benefits of programming' 
# it is in the same block than a question designed for grad students.
# So we asked it once for grad students (Q41) and in alternative another block without the
# question for the grad student (Q43).  Same thing for question about the 
# 'supervisors expectations' which depend on how many advisors a student has (Q22,29,27,31,33,35,37).
# For some reason, as we export survey with more responses the way to identify these columns
# change. For example, Q43 when from Selected Choice...73 => Selected Choice...74 and Text...74 => Text...75.
# Similarly, expect on program?...57 => expect on program?...58 and same for all others.
# right now we are fixing them by hand. 

# affected_questions <- c("What is your second advisor", 
# "What benefits do you see in programming", 
# "For any of your current projects,,",
# "At what age did you write your first line of code or program")

# read_csv(here("dat", "survey_2022_08_12.csv"), skip = 1, show_col_types = FALSE) |>
#     slice(2:n()) %>% # the first line is junk
#       replace(is.na(.), "") |> 
#     select(starts_with(affected_questions[4])) |> View()

# read_csv(here("dat", "survey_2022_11_16.csv"), skip = 1, show_col_types = FALSE) |>
#     slice(2:n()) %>% # the first line is junk
#       replace(is.na(.), "") |> 
#     select(starts_with(affected_questions[4])) |> View()  

col2del <- c(
    "What benefits do you see in programming? Please select all that apply. - Selected Choice...73",
    "What benefits do you see in programming? Please select all that apply. - Other - Text...74"
    # "At what age did you write your first line of code or program? (e.g., webpage, Hello World, Scratch project)...25"
)


df <- read_csv(here("dat", "2022_11_20.csv"), skip = 1, show_col_types = FALSE) |>
  slice(2:n()) %>% # the first line is junk
  replace(is.na(.), "") |> # get rid of NAs in favor of empty strings
  select(-col2del) |>
  update_other_in_main_col() |>
  clean_time_expectation_cols() |>
  safely_rename(question2labs_lookup) |>
  filter(!email %in% c("jstonge1@uvm.edu", "Erik.weis@uvm.edu")) |>
  select(-c('progress', 'end_survey', 'start_survey', 
            'agree_term', 'captcha_score', 'do_del',
            'is_finished')) |>
  mutate(
    duration_sec = as.integer(duration_sec),
    underrep_group = ifelse(underrep_group == "Yes", "underrep", "not-underrep"),
    dept_students_lab = recode(dept_students, !!!dep_lookup),
    enough_instit_support_ord = case_when(
            enough_instit_support == "No and I wish there were" ~ 1,
            enough_instit_support == "No and that's fine, I'm learning on my own" ~ 2,
            enough_instit_support == "Yes" ~ 3,
            TRUE ~ 999,
      ),
    more_time_learning_to_code_ord = case_when(
            more_time_learning_to_code == "Not really" ~ 3,
            more_time_learning_to_code == "Yes but that's fine, I'm learning while doing" ~ 2,
            more_time_learning_to_code == "Yes, I wish I had more time to learn programming" ~ 1,
            TRUE ~ 999,
      ),
    years_coding_c = recode(years_coding, !!!lookup_ord_5),
    first_line_code_c = recode(first_line_code, !!!lookup_ord_4)
    )
# 2. Recoding -----------------------------------------------

THIRTY_MINS <- 60*30

df <- df |>
    mutate(
        is_coder = if_else(reason_coding != "I  do not know how to code", "coder", "non coder"),
        gender_binary = case_when(
            pref_pronouns == "he/him" ~ "male",
            pref_pronouns == "he/they" ~ "male",
            pref_pronouns == "she/her" ~ "female",
            pref_pronouns == "she/they" ~ "female",
            TRUE ~ "",
        ),
        pct_social_contacts_coding_ord = case_when(
            pct_social_contacts_coding <= 30 ~ "1",
            pct_social_contacts_coding <= 60 ~ "2",
            pct_social_contacts_coding <= 100 ~ "3",
            TRUE ~ ""
        ),
        first_line_code_before_18 = case_when(
            first_line_code == "5 - 10 years" ~ "yes",
            first_line_code == "11 - 17 years" ~ "yes",
            first_line_code == "25 - 34 years" ~ "no",
            first_line_code == "18 - 24 years" ~ "no",
            TRUE ~ "",
        ),
        duration_sec = ifelse(duration_sec > THIRTY_MINS, THIRTY_MINS, duration_sec ),
        dept_students_binary = recode(dept_students, !!!dep_binary_lookup),
        ethnicity_binary = recode(ethnicity, !!!ethnicity_binary_lookup),
        value_comp_skills_wrt_domain_ord = recode(value_comp_skills_wrt_domain, !!!lookup_ord_10),
        value_learn_code_in_field_ord = recode(value_learn_code_in_field, !!!lookup_ord_2),
        self_expect_time_coding_ord = recode(self_expect_time_coding, !!!lookup_ord_9),
        coding_on_future_opportunities_ord = recode(coding_on_future_opportunities, !!!lookup_ord_1),
        # open source attitude
        value_oss_license_ord = recode(value_oss_license, !!!lookup_ord_3),
        value_coc_ord = recode(value_coc, !!!lookup_ord_3),
        value_contrib_guide_ord = recode(value_contrib_guide, !!!lookup_ord_3),
        value_cla_ord = recode(value_cla, !!!lookup_ord_3),
        value_active_ord = recode(value_active, !!!lookup_ord_3),
        value_responsive_maintainers_ord = recode(value_responsive_maintainers, !!!lookup_ord_3),
        value_welcoming_community_ord = recode(value_welcoming_community, !!!lookup_ord_3),
        value_widespread_use_ord = recode(value_widespread_use, !!!lookup_ord_3),
        # open science attitude
        value_paper_code_citability_ord = recode(value_paper_code_citability, !!!lookup_ord_3),
        value_accessibility_paper_code_ord = recode(value_accessibility_paper_code, !!!lookup_ord_3),
        value_share_code_ord = recode(value_share_code, !!!lookup_ord_3),
    )


raw_oss_enthuasiast_cols <- c("value_oss_license_ord", "value_coc_ord", "value_contrib_guide_ord", "value_cla_ord", "value_active_ord",
                              "value_responsive_maintainers_ord", "value_welcoming_community_ord", "value_widespread_use_ord")
                            
raw_open_sci_cols <- c("value_share_code_ord", "value_accessibility_paper_code_ord", "value_paper_code_citability_ord")


possible_score_oss = length(raw_oss_enthuasiast_cols) * 5
df$value_oss_raw_score <- rowSums(select(df, all_of(raw_oss_enthuasiast_cols)))
df$value_oss_avg <- df$value_oss_raw_score / possible_score_oss

possible_score_open_sci = length(raw_open_sci_cols) * 5
df$value_open_sci_raw_score <- rowSums(select(df, all_of(raw_open_sci_cols)))
df$value_open_sci_avg <- df$value_open_sci_raw_score / possible_score_open_sci

# other stufff
df$gender_binary <- ifelse(df$response_id == "R_2q97KoU52AZ4j4m", "female", df$gender_binary)
df$gender_binary <- ifelse(df$response_id == "R_1IzbZEKKlq8p3LG", "male", df$gender_binary)
df <- subset(df, response_id != "R_bJEi7XQLGWe6GPf") # uncomplete survey

write_csv(df, here("output", "data_clean.csv"))
write_csv(df, "/home/jstonge/Documents/phd/side_quest/shambolics/posts/survey-programming/data_clean.csv")



# df_enthuasiast_fa <- factanal(df_enthuasiast, factors = 1)

# broom::tidy(df_enthuasiast_fa) 
# #   mutate(across(c(fl1, fl2), ~ifelse(abs(.x) < .1, 0, .x))) |>
#     # arrange(fl1, fl2)

# Lambda <- df_enthuasiast_fa$loadings
# Psi <- diag(df_enthuasiast_fa$uniquenesses)
# S <- df_enthuasiast_fa$correlation
# Sigma <- Lambda %*% t(Lambda) + Psi

# round(S - Sigma, 6) |> View()
