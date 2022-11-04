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

source("src/lookup_questions.R") # import question names to label lookup 


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

    lookup_sel_other <- c(
        cols[str_detect(cols, " - Selected Choice(...\\d{1,2})?")],
        "How many advisors do you have?...23", 
        "What is your second advisor's main department?...58"
    )
    
    names(lookup_sel_other) <- c(
        cols[str_detect(cols, " - (Other|Fill in the blank:) - Text(...\\d{1,2})?")],
         "How many advisors do you have?...20", 
         "What is your second advisor's main department?...62"
    )

    for (i in seq_along(lookup_sel_other)) {
        if (str_detect(names(lookup_sel_other[i]), "Fill in the blank")) {
            # when fill in the blank
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

# Empty cols from questions to PIs in the survey. We just get rid of them for the moment.
col2del <- c(
    "What benefits do you see in programming? Please select all that apply. - Selected Choice...73",
    "What benefits do you see in programming? Please select all that apply. - Other - Text...74"
)

df <- read_csv(here("dat", "survey_2022_08_12.csv"), skip = 1) |>
  slice(2:n()) %>% # the first line is junk
  replace(is.na(.), "") |> # get rid of NAs in favor of empty strings
  select(-col2del) |>
  update_other_in_main_col() |>
  clean_time_expectation_cols() |>
  safely_rename(question2labs_lookup)


# 2. Recoding -----------------------------------------------

df["is_coder"] = if_else(df["reason_coding"] != "I  do not know how to code", "coder", "non coder")

write_csv(df, here("output", "data_clean.csv"))
write_csv(df, "/home/jstonge/Documents/phd/side_quest/shambolics/posts/survey-programming/data_clean.csv")
