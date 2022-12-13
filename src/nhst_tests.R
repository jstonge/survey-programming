library(here)
library(tidyverse)
library(patchwork)
library(janitor)

set.seed(42)

df <- readr::read_csv(here("output", "data_clean.csv"))

coder <- subset(df, is_coder == "coder")

# Openess related cols: all on the same scale
raw_oss_enthuasiast_cols <- c("value_oss_license_ord", "value_coc_ord", "value_contrib_guide_ord", "value_active_ord", "value_cla_ord", "value_responsive_maintainers_ord", "value_welcoming_community_ord", "value_widespread_use_ord")
raw_open_sci_cols <- c("value_share_code_ord", "value_accessibility_paper_code_ord", "value_paper_code_citability_ord")


# ------------------------------- Hypothesis 1 ------------------------------- #


# H0: There is no relationship between *gender* and _self-identifying as a programmer_ (two-tailed)
# HA1: There is a relationship between *identifying as a male* and self-identifying as a programmer (one-tailed)

# IV: *Gender* (categorical-binary)
# DV: _Self-identifying as a programmer_ (categorical-3 levels)

# Test: Chi-squared test

coder_1 <- coder |> 
    select(self_id_as_coder, gender_binary, email) |>
    filter(!is.na(gender_binary))


DV1 <- factor(coder_1$self_id_as_coder, levels = c("Yes", "Maybe", "No"))
IV1 <- coder_1$gender_binary

chisq1 <- broom::glance(chisq.test(table(DV1, IV1)))

# Also see lin model 1


# ------------------------------- Hypothesis 2 ------------------------------- #


# H0: There is no relationship between *academic background* (STEM or Not-STEM) and feeling that _coding skills 
# are valued_ as much as other types of knowledge (two-tailed)
# HA1: There is a relationship between *being in STEM* feeling that _coding skills are valued_ 
# as much as other types of knowledge (one-tailed)

# IV: *Academic background* (categorical-binary)
# DV: How _coding skills are valued_ compared to domain expertise? (ordinal)

# Test: Wilcoxon-Mann Whitney test

tabyl(coder, value_comp_skills_wrt_domain, is_male, show_na = FALSE)

# DV=1 Ordinal; IV=2levels => Wilcoxon-Mann Whitney test
wilcox.test(value_comp_skills_wrt_domain_ord ~ is_male, 
             data = coder, 
             subset = value_comp_skills_wrt_domain_ord != 999)


# ------------------------------- Hypothesis 3 ------------------------------- #


# H0: *Gender* is not associated with the _proportion of social contacts_ likely to participate in a project that
# requires programming in the upcoming academic year.
# HA1: *Gender* is associated with the _proportion of social contacts_ that are likely to participate in a project
# that requires programming in the upcoming academic year (two-tailed)

# IV: *Gender* (categorical-binary)
# DV: _Prop social contacts_ who are likely to participate in a project that req prog in the upcoming academic year (categorical-3 levels)

# If we do not recode prop social contacts, we use a t-test.

pct_social_contacts <- coder$pct_social_contacts_coding
is_male <- coder$is_male



coder |> 
select(is_male, pct_social_contacts_coding) |> 
drop_na() |>
group_by(is_male) |> 
summarise(x = mean(pct_social_contacts_coding))

t.test(pct_social_contacts, is_male)

cohensD(
    pct_social_contacts[which(is_male==1)], 
    pct_social_contacts[which(is_male==0)]
)


# If we do recode as factors, we use Wilcoxon-Mann Whitney test

# coder_3 <- coder |> 
#     select(pct_social_contacts_coding, pct_social_contacts_coding_ord, gender_binary) |> 
#     filter(!is.na(gender_binary), !is.na(pct_social_contacts_coding_ord)) |>
#     mutate(pct_social_contacts_coding_binary = ifelse(pct_social_contacts_coding >= 70, "surrounded", "isolated"))

# DV3 <- factor(coder_3$pct_social_contacts_coding_ord, levels=c(1,2,3))
# IV3 <- coder_3$gender_binary

# chisq3 <- broom::glance(chisq.test(IV3, DV3))


# ------------------------------- Hypothesis 4 ------------------------------- #


# H0: There is no relationship between *early programming experience* and the _perceived 
# importance of sharing code_ associated with an academic paper
# HA1: There is a relationship between *early programming experience* and the _perceived importance 
# of sharing code_ associated with an academic paper (one-tailed)

# IV: *Experience programming before 18* (categorical-binary)
# DV: _Importance of sharing code associated with an academic paper (ordinal)_

# Test: Wilcoxon-Mann Whitney tes


# -------------------------------- Bootstrap 1 ------------------------------- #


oss_enthus <- coder[c(raw_oss_enthuasiast_cols, raw_open_sci_cols, "is_male")] |> 
    drop_na() |>
    pivot_longer(-is_male, names_to = "questions")

bootstrap_oss_enthuasiasm_sex <- function(N) {
    cols <- unique(oss_enthus$questions)
    nb_qs = length(cols)
    bootstrap_outer = vector("double", length = nb_qs)
    for (i in 1:nb_qs) {
        bootstrap_inner <- vector("logical", length = N)
        pop <- subset(oss_enthus, cols[i] == questions)
        pop_male <- subset(pop, is_male == TRUE)
        pop_female <- subset(pop, is_male == FALSE)
        for (j in 1:N) {
            attitude_m <- sample(pop_male$value, nrow(pop_male), replace=TRUE)
            attitude_f <- sample(pop_female$value, nrow(pop_female), replace=TRUE) 
            bootstrap_inner[[j]] <- mean(attitude_m) > mean(attitude_f)
        }
        bootstrap_outer[[i]] = sum(bootstrap_inner) / N
    }
    boots <- bootstrap_outer
    names(boots) <- cols
    return(boots)
}

(boots <- bootstrap_oss_enthuasiasm_sex(1000))

boots
