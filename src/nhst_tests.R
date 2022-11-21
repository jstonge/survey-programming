library(here)
library(tidyverse)
library(patchwork)

df <- readr::read_csv(here("output", "data_clean.csv"))
df$pct_social_contacts_coding_ord <- factor(df$pct_social_contacts_coding_ord, levels = c(1,2,3))
coder <- subset(df, is_coder == "coder")

relevant_dv_cols <- c("gender_binary", "first_line_code_before_18", "dept_students_binary")
relevant_iv_cols <- c("self_id_as_coder", "pct_social_contacts_coding_ord", "value_comp_skills_wrt_domain_ord", 
                       "value_oss_avg", "value_open_sci_avg")

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

# Broad question: 
#   What factors influence the perceived benefits and costs of learning to program among
#   individuals and fields that traditionally do not engage in computational methods?

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


# ------------------------------- Hypothesis 2 ------------------------------- #

# H0: There is no relationship between *academic background* (STEM or Not-STEM) and feeling that _coding skills 
# are valued_ as much as other types of knowledge (two-tailed)
# HA1: There is a relationship between *being in STEM* feeling that _coding skills are valued_ 
# as much as other types of knowledge (one-tailed)

# IV: *Academic background* (categorical-binary)
# DV: How _coding skills are valued_ compared to domain expertise? (ordinal)

# Test: Wilcoxon-Mann Whitney test

# ------------------------------- Hypothesis 3 ------------------------------- #

# H0: *Gender* is not associated with the _proportion of social contacts_ likely to participate in a project that
# requires programming in the upcoming academic year.
# HA1: *Gender* is associated with the _proportion of social contacts_ that are likely to participate in a project
# that requires programming in the upcoming academic year (two-tailed)

# IV: *Gender* (categorical-binary)
# DV: _Prop social contacts_ who are likely to participate in a project that req prog in the upcoming academic year (categorical-3 levels)


# Test: Wilcoxon-Mann Whitney test


coder_3 <- coder |> 
    select(pct_social_contacts_coding, pct_social_contacts_coding_ord, gender_binary) |> 
    filter(!is.na(gender_binary), !is.na(pct_social_contacts_coding_ord)) |>
    mutate(pct_social_contacts_coding_binary = ifelse(pct_social_contacts_coding >= 70, "surrounded", "isolated"))

DV3 <- factor(coder_3$pct_social_contacts_coding_ord, levels=c(1,2,3))
IV3 <- coder_3$gender_binary

chisq3 <- broom::glance(chisq.test(IV3, DV3))

# when using full data
# wilcox.test(pct_social_contacts_coding ~ gender_binary, data=coder_3)

# ------------------------------- Hypothesis 4 ------------------------------- #

# H0: There is no relationship between *early programming experience* and the _perceived 
# importance of sharing code_ associated with an academic paper
# HA1: There is a relationship between *early programming experience* and the _perceived importance 
# of sharing code_ associated with an academic paper (one-tailed)

# IV: *Experience programming before 18* (categorical-binary)
# DV: _Importance of sharing code associated with an academic paper (ordinal)_

# Test: Wilcoxon-Mann Whitney tes
