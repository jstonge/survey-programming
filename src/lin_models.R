library(here)
library(janitor)
library(tidyverse)
library(brms)
library(tidybayes)
library(rstanarm)
library(scales)
library(patchwork)
library(broom.mixed)

source("src/helpers.R")

BAYES_SEED <- 1234
set.seed(1234)

df <- readr::read_csv(here("output", "data_clean.csv"))

df$value_oss_license_ord <- factor(df$value_oss_license_ord, levels = c(0,1,2,3,4,5), ordered = TRUE)
df$self_id_as_coder <- factor(df$self_id_as_coder, levels = c("No","Maybe","Yes"), ordered = TRUE)
df$value_welcoming_community_ord <- factor(df$value_welcoming_community_ord, levels = c(0,1,2,3,4,5), ordered = TRUE)
df$value_responsive_maintainers_ord <- factor(df$value_responsive_maintainers_ord, levels = c(0,1,2,3,4,5), ordered = TRUE)

coder <- subset(df, is_coder == "coder")


# --------------- Model 1: Self_id ~ Gender + Early experience --------------- #

priors <- c(prior(normal(0, 1.5), class = Intercept))
inits <- list(Intercepts = c(-1, 0, 1.2))

tabyl(coder, self_id_as_coder, is_male, first_line_code_before_18)

fit1 <- brm(
  bf(self_id_as_coder ~ is_male + first_line_code_before_18),
  data = coder,
  family = cumulative(link = "logit"),
  prior = priors,
  init = rep(list(inits), 4),
  save_warmup = TRUE,
  chains = 4, iter = 2000, seed = BAYES_SEED, cores = 4,
  backend = "cmdstanr", refresh = 0,
  file = ".cache/fit1"
)

simulated_conditions <- tribble(
  ~title, ~newdata,
  "Is Male = 0, Early Exp. = 0", tibble(is_male = 0, first_line_code_before_18 = 0),
  "Is Male = 1, Early Exp. = 0", tibble(is_male = 1, first_line_code_before_18 = 0),
  "Is Male = 0, Early Exp. = 1", tibble(is_male = 0, first_line_code_before_18 = 1),
  "Is Male = 1, Early Exp. = 1", tibble(is_male = 1, first_line_code_before_18 = 1),
) |> 
  mutate(pred_plot = map2(newdata, title, ~{
    fit1 |> 
      add_predicted_draws(newdata = .x) |> 
      ungroup() |> 
      count(.prediction) |> 
      mutate(prop = n / sum(n),
             prop_nice = label_percent(accuracy = 0.1)(prop)) |> 
      ggplot(aes(x = .prediction, y = n)) +
      geom_col(aes(fill = .prediction)) +
      geom_text(aes(y = 50, label = prop_nice), color = "white", size = 3.5, 
                angle = 90, hjust = 0) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_fill_viridis_d(option = "rocket", end = 0.85, guide = "none") +
      labs(x = "", y = "draws", title = .y) +
      theme(plot.title = element_text(size = rel(1), hjust = 0.5))
  }))

# png("ordered_log_self_id.png", width=2000, height=2000, res=300)
wrap_plots(simulated_conditions$pred_plot, nrow = 2, byrow = FALSE) +
  plot_annotation("Do you consider yourself to be a coder/programmer?", 
  tag_levels = 'A') + theme(text = element_text('Arial Narrow')) 
# dev.off()


# --------------- Model 2: value ~ Gender + Early experience --------------- #

priors <- c(prior(normal(0, 1.5), class = Intercept))
inits <- list(Intercepts = c(-1, 0, 1.2))

coder2 <- coder |> 
  select(value_responsive_maintainers_ord, is_male, experienced_coder, value_oss_license_ord,
  value_welcoming_community_ord, first_line_code_before_18) |>
  drop_na()

tabyl(coder2, value_welcoming_community_ord, is_male, first_line_code_before_18)
tabyl(coder2, value_responsive_maintainers_ord, is_male, first_line_code_before_18)
tabyl(coder2, value_oss_license_ord, is_male, first_line_code_before_18)

formula1 <- bf(value_welcoming_community_ord ~ is_male + first_line_code_before_18)        # social
formula2 <- bf(value_responsive_maintainers_ord ~ is_male + first_line_code_before_18)     # expectations toward maintainer
formula3 <- bf(value_oss_license_ord ~ is_male + first_line_code_before_18)                # technical document tied to open source ideology

fit2_1 <- brm(formula1, data = coder2, family = cumulative(link = "logit"), prior = priors,
              init = rep(list(inits), 4), save_warmup = TRUE, chains = 4, 
              iter = 2000, seed = BAYES_SEED, cores = 4, backend = "cmdstanr", refresh = 0,
              file = ".cache/fit2_1")
 
fit2_2 <- brm(formula2, data = coder2, family = cumulative(link = "logit"), prior = priors,
              init = rep(list(inits), 4), save_warmup = TRUE, chains = 4, 
              iter = 2000, seed = BAYES_SEED, cores = 4, backend = "cmdstanr", refresh = 0,
              file = ".cache/fit2_2")

fit2_3 <- brm(formula3, data = coder2, family = cumulative(link = "logit"), prior = priors,
              init = rep(list(inits), 4), save_warmup = TRUE, chains = 4, 
              iter = 2000, seed = BAYES_SEED, cores = 4, backend = "cmdstanr", refresh = 0,
              file = ".cache/fit2_3")

simulated_conditions <- function(fit) {
  tribble(
  ~title, ~newdata,
  "Is Male = 0, Early Exp. = 0", tibble(is_male = 0, first_line_code_before_18 = 0),
  "Is Male = 1, Early Exp. = 0", tibble(is_male = 1, first_line_code_before_18 = 0),
  "Is Male = 0, Early Exp. = 1", tibble(is_male = 0, first_line_code_before_18 = 1),
  "Is Male = 1, Early Exp. = 1", tibble(is_male = 1, first_line_code_before_18 = 1),
) |> 
  mutate(pred_plot = map2(newdata, title, ~{
    {{ fit }} |> 
      add_predicted_draws(newdata = .x) |> 
      ungroup() |> 
      count(.prediction) |> 
      mutate(prop = n / sum(n),
             prop_nice = label_percent(accuracy = 0.1)(prop)) }
  )) 
}

sim_fit_2_1 <- simulated_conditions(fit2_1)
sim_fit_2_2 <- simulated_conditions(fit2_2)
sim_fit_2_3 <- simulated_conditions(fit2_3)

# pred_plot.x => WC
# pred_plot.y => RM
# pred_plot => Oss License
sim_fit_2 <- sim_fit_2_1 |> 
  left_join(sim_fit_2_2, by = c("title", "newdata")) |>
  left_join(sim_fit_2_3, by = c("title", "newdata"))

png("ordered_log_oss.png", width=3500, height=2500, res=300)
sim_fit_2 |>
  unnest(c(pred_plot.y, pred_plot), names_repair = "unique") |>
  select(title,  `.prediction...4`, `n...5`, `prop_nice...7`,  `n...9`, `prop_nice...11`)   |>
  rename(.prediction = `.prediction...4`) |>
  left_join(
    sim_fit_2 |>
      unnest(c(pred_plot.x)) |>
      select(title, .prediction, n, prop_nice), by = c("title", ".prediction")
  ) |>
  replace_na(list(n=0, prop_nice="0%")) |>
  pivot_longer(cols = starts_with("n"), values_to = "Count")  |>
  ggplot(aes(x = .prediction, y = Count)) +
      geom_col(aes(fill = .prediction, group = name), position = "dodge2") +
      facet_wrap(~title) +
      geom_text(
        aes(y = 50, label = glue::glue("{`prop_nice...7`} (RN)"), color = .prediction), 
        size = 5.5,  angle = 90,  hjust = 0, vjust=0.5, show.legend = FALSE
      ) +
      geom_text(
        aes(y = 50, label = glue::glue("{`prop_nice...11`} (L)"), color = .prediction), 
        size = 5.5,  angle = 90, hjust = 0, vjust=2.5, show.legend = FALSE
      ) +
      geom_text(
        aes(y = 50, label = glue::glue("{`prop_nice`} (WC)"), color = .prediction), 
            size = 5.5,  angle = 90, hjust = 0, vjust=-1.5, show.legend = FALSE
        ) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_fill_viridis_d(option = "rocket", end = 0.85, guide = "none") +
      scale_color_manual(values=rep(c("#d3d3d3c3", "white"), each=2)) +
      hrbrthemes::theme_ipsum_es() +
      xlab("Response") +
      theme(
        # plot.title = element_text(size = rel(1), hjust = 0.5),
        strip.text = element_text(size=20),
        axis.title.x = element_text(size=20, hjust = 0.5, vjust = -1.5),
        axis.title.y = element_text(size=20, hjust = 0.6, vjust = 2.5),
        axis.text.y = element_text(size=18)
  ) 
dev.off()

