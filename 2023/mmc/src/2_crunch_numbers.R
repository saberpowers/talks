
get_batter_event <- function(event) {

  batter_event <- dplyr::case_when(
    grepl("Pickoff", event) ~ "Other",
    grepl("Caught Stealing", event) ~ "Other",
    event %in% c("Wild Pitch", "Game Advisory", "Sac Bunt", "Intent Walk", "Triple Play",
      "Field Out", "Forceout", "Fielders Choice", "Passed Ball", "Stolen Base 2B", "Batter Out",
      "Field Error", "Runner Double Play", "Sac Bunt Double Play", "Catcher Interference") ~ "Other",
    event %in% c("Runner Out", "Balk") ~ "Strikeout",
    event %in% c("Strikeout Double Play", "Strikeout") ~ "Strikeout",
    event %in% c("Grounded Into DP", "Fielders Choice Out", "Groundout", "Bunt Groundout") ~ "Groundout",
    event %in% c("Double Play", "Sac Fly Double Play", "Bunt Lineout", "Bunt Pop Out", "Pop Out",
      "Flyout", "Lineout", "Sac Fly"
    ) ~ "Flyout",
    TRUE ~ event
  )

  return(batter_event)
}

is_on_base <- function(batter_event) {

  is_on_base <- as.numeric(batter_event %in% c("Single", "Double", "Triple", "Home Run", "Walk", "Hit By Pitch"))
  return(is_on_base)
}

is_hit <- function(batter_event) {

  is_hit <- as.numeric(batter_event %in% c("Single", "Double", "Triple", "Home Run"))
  return(is_hit)
}

is_at_bat <- function(batter_event) {

  is_at_bat <- as.numeric(batter_event %in% c("Single", "Double", "Triple", "Home Run", "Strikeout", "Groundout", "Flyout"))
  return(is_at_bat)
}

estimate_population_variance <- function(
  observed_value,
  noise_variance,
  population_mean = weighted.mean(observed_value, w = 1 / noise_variance, na.rm = TRUE),
  max_iterations = 1e4,
  tolerance = 1e-7 * var(observed_value)
) {

  point_estimate = (observed_value - population_mean)^2 - noise_variance
  last_population_variance = 0
  population_variance = mean((observed_value - population_mean)^2)

  t = 0
  while(abs(population_variance - last_population_variance) > tolerance & t < max_iterations) {

    t = t + 1
    last_population_variance = population_variance
    weight = (noise_variance + population_variance)^{-2}
    population_variance = weighted.mean(point_estimate, w = weight)
  }

  return(population_variance)
}
  


data_list <- list()
for (season in 2017:2022) {
  files <- list.files(glue::glue("~/Desktop/mlb_data/{season}"))
  for (file in files) {
    game_id <- gsub(".*?([0-9]+).*", "\\1", file)
    data_list[[game_id]] <- read.csv(glue::glue("~/Desktop/mlb_data/{season}/{file}")) |>
      tibble::add_column(game_id = as.integer(game_id), season = season, .before = 1)
  }
}


data <- do.call(dplyr::bind_rows, args = data_list) |>
  dplyr::filter(event != "Game Advisory")

data |>
  dplyr::filter(event != "Wild Pitch") |>
  dplyr::mutate(
    runs_on_play = runs_on_event - mid_event_runs,
    pre_event_num = 1 + pre_event_outs + (!is.na(pre_event_runner_1b_id)) + (!is.na(pre_event_runner_2b_id)) + (!is.na(pre_event_runner_3b_id)),
    pre_play_num = 1 + pre_play_outs + (!is.na(pre_play_runner_1b_id)) + (!is.na(pre_play_runner_2b_id)) + (!is.na(pre_play_runner_3b_id)),
    post_event_num = runs_on_event + post_outs + (!is.na(post_runner_1b_id)) + (!is.na(post_runner_2b_id)) + (!is.na(post_runner_3b_id)),
    post_play_num = runs_on_play + post_outs + (!is.na(post_runner_1b_id)) + (!is.na(post_runner_2b_id)) + (!is.na(post_runner_3b_id))
  ) |>
  dplyr::filter(
    inning <= 9,
    (post_outs != 3 & pre_event_num != post_event_num) |
      (post_outs != 3 & pre_play_num != post_play_num) |
      (post_outs == 3 & pre_event_num < 3) |
      (post_outs == 3 & pre_play_num < 3)
  ) |>
  as.data.frame() |>
  nrow()

baseout_transitions <- data |>
  dplyr::filter(
    inning <= 9,
#    event %in% c(
#      "Bunt Groundout", "Bunt Lineout", "But Pop Out", "Catcher Interference", "Double",
#      "Double Play", "Field Error", "Field Out", "Fielders Choice", "Fielders Choice Out", "Flyout",
#      "Forceout", "Grounded Into DP", "Groundout", "Hit By Pitch", "Home Run", "Lineout", "Pop Out",
#      "Sac Bunt", "Sac Fly", "Sac Fly Double Play", "Single", "Strikeout", "Strikeout Double Play",
#      "Triple", "Triple Play", "Walk"
#    )
  ) |>
  dplyr::mutate(
    pre_base_out_state = glue::glue("{1 * !is.na(pre_event_runner_1b_id)}{1 * !is.na(pre_event_runner_2b_id)}{1 * !is.na(pre_event_runner_3b_id)}{pre_event_outs}"),
    post_base_out_state = glue::glue("{1 * !is.na(post_runner_1b_id)}{1 * !is.na(post_runner_2b_id)}{1 * !is.na(post_runner_3b_id)}{post_outs}"),
    runs = runs_on_event
  ) |>
  dplyr::count(pre_base_out_state, post_base_out_state, runs) |>
  dplyr::group_by(pre_base_out_state) |>
  dplyr::mutate(pre_runs = 0, post_runs = runs, prob = n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::bind_rows(
    tibble::tibble(
      pre_base_out_state = c("0000", "0003"),
      pre_runs = c(0, 0),
      post_base_out_state = c("0000", "0003"),
      post_runs = c(0, 0),
      prob = c(0, 1)
    )
  ) |>
  dplyr::select(dplyr::starts_with("pre_"), dplyr::starts_with("post_"), prob)

baseout_transitions_augmented <- tibble::tibble()
for (extra_runs in 0:9) {
  baseout_transitions_augmented <- baseout_transitions_augmented |>
    dplyr::bind_rows(
      baseout_transitions |>
        dplyr::mutate(
          pre_runs = pre_runs + extra_runs,
          post_runs = pmin(post_runs + extra_runs, 9),
          pre_state = glue::glue("{pre_base_out_state}{pre_runs}"),
          post_state = glue::glue("{post_base_out_state}{post_runs}")
        ) |>
        dplyr::select(pre_state, post_state, prob)
    )
}

transition_matrix <- baseout_transitions_augmented |>
  dplyr::group_by(pre_state, post_state) |>
  dplyr::summarize(prob = sum(prob), .groups = "drop") |>
  dplyr::arrange(post_state) |>
  tidyr::pivot_wider(names_from = post_state, values_from = prob, values_fill = 0) |>
  dplyr::arrange(pre_state) |>
  dplyr::select(-pre_state) |>
  as.matrix()

transition_matrix_20_steps <- transition_matrix
for (step in 1:19) {
  transition_matrix_20_steps <- transition_matrix_20_steps %*% transition_matrix
}

base_out_run_exp <- transition_matrix_20_steps |>
  tibble::as_tibble() |>
  tibble::add_column(
    pre_state = sort(unique(baseout_transitions_augmented$pre_state)), .before = 1
  ) |>
  tidyr::pivot_longer(cols = -pre_state, names_to = "post_state", values_to = "prob") |>
  dplyr::mutate(outs = as.integer(substring(post_state, 4, 4)), runs = as.integer(substring(post_state, 5, 5))) |>
  dplyr::group_by(pre_state) |>
  dplyr::summarize(exp_runs = weighted.mean(runs, w = prob), .groups = "drop") |>
  dplyr::filter(substring(pre_state, 5, 5) == "0") |>
  dplyr::transmute(
    runner_1b = substring(pre_state, 1, 1) == 1,
    runner_2b = substring(pre_state, 2, 2) == 1,
    runner_3b = substring(pre_state, 3, 3) == 1,
    outs = as.integer(substring(pre_state, 4, 4)),
    exp_runs
  )

data <- data |>
  dplyr::mutate(
    spray_angle = (-atan((hit_loc_x - 130) / (213 - hit_loc_y)) + pi / 2) * 180 / pi,
    batter_event = get_batter_event(event),
    obp = is_on_base(batter_event),
    hit = is_hit(batter_event),
    at_bat = is_at_bat(batter_event),
    runner_1b = !is.na(pre_play_runner_1b_id),
    runner_2b = !is.na(pre_play_runner_2b_id),
    runner_3b = !is.na(pre_play_runner_3b_id),
    outs = pre_play_outs
  ) |>
  dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
  dplyr::mutate(
    pre_play_exp_runs = exp_runs,
    runner_1b = !is.na(post_runner_1b_id),
    runner_2b = !is.na(post_runner_2b_id),
    runner_3b = !is.na(post_runner_3b_id),
    outs = post_outs
  ) |>
  dplyr::select(-exp_runs) |>
  dplyr::left_join(base_out_run_exp, by = c("runner_1b", "runner_2b", "runner_3b", "outs")) |>
  dplyr::mutate(
    post_exp_runs = exp_runs,
    re24 = runs_on_event + post_exp_runs - pre_play_exp_runs
  ) |>
  dplyr::select(-runner_1b, -runner_2b, -runner_3b, -outs)

linear_weight <- data |>
  dplyr::group_by(batter_event) |>
  dplyr::summarize(linear_weight = mean(runs_on_event + post_exp_runs - pre_play_exp_runs)) |>
  dplyr::arrange(linear_weight)


# Modeling ----

model_data <- data |>
  dplyr::left_join(linear_weight, by = "batter_event") |>
  dplyr::filter(batter_event != "other", !is.na(exit_velo), !is.na(launch_angle), !is.na(spray_angle)) |>
  dplyr::select(game_id, pa_index, batter_event, linear_weight, exit_velo, launch_angle, spray_angle)

model <- ranger::ranger(linear_weight ~ exit_velo + launch_angle, data = model_data, min.node.size = 50)

xlw <- model_data |>
  dplyr::mutate(xlw = model$predictions) |>
  dplyr::select(game_id, pa_index, xlw)

big_data <- data |>
  dplyr::filter(batter_event != "Other") |>
  dplyr::left_join(linear_weight, by = c("batter_event")) |>
  dplyr::left_join(xlw, by = c("game_id", "pa_index")) |>
  dplyr::mutate(xlw = dplyr::coalesce(xlw, linear_weight))

avg_data <- data |>
  dplyr::filter(at_bat == 1) |>
  dplyr::group_by(batter_id, season) |>
  dplyr::summarize(
    n = dplyr::n(),
    avg = mean(hit),
    var = var(hit),
    .groups = "drop"
  ) |>
  dplyr::filter(n >= 100) |>
  dplyr::mutate(noise = weighted.mean(var, w = n) / n) |>
  dplyr::summarize(
    pop_var = estimate_population_variance(observed_value = avg, noise_variance = noise),
    noise = weighted.mean(var, w = n)
  )


summary <- big_data |>
  dplyr::transmute(batter_id, season, obp, re24, linear_weight, xlw, re24_lw = re24 - linear_weight, lw_xlw = linear_weight - xlw) |>
  tidyr::pivot_longer(-(batter_id:season), names_to = "stat") |>
  dplyr::group_by(batter_id, season, stat) |>
  dplyr::summarize(
    n = dplyr::n(),
    mean = mean(value),
    var = var(value),
    .groups = "drop"
  ) |>
  dplyr::filter(n >= 100) |>
  dplyr::group_by(stat) |>
  dplyr::mutate(noise = weighted.mean(var, w = n) / n) |>
  dplyr::summarize(
    pop_var = estimate_population_variance(observed_value = mean, noise_variance = noise),
    noise = weighted.mean(var, w = n)
  )

plot_stabilization <- function(pop_var, noise, col = "black", lwd = 1, grid = 1:1000, x_scale = 1) {
  lines(x = grid / x_scale, y = pop_var / (pop_var + noise / grid), col = col, lwd = lwd)
}

plot_all <- function(stats, colors, legend = NULL) {
  plot(
    x = 1:1000,
    y = rep(NA, 1000),
    ylim = c(0, 1),
    type = "n",
    axes = FALSE,
    xlab = "# of PA (in each sample)",
    ylab = "Split-sample correlation"
  )
  axis(1, at = c(0, 600, 1000))
  axis(2, at = c(0, 0.5, 1))
  abline(h = 0.5, lty = 2)
  abline(v = 600, lty = 2)

  for (i in 1:length(stats)) {
    summary |>
      dplyr::filter(stat == stats[i]) |>
      with(plot_stabilization(pop_var = pop_var, noise = noise, col = colors[i], lwd = 2))
  }

  if (!is.null(legend)) {
    legend("topleft", legend = legend, col = colors, lty = 1, lwd = 2, bty = "n")
  }
}

rice_blue <- rgb(0, 32, 91, maxColorValue = 255)
rice_gray <- rgb(124, 126, 127, maxColorValue = 255)
rice_rich_blue <- rgb(10, 80, 150, maxColorValue = 255)
rice_medium_blue <- rgb(77, 154, 212, maxColorValue = 255)

pdf("~/Downloads/re24_lw.pdf", height = 5, width = 10)
plot_all(
  stats = c("linear_weight", "re24"),
  colors = c(rice_blue, rice_gray),
  legend = c("LW", "RE24")
)
dev.off()

pdf("~/Downloads/re24_lw_diff.pdf", height = 5, width = 10)
plot_all(
  stats = c("linear_weight", "re24", "re24_lw"),
  colors = c(rice_blue, rice_gray, rice_rich_blue),
  legend = c("LW", "RE24", "RE24 - LW")
)
dev.off()

pdf("~/Downloads/re24_lw_xlw.pdf", height = 5, width = 10)
plot_all(
  stats = c("xlw", "linear_weight", "re24"),
  colors = c(rice_medium_blue, rice_blue, rice_gray),
  legend = c("xLW", "LW", "RE24")
)
dev.off()

pdf("~/Downloads/re24_lw_xlw_diff.pdf", height = 5, width = 10)
plot_all(
  stats = c("xlw", "linear_weight", "re24", "lw_xlw"),
  colors = c(rice_medium_blue, rice_blue, rice_gray, rice_rich_blue),
  legend = c("xLW", "LW", "RE24", "LW - xLW")
)
dev.off()

pdf("~/Downloads/avg_obp.pdf", height = 5, width = 10)
plot_all(stats = "obp", colors = rice_blue)
avg_data |>
  with(plot_stabilization(pop_var = pop_var, noise = noise, col = rice_gray, lwd = 2, x_scale = 0.9))
legend("topleft", legend = c("OBP", "AVG"), col = c(rice_blue, rice_gray), lty = 1, lwd = 2, bty = "n")
dev.off()
