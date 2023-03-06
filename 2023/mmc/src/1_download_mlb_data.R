
extract_schedule <- function(year) {

  schedule_filter <- glue::glue("sportId=1&gameType=R&startDate=01/01/{year}&endDate=12/31/{year}")
  endpoint <- glue::glue("http://statsapi.mlb.com:80/api/v1/schedule?{schedule_filter}")

  schedule_json <- jsonlite::fromJSON(endpoint)

  schedule <- do.call(dplyr::bind_rows, args = schedule_json$dates$games)

  return(schedule)
}

#' Track base-out state
#' 
#' Post-event base-out state is given to us for free. We get pre-event base-out state based on the
#' post-event base-out state of the previous event. However, we also need to get the pre-play
#' base-out state, which describes the base-out state prior to the final pitch of the event.
#' To do so, we start with the pre-event base-out state and account for mid-event runner movement.
#'
#' @param events This is the liveData$plays$allPlays section of the JSON from a game's GUMBO feed
#' @param runners_movement
#' 
#' @return a tibble with the following columns:
#'  pa_index, mid_event_outs, mid_event_runs,
#'  pre_event_runner_1b_id, pre_event_runner_2b_id, pre_event_runner_3b_id, pre_event_out,
#'  pre_play_runner_1b_id, pre_play_runner_2b_id, pre_play_runner_3b_id, pre_play_out,
#'  post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_out
#' 
track_base_out_state <- function(events, runners_movement) {

  # Step 1. Get the post state ----

  post_state <- tibble::tibble(
    pa_index = 1:nrow(events$matchup),
    # We coalesce with NA to handle the case where the value is NULL (e.g. on one reached 3B)
    post_runner_1b_id = dplyr::coalesce(events$matchup$postOnFirst$id, NA),
    post_runner_2b_id = dplyr::coalesce(events$matchup$postOnSecond$id, NA),
    post_runner_3b_id = dplyr::coalesce(events$matchup$postOnThird$id, NA),
    post_outs = events$count$outs
  )

  # Step 2. Get the pre-event state ----

  pre_event_state <- post_state |>
    dplyr::transmute(
      pa_index,
      pre_event_runner_1b_id = dplyr::lag(post_runner_1b_id, 1),
      pre_event_runner_2b_id = dplyr::lag(post_runner_2b_id, 1),
      pre_event_runner_3b_id = dplyr::lag(post_runner_3b_id, 1),
      pre_event_outs = dplyr::lag(post_outs, 1, default = 0) %% 3
    )

  zombie_runner <- dplyr::coalesce(
    sapply(events$playEvents, function(x) any(x$details$event == "Runner Placed On Base")),
    FALSE
  )

  zombie_runner_id <- do.call(dplyr::bind_rows, args = events$playEvents) |>
    dplyr::filter(details$event == "Runner Placed On Base") |>
    with(player$id)

  pre_event_state$pre_event_runner_2b_id[zombie_runner] <- zombie_runner_id

  # Step 3. Get the pre-play state (by far the most involved step) ----

  # Find the index of the last play of each event
  event_terminal_play_index <- tibble::tibble(
    pa_index = 1:length(events$playEvents),
    terminal_play_index = sapply(events$playEvents, function(x) max(x$index))
  )

  # Create a mid-event runners tibble which excludes runner movement on the final play of the event
  runners_movement_mid_event <- runners_movement |>
    dplyr::inner_join(event_terminal_play_index, by = "pa_index") |>
    dplyr::filter(play_index < terminal_play_index) |>
    dplyr::group_by(pa_index, runner_id)
    
  runners_movement_mid_event_start <- runners_movement_mid_event |>  
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(pa_index, runner_id, start_base)

  runners_movement_mid_event_end <- runners_movement_mid_event |>  
    dplyr::slice(dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(pa_index, runner_id, end_base, is_out, is_scoring_event)

  # Count outs and runs scored on mid-event runner movement
  mid_event_outs_runs <- runners_movement_mid_event_end |>
    dplyr::group_by(pa_index) |>
    dplyr::summarize(
      mid_event_outs = sum(is_out),
      mid_event_runs = sum(is_scoring_event),
      .groups = "drop"
    )

  # Initiate the pre-play state from the pre-event state (then we'll adjust for mid-event changes)
  pre_play_state <- pre_event_state |>
    dplyr::left_join(mid_event_outs_runs, by = "pa_index") |>
    dplyr::transmute(
      pa_index,
      # If mid-event outs is NA, that means there weren't any
      mid_event_outs = dplyr::coalesce(mid_event_outs, 0),
      # If mid-event runs is NA, that means there weren't any
      mid_event_runs = dplyr::coalesce(mid_event_runs, 0),
      pre_play_runner_1b_id = pre_event_runner_1b_id,
      pre_play_runner_2b_id = pre_event_runner_2b_id,
      pre_play_runner_3b_id = pre_event_runner_3b_id,
      pre_play_outs = pre_event_outs + mid_event_outs
    )

  for (base in c("1B", "2B", "3B")) {

    id_string <- glue::glue("pre_play_runner_{tolower(base)}_id")

    # If a runner left a base in the middle of a PA, remove the ID from that base
    runners_from_base <- runners_movement_mid_event_start |>
      dplyr::filter(start_base == base)
    pre_play_state[[id_string]][runners_from_base$pa_index] <- NA

    # If a runner reached a base in the middle of a PA, add the runner's ID to that base
    runners_to_base <- runners_movement_mid_event_end |>
      dplyr::filter(end_base == base)
    pre_play_state[[id_string]][runners_to_base$pa_index] <- runners_to_base$runner_id
  }

  # Step 4. Put it all together ----

  base_out_state <- pre_event_state |>
    dplyr::left_join(pre_play_state, by = "pa_index") |>
    dplyr::left_join(post_state, by = "pa_index") |>
    dplyr::select(
      pa_index,
      dplyr::starts_with("mid_event_"),
      dplyr::starts_with("pre_event_"),
      dplyr::starts_with("pre_play_"),
      dplyr::starts_with("post_")
    )

  return(base_out_state)
}


extract_events <- function(game_id) {

  endpoint <- glue::glue("https://statsapi.mlb.com/api/v1.1/game/{game_id}/feed/live")

  events_json <- jsonlite::fromJSON(endpoint)

  events <- events_json$liveData$plays$allPlays

  runners_detail_list <- lapply(events$runners, function(x) x$detail)
  runners_movement_list <- lapply(events$runners, function(x) x$movement)
  runners_length <- sapply(runners_detail_list, function(x) if(is.null(x)) 0 else nrow(x))
  runners_movement <- dplyr::bind_cols(
    do.call(dplyr::bind_rows, args = runners_detail_list),
    do.call(dplyr::bind_rows, args = runners_movement_list)
  ) |>
    tibble::add_column(pa_index = rep(1:length(runners_length), times = runners_length)) |>
    dplyr::group_by(pa_index, play_index = playIndex, runner$id) |>
    dplyr::slice(dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      pa_index,
      play_index,
      runner_id = runner$id,
      start_base = dplyr::coalesce(originBase, "batter"),
      end_base = dplyr::coalesce(end, "out"),
      is_out = isOut,
      is_scoring_event = isScoringEvent
    )
  
  base_out_state <- track_base_out_state(events, runners_movement)

  hit_data_list <- lapply(events$playEvents, function(x) x$hitData)
  batted_balls_per_pa <- sapply(hit_data_list, function(x) if (length(x) == 0) 0 else nrow(x))
  batted_ball_pa_index <- rep(1:length(hit_data_list), times = batted_balls_per_pa)

  hit_data <- do.call(dplyr::bind_rows, args = hit_data_list) |>
    tibble::add_column(at_bat_index = batted_ball_pa_index, .before = 1) |>
    dplyr::filter(!is.na(trajectory))
  if (is.null(hit_data$launchSpeed)) hit_data$launchSpeed = NA
  if (is.null(hit_data$launchAngle)) hit_data$launchAngle = NA
  if (is.null(hit_data$coordinates$coordX)) hit_data$coordinates$coordX = NA
  if (is.null(hit_data$coordinates$coordY)) hit_data$coordinates$coordY = NA

  data <- tibble::tibble(
    pa_index = 1:nrow(events$result),
    inning = events$about$inning,
    batter_id = events$matchup$batter$id,
    pitcher_id = events$matchup$pitcher$id,
    event = events$result$event,
    is_out = events$result$isOut,
    runs_on_event = sapply(events$runners,
      FUN = function(x) sum(dplyr::coalesce(x$movement$end, "") == "score")
    ),
    exit_velo = NA,
    launch_angle = NA,
    hit_loc_x = NA,
    hit_loc_y = NA,
  ) |>
    dplyr::left_join(base_out_state, by = "pa_index")

  data$exit_velo[hit_data$at_bat_index] <- hit_data$launchSpeed
  data$launch_angle[hit_data$at_bat_index] <- hit_data$launchAngle
  data$hit_loc_x[hit_data$at_bat_index] <- hit_data$coordinates$coordX
  data$hit_loc_y[hit_data$at_bat_index] <- hit_data$coordinates$coordY

  return(data)
}


for (season in 2017:2022) {
  games <- extract_schedule(season) |>
    dplyr::filter(!status$detailedState %in% c("Cancelled", "Postponed")) |>
    dplyr::arrange(gameDate)
  for (i in 1:nrow(games)) {
    cat(games$gameDate[i], "\n")
    season <- lubridate::year(games$gameDate[i])
    game_id <- games$gamePk[i]
    data <- extract_events(game_id)
    write.csv(data, file = glue::glue("~/Desktop/mlb_data/{season}/{game_id}.csv"), row.names = FALSE)
  }
}
