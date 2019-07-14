# Markov Attribution from Clickstream Data

clickChannel <- function(data,
                         date_from,
                         date_to,
                         look_back,
                         channels,
                         goal_number) {
  # Set tibble


  # Set negate
  '%ni%' <- Negate('%in%')

  # Set day sequence
  #days <- seq.Date(from = date_from, to = date_to, "days")

  # Set goal variable
  goal <- paste0("M_goal", goal_number, "Completions")

  # Set lookback date from window
  date_lb <- as.Date(date_from) - look_back

  # Set classes & filter
  main_data <- data %>%
    #tibble() %>%
    mutate(date         = as.Date(date)) %>%
    mutate(sl_timeStamp = ymd_hms(sl_timeStamp)) %>%
    #mutate(sl_userId    = as.factor(as.character(sl_userId))) %>%
    filter(date >= as.Date(date_lb) & date <= date_to)

  # Filter for conversions
  main_data2 <- main_data %>%
    mutate(entry = (if_else(sl_hitOrder == 1, 1, 0))) %>%
    select(
      sl_userId,
      sl_timeStamp,
      sl_hitOrder,
      UQ(as.name(goal)),
      M_transactionRevenue,
      entry,
      UQ(as.name(channels))
    ) %>%
    #filter(M_goal4Completions == 1 | entry == 1) %>%
    filter(UQ(as.name(goal)) == 1 | entry == 1) %>%
    arrange(sl_userId, desc(sl_timeStamp)) %>%
    mutate(time_offset = if_else(
      lag(sl_userId, 1) == sl_userId,
      lag(sl_timeStamp, 1) - sl_timeStamp,
      0
    )) %>%
    distinct()

  # Can extract all those that have converted and those that havent.

  # 3  types of visit paths:
  ## 1. Ones that didn't lead to a conversion
  ## 2. Ones that didn't lead to a conversion from someone that previously converted
  ## 3. Ones that led directly to a conversion

  # 1. Ones that didn't lead to a conversion
  # Get unique converters IDs
  converter_IDs <- main_data2 %>%
    #filter(eval(goal) == 1) %>%
    filter(UQ(as.name(goal)) == 1) %>%
    select(sl_userId) %>%
    distinct() %>%
    mutate(sl_userId = as.factor(as.character(sl_userId)))

  # Collapse paths for non converters by referencing the above converters
  paths_non_converted <- main_data2 %>%
    filter(sl_userId %ni% converter_IDs$sl_userId) %>%
    #arrange(sl_userId) %>%
    group_by(sl_userId) %>%
    dplyr::summarise(
      revenue      = sum(M_transactionRevenue),
      path         = paste0(mod_source, collapse = " > "),
      max_datetime = max(sl_timeStamp)
    ) %>%
    filter(max_datetime   >= date_from)


  # 2. Ones that led directly to a conversion or just after
  # Check for how many / who bought multiple times
  purchase_freq <- main_data2 %>%
    filter(sl_userId %in% converter_IDs$sl_userId) %>%
    group_by(sl_userId) %>%
    summarise(conversions = sum(UQ(as.name(goal)))) %>%
    arrange(desc(conversions))

  # Create rolling sum by user of conversions (validated manually),
  # and lagged rolling sum to show effectively the session per user
  # Use the latter as a grouping variable with userId
  paths_converters_all <- main_data2 %>%
    filter(sl_userId %in% converter_IDs$sl_userId) %>%
    group_by(sl_userId) %>%
    arrange(sl_userId, sl_timeStamp) %>%
    mutate(roll_convert   = cumsum(UQ(as.name(goal)))) %>%
    arrange(sl_userId, sl_timeStamp) %>%
    mutate(current_convs  = if_else(
      roll_convert == lag(roll_convert, n = 1L, default = 0) + 1,
      roll_convert - 1,
      roll_convert
    )) %>%
    ungroup() %>%
    group_by(sl_userId, current_convs) %>%
    arrange(sl_userId, sl_timeStamp) %>%
    summarise(
      path    = paste0(UQ(as.name(channels)), collapse = " > "),
      conv    = sum(UQ(as.name(goal))),
      revenue = sum(M_transactionRevenue),
      max_datetime = max(sl_timeStamp)
    ) %>%
    filter(max_datetime   >= date_from) %>%
    ungroup()

  # Create model ready frame
  paths_non <- paths_non_converted %>%
    mutate(conv      = 0,
           conv_null = 1) %>%
    select(conv, conv_null, path, revenue, max_datetime)

  paths_con <- paths_converters_all %>%
    mutate(conv_null = if_else(conv == 0, 1, 0)) %>%
    select(conv, conv_null, path, revenue, max_datetime)

  paths_all <- bind_rows(paths_con, paths_non)

  paths_all_sum <- paths_all %>%
    group_by(path) %>%
    summarise(
      conv      = sum(conv),
      conv_null = sum(conv_null),
      revenue   = sum(revenue)
    )

  # Channel Attribution
  # Reference data is below for format and columns

  markov1 <- markov_model(
    paths_all_sum,
    var_path  = 'path',
    var_conv  = 'conv',
    var_value = "revenue",
    var_null  = 'conv_null',
    out_more  = FALSE
  ) %>%
    rename(markov_conversion_value  = total_conversion_value,
           markov_conversions       = total_conversion)

  herustic1 <- heuristic_models(
    paths_all,
    var_path  = 'path',
    var_conv  = 'conv',
    var_value = "revenue"
  )

  merged_models <-
    merge(markov1, herustic1, by = "channel_name", all = TRUE) %>%
    arrange(-markov_conversion_value)
}
