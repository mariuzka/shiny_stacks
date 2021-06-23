library(shiny)
library(ggplot2)

run_simulation_on_click <- function(
  initial_population,
  input_n_agents,
  input_gm_min,
  input_gm_max,
  input_gm_random,
  input_rm_min,
  input_rm_max,
  input_rm_random,
  input_n_hours,
  input_reps,
  input_duration,
  input_n_lines,
  input_n_carts,
  input_n_max_carts,
  ticks_per_hour
){
  df <- multi_sim(
    initial_population = initial_population,
    n_ticks = input_n_hours * ticks_per_hour,
    reps = input_reps,
    n_agents = input_n_agents,
    get_mode_prop_list = get_mode_prop_list,
    return_mode_prop_list = return_mode_prop_list,
    duration = input_duration * ticks_per_hour,
    n_lines = input_n_lines,
    n_carts = input_n_carts,
    n_max_carts = input_n_max_carts,
    ticks_per_hour = ticks_per_hour
  )
  return(df)
}


selected_barplots <- function(df, upper_y_lim){
  
  # select time steps
  half_time_tick <- median(df$tick)
  last_tick <- max(df$tick)
  selected_ticks <- c(0, half_time_tick, last_tick)
  
  # filter data
  df <- df[df$tick %in% selected_ticks, ]
  
  # calculate mean line length per rank and time step
  means <- aggregate(
    df$n_carts,
    by = list("hour" = df$hour, "rank" = df$rank),
    FUN = mean
  )
  names(means) <- c("hour", "rank", "n_carts")
  
  # create bar plot
  p <- ggplot(data = means, aes(x = rank, y = n_carts)) +
    geom_col() +
    facet_wrap(~hour) +
    geom_hline(yintercept = df[1, "n_carts"], linetype = "dashed") +
    geom_hline(yintercept = df[1, "n_max_carts"]) +
    ylim(0, upper_y_lim + 5) +
    theme(legend.position = "none", plot.title = element_text(size=22)) +
    ggtitle("average line length at certain time steps")
  return(p)
}


agg_lines <- function(df, upper_y_lim){
  df$rank <- factor(df$rank)
  
  df_mean_len <- aggregate(
    df$n_carts,
    by = list("rank" = df$rank, "hour" = df$hour),
    FUN = mean,
  )
  names(df_mean_len) <- c("rank", "hour", "mean_n_carts")
  
  p <- ggplot() +
    geom_line(data = df_mean_len, aes(x = hour, y = mean_n_carts, group = rank, color = rank)) +
    #geom_smooth(data = df, aes(x = hour, y = n_carts, group = rank, color = rank)) +
    geom_hline(yintercept = df[1, "n_carts"], linetype = "dashed") +
    geom_hline(yintercept = df[1, "n_max_carts"]) +
    ylim(0, upper_y_lim + 5) +
    theme(legend.position = "bottom") +
    labs(
      title = "Average line length",
      subtitle = "Average number of carts per line rank during simulation",
      caption = "The shown lines do not correspond to a single line of carts, but to the line length of a certain ran"
    ) +
    theme(
      plot.title = element_text(size=18, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return(p)
}


scatter1 <- function(df, upper_y_lim){
  p <- ggplot(data = df, aes(x = hour, y = n_carts)) +
    geom_jitter(alpha = 0.2) +
    ylim(0, upper_y_lim)
  return(p)
}

gini_plot <- function(df){
  
  df_gini <- aggregate(
    df$n_carts,
    by = list("rep" = df$rep, "hour" = df$hour),
    FUN = DescTools::Gini
  )
  names(df_gini) <- c("rep", "hour", "gini")
  p <- ggplot(data = df_gini,aes(x=hour, y = gini)) +
    geom_smooth() +
    ylim(0,1)
  return(p)
}

example_runs <- function(df, upper_y_lim){
  df <- df[df$rep %in% c(1,2,3,4),]
  df$line_id <- factor(df$line_id)
  
  p <- ggplot(data = df, aes(x = hour, y = n_carts, group = line_id, color = line_id)) +
    geom_line() +
    facet_wrap(~rep) +
    ylim(0, upper_y_lim + 5) +
    geom_hline(yintercept = df[1, "n_carts"], linetype = "dashed") +
    geom_hline(yintercept = df[1, "n_max_carts"], linetype = "dashed") +
    theme(legend.position = "none") +
    labs(
      title = "Raw example runs",
      subtitle = "Number of carts per line during simulation"
    ) +
    theme(
      plot.title = element_text(size=18, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return(p)
}


plot_dev <- function(df1, df2){
  
  df1["dev"] <- abs(df1$n_carts - df1[1, "n_carts"]) / 2
  df1["scenario"] <- "A"
  
  df2["dev"] <- abs(df2$n_carts - df2[1, "n_carts"]) / 2
  df2["scenario"] <- "B"

  df <- rbind(df1, df2)
  
  df <- df[df$tick == max(df$tick), ]
  
  df_dev <- aggregate(
    df$dev,
    by = list("scenario" = df$scenario, "rep" = df$rep, "hour" = df$hour),
    FUN = sum,
    na.rm = TRUE
  )
  names(df_dev) <- c("scenario", "rep", "hour", "sum_dev")
  
  p <- ggplot() +
    geom_jitter(data = df_dev, mapping = aes(x = scenario, y = sum_dev)) +
    geom_point(data = df, mapping = aes(x = scenario, y = max_dev), color = "red")
  
  return(p)
    
}




plot_both_diff_to_start <- function(df1, df2){
  
  df1["diff_to_start_n"] <- abs(df1$n_carts - df1[1, "n_carts"])
  df1["scenario"] <- "A"
  
  df2["diff_to_start_n"] <- abs(df2$n_carts - df2[1, "n_carts"])
  df2["scenario"] <- "B"
  
  df <- rbind(df1, df2)
  
  df_diff <- aggregate(
    df$diff_to_start_n,
    by = list("scenario" = df$scenario, "rep" = df$rep, "hour" = df$hour),
    FUN = sum,
    na.rm = TRUE
  )
  names(df_diff) <- c("scenario", "rep", "hour", "sum_abs_diff")
  
  df_mean_diff <- aggregate(
    df_diff$sum_abs_diff,
    by = list("scenario" = df_diff$scenario, "hour" = df_diff$hour),
    FUN = mean
  )
  names(df_mean_diff) <- c("scenario", "hour", "mean_sum_abs_diff")
  
  p <- ggplot() +
    geom_line(
      data = df_mean_diff, 
      mapping = aes(x = hour, y = mean_sum_abs_diff, color = scenario),
      ) +
    theme(legend.position = "bottom") +
    labs(
      title = "Total deviation of line length",
      subtitle = "",
      caption = ""
    ) +
    theme(
      plot.title = element_text(size=18, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  return(p)
}


geom_diff_to_start <- function(df, label, color){
  
  df["diff_to_start_n"] <- abs(df$n_carts - df[1, "n_carts"])
  
  df_diff <- aggregate(
    df$diff_to_start_n,
    by = list("rep" = df$rep, "hour" = df$hour),
    FUN = sum,
    na.rm = TRUE
  )
  names(df_diff) <- c("rep", "hour", "sum_abs_diff")
  
  df_mean_diff <- aggregate(
    df_diff$sum_abs_diff,
    by = list("hour" = df_diff$hour),
    FUN = mean
  )
  names(df_mean_diff) <- c("hour", "mean_sum_abs_diff")
  
  g <- geom_line(
    data = df_mean_diff, 
    mapping = aes(x = hour, y = mean_sum_abs_diff),
    color = color,
    label = label
    )
  return(g)
}



plot_diff_to_start <- function(df){
  
  df["diff_to_start_n"] <- abs(df$n_carts - df[1, "n_carts"])
  
  df_diff <- aggregate(
    df$diff_to_start_n,
    by = list("rep" = df$rep, "hour" = df$hour),
    FUN = sum,
    na.rm = TRUE
  )
  names(df_diff) <- c("rep", "hour", "sum_abs_diff")
  
  df_mean_diff <- aggregate(
    df_diff$sum_abs_diff,
    by = list("hour" = df_diff$hour),
    FUN = mean
  )
  names(df_mean_diff) <- c("hour", "mean_sum_abs_diff")
  
  p <- ggplot() +
    geom_line(data = df_mean_diff, mapping = aes(x = hour, y = mean_sum_abs_diff))
    #geom_smooth(data = df_diff, mapping = aes(x = hour, y = sum_abs_diff))
  return(p)
}


perc_observer <- function(focal_controller, controller_vec){
  observeEvent(
    controller_vec(),
    shinyFeedback::feedbackWarning(
      focal_controller,
      sum(controller_vec()) != 100,
      NULL
    ))
}

info_table01 <- function(
  n_carts,
  n_lines,
  n_agents,
  n_hours
){
  n_total_carts <- c(n_carts * n_lines)
  carts_per_agents <- n_total_carts / n_agents
  agents_per_hour <- n_agents / n_hours
  carts_per_agents_per_hour <- n_total_carts / (n_agents / n_hours)
  
  df <- data.frame(
    n_total_carts, 
    carts_per_agents,
    agents_per_hour,
    carts_per_agents_per_hour
    )
  
  names(df) <- c(
    "carts",
    "carts/agents", 
    "agents/hour",
    "carts/(agents/hour)"
    )
  return(df)
}

info_table_modes <- function(population_part1_list, show_gm_freqs, show_rm_freqs){
  gm <- population_part1_list$get_mode
  rm <- population_part1_list$return_mode
  
  freq_gm_min <- length(gm[gm == "min"])
  freq_gm_max <- length(gm[gm == "max"])
  freq_gm_random <- length(gm[gm == "random"])
  
  freq_rm_min <- length(rm[rm == "min"])
  freq_rm_max <- length(rm[rm == "max"])
  freq_rm_random <- length(rm[rm == "random"])
  
  df <- data.frame(
    freq_gm_min, 
    freq_gm_max, 
    freq_gm_random,
    freq_rm_min, 
    freq_rm_max, 
    freq_rm_random
    )
  names(df) <- c(
    "get min",
    "get max",
    "get ran",
    "ret min",
    "ret max",
    "ret ran"
  )
  
  if (!show_gm_freqs){
    df[1,c("get min", "get max", "get ran")] <- "?"
  }
  if (!show_rm_freqs){
    df[1,c("ret min", "ret max", "ret ran")] <- "?"
  }
  
  return(df)
}




plot_max_and_realized_dev <- function(
  df_max_devs,
  df
) {
  
  n_carts <- df[1, "n_carts"]
  
  df["dev"] <- abs(df$n_carts - df[1, "n_carts"]) / 2
  df <- df[df$tick == max(df$tick), ]
  df_dev <- aggregate(
    df$dev,
    by = list("scenario" = df$scenario, "rep" = df$rep, "hour" = df$hour),
    FUN = sum,
    na.rm = TRUE
  )
  names(df_dev) <- c("scenario", "rep", "hour", "sum_dev")
  
  p <- ggplot() +
    geom_point(data = df_dev, mapping = aes(x = scenario, y = sum_dev)) +
    geom_point(data = df, mapping = aes(x = scenario, y = max_dev), color = "red") +
    geom_point(data = df_max_devs, mapping = aes(x =))
  
  return(p)
  
}

plot_average_line_length <- function(df, upper_y_lim){
  df_mean <- aggregate(
    df$n_carts,
    by = list("hour" = df$hour),
    FUN = mean
  )
  names(df_mean) <- c("hour", "average_line_length")
  p <- ggplot(data = df_mean, mapping = aes(x = hour, y = average_line_length))+
    geom_point() +
    ylim(0, upper_y_lim + 5)+
    geom_hline(yintercept = mean(df_mean$average_line_length), color = "red")
  
  return(p)
}


plot_cart_use <- function(df, upper_y_lim){
  total_carts <- df[1, "n_lines"] * df[1, "n_carts"]
  
  df <- aggregate(
    df$n_carts,
    by = list("rep" = df$rep, "hour" = df$hour),
    FUN = sum,
    na.rm = TRUE
  )
  
  names(df) <- c("rep", "hour", "sum_carts")
  
  df <- aggregate(
    df$sum_carts,
    by = list("hour" = df$hour),
    FUN = mean,
  )
  
  names(df) <- c("hour", "mean_carts")
  
  df$cart_use <- total_carts - df$mean_carts
  
  p <- ggplot(data = df, mapping = aes(x = hour, y = cart_use))+
    geom_point() +
    ylim(0, upper_y_lim + 5) +
    geom_hline(yintercept = mean(df$cart_use), color = "red")
  
  return(p)
}

plot_average_utilization <- function(df){
  n_carts <- df[1, "n_carts"]
  df <- aggregate(
    df$n_carts,
    by = list("hour" = df$hour),
    FUN = mean
  )
  names(df) <- c("hour", "average_line_length")
  df$average_utilization <- (n_carts - df$average_line_length) / n_carts
  
  p <- ggplot(data = df, mapping = aes(x = hour, y = average_utilization))+
    geom_point() +
    ylim(0, 1) +
    geom_hline(yintercept = mean(df$average_utilization), color = "red")
  
  return(p)
}



desc_agents_and_carts <- function(n_agents, n_hours, duration, n_lines, n_carts){
  total_carts <- n_lines * n_carts
  agents_at_a_time <- (n_agents/n_hours)*duration
  average_line_length <- n_carts - agents_at_a_time/n_lines
  average_utilization <- paste(round((agents_at_a_time / total_carts)*100), "%")
  
  df <- data.frame(
    total_carts, 
    agents_at_a_time,
    average_line_length,
    average_utilization
    )
  return(df)
}



plot_dev_in_distribution <- function(
  df,
  elements_per_stack_range,
  stacks,
  max_elements_per_stack,
  upper_x_lim,
  upper_y_lim
){
  
  n_carts <- df[1, "n_carts"]
  
  max_dev <- calculate_max_dev(stacks, n_carts, max_elements_per_stack)
  
  df <- df[df$tick == max(df$tick), ]
  df["dev"] <- abs(df$n_carts - n_carts) / 2
  
  df_dev <- aggregate(
    df$dev,
    by = list("rep" = df$rep),
    FUN = sum,
    na.rm = TRUE
  )
  names(df_dev) <- c("rep", "sum_dev")
  
  mean_dev <- mean(df_dev$sum_dev)
  
  
  df_max_dev <- create_df(
    elements_per_stack_range,
    1,
    c(0, stacks + 5),
    1,
    max_elements_per_stack
  )
  
  p <- ggplot()+
    geom_line(
      data = df_max_dev[df_max_dev$stacks == stacks, ],
      mapping = aes(x = elements_per_stack, y = max_deviation),
      color = "black",
      lwd = 1
    ) +
    geom_line(
      data = df_max_dev[df_max_dev$stacks != stacks, ],
      mapping = aes(x = elements_per_stack, y = max_deviation, group = stacks),
      color = "grey",
      lwd = 0.5
    ) +
    geom_point(mapping = aes(x = n_carts, y = max_dev), color = "red", size = 2) +
    geom_point(mapping = aes(x = n_carts, y = mean_dev), color = "blue", shape=8, size = 2)+
    xlim(0, upper_x_lim) +
    ylim(0, upper_y_lim/2)
    
    
  return(p)
}
