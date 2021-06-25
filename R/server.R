library(shiny)
library(ggplot2)

# run the simulation by pressing the button
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

# bar plot showing the distribution of vehicles at the start, in the middle and at the end
selected_barplots <- function(df, upper_y_lim){
  
  # select time steps
  half_time_tick <- median(df$tick)
  last_tick <- max(df$tick)
  selected_ticks <- c(0, half_time_tick, last_tick)
  
  # filter data
  df <- df[df$tick %in% selected_ticks, ]
  df$time_points <- factor(df$tick, levels=c(0, half_time_tick, last_tick), labels=c("Start", "Middle", "End"))
  print(df$time_points)
  # calculate mean line length per rank and time step
  means <- aggregate(
    df$n_carts,
    by = list("time_points" = df$time_points, "rank" = df$rank),
    FUN = mean
  )
  names(means) <- c("time_points", "rank", "n_carts")
  
  # create bar plot
  p <- ggplot(data = means, aes(x = rank, y = n_carts)) +
    geom_col() +
    facet_wrap(~time_points) +
    geom_hline(yintercept = df[1, "n_carts"], linetype = "dashed") +
    geom_hline(yintercept = df[1, "n_max_carts"]) +
    ylim(0, upper_y_lim + 5) +
    theme(
      legend.position = "none", 
      plot.title = element_text(size=18, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_blank()) +
    labs(
      title = "Plot I",
      subtitle = "Average distribution of vehicles at specific time steps"
    ) +
    xlab("") +
    ylab("Average number of vehicles")
    
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
    geom_line(data = df_mean_len, aes(x = hour, y = mean_n_carts, group = rank), color = "steelblue2") +
    #geom_smooth(data = df, aes(x = hour, y = n_carts, group = rank, color = rank)) +
    geom_hline(yintercept = df[1, "n_carts"], linetype = "dashed") +
    geom_hline(yintercept = df[1, "n_max_carts"]) +
    ylim(0, upper_y_lim + 5) +
    theme(legend.position = "none") +
    labs(
      title = "Plot II",
      subtitle = "Average distribution of vehicles at each time step",
      legend.position = "none"
    ) +
    theme(
      plot.title = element_text(size=18, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    ylab("Average number of vehicles")
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
    geom_hline(yintercept = df[1, "n_max_carts"]) +
    theme(legend.position = "none") +
    labs(
      title = "Plot III",
      subtitle = "Distribution of vehicles at each time step from randomly selected replications"
    ) +
    theme(
      plot.title = element_text(size=18, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    ylab("Number of vehicles")
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



get_info_table_results <- function(df){
  
  n_carts <- df[1, "n_carts"]
  n_max_carts <- df[1, "n_max_carts"]
  n_lines = df[1, "n_lines"]
  
  max_abs_dev <- calculate_max_dev(n_lines, n_carts, n_max_carts)
  
  df <- df[df$tick == max(df$tick), ]
  df["abs_dev"] <- abs(df$n_carts - n_carts) / 2
  df_dev <- aggregate(
    df$abs_dev,
    by = list("rep" = df$rep),
    FUN = sum,
    na.rm = TRUE
  )
  names(df_dev) <- c("rep", "sum_abs_dev")
  
  # Absolute Abweichung vom Sollwert
  mean_abs_dev <- mean(df_dev$sum_abs_dev)
  
  # Verhältnis von absoluter Abweichung vom Sollwert zur theoretisch möglichen Abweichung
  rel_dev <- mean_abs_dev / max_abs_dev
  
  # Absolute Abweichung vom Sollwert im Verhältnis zur Systemgröße
  rel_system_dev <- mean_abs_dev / (n_max_carts * n_lines)
  
  df <- data.frame(
    mean_abs_dev,
    max_abs_dev,
    rel_dev,
    rel_system_dev
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
    c(stacks - 2, stacks + 2),
    1,
    max_elements_per_stack
  )
  
  p <- ggplot()+
    geom_line(
      data = df_max_dev[df_max_dev$stacks == stacks, ],
      mapping = aes(x = elements_per_stack, y = max_deviation),
      color = "grey",
      lwd = 3
    ) +
    geom_line(
      data = df_max_dev,
      mapping = aes(x = elements_per_stack, y = max_deviation, group = stacks, color = as.factor(stacks)),
      lwd = 0.5,
      linetype="dashed"
    ) +
    #geom_point(mapping = aes(x = n_carts, y = max_dev), color = "red", size = 2) +
    geom_point(mapping = aes(x = n_carts, y = mean_dev), color = "black", size = 3)+
    xlim(0, upper_x_lim) +
    ylim(0, upper_y_lim/2.5) +
    labs(
      x = "Number of vehicles per line",
      y = "Absolute deviation",
      color = "Number of lines",
      title = "Plot IV",
      subtitle = "The realized deviation in context of the relationship of the number of vehicles and the theoretically possible deviation"
    ) +
    theme(
      plot.title = element_text(size=18, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
    
    
  return(p)
}
