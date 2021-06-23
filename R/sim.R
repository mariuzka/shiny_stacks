library(shiny)
library(ggplot2)

create_cart_station <- function(n_lines, n_carts, n_max_carts){
  line_id <- 1:n_lines
  n_carts <- rep(n_carts, n_lines)
  n_max_carts <- rep(n_max_carts, n_lines)
  
  cart_station <- data.frame(line_id, n_carts, n_max_carts)
  return(cart_station)
}

create_population_helper <- function(n_agents, mode_prop_list){
  
  # Vektor mit entsprechend häufigen get/return-Modes erstellen
  # Problem: die relativen Häufigkeiten ergeben nicht immer gerade Agenten-Zahlen
  mode_vec <- c()
  for (m in names(mode_prop_list)){
    
    n <- floor(n_agents * mode_prop_list[[m]])
    mode_vec <- c(mode_vec, rep(m, n))
  }
  
  # Differenz zur Soll-Länge berechnen
  diff <- n_agents - length(mode_vec)
  
  # wenn zu wenige, dann "random" als mode anhängen
  if (diff > 0){
  for (i in 1:diff){
    mode_vec <- c(mode_vec, sample("random", 1))
  }}
  # Vektor mischen
  mode_vec <- sample(mode_vec)
  return(mode_vec)
}

create_population_part1 <- function(
  n_agents, 
  get_mode_prop_list, 
  return_mode_prop_list
){
  # Alle Vektoren mit Agenten-Eigenschaften erstellen
  get_mode <- create_population_helper(n_agents, get_mode_prop_list)
  return_mode <- create_population_helper(n_agents, return_mode_prop_list)
  
  return(list(
    "get_mode" = get_mode, 
    "return_mode" = return_mode
    ))
}

create_population_part2 <- function(
  population_part1_list,
  duration, 
  n_ticks
  ){
  population_part1 <- data.frame(
    population_part1_list$get_mode,
    population_part1_list$return_mode
  )
  names(population_part1) <- c("get_mode", "return_mode")
  
  get_tick <- sample(1:(n_ticks-duration), nrow(population_part1), replace = TRUE)
  return_tick <- get_tick + duration
  cart <- rep(FALSE, nrow(population_part1))
  
  population_part2 <- data.frame(
    get_tick,
    return_tick,
    cart
    )
  population <- cbind(population_part1, population_part2)
  return(population)
}

agent.get_cart <- function(agent, cart_station){
  # for each line-index in sorted list of line-indices
  for (i in get_pref_order(agent$get_mode, cart_station)) {
    # if there is a cart available in this line
    if (cart_station[i, "n_carts"] > 0) {
      # get cart
      cart_station[i, "n_carts"] <- cart_station[i, "n_carts"] - 1
      agent$cart <- TRUE
      break
    }
  }
  return(list("agent" = agent, "cart_station" = cart_station))
}




agent.return_cart <- function(agent, cart_station){
  
  #  for each line-index in sorted list of line-indices
  for (i in get_pref_order(agent$return_mode, cart_station)) {
    
    # if the line is not full
    if (cart_station[i,"n_carts"] < cart_station[i,"n_max_carts"]) {
      
      # return cart
      cart_station[i,"n_carts"] <- cart_station[i,"n_carts"] + 1
      agent$cart <- FALSE
      break
    }
  }
  return(list("agent" = agent, "cart_station" = cart_station))
}

get_pref_order <- function(mode, cart_station) {
  if (mode == "min"){
    return(order(cart_station$n_carts))
  }
  else if (mode == "max"){
    return(order(cart_station$n_carts, decreasing = TRUE))
  }
  else if (mode == "random"){
    return(sample(1:nrow(cart_station)))
  }
}

multi_sim <- function(
  population_part1,
  duration,
  n_ticks, 
  n_agents,
  n_lines,
  n_carts,
  n_max_carts,
  reps,
  ticks_per_hour
){
  
  start_time <- Sys.time()

  # create dataframe for collecting simulated data
  col_names <- c("rep", "tick", "line_id", "n_carts", "n_max_carts", "rank", "n_lines")
  df <- data.frame(matrix(ncol=length(col_names), nrow=0))
  names(df) <- col_names
  
  
  
  #progress <- Progress$new(max = reps)
  #on.exit(progress$close())
  #progress$set(message = "running the simulation...")
  
  
  for (rep in 1:reps){
    #progress$inc(1)

    population <- create_population_part2(
      population_part1,
      duration,
      n_ticks
      )
    
    cart_station <- create_cart_station(n_lines, n_carts, n_max_carts)
    n_lines <- nrow(cart_station)
    
    tickdata <- as.data.frame(matrix(NA, nrow = n_lines *(n_ticks+1), ncol = 5))
    names(tickdata) <- c(names(cart_station), "tick", "rank")
    
    data_cart_station <- cart_station
    data_cart_station["tick"] <- 0
    data_cart_station["rank"] <- 1:n_lines
    tickdata[1:n_lines,] <- data_cart_station
    
    schedule <-  list()
    for (i in 1:n_ticks){
      schedule[[i]] <- which(population$get_tick == i | population$return_tick == i)
    }
    
    for (tick in 1:n_ticks) {
      
      for (agent_index in schedule[[tick]]) {
        
        if (population[agent_index, "get_tick"] == tick){
          new_agent_and_cart_station <- agent.get_cart(population[agent_index,], cart_station)
        }
        else if (population[agent_index, "cart"] == TRUE){
          new_agent_and_cart_station <- agent.return_cart(population[agent_index,], cart_station)
        }
        else {
          new_agent_and_cart_station <- list(
            "agent" = population[agent_index,], 
            "cart_station" = cart_station
            )
        }
        population[agent_index,] <- new_agent_and_cart_station$agent
        cart_station <- new_agent_and_cart_station$cart_station
      }
      
      data_cart_station <- cart_station[order(cart_station$n_carts), ]
      data_cart_station["tick"] <- tick
      data_cart_station["rank"] <- 1:nrow(data_cart_station)
      tickdata[(n_lines*tick +1):(n_lines*tick+n_lines), ] <- data_cart_station
    }
    
    tickdata["rep"] <- rep
    df <- rbind(df, tickdata)
    
  }
  df["hour"] <- df$tick / ticks_per_hour
  
  df$n_lines <- n_lines
  
  df$max_dev <- calculate_max_dev(n_lines, n_carts, n_max_carts)
  
  end_time <- Sys.time()
  #print(end_time - start_time)
  return(df)
}


##########################################################################




calculate_max_dev <- function(r, n, m){
  fl <- (n*r)/m  # lines filled up with carts
  cfl <- floor(fl) # completely filled lines
  max_deviation <- cfl*(m-n) + max(0, ((fl - cfl - n/m) * m))
  return(max_deviation)
}


create_df <- function(
  elements_per_stack_range,
  elements_per_stack_inc,
  stacks_range,
  stacks_inc,
  max_elements_per_stack
){
  stacks <- seq(stacks_range[1], stacks_range[2], stacks_inc)
  elements_per_stack <- seq(elements_per_stack_range[1], elements_per_stack_range[2], elements_per_stack_inc)
  
  df <- data.frame(matrix(ncol=4, nrow=length(elements_per_stack)*length(stacks)))
  names(df) <- c("stacks", "elements_per_stack", "max_elements_per_stack", "max_deviation")
  
  df$stacks <- stacks
  df$stacks <- sort(df$stacks)
  df$elements_per_stack <- elements_per_stack
  df$max_elements_per_stack <- max_elements_per_stack
  
  for (i in 1:nrow(df)){
    df[i, "max_deviation"] <- calculate_max_dev(df[i, "stacks"], df[i, "elements_per_stack"], df[i, "max_elements_per_stack"])
  }
  
  df$elements_total <- df$stacks * df$elements_per_stack
  df$ratio1 <- df$max_deviation/df$elements_total
  df$ratio2 <- df$elements_total/df$max_deviation
  
  return(df)
}