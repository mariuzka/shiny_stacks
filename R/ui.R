library(shiny)
library(ggplot2)

get_ids <- function(i){
  ids <- list(
    "n_lines" = "n_lines",
    "n_carts" = "n_carts",
    "n_max_carts" = "n_max_carts",
    "info_table" = "info_table",
    "n_hours" = "n_hours",
    "n_agents" = "n_agents",
    "duration" = "duration",
    "gm_min" = "gm_min",
    "gm_max" = "gm_max",
    "gm_random" = "gm_random",
    "rm_min" = "rm_min",
    "rm_max" = "rm_max",
    "rm_random" = "rm_random",
    "n_gm_min" = "n_gm_min",
    "n_gm_max" = "n_gm_max",
    "n_gm_random" = "n_gm_random",
    "n_rm_min" = "n_rm_min",
    "n_rm_max" = "n_rm_max",
    "n_rm_random" = "n_rm_random",
    "gm_perc_sum" = "gm_perc_sum",
    "rm_perc_sum" = "rm_perc_sum",
    "mode_freqs" = "mode_freqs",
    "reps" = "reps",
    "simulate" = "simulate",
    "total_carts" = "total_carts",
    "carts_per_agent" = "carts_per_agent",
    "agents_per_hour" = "agents_per_hour",
    "carts_per_agent_per_hour" = "carts_per_agent_per_hour",
    
    "average_agents_at_a_time" = "average_agents_at_a_time",
    "average_utilization" = "average_utilization",
    "average_line_length" = "average_line_length",
    
    "selected_barplots" = "selected_barplots",
    "agg_line" = "agg_line",
    "example_runs" = "example_runs",
    "scatter1" = "scatter1",
    "gini_plot" = "gini_plot",
    "plot_diff_to_start" = "plot_diff_to_start",
    "plot_average_line_length" = "plot_average_line_length",
    "plot_cart_use" = "plot_cart_use",
    "plot_average_utilization" = "plot_average_utilization",
    "plot_dev_in_distribution" = "plot_dev_in_distribution"
    
    
  )
  
  for (name in names(ids)){
    ids[[name]] <- paste(ids[[name]],"_",i, sep = "")
  }
  return(ids)
}

ui_func <- function(ids){
  s1 <- 4
  s2 <- 3
  s3 <- 2
  column(12,
    #h3("Agents, carts and time"),
    wellPanel(
      fluidRow(
        
        column(s1, numericInput(ids$n_lines, label = "Lines", min = 1, max = 15, value = 5)),
        column(s1, numericInput(ids$n_carts, label = "Vehicles per line", min = 1, max = 100, value = 40)),
        column(s1, numericInput(ids$n_max_carts, label = "Max. capacity per line", min = 1, max = 200, value = 50)),
      ),
      fluidRow(
        column(s1, numericInput(ids$n_agents, label = "Customers per simulation", min = 1, max = 2000, value = 1000, step = 1)),
        column(s1, numericInput(ids$duration, label = "Hours of vehicle-use per customer", min = 0.5, max = 5, value = 0.5, step = 0.5)),
        column(s1, numericInput(ids$n_hours, label = "Simulated hours per simulation", min = 1, max = 16, value = 8, step = 1)),
    )),
    fluidRow(
      column(s2),
      column(s2,
             strong("Fleet size"),
             br(),
             textOutput(ids$total_carts)),
      #column(s2,
      #       strong("Avg. line length"),
      #       br(),
      #       textOutput(ids$average_line_length)),
      column(s2,
             strong("Customers at a time"),
             br(),
             textOutput(ids$average_agents_at_a_time)),
      column(s2,
             strong("Fleet utilization rate"),
             br(),
             textOutput(ids$average_utilization))
      
    
    ),

    hr(),

    fluidRow(
      column(6,
             h4("% of customers taking vehicle from ...", align = "center"),
             wellPanel(
               sliderInput(ids$gm_min, label = "... the shortest line. ('min')", min = 0, max = 100, value = 0, step = 5),
               sliderInput(ids$gm_max, label = "... the longest line. ('max')", min = 0, max = 100, value = 0, step = 5),
               sliderInput(ids$gm_random, label = "... a random line. ('random')", min = 0, max = 100, value = 100, step = 5)
             ),
             fluidRow(
               column(s3),
               column(s3,
                      strong("min"),
                      br(),
                      textOutput(ids$n_gm_min)),
               column(s3,
                      strong("max"),
                      br(),
                      textOutput(ids$n_gm_max)),
               column(s3,
                      strong("random"),
                      br(),
                      textOutput(ids$n_gm_random)),
               column(s3,
                      strong("perc."),
                      br(),
                      textOutput(ids$gm_perc_sum))
               
             ),
    ),
    column(6,
           h4("% of customers returning vehicle to ...", align = "center"),
           wellPanel(
             sliderInput(ids$rm_min, label = "... the shortest line. ('min')", min = 0, max = 100, value = 0, step = 5),
             sliderInput(ids$rm_max, label = "... the longest line. ('max')", min = 0, max = 100, value = 0, step = 5),
             sliderInput(ids$rm_random, label = "... a random line. ('random')", min = 0, max = 100, value = 100, step = 5)
           ),
           fluidRow(
             column(s3),
             column(s3,
                    strong("min"),
                    br(),
                    textOutput(ids$n_rm_min)),
             column(s3,
                    strong("max"),
                    br(),
                    textOutput(ids$n_rm_max)),
             column(s3,
                    strong("random"),
                    br(),
                    textOutput(ids$n_rm_random)),
             column(s3,
                    strong("perc."),
                    br(),
                    textOutput(ids$rm_perc_sum))
             
           ),
    )),
    hr(),
    fluidRow(
      column(2),
        #
      
      column(8,
             wellPanel(
               fluidRow(
                 column(7,
                        sliderInput(ids$reps, label = "sim. repettitions.", min = 1, max = 50, value = 10),
                 ),
                 column(1),
                 column(4,
                        br(),
                        br(),
                        actionButton(ids$simulate, "Simulate!", icon = icon("shopping-cart")),
                 )
               )
             )),
        
      column(2)
      ),
    plotOutput(ids$selected_barplots),
    hr(),
    plotOutput(ids$agg_line),
    hr(),
    plotOutput(ids$example_runs),
    hr(),
    #plotOutput(ids$plot_average_line_length),
    #hr(),
    #plotOutput(ids$plot_cart_use),
    #hr(),
    #plotOutput(ids$plot_average_utilization),
    #hr(),
    plotOutput(ids$plot_dev_in_distribution)
    )}
