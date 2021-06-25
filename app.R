
# todo:

# - wenn kein cart mehr da ist, agenten warten lassen, bis eins wieder kommt

library(shiny)
library(shinythemes)
library(ggplot2)
library(DescTools)

TICKS_PER_HOUR <- 12
ids_1 <- get_ids("1")
ids_2 <- get_ids("2")


description <- "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. 
Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. 
At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.
"

###################################################################
# ui
###################################################################

ui <- fluidPage(theme = shinytheme("spacelab"),
    shinyFeedback::useShinyFeedback(),
    titlePanel(
      h1("Show me the mess", align = "center"),
      ),
    h3("An interactive simulator of imbalance generation in non-floating one-way (vehicle) sharing systems.", align = "center"),
    br(),
    #HTML('<center><img src="carts02_cut.jpg"></center>'),
    #hr(),
    fluidRow(
      column(2),
      #column(8,
      #      p(description),),
      column(2)
    ),
    hr(),
    fluidRow(
      column(6,
             h3("Scenario A", align = "center"),
             hr(),
             ui_func(ids_1),
      ),
      column(2, ""),
      column(6,
             h3("Scenario B", align = "center"),
             hr(),
             ui_func(ids_2),
      ),
    )
)
 
###################################################################
# server
###################################################################

server <- function(input, output, session){
  
  population_1 <- reactive({
    create_population_part1(
      n_agents = input$n_agents_1, 
      get_mode_prop_list = list(
        "min" = input$gm_min_1 / 100,
        "max" = input$gm_max_1 / 100,
        "random" = input$gm_random_1 / 100),
      return_mode_prop_list = list(
        "min" = input$rm_min_1 / 100,
        "max" = input$rm_max_1 / 100,
        "random" = input$rm_random_1 / 100)
    )
    })
  
  population_2 <- reactive({
    create_population_part1(
      n_agents = input$n_agents_2, 
      get_mode_prop_list = list(
        "min" = input$gm_min_2 / 100,
        "max" = input$gm_max_2 / 100,
        "random" = input$gm_random_2 / 100),
      return_mode_prop_list = list(
        "min" = input$rm_min_2 / 100,
        "max" = input$rm_max_2 / 100,
        "random" = input$rm_random_2 / 100)
    )
  })
    
    # run simulation 1
    df_1 <- eventReactive(input$simulate_1, {
      multi_sim(
        population_part1 = population_1(),
        duration = input$duration_1 * TICKS_PER_HOUR,
        n_ticks = input$n_hours_1 * TICKS_PER_HOUR, 
        n_agents = input$n_agents_1,
        n_lines = input$n_lines_1,
        n_carts = input$n_carts_1,
        n_max_carts = input$n_max_carts_1,
        reps = input$reps_1,
        ticks_per_hour = TICKS_PER_HOUR
      )})
    
    # run simulation 2
    df_2 <- eventReactive(input$simulate_2, {
      multi_sim(
        population_part1 = population_2(),
        duration = input$duration_2 * TICKS_PER_HOUR, 
        n_ticks = input$n_hours_2 * TICKS_PER_HOUR, 
        n_agents = input$n_agents_2,
        n_lines = input$n_lines_2,
        n_carts = input$n_carts_2,
        n_max_carts = input$n_max_carts_2,
        reps = input$reps_2,
        ticks_per_hour = TICKS_PER_HOUR
      )})
    
    # get the upper y-limit of both sides
    upper_y_lim <- reactive(max(input$n_max_carts_1,input$n_max_carts_2))
    
    # barplots
    output$selected_barplots_1 <- renderPlot(selected_barplots(df_1(), upper_y_lim()))
    output$selected_barplots_2 <- renderPlot(selected_barplots(df_2(), upper_y_lim()))
    
    # aggregated line plot
    output$agg_line_1 <- renderPlot(agg_lines(df_1(), upper_y_lim()))
    output$agg_line_2 <- renderPlot(agg_lines(df_2(), upper_y_lim()))
    
    # line plot of example runs
    output$example_runs_1 <- renderPlot(example_runs(df_1(), upper_y_lim()))
    output$example_runs_2 <- renderPlot(example_runs(df_2(), upper_y_lim()))
    
    # plot_dev_in_distribution
    output$plot_dev_in_distribution_1 <- renderPlot(plot_dev_in_distribution(
      df_1(),
      c(0, input$n_max_carts_1),
      input$n_lines_1,
      input$n_max_carts_1,
      max(input$n_max_carts_1, input$n_max_carts_2),
      max(input$n_lines_1 * input$n_max_carts_1, input$n_lines_2 * input$n_max_carts_2)
    ))
    output$plot_dev_in_distribution_2 <- renderPlot(plot_dev_in_distribution(
      df_2(),
      c(0, input$n_max_carts_2),
      input$n_lines_2,
      input$n_max_carts_2,
      max(input$n_max_carts_1, input$n_max_carts_2),
      max(input$n_lines_1 * input$n_max_carts_1, input$n_lines_2 * input$n_max_carts_2)
    ))
    
    # observe behavior-mode-percentages 1
    gm_1 <- reactive(c(input$gm_min_1, input$gm_max_1, input$gm_random_1))
    rm_1 <- reactive(c(input$rm_min_1, input$rm_max_1, input$rm_random_1))
    perc_observer("gm_min_1", gm_1)
    perc_observer("gm_max_1", gm_1)
    perc_observer("gm_random_1", gm_1)
    perc_observer("rm_min_1", rm_1)
    perc_observer("rm_max_1", rm_1)
    perc_observer("rm_random_1", rm_1)
    
    # observe behavior-mode-percentages 2
    gm_2 <- reactive(c(input$gm_min_2, input$gm_max_2, input$gm_random_2))
    rm_2 <- reactive(c(input$rm_min_2, input$rm_max_2, input$rm_random_2))
    perc_observer("gm_min_2", gm_2)
    perc_observer("gm_max_2", gm_2)
    perc_observer("gm_random_2", gm_2)
    perc_observer("rm_min_2", rm_2)
    perc_observer("rm_max_2", rm_2)
    perc_observer("rm_random_2", rm_2)
    
    # infotable on agents & carts
    desc_agents_and_carts_1 <- reactive(desc_agents_and_carts(input$n_agents_1, input$n_hours_1, input$duration_1, input$n_lines_1, input$n_carts_1))
    desc_agents_and_carts_2 <- reactive(desc_agents_and_carts(input$n_agents_2, input$n_hours_2, input$duration_2, input$n_lines_2, input$n_carts_2))
    output$total_carts_1 <- renderText(desc_agents_and_carts_1()[["total_carts"]])
    output$average_utilization_1 <- renderText(desc_agents_and_carts_1()[["average_utilization"]])
    output$average_agents_at_a_time_1 <- renderText(desc_agents_and_carts_1()[["agents_at_a_time"]])
    output$average_line_length_1 <- renderText(desc_agents_and_carts_1()[["average_line_length"]])
    output$total_carts_2 <- renderText(desc_agents_and_carts_2()[["total_carts"]])
    output$average_utilization_2 <- renderText(desc_agents_and_carts_2()[["average_utilization"]])
    output$average_agents_at_a_time_2 <- renderText(desc_agents_and_carts_2()[["agents_at_a_time"]])
    output$average_line_length_2 <- renderText(desc_agents_and_carts_2()[["average_line_length"]])
    
    # info table about mode freqs
    df_mode_freqs_1 <- reactive(info_table_modes(population_1(), sum(gm_1()) == 100, sum(rm_1()) == 100))
    output$n_gm_min_1 <- renderText(df_mode_freqs_1()[1, "get min"])
    output$n_gm_max_1 <- renderText(df_mode_freqs_1()[1, "get max"])
    output$n_gm_random_1 <- renderText(df_mode_freqs_1()[1, "get ran"])
    output$gm_perc_sum_1 <- renderText(sum(input$gm_min_1, input$gm_max_1, input$gm_random_1))
    output$n_rm_min_1 <- renderText(df_mode_freqs_1()[1, "ret min"])
    output$n_rm_max_1 <- renderText(df_mode_freqs_1()[1, "ret max"])
    output$n_rm_random_1 <- renderText(df_mode_freqs_1()[1, "ret ran"])
    output$rm_perc_sum_1 <- renderText(sum(input$rm_min_1, input$rm_max_1, input$rm_random_1))
    
    df_mode_freqs_2 <- reactive(info_table_modes(population_2(), sum(gm_2()) == 100, sum(rm_2()) == 100))
    output$n_gm_min_2 <- renderText(df_mode_freqs_2()[1, "get min"])
    output$n_gm_max_2 <- renderText(df_mode_freqs_2()[1, "get max"])
    output$n_gm_random_2 <- renderText(df_mode_freqs_2()[1, "get ran"])
    output$gm_perc_sum_2 <- renderText(sum(input$gm_min_2, input$gm_max_2, input$gm_random_2))
    output$n_rm_min_2 <- renderText(df_mode_freqs_2()[1, "ret min"])
    output$n_rm_max_2 <- renderText(df_mode_freqs_2()[1, "ret max"])
    output$n_rm_random_2 <- renderText(df_mode_freqs_2()[1, "ret ran"])
    output$rm_perc_sum_2 <- renderText(sum(input$rm_min_2, input$rm_max_2, input$rm_random_2))
    
    
    # info table results
    info_table_results_1 <- reactive(get_info_table_results(df_1()))
    output$absolute_deviation_1 <- renderText(info_table_results_1()[1, "mean_abs_dev"])
    output$max_deviation_1 <- renderText(info_table_results_1()[1, "max_abs_dev"])
    output$rel_deviation_1 <- renderText(info_table_results_1()[1, "rel_dev"])
    output$rel_system_deviation_1 <- renderText(info_table_results_1()[1, "rel_system_dev"])
    
    info_table_results_2 <- reactive(get_info_table_results(df_2()))
    output$absolute_deviation_2 <- renderText(info_table_results_2()[1, "mean_abs_dev"])
    output$max_deviation_2 <- renderText(info_table_results_2()[1, "max_abs_dev"])
    output$rel_deviation_2 <- renderText(info_table_results_2()[1, "rel_dev"])
    output$rel_system_deviation_2 <- renderText(info_table_results_2()[1, "rel_system_dev"])
}


###################################################################
# run
###################################################################

shinyApp(ui, server)