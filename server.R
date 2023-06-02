#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shiny.fluent)
library(DRpower)

load("dummy_data.RData")

set.seed(10)

function(input, output, session) {
  
  ##################################################
  # TESTING
  ##################################################
  
  # shiny::observeEvent(input$user_nclust, {
  #   print("Test button clicked")
  #   
  #   plot_data <- data.frame(
  #     cluster = c(rep(1:input$user_nclust)),
  #     sample_size = c(rep(100, input$user_nclust)),
  #     prop_dropout = c(rep(0.1, input$user_nclust))
  #   )
  # 
  #   output$test_plot <- renderPlot({
  #     ggplot(plot_data, aes(x = cluster, y = sample_size)) +
  #       geom_point()
  #   })
  # })
  
  shiny::observeEvent(input$test_button, {
    plot_data <- rnorm(100)
    print("Test button clicked")
    
    output$test_plot <- renderPlot({
      hist(plot_data, main = "Histogram of Random Data")
    })
    
    session$sendCustomMessage(type = "testmessage",
                              message = "Thanks for clicking")
  })

  ##################################################
  # DESIGN
  ##################################################
  
  # ----------------------------------
  # Sample size table
  # ----------------------------------
  
  # TODO: make sure this shows the table representing the correct power selected by the user - when Bob sends final sample size estimates
  
  observeEvent(input$user_pow, ignoreNULL = T, ignoreInit = F, {
    print("Target power selected")
    
    output$sample_size_table <- renderDT({
      if(input$user_pow==0.8){
        datatable(df_samp_size2, 
                  colnames = c("Number of clusters", "6%", "7%", "8%", "9%", "10%"),
                  rownames = FALSE,
                  extensions = c("Buttons", "FixedHeader"), 
                  options = list(autoWidth = T, 
                                 pageLength = 12,
                                 fixedHeader = TRUE,
                                 columnDefs = list(list(className = "dt-center", 
                                                        targets = "_all")),
                                 searchHighlight = TRUE,
                                 language = list(search = 'Filter:'),
                                 dom = 'rtB',
                                 buttons = c('copy', 'csv', 'excel')
                  ))
      }
    })
    
    output$table_NA <- renderText({
      if(input$user_pow !=0.8){
        "There is no table for that target power yet"
      }
    })
  })     
  
  
  
  # ----------------------------------
  #  User-input table: sample size and proportion drop-out
  # ----------------------------------
  # TODO: n clusters/sample sizes default
  
  # get input value from user-specified n clusters
  observeEvent(input$user_nclust, {
    print("Number of cluster selected")
    
    n_rows <- input$user_nclust

    # create the data frame with fixed columns and rows based on user input
    df <- data.frame(
      cluster = c(rep(1:n_rows)),
      sample_size = c(rep(100, n_rows)),
      prop_dropout = c(rep(0.1, n_rows))
    )

    output$user_table <- renderDT({
      datatable(df,
                editable = list(
                  target = 'column',
                  disable = list(
                    columns = c(0,3)
                    )
                  ),
                rownames = FALSE,
                options = list(dom = 'rtip',
                               autoWidth = TRUE))
    })
    
    # output$output_text <- renderText(paste0("Number of clusters: ", input$user_nclust))
  })

  # ----------------------------------
  #  Calculate final sample sizes
  # ----------------------------------
  
  # TODO: add function to calculate final sample size in 3rd column once user has edited the table - maybe have a button to calculate final sample size
  # need to check how to save table from before with user-entered values
  
   observeEvent(input$calc_sizes, {
    print("Calculate final sample sizes values button clicked")
    
     n_rows <- input$user_nclust
     
     final_df <- data.frame(
       cluster = c(rep(1:n_rows)),
       final_sample_size = c(rep(150, n_rows))
     )
     
    output$final_sizes_table <- renderDT({
      datatable(final_df, 
                rownames=F,
                options = list(dom = 'rti', 
                               width=4))
    })
  })
  
  # ----------------------------------
  #  Results plot: estimated power
  # ----------------------------------
  # TODO: store the user-input values from the table and use them as params for DRpower::get_power_threshold()
  
  # Display results once estimate power is clicked
  observeEvent(input$est_pow, {
    
    output$title_powbox <- renderText("The estimated power is below: ")
    output$text_powbox <- renderText(paste0("The plot shows the mean and lower and upper credible interval based on the parameters: ",
                                            "prev=", input$param_prev, ", ICC=", input$param_icc, ", sims=", input$param_n_sims))

    # ATM this is hard-coded but will be flexible eventually
    power_output <- DRpower::get_power_threshold(N = c(rep(100, 10)),
                                                 prevalence = 0.06,
                                                 ICC = 0.1,
                                                 reps = 100
    )
    

    output$est_power_plot <- renderPlot({
      ggplot(power_output) +
        geom_segment(aes(x = " ", xend = " ",y = lower, yend = upper), color = "black", linewidth = 1) +
        geom_point(aes(x = " ", y = power),
                   size = 4,
                   shape = 21,
                   fill = "skyblue3") +
        scale_y_continuous(labels = scales::percent_format(1), limits = c(0,1)) +
        labs(x = "",
             y = "Estimated power",
             caption = paste0("Parameters: prev=", input$param_prev, ", ICC=", input$param_icc, ", sims=", input$param_n_sims)) +
        theme_light() +
        theme(text = element_text(size = 16))
    })
  })
  
  # ----------------------------------
  #  Render downloadable design report
  # ----------------------------------
  # TODO: fix the reactive plot issue 
  
  output$design_report <- downloadHandler(
    filename = paste0("PfHRP2_Planner_Design_Report_", Sys.Date(), ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "test_design_report.Rmd")
      file.copy("test_design_report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(est_prev_plot = est_prev_plot(),
                     est_icc_plot = est_icc_plot())
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
      )
    }
  )

  ##################################################
  # ANALYSIS
  ##################################################
  
  # ----------------------------------
  #  User-input table: number of deletions and final sample sizes
  # ----------------------------------

  # get input value from user-specified n clusters
  observeEvent(input$analysis_nclust, {
    print("Number of final clusters selected")
    
    n_rows <- input$analysis_nclust
    
    # create the data frame with fixed columns and rows based on user input
    analysis_table <- data.frame(
      cluster = c(rep(1:n_rows)),
      n_deletions = c(rep(5, n_rows)),
      sample_size = c(rep(100, n_rows))
    )
    
    output$analysis_table <- renderDT({
      datatable(analysis_table, 
                editable = list(
                  target = 'column',
                  disable = list(
                    columns = c(0)
                  )
                ),
                rownames = FALSE,
                options = list(dom = 'rtip',
                               autoWidth = TRUE)) 
  

  })
  }) 
  
  # ----------------------------------
  #  Results table/plot: estimated prevalence
  # ----------------------------------
  
  # Display results once estimate prevalence is clicked
  observeEvent(input$est_prev, {
    print("Estimate prevalence button clicked")
    
    # df_prev_output <- df_prev_output %>% select(simple_mean, lower_CrI, upper_CrI, prob_above_threshold)
    
    output$title_prevbox <- renderText("The estimated prevalence value is below: ")
    output$text_prevbox <- renderText("The table and plot show the mean and lower and upper credible interval")
    
    # ATM this is hard-coded but will be flexible eventually
    prev_output <- DRpower::get_prevalence(n = c(rep(5, 10)), N = c(rep(100, 10)))

    output$est_prev_table <- renderDT({
      datatable(prev_output,
                rownames = FALSE,
                colnames = c("Mean prevalence", "Lower CrI", "Upper CrI", "Probability above threshold"),
                options = list(autoWidth = TRUE,
                               fixedHeader = TRUE,
                               columnDefs = list(list(className = "dt-center",
                                                      targets = "_all")),
                               dom = 't'))
    })

    # NOTE need to divide by 100 to convert to proportion

    est_prev_plot <- reactive({
      ggplot(prev_output) +
        geom_segment(aes(x = " ", xend = " ", y = CrI_lower/100, yend = CrI_upper/100),
                     color = "black", linewidth = 1) +
        geom_point(aes(x = " ", y = MAP/100),
                   size = 3,
                   shape = 21,
                   fill = "skyblue3") +
        # geom_hline(aes(y=0.06),
        #            color = "darkgrey",
        #            linetype = "dashed") +
        scale_y_continuous(labels = scales::percent_format(1), limits = c(0,1)) +
        labs(x = "",
             y = "Estimated prevalence") +
        # coord_flip() +
        theme_light() +
        theme(text = element_text(size = 16))
    })

    output$est_prev_plot <- renderPlot(est_prev_plot())
    
  })
  
  # ----------------------------------
  #  Results table/plot: estimated ICC
  # ----------------------------------
  
  observeEvent(input$est_icc, {
    print("Estimate ICC button clicked")
    
    df_prev_output <- df_prev_output %>% select(simple_mean, lower_CrI, upper_CrI, prob_above_threshold)
    
    output$title_iccbox <- renderText("The estimated ICC value is below: ")
    output$text_iccbox <- renderText("The table and plot show the mean and lower and upper credible interval")
    
    output$est_icc_table <- renderDT({
      datatable(df_prev_output,
                rownames = FALSE,
                options = list(autoWidth = TRUE,
                               fixedHeader = TRUE,
                               columnDefs = list(list(className = "dt-center",
                                                      targets = "_all")),
                               dom = 't'))
    })
  
    est_icc_plot <- reactive({
      ggplot() +
        geom_segment(aes(x = " ", xend = " ", y = 0.2, yend = 0.6), 
                     linewidth = 2,
                     color = "darkgrey",
                     alpha = 0.8) +
        geom_point(aes(x = " ", y = 0.46), 
                   size = 4, 
                   shape = 21,
                   fill = "skyblue3") +
        scale_y_continuous(labels = scales::percent_format(1), limits = c(0,1)) +
        labs(x = "",
             y = "Estimated ICC") +
        # coord_flip() +
        theme_light() +
        theme(text = element_text(size = 16)) 
    })
  
    output$est_icc_plot <- renderPlot(est_icc_plot())
  })
  
  # ----------------------------------
  #  Render downloadable analysis report  
  # ----------------------------------
  # TODO: fix the reactive plot issue 
  
  output$analysis_report <- downloadHandler(
    filename = paste0("PfHRP2_Planner_Analysis_Report_", Sys.Date(), ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "test_design_report.Rmd")
      file.copy("test_design_report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(est_prev_plot = est_prev_plot(),
                     est_icc_plot = est_icc_plot())
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
      )
    }
  )
  
  
}
