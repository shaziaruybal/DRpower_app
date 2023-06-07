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
  
  # shiny::observeEvent(input$test_button, {
  #   plot_data <- rnorm(100)
  #   print("Test button clicked")
  #   
  #   output$test_plot <- renderPlot({
  #     hist(plot_data, main = "Histogram of Random Data")
  #   })
  #   
  #   session$sendCustomMessage(type = "testmessage",
  #                             message = "Thanks for clicking")
  # })
  
  # TESTING: user-editable table and storing user-entered values
  
  # make sure data and original_data are reactive
  # values <- reactiveValues(data = NULL, original_data = NULL)
  # 
  # df <- data.frame(
  #   cluster = c(rep(1:5)),
  #   sample_size = c(rep(100, 5)),
  #   prop_dropout = c(rep(0.1, 5))
  # )
  # 
  # # render editable tab
  # output$editable_table <- renderDT({
  #   datatable(df, editable = TRUE)
  # })
  # 
  # # observe when table is edited
  # observeEvent(input$editable_table_cell_edit, {
  #   print("Design table has been edited")
  #   # Update the data frame with edited values
  #   df <<- editData(df, input$editable_table_cell_edit)
  # })
  # 
  # # observe when test button is clicked
  # observeEvent(input$test_button, {
  #   # check if data has been edited
  #   if (!identical(df, values$original_data)) {
  #     # update the reactiveValues object with new data
  #     values$data <- df
  # 
  #     # update the original_data with the latest changes
  #     values$original_data <- df
  #   }
  # })
  # 
  # # render updated table and reactive object
  # observe({
  #   # get reactive object
  #   reactive_data <- values$data
  # 
  #   # render the edited table
  #   output$editable_table <- renderDT({
  #     datatable(reactive_data, editable = TRUE)
  #   })
  # 
  #   # print the reactive object
  #   output$reactive_values <- renderPrint({
  #     reactive_data
  #   })
  # })
  
  ##################################################
  # DESIGN
  ##################################################
 
  # ----------------------------------
  # Sample size table
  # ----------------------------------
  
  # TODO: make sure this shows the table representing the correct power selected by the user - when Bob sends final sample size estimates
  # TODO: add eventReactive() once we have all the tables and make selection reactive vs occurring in observeEvent()
  
  observeEvent(input$user_pow, {
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
  
  # make all design table values reactive
  design_values <- reactiveValues(orig_design_data = NULL, final_design_data = NULL)
  
  # initialize empty data frame
  df <- data.frame(cluster = integer(),
                   sample_size = integer(),
                   prop_dropout = numeric())
  
  # render editable table
  output$editable_clusttab <- renderDT({
    datatable(df,
              editable = list(
                target = 'cell',
                disable = list(columns = c(1))
              ),
              colnames = c("Cluster", "Target sample size", "% drop-out"),
              options = list(dom = 'rt', autoWidth = TRUE, pageLength = 20))
  })
  
  # get input value from user-specified n clusters
  observeEvent(input$user_nclust, ignoreNULL=T, ignoreInit=T, {
    print("Number of clusters selected")

    # session$sendCustomMessage(type = "testmessage",
    #                           message = paste0("You have entered ", input$user_nclust, " clusters in your study"))
    output$text_edit_clusttab <- renderText("The table below now has rows corresponding to the number of clusters in your study.
                                            Please edit the target sample size and expected proportion of participant drop-out for each cluster by double-clicking
                                            and editing the table below. When you are finished click the 'Calculate final sample sizes' button")
    
    print("After user selects N clusters, this is the df:")
    print(df_sizes())
  })
  
  # Make the editable data frame reactive and dependent on the number of clusters entered by the user
  df_sizes <- eventReactive(input$user_nclust, ignoreNULL=T, ignoreInit=T, {
    
    # create the data frame with fixed columns and rows based on user input
    # TODO: This needs to be updated to ideal numbers based on final simulations
    data.frame(
      cluster = rep(1:input$user_nclust),
      sample_size = rep(100, input$user_nclust),
      prop_dropout = rep(0.1, input$user_nclust)
    )
  })
  
    # render editable table
    output$editable_clusttab <- renderDT({
      datatable(df_sizes(), 
                editable = list(
                  target = 'cell',
                  disable = list(
                    columns = c(1)
                    )
                  ),
                #rownames = FALSE, # remove rownames so that indexing is accurate
                colnames = c("Cluster", "Target sample size", "% drop-out"),
                options = list(dom = 'rt',
                               autoWidth = TRUE,
                               pageLength=20))
    })

  # observe when table is edited
  observeEvent(input$editable_clusttab_cell_edit, {
    print("Editable design table has been edited")
  })
  
  # ----------------------------------
  #  Calculate final sample sizes
  # ----------------------------------
  
  # observe when calculate sample sizes button is clicked 
   observeEvent(input$calc_sizes, {
     print("Calculate final sample sizes values button clicked")
     # debugging, remove later
     print("After user clicks the calc sample size button, this is the original df: ")
     print(df_sizes()) 
     print("And this is the edited df: ")
     print(df_sizes_update())
     
     # output text
     output$title_finalsizesbox <- renderText("The final sample sizes are below: ")
     output$text_finalsizesbox <- renderText("Based on the values you entered for sample size and taking into account the proportion drop-out,
                                             the final adjusted sample sizes are calculated using the formula: Nadj=n/(1-d) where Nadj is the adjusted sample size,
                                             n is the target sample size, and d is the expected drop-out proportion")
   })
     
  # update the data frame with edited values
  df_sizes_update <- eventReactive(input$calc_sizes, {
    df <- df_sizes()
    
    # iterate over each cell edit event
    for (i in seq_along(input$editable_clusttab_cell_edit$row)) {
      row <- input$editable_clusttab_cell_edit$row[i]
      col <- input$editable_clusttab_cell_edit$col[i]
      value <- input$editable_clusttab_cell_edit$value[i]

      # update the corresponding cell in the new data frame
      df[row, col] <- value 
    }
    
    # calculate adjusted sample size
    df <- df %>% mutate(final_sample_size = ceiling(sample_size/(1-prop_dropout)))
    
    return(df)
  })
     
  # render the edited table
  output$final_sizes_table <- renderDT({
    datatable(df_sizes_update(),
              #rownames=F, # remove rownames so that indexing is accurate
              colnames = c("Cluster", "Target sample size", "% drop-out", "Final adjusted sample size"),
              options = list(dom = 'rt',
                             width=4))
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
                                                 prevalence = as.numeric(input$param_prev),
                                                 ICC = as.numeric(input$param_icc),
                                                 reps = as.numeric(input$param_n_sims)
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
