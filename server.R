######### DRpower App: Server ####################################################################
# Authors: Shazia Ruybal-Pesántez (sruybal@imperial.ac.uk)
##################################################################################################

library(shiny)
library(tidyverse)
library(shiny.fluent)
library(shinyWidgets)
library(DRpower)

load("dummy_data.RData")

set.seed(10)

function(input, output, session) {
  
  ##################################################
  # TESTING
  ################################################## 
  
  # shiny::observeEvent(input$design_nclust, {
  #   print("Test button clicked")
  # 
  #   plot_data <- data.frame(
  #     cluster = c(rep(1:input$design_nclust)),
  #     sample_size = c(rep(100, input$design_nclust)),
  #     prop_dropout = c(rep(0.1, input$design_nclust))
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
# 
#     session$sendCustomMessage(type = "testmessage",
#                               message = "Thanks for clicking")
  })
  
  # output$design_report <- downloadHandler(
  #   filename = paste0("PfHRP2_Planner_Design_Report_", Sys.Date(), ".html"),
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "test_design_report.Rmd")
  #     file.copy("test_design_report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     params <- list(
  #       design_final_sizes = input$test_button
  #     )
  #     
  #     rmarkdown::render(tempReport,
  #                       output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv()),
  #     )
  #   }
  # )
  
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
  
  # initialize empty data frame
  df <- data.frame(cluster = integer(),
                   sample_size = integer(),
                   prop_dropout = numeric())
  
  # render the initial table (no rows)
  output$editable_clusttab <- renderDT({
    datatable(df,
              editable = list(
                target = 'cell',
                disable = list(columns = c(1))
              ),
              colnames = c("Cluster", "Target sample size", "% drop-out"),
              options = list(dom = 'rt', autoWidth = TRUE, pageLength = 20))
  })
  
  
  # create a reactive value for df_sizes_update
  design_rv <- reactiveValues(df_sizes_update = NULL)
  
  # observe when the user specifies n clusters
  observeEvent(input$design_nclust, ignoreNULL=T, ignoreInit=T, {
    print("Number of clusters selected")

    # session$sendCustomMessage(type = "testmessage",
    #                           message = paste0("You have entered ", input$design_nclust, " clusters in your study"))
    output$text_edit_clusttab <- renderText("The table below now has rows corresponding to the number of clusters in your study.
                                            Please edit the target sample size and expected proportion of participant drop-out for each cluster by double-clicking
                                            and editing the table below. When you are finished click the 'Calculate final sample sizes' button")
    
    print("After user selects N clusters, this is the df:")
    print(df_sizes())
    
    # when df_sizes() is created, store the initial values in df_sizes_update()
    design_rv$df_sizes_update <- df_sizes()
  })
  
  # Make the editable data frame reactive and dependent on the number of clusters entered by the user
  df_sizes <- eventReactive(input$design_nclust, ignoreNULL=T, ignoreInit=T, {
    # create the data frame with fixed columns and rows based on user input
    # TODO: This needs to be updated to ideal numbers based on final simulations (need to make design_nclust reactive)
    data.frame(
      cluster = rep(1:input$design_nclust),
      sample_size = rep(100, input$design_nclust),
      prop_dropout = rep(0.1, input$design_nclust)
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

  # observe when table is edited and update the data frame with the user entered values
  observeEvent(input$editable_clusttab_cell_edit, {
    print("Editable design table has been edited")
    
    # get the latest updated data frame
    df <- design_rv$df_sizes_update

    # iterate over each cell edit event
    for (i in seq_along(input$editable_clusttab_cell_edit$row)) {
      row <- input$editable_clusttab_cell_edit$row[i]
      col <- input$editable_clusttab_cell_edit$col[i]
      value <- input$editable_clusttab_cell_edit$value[i]

      # update the corresponding cell in the new data frame
      df[row, col] <- value
    }

    # assign the updated data frame to df_sizes_update
    design_rv$df_sizes_update <- df
    
  })
  
  
  # ----------------------------------
  #  Calculate final sample sizes
  # ----------------------------------
  
  # observe when calculate sample sizes button is clicked 
   observeEvent(input$calc_sizes, {
     print("Calculate final sample sizes values button clicked")
     # debugging, remove later
     print("After user clicks the calc sample size button, this is the edited df: ")
     print(design_rv$df_sizes_update)
     
     # output text
     output$title_finalsizesbox <- renderText({
       # error message if the user has not chosen the number of clusters
       if(input$design_nclust==""){
         createAlert(session, 
                     anchorId = "error_noclusters", 
                     alertId = "alert_noclusters",
                     style = "danger",
                     title = "Error", 
                     content = "You have not chosen the number of clusters. Please go back to Step 1 and choose the number of clusters and enter the values in the table.", 
                     append = FALSE)
       }
       else{
         closeAlert(session, "alert_noclusters")
         return("The final sample sizes are below: ")
       }
     })
       
     output$text_finalsizesbox <- renderText({
       if(input$design_nclust==""){
         return(NULL)
       }
       else{
         return("Based on the values you entered for sample size and taking into account the proportion drop-out,
                                             the final adjusted sample sizes are calculated using the formula: Nadj=n/(1-d) where Nadj is the adjusted sample size,
                                             n is the target sample size, and d is the expected drop-out proportion")
       }
     })
   })     
   
  # update the data frame with edited values
  df_sizes_final <- eventReactive(input$calc_sizes, {

    df <- design_rv$df_sizes_update

    # calculate adjusted sample size
    df <- df %>% mutate(final_sample_size = ceiling(sample_size/(1-prop_dropout)))

    return(df)
  })

  # render the edited table
  output$final_sizes_table <- renderDT({
    datatable(df_sizes_final(),
              #rownames=F, # remove rownames so that indexing is accurate
              colnames = c("Cluster", "Target sample size", "% drop-out", "Final adjusted sample size"),
              options = list(dom = 'rt',
                             width=4, pageLength=20))
  })

  # ----------------------------------
  #  Results plot: estimated power
  # ----------------------------------
  
  # Display results once estimate power is clicked
  observeEvent(input$est_pow, {
    output$title_powbox <- renderText({
      # error message if the user has not entered the sample sizes 
      if(is.null(design_rv$df_sizes_update)){
        createAlert(session, 
                    anchorId = "error_nosizes", 
                    alertId = "alert_nosizes",
                    style = "danger",
                    title = "Error", 
                    content = "You have not entered the sample sizes. Please go back to Step 1 and choose the number of clusters and enter the values in the table.", 
                    append = FALSE)
      }
      else{
      closeAlert(session, "alert_nosizes")
      return("The estimated power is below: ")
      }
    })
    
    
    # TODO seems like the text is updating when params are changed, not just when button is clicked
    output$text_powbox <- renderText({
      if(is.null(design_rv$df_sizes_update)){
        return(NULL)
      }
      else{
      paste0("The plot shows the mean and lower and upper credible interval based on the parameters: ",
                                            "prev=", input$param_prev, ", ICC=", input$param_icc, ", sims=", input$param_n_sims)
      }
    })
  })
  
  power_output <- eventReactive(input$est_pow, { 
    id <- showNotification(paste0("Estimating power ( ", input$param_n_sims, " simulations)..."), 
                           duration = NULL, 
                           closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    DRpower::get_power_threshold(N = df_sizes_final()$sample_size, # this needs to be based on df_sizes_final
                                   prevalence = as.numeric(input$param_prev),
                                   ICC = as.numeric(input$param_icc),
                                   reps = as.numeric(input$param_n_sims)
      )
  })
  
  # TODO seems like the plot is not re-loading when params are changed, but working for button click
  # TODO display an error if sample sizes haven't been entered and plot doesn't render
  output$est_power_plot <- renderPlot({
    ggplot(power_output()) +
      geom_segment(aes(x = " ", xend = " ",y = lower, yend = upper), color = "black", linewidth = 1) +
      geom_point(aes(x = " ", y = power),
                 size = 4,
                 shape = 21,
                 fill = "skyblue3") +
      geom_hline(yintercept = 0.8, color = "darkgrey", linetype = "dashed") +
      geom_text(aes(x= " ", y = 0.825, label = "80% threshold"), color = "darkgrey") +
      scale_y_continuous(labels = scales::percent_format(1), limits = c(0,1)) +
      labs(x = "",
           y = "Estimated power",
           caption = paste0("Parameters: prev=", input$param_prev, ", ICC=", input$param_icc, ", sims=", input$param_n_sims)) +
      theme_light() +
      theme(text = element_text(size = 16))
  })
  
  # ----------------------------------
  #  Render downloadable design report
  # ----------------------------------

  output$design_report <- downloadHandler(
    filename = paste0("PfHRP2_Planner_Design_Report_", Sys.Date(), ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "template_design_report.Rmd")
      file.copy("template_design_report.Rmd", tempReport, overwrite = TRUE)

      params <- list(
                     design_powerthreshold = input$user_pow,
                     design_final_sizes = df_sizes_final(),
                     design_nclusters = input$design_nclust,
                     design_paramprev = input$param_prev,
                     design_paramicc = input$param_icc,
                     design_paramsims = input$param_n_sims,
                     design_poweroutput = power_output()
                     
      )

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
  # TODO: make sure an error appears if the user has not entered the sizes
  
  # initialize empty data frame
  df_a <- data.frame(
    cluster = integer(),
    n_deletions = integer(),
    sample_size = integer()
  )
  
  output$editable_deltab <- renderDT({
    datatable(df_a, 
              editable = list(
                target = 'cell',
                disable = list(
                  columns = c(0)
                )
              ),
              # rownames = FALSE,
              # colnames = c(), # add colnames
              options = list(dom = 'rt',
                             autoWidth = TRUE, pageLength = 20)) 
    
    
  })
  
  # create a reactive value for df_analysis_update
  analysis_rv <- reactiveValues(df_analysis_update = NULL)
  
  # get input value from user-specified n clusters
  observeEvent(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
    print("Number of final clusters selected")
    
    analysis_rv$df_analysis_update <- df_deletions()
  }) 

  # Make the editable data frame reactive and dependent on the deletion and sample sizes entered by the user
  df_deletions <- eventReactive(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
    # create the data frame with fixed columns and rows based on user input
    data.frame(
      cluster = c(rep(1:input$analysis_nclust)),
      n_deletions = c(rep(5, input$analysis_nclust)),
      sample_size = c(rep(100, input$analysis_nclust))
    )
  })  
  
  # Render editable table
  output$editable_deltab <- renderDT({
    datatable(df_deletions(), 
              editable = list(
                target = 'cell',
                disable = list(
                  columns = c(0)
                )
              ),
              # rownames = FALSE,
              colnames = c("Number of clusters", "Number of deletions", "Sample size"), 
              options = list(dom = 'rt',
                             autoWidth = TRUE, pageLength = 20)) 
  })
  
  # observe when table is edited and update the data frame with the user entered values
  observeEvent(input$editable_deltab_cell_edit, {
    print("Editable analysis table has been edited")
    
    # get the latest updated data frame
    df <- analysis_rv$df_analysis_update
    
    # iterate over each cell edit event
    for (i in seq_along(input$editable_deltab_cell_edit$row)) {
      row <- input$editable_deltab_cell_edit$row[i]
      col <- input$editable_deltab_cell_edit$col[i]
      value <- input$editable_deltab_cell_edit$value[i]
      
      # update the corresponding cell in the new data frame
      df[row, col] <- value
    }
    
    # assign the updated data frame to df_sizes_update
    analysis_rv$df_analysis_update <- df
    
  })
  
  # ----------------------------------
  #  Results table/plot: estimated prevalence
  # ----------------------------------
  
  # Display results once estimate prevalence is clicked
  observeEvent(input$est_prev, {
    print("Estimate prevalence button clicked")
    
    # debugging, remove later
    print("After user clicks the estimate prev button, this is the edited df: ")
    print(analysis_rv$df_analysis_update)
    
    output$title_prevbox <- renderText({
      
      # display error message if the user has not entered the deletions and sample sizes
      if(input$analysis_nclust==""){
        createAlert(session, 
                    anchorId = "error_nodeletions", 
                    alertId = "alert_nodeletions",
                    style = "danger",
                    title = "Error", 
                    content = "You have not selected the number of clusters or entered the values for your study. Please go back to Step 1 and choose the number of clusters and enter the values in the table.", 
                    append = FALSE)
      }
      else{
        closeAlert(session, "alert_nodeletions")
        return("The estimated prevalence value is below: ")
      }
    })
    
    output$text_prevbox <- renderText({
      # display nothing if the user has not entered the deletions and sample sizes
      if(input$analysis_nclust==""){
        return(NULL)
      }
      else{
        paste("The table and plot show the mean and lower and upper credible interval. There is a ",
              ceiling(prev_output()$prob_above_threshold*100),
              "% probability that the ",
              "pfhrp2 prevalence is above the 5% threshold.")
      }
    })
    
    # print(prev_output())
  })    
  
  # Calculate prevalence using DRpower 
  prev_output <- eventReactive(input$est_prev, {
    df <- analysis_rv$df_analysis_update
    
    DRpower::get_prevalence(n = df$n_deletions, 
                            N = df$sample_size)
    
  })
  
  output$est_prev_table <- renderTable({
    # TODO: figure out how to add column names
    prev_output() %>% 
      rename("Mean prevalence" = MAP, "Lower CrI" = CrI_lower, "Upper CrI" = CrI_upper, "Probability above threshold" = prob_above_threshold)
  }, colnames = T)

    # NOTE need to divide by 100 to convert to proportion
    est_prev_plot <- reactive({
      ggplot(prev_output()) +
        geom_segment(aes(x = " ", xend = " ", y = CrI_lower/100, yend = CrI_upper/100),
                     color = "black", linewidth = 1) +
        geom_point(aes(x = " ", y = MAP/100),
                   size = 3,
                   shape = 21,
                   fill = "skyblue3") +
        geom_hline(aes(yintercept=0.05),
                   color = "darkgrey",
                   linetype = "dashed") +
        geom_text(aes(x= " ", y = 0.07, label = "5% threshold"), color = "darkgrey") +
        scale_y_continuous(labels = scales::percent_format(1), limits = c(0,1)) +
        labs(x = "",
             y = "Estimated prevalence",
             caption = paste0("Result: there is a ", ceiling(prev_output()$prob_above_threshold*100), "% probability that pfhrp2 prevalence is above 5%")) +
        theme_light() +
        theme(text = element_text(size = 16))
    })

    output$est_prev_plot <- renderPlot(est_prev_plot())
  
  # ----------------------------------
  #  Results table/plot: estimated ICC
  # ----------------------------------
  
  observeEvent(input$est_icc, {
    print("Estimate ICC button clicked")
    
    # debugging, remove later
    print("After user clicks the estimate ICC button, this is the edited df: ")
    print(analysis_rv$df_analysis_update)
    
    output$title_iccbox <- renderText("The estimated ICC value is below: ")
    output$text_iccbox <- renderText("The table and plot show the mean and lower and upper credible interval")
  
    print(icc_output())
  })
    
  # Calculate ICC using DRpower
    icc_output <- eventReactive(input$est_icc, {
      df <- analysis_rv$df_analysis_update
      
      DRpower::get_ICC(n = df$n_deletions,
                       N = df$sample_size)
      
    })
    
    output$est_icc_table <- renderTable({
      # TODO: figure out how to add column names
      icc_output()
    
    })
  
    # NOTE need to divide by 100 to convert to proportion
    est_icc_plot <- reactive({
      ggplot(icc_output()) +
        geom_segment(aes(x = " ", xend = " ", y = CrI_lower/100, yend = CrI_upper/100), 
                     color = "black", linewidth = 1) +
        geom_point(aes(x = " ", y = MAP/100), 
                   size = 4, 
                   shape = 21,
                   fill = "skyblue3") +
        scale_y_continuous(labels = scales::percent_format(1), limits = c(0,1)) +
        labs(x = "",
             y = "Estimated ICC") +
        theme_light() +
        theme(text = element_text(size = 16)) 
    })
  
    output$est_icc_plot <- renderPlot(est_icc_plot())

  # ----------------------------------
  #  Render downloadable analysis report  
  # ----------------------------------
  # TODO: fix the reactive plot issue 
  
  output$analysis_report <- downloadHandler(
    filename = paste0("PfHRP2_Planner_Analysis_Report_", Sys.Date(), ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "template_analysis_report.Rmd")
      file.copy("template_analysis_report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(analysis_nclusters = input$analysis_nclust,
                     analysis_study_data = analysis_rv$df_analysis_update,
                     analysis_prevoutput = prev_output(),
                     analysis_iccoutput = icc_output()
                     )
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
      )
    }
  )
  
  
}
