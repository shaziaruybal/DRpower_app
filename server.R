######### DRpower App: Server ####################################################################
# Authors: Shazia Ruybal-Pesántez (sruybal@imperial.ac.uk)
##################################################################################################

library(shiny)
library(tidyverse)
library(shiny.fluent)
library(shinyWidgets)
library(shinyBS)
library(DRpower)

set.seed(10)

df_ss <- readRDS("df_ss.rds")

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
  
  # When user selects and ICC value and prevalence threshold, create a reactive data frame df_sample_sizes() filtered on these values
  df_sample_sizes <- reactive({ 
    
    # require the user inputs to create the table
    req(input$ss_icc, input$ss_prev)
    
    icc <- as.numeric(input$ss_icc)
    prev <- as.numeric(input$ss_prev)/100
    
    df_ss %>% 
    filter(ICC == icc) %>% 
    filter(prev_thresh == prev) %>% 
    # filter(prior_ICC_shape2==9) %>% # fixed at 9 once we have the new table
    select(n_clust, prevalence, N_opt) %>% 
    pivot_wider(names_from = prevalence, values_from = N_opt) 
  })
  
  # render explanatory text for the sample sizes table that should appear when the user selects ICC and prev
  output$text_ss <- renderText({
    # require the user inputs to render the text
    req(input$ss_icc, input$ss_prev)
    
    "Columns give the assumed true prevalence of pfhrp2/3 deletions in the province. 10% is highlighted as the suggested default. Rows give the number of clusters (e.g., health facilities) within the province. Scroll the table to view all suggested values."
    
  })
  
  output$sample_size_table <- renderDT({
    datatable(df_sample_sizes(), 
              colnames = c("Number of clusters", "1%", "2%", "3%", "4%", "5%", "6%", "7%", "8%", "9", "10%", "11%", "12%", "13%", "14%", "15%", "16%", "17%", "18%", "19%", "20%"),
              rownames = FALSE,
              extensions = c("Buttons", "FixedHeader", "FixedColumns"),
              options = list(# autoWidth = T,
                             pageLength = 20,
                             fixedHeader = TRUE,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")),
                             fixedColumns = list(leftColumns = 1),
                             scrollX = '500px',
                             dom = 'tB',
                             buttons = c('copy', 'csv', 'excel')
              ), 
              # caption = paste0("This table shows target sample sizes assuming an intra-cluster correlation of: ", input$ss_icc, " \n and a prevalence threshold of: ", input$ss_prev)
              ) %>% 
      formatStyle("0.1",
                  backgroundColor = "green",
                  fontWeight = 'bold'
      )
    })
  
  # ----------------------------------
  #  User-input table: sample size and proportion drop-out
  # ----------------------------------
  
  # create a reactive value for df_sizes_update
  design_rv <- reactiveValues(df_sizes_update = NULL)

  # Make the editable data frame reactive and dependent on the number of clusters entered by the user
  df_sizes <- eventReactive(input$design_nclust, ignoreNULL=T, ignoreInit=T, {
    
    # TODO: question for Bob - do we want this to populate based on df_sample_sizes() or defaults? 
    df_targets <- df_ss %>% 
        filter(ICC == 0.05) %>% 
        filter(prev_thresh == 0.05) %>% 
        # filter(prior_ICC_shape2==9) %>% # fixed at 9 once we have the new table
        select(n_clust, prevalence, N_opt) %>% 
        pivot_wider(names_from = prevalence, values_from = N_opt) 
    
    # get the target sample sizes from table with fixed prev of 10%, fix it at 500 if nclust is 2 or 3 (because NA)
    if(input$design_nclust==2 | input$design_nclust==3){
      target_size <- 500
    }
    else{
      target_size <- df_targets %>% filter(n_clust == input$design_nclust) %>% select(`0.1`) %>% as.integer()
    }

    # create the data frame with fixed columns and rows based on user input and target sample sizes as defaults
    data.frame(
      cluster = rep(1:input$design_nclust),
      sample_size = rep(target_size, input$design_nclust),
      percent_dropout = rep(10, input$design_nclust)
    )
  })
  
  # observe when the user specifies n clusters
  observeEvent(input$design_nclust, ignoreNULL=T, ignoreInit=T, {
    print("Number of clusters selected")

    output$text_edit_clusttab <- renderText("The table below now has rows corresponding to the number of clusters in your study.
                                            Please edit the target sample size and expected proportion of participant drop-out for each cluster by double-clicking
                                            and editing each cell in the table below. When you are finished click the 'Calculate final sample sizes' button. ")
    
    print("After user selects N clusters, this is the df:")
    print(df_sizes())
    
    # when df_sizes() is created, store the initial values in df_sizes_update()
    design_rv$df_sizes_update <- df_sizes()
  })
  
  # render editable table
  output$editable_clusttab <- renderDT({
    datatable(df_sizes(), 
              editable = list(
                target = 'cell',
                numeric = c(2,3),
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

    # iterate over each cell edit event, make sure the values are numeric
    for (i in seq_along(input$editable_clusttab_cell_edit$row)) {
      row <- input$editable_clusttab_cell_edit$row[i]
      col <- input$editable_clusttab_cell_edit$col[i]
      value <- as.numeric(input$editable_clusttab_cell_edit$value[i])

      # update the corresponding cell in the new data frame
      df[row, col] <- value
    }

    # assign the updated data frame to df_sizes_update
    design_rv$df_sizes_update <- df
    
  })
  
  
  # ----------------------------------
  #  Calculate final sample sizes
  # ----------------------------------
  
  # When 'calculate sample sizes' button is clicked:
  # update the data frame with the user-entered values, calculate the adjusted sample size, and create a final df that is reactive
  df_sizes_final <- eventReactive(input$calc_sizes, {
    
    # require n clusters to be defined
    req(input$design_nclust)
    
    # get the stored (and edited) data frame with sample sizes
    df <- design_rv$df_sizes_update
    
    # check that sample size values are numeric and that no value is NA (and if so show pop-up error message)
    if(is.numeric(df$sample_size) && !any(is.na(df$sample_size))){
      
      # calculate adjusted sample size
      df <- df %>% mutate(final_sample_size = ceiling(sample_size/(1-(percent_dropout/100))))
    
      return(df)
    }
    else{
      print(design_rv$df_sizes_update)
      show_alert(
        title = "Error!",
        text = "Make sure you have only entered integers in your table and/or make sure you have filled in all the cells. Please go back and enter the values again.",
        type = "error"
      )
    }
  }) 
  
  # observe when calculate sample sizes button is clicked 
   observeEvent(input$calc_sizes, {
     print("Calculate final sample sizes values button clicked")
     #------- debugging, remove later ----
     print("After user clicks the calc sample size button, this is the edited df: ")
     print(design_rv$df_sizes_update)
     #------------------------------------
     
     # error message if the user has not chosen the number of clusters
     if(input$design_nclust==""){
       show_alert(
         title = "Error!",
         text = "You have not chosen the number of clusters. Please go back to Step 1 and choose the number of clusters and enter the values in the table.",
         type = "error"
       )
     }
     else{
       return(NULL)
     }
   })     
     
   # output text describing the final adjusted sample size table
   output$title_finalsizesbox <- renderText({
     # require n clusters to be defined and calculate sizes button to be clicked
     req(input$design_nclust, input$calc_sizes)
     
     # check if df_sizes_final() has been created, which means the user has selected n clusters, edited the data (or not), and clicked 'calculate sizes' button
     if(!is.null(df_sizes_final())){
       # TODO debugging, remove later
       print("title_finalsizebox should have printed")
       return("Adjusted sample sizes")
     }
     # if it hasn't been created then display nothing
     else{
       # TODO debugging, remove later
       print("title_finalsizebox didn't print")
       return(NULL)
     }
     
   })
   
   output$text_finalsizesbox <- renderText({
     # require n clusters to be defined and calculate sizes button to be clicked
     req(input$design_nclust, input$calc_sizes)
     
     # check if df_sizes_final() has been created, which means the user has selected n clusters, edited the data (or not), and clicked 'calculate sizes' button
     if(!is.null(df_sizes_final())){
       return("Based on the values you entered for sample size (n) and taking into account the proportion drop-out (d), the adjusted sample size is calculated using the formula n_adj = n/(1-d). This still refers to confirmed malaria positive cases. Scroll the table to view.")
     }
     # if it hasn't been created then display nothing
     else{
       return(NULL)
     }
   })
  
  # render the edited table
  output$final_sizes_table <- renderDT({
    datatable(df_sizes_final(), 
              colnames = c("Cluster", "Target sample size", "% drop-out", "Final adjusted sample size"),
              extensions = c("FixedHeader", "FixedColumns"),
              options = list(dom = 'rt',
                             width=4,
                             pageLength=20,
                             fixedHeader = T,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")),
                             fixedColumns = list(leftColumns = c(2)),
                             scrollX = '400px'
                             )
              )
  })

  # ----------------------------------
  #  Results plot: estimated power
  # ----------------------------------
  
  # When 'Estimate power' button is clicked:
  # calculate power using DRpower::get_power_threshold() with the user-entered params
  power_output <- eventReactive(input$est_pow, { 
    
    req(df_sizes_final())
    
    # create a progress notification pop-up telling the user that power is being estimated based on n_sims
    id <- showNotification(paste0("Estimating power ( ", input$param_n_sims, " simulations)..."), 
                           duration = NULL, 
                           closeButton = FALSE)
    
    # remove notification when calculation finishes
    on.exit(removeNotification(id), add = TRUE)
      
    DRpower::get_power_threshold(N = df_sizes_final()$sample_size, 
                                 prevalence = as.numeric(input$param_prev),
                                 ICC = as.numeric(input$param_icc),
                                 reps = as.numeric(input$param_n_sims))
  })
  
  # If user clicks 'estimate power' button before entering sample sizes, an error message will pop-up
  observeEvent(input$est_pow, {
      print("Estimate power button has been clicked")
      
      # error message pops up if the user has not entered the sample sizes (check that calculate sizes button has been clicked)
      if(input$calc_sizes==0){
        # TODO debugging
        print("calculate sizes is NULL")
        print("error should have popped up")
        show_alert(
          title = "Error!",
          text = "You have not entered the sample sizes correctly. Please go back to Step 1 and choose the number of clusters and enter the values in the table, and then click the 'Calculate final sample sizes' button.",
          type = "error"
        )
      }
      else{
        print("button has been clicked and df_sizes_final has been calculated (so no pop-up error needed):")
        print(df_sizes_final())
        return(NULL)
      }
    })
    
  # Display title text once estimate power is clicked and power_output() has been created
  output$title_powbox <- renderText({
    
    # require estiamte power button click
    req(input$est_pow)
    
    # check if power_output() has been created, which means the results have been calculated and can be displayed
    if(!is.null(power_output())){
      return("The estimated power is below: ")
    }
    # if it hasn't been created yet then return nothing (note error message will pop-up based on other reactivity vals)
    else{
      return(NULL)
    }
  })
    
  # Display results text once estimate power is clicked and power_output() has been created
  output$text_powbox <- renderText({
    
    # require estimate power to have been clicked
    req(input$est_pow)
    
    # check if power_output() has been created, which means the results have been calculated and can be displayed
    if(!is.null(power_output())){
      paste0("The plot shows the mean and lower and upper 95% confidence interval based on cluster sizes and parameters chosen above.  ")
      
    }
    # if it hasn't been created yet then return nothing
    else{
      return(NULL)     
    }
  })
  
  output$est_power_table <- renderTable({
    
    # require prev_output() to exist
    req(power_output())
    
    power_output() %>% 
      rename("Power" = power, "Lower 95%CI" = lower, "Upper 95%CI" = upper)
  }, colnames = T
  )
  
  # TODO seems like the plot is not re-loading when params are changed, but working for button click
  est_power_plot <- reactive({
    
    # require power_output() to exist
    req(power_output())
    
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
           y = "Estimated power" # ,
           # caption = paste0("Parameters: prev=", input$param_prev, ", ICC=", input$param_icc, ", sims=", input$param_n_sims)
           ) +
      theme_light() +
      theme(text = element_text(size = 16))
  })
  
output$est_power_plot <- renderPlot(est_power_plot())
  
  
  # ----------------------------------
  #  Save results and render downloadable design report
  # ----------------------------------
  
  # Store a reactive value that checks whether the summary data is complete or not (T/F)
  design_rv <- reactiveValues(design_data_ready = FALSE)
  
  # TODO I think we should save all the outputs as a list to feed into the report?! 
  
  # The save button allows the user to cross-check the assumed parameters entered and check the numbers that will be printed in the report
  # - if the user has not entered the values correctly in the previous tab, an error message will pop-up and the design_data_ready reactive val will be set to FALSE
  # - if it passes all validation checks (ie user has entered everything), design_data_ready will be set to TRUE 
  observeEvent(input$save_design_data, {
    print("Save design data button has been clicked")

    # If all conditions are not met - ie the user has gone through the entire Step 2 Final cluster sizes tab, set design_data_ready as FALSE
    if (input$design_nclust=="" || input$calc_sizes==0 || input$est_pow==0 || is.null(df_sizes_final()) || is.null(power_output())) {
      print("error should pop up when save results is clicked")
      show_alert(
        title = "Error!",
        text = "The summary cannot be displayed because you haven't completed Step 2. Please go back to 'Final cluster sizes' and follow all the steps.",
        type = "error"
      )
      
      design_rv$design_data_ready <- FALSE
      print("design data is not ready")
      print(design_rv$design_data_ready)
    }
    
    # If all conditions have been met, set design_data_ready to TRUE
    else {
      design_rv$design_data_ready <- TRUE
      print("design data is ready for download")
      print(design_rv$design_data_ready)
    }

  })
  
  # Display a summary of the assumed parameters and data once the save button is clicked 
  output$text_design_summary <- renderUI({
    
    req(input$save_design_data)
    
    if (design_rv$design_data_ready==TRUE) {
      print("text design summary should print")
      
      box(width = 12, 
          # solidHeader = "purple",
          # background = "purple", 
          collapsible = TRUE,
          title = "Data summary",
          h4("Final cluster sizes:"),
          renderTable(df_sizes_final()),
          br(), br(),
          h4("Parameters for power calculation:"),
          p("ICC: ", input$param_icc),
          p("Prevalence: ", ceiling(as.numeric(input$param_prev)*100), "%"), 
          p("Number of simulations: ", input$param_n_sims),
          br(), br(),
          h4("Power estimates:"),
          renderTable(power_output()),
          renderPlot(est_power_plot()),
      )
    }
  })
  
  # Render the download button only if the user has clicked on the save button and the data is ready to be downloaded (ie design_data_ready==TRUE)
  output$design_download <- renderUI({
    req(input$save_design_data)
    
    if(design_rv$design_data_ready==TRUE){
      print("download button shown because everything has been entered")
      
      box(width = 12,
          title = "Click below to download your design report.",
          em("This creates a pdf summary of the assumed parameters and your results, with standardised text to minimise mistakes."),
          br(), br(),
          downloadButton("design_report", "Download design report", icon("download")))
    }

  })
  
  # The downloadHandler() for the design report will be triggered if the downloadButton() is clicked 
  output$design_report <- downloadHandler(
    
      filename = paste0("PfHRP2_Planner_Design_Report_", Sys.Date(), ".html"),
      content = function(file) {
        # create a progress notification pop-up telling the user that the report is rendering
        id <- showNotification(paste0("Preparing report..."), 
                               duration = 10, 
                               closeButton = FALSE)
        
        # remove notification when calculation finishes
        on.exit(removeNotification(id), add = TRUE)
        
        tempReport <- file.path(tempdir(), "template_design_report.Rmd")
        file.copy("template_design_report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
          design_ss_icc = input$ss_icc,
          design_ss_prev = input$ss_prev,
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

  # create a reactive value for df_analysis_update
  analysis_rv <- reactiveValues(df_analysis_update = NULL)
  
  # Make the editable data frame reactive and dependent on the deletion and sample sizes entered by the user
  df_deletions <- eventReactive(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
    
    print("number of analysis clusters selected")
    
    # create the data frame with fixed columns and rows based on user input
    data.frame(
      cluster = c(rep(1:input$analysis_nclust)),
      n_deletions = c(rep(NA, input$analysis_nclust)),
      sample_size = c(rep(NA, input$analysis_nclust))
    )
  })  
  
  # When the user selects the number of clusters, we store the initial values in df_sizes_update() so we can keep track of any user edits to the table
  observeEvent(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
    print("Number of final clusters selected")
    
    analysis_rv$df_analysis_update <- df_deletions()
  }) 
  
  # Render editable table
  output$editable_deltab <- renderDT({
    datatable(df_deletions(), 
              editable = list(
                target = 'cell',
                numeric = c(2,3),
                disable = list(
                  columns = c(1)
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
      # make sure edited value is numeric
      value <- as.numeric(input$editable_deltab_cell_edit$value[i])
      
      # update the corresponding cell in the new data frame
      df[row, col] <- value
    }
    
    # assign the updated data frame to df_sizes_update
    analysis_rv$df_analysis_update <- df
    
  })
  
  # ----------------------------------
  #  Results table/plot: estimated prevalence
  # ----------------------------------
 
  # When 'Estimate prevalence' button is clicked:
  # Calculate prevalence using DRpower ::get_prevalence() with the user-entered deletions and sample sizes
  prev_output <- eventReactive(input$est_prev, {
    
    # require the updated data frame to have been created to make sure there is a data frame to get values from
    req(input$analysis_prevthresh, analysis_rv$df_analysis_update)

    # create a progress notification pop-up telling the user that prevalence is being estimated
    id <- showNotification(paste0("Estimating prevalence..."), 
                           duration = 10, 
                           closeButton = FALSE)
    
    # remove notification when calculation finishes
    on.exit(removeNotification(id), add = TRUE)
    
    df <- analysis_rv$df_analysis_update
    
    # check that values are numeric and that no value is NA (and if so show pop-up error message)
    if(is.numeric(df$n_deletions) && !any(is.na(df$n_deletions)) && is.numeric(df$sample_size) && !any(is.na(df$sample_size))){
      print(str(df))

      DRpower::get_prevalence(n = df$n_deletions,
                              N = df$sample_size,
                              prev_thresh = as.numeric(input$analysis_prevthresh))
    }
    else {
      print(str(df))
      show_alert(
        title = "Error!",
        text = "Make sure you have only entered integers in your table and/or make sure you have filled in all the cells. Please go back and enter the values again.",
        type = "error"
      )
    }
    
  })
  
  # If user clicks 'estimate prevalence' button before selecting clusters and entering sample sizes, an error message will pop-up
  observeEvent(input$est_prev, {
    print("Estimate prevalence button clicked")
    
      # display error message if the user has not selected prev_thresh and/or entered the deletions and sample sizes, require the reactiveVal 'df_analysis_update' to have been created
      if(input$analysis_prevthresh=="" || is.null(analysis_rv$df_analysis_update)){
        # TODO debugging
        print("estimate prev is NULL")
        print("error should have popped up")
        
        show_alert(
          title = "Error!",
          text = "You have not selected the prevalence threshold, number of clusters and/or entered the values for your study. Please select the prevalence threshold and number of clusters from the drop-down menu and enter the values in the table.",
          type = "error"
        )
      }
      else{
        # debugging, remove later
        print("After user clicks the estimate prev button, this is the edited df (no pop-up error msg needed): ")
        print(analysis_rv$df_analysis_update)
        return(NULL)
      }
  })
  
  # Display title text once estimate prevalence is clicked and power_output() has been created
  output$title_prevbox <- renderText({
    
    # require estimate prevalence button click
    req(input$est_prev)
    
    # check if prev_output() has been created, which means the results have been calculated and can be displayed
    if(!is.null(prev_output())){
      return("Prevalence estimates")
    }
    # if it hasn't been created yet then return nothing (note error message will pop-up based on other reactivity vals)
    else{
      return(NULL)
    }
  })
  
  output$text_prevbox <- renderText({
    
    # require estimate pevalence button click
    req(input$est_prev)
    
    # check if prev_output() has been created, which means the results have been calculated and can be displayed
    if(!is.null(prev_output())){
      paste0("The table and the plot below show the maximum a posteriori (MAP) estimate of the prevalence, along with a 95% credible interval (CrI). The MAP estimate can be used as a central estimate of the prevalence, but it should always be reported alongside the CrI to give a measure of uncertainty. ",
             "The table also gives the probability of being above the threshold. ",
             # "The table also gives the probability of being above the threshold ", "( ", ceiling(prev_output()$prob_above_threshold*100), "% probability that the pfhrp2 prevalence is above the ", ceiling(input$analysis_prevthresh*100), "% threshold).", 
             "As mentioned above, if you are using this value in a hypothesis test then we recommend accepting that prevalence is above the threshold if probability is 0.95 or higher.")
             
    }
    else{
      return(NULL)
    }
  })
  
  output$est_prev_table <- renderTable({
    
    # require prev_output() to exist
    req(prev_output())
    
    prev_output() %>% 
      rename("Mean prevalence" = MAP, "Lower CrI" = CrI_lower, "Upper CrI" = CrI_upper, "Probability above threshold" = prob_above_threshold)
  }, colnames = T
  )

  # TODO: make sure plot re-renders (or fades out) when recalculating - power plot does this! 
    # NOTE need to divide by 100 to convert to proportion
    est_prev_plot <- reactive({
      # require prev_output() to exist
      req(prev_output())
      
      ggplot(prev_output()) +
        geom_segment(aes(x = " ", xend = " ", y = CrI_lower/100, yend = CrI_upper/100),
                     color = "black", linewidth = 1) +
        geom_point(aes(x = " ", y = MAP/100),
                   size = 3,
                   shape = 21,
                   fill = "skyblue3") +
        # use the user-entered prev_thresh to plot threshold line
        geom_hline(aes(yintercept = as.numeric(input$analysis_prevthresh)),
                   color = "darkgrey",
                   linetype = "dashed") +
        # use the user-entered prev_thresh to plot threshold line
        geom_text(aes(x= " ", 
                      y = as.numeric(input$analysis_prevthresh)+0.02, 
                      label = paste0(ceiling(as.numeric(input$analysis_prevthresh)*100),"% threshold")), 
                      color = "darkgrey") +
        scale_y_continuous(labels = scales::percent_format(1), limits = c(0,1)) +
        labs(x = "",
             y = "Estimated prevalence",
             caption = paste0("Result: there is a ", ceiling(prev_output()$prob_above_threshold*100), "% probability that pfhrp2/3 prevalence is above ", ceiling(as.numeric(input$analysis_prevthresh)*100), "%")) +
        theme_light() +
        theme(text = element_text(size = 16))
    })

    output$est_prev_plot <- renderPlot(est_prev_plot())
  
  # ----------------------------------
  #  Results table/plot: estimated ICC
  # ----------------------------------
  
    # When 'Estimate ICC' button is clicked:
    # Calculate ICC using DRpower::get_ICC() with the user-entered deletions and sample sizes
    icc_output <- eventReactive(input$est_icc, {
      
      # require the updated data frame to have been created to make sure there is a data frame to get values from
      req(analysis_rv$df_analysis_update)
      
      # create a progress notification pop-up telling the user that ICC is being estimated
      id <- showNotification(paste0("Estimating intra-cluster correlation..."), 
                             duration = 10, 
                             closeButton = FALSE)
      
      # remove notification when calculation finishes
      on.exit(removeNotification(id), add = TRUE)
      
      df <- analysis_rv$df_analysis_update
      
      DRpower::get_ICC(n = df$n_deletions,
                       N = df$sample_size)
      
    })
    
  # If user clicks 'estimate ICC' button before selecting clusters and entering sample sizes, an error message will pop-up
  observeEvent(input$est_icc, {
    print("Estimate ICC button clicked")
    
    # display error message if the user has not entered the deletions and sample sizes, require the reactiveVal 'df_analysis_update' to have been created
    if(is.null(analysis_rv$df_analysis_update)){
      # TODO debugging
      print("error should have popped up")
      
      show_alert(
        title = "Error!",
        text = "You have not selected the number of clusters or entered the values for your study. Please go back to the previous section ('Estimate prevalence') and select the number of clusters from the drop-down menu and enter the values in the table.",
        type = "error"
      )
    }
    else{
      # debugging, remove later
      print("After user clicks the estimate ICC button, this is the edited df (no pop-up error msg needed): ")
      print(analysis_rv$df_analysis_update)
      return(NULL)
    }
    
  })
  
  # Display title text once estimate ICC is clicked and icc_output() has been created
  output$title_iccbox <- renderText({
    
    # require estimate ICC button click
    req(input$est_icc)
    
    # check if icc_output() has been created, which means the results have been calculated and can be displayed
    if(!is.null(icc_output())){
      return("ICC estimates")
    }
    # if it hasn't been created yet then return nothing (note error message will pop-up based on other reactivity vals)
    else{
      return(NULL)
    }
  })
  
  # Display text once estimate ICC is clicked and icc_output() has been created
  output$text_iccbox <- renderText({
    
    # require estimate ICC button click
    req(input$est_icc)
    
    # check if icc_output() has been created, which means the results have been calculated and can be displayed
    if(!is.null(icc_output())){
      return("The table and the plot below show the maximum a posteriori (MAP) estimate of the ICC, along with a 95% credible interval (CrI). For context, an ICC of 0.05 is used by default in the Design tab based on an analysis of historical studies. ")
    }
    # if it hasn't been created yet then return nothing (note error message will pop-up based on other reactivity vals)
    else{
      return(NULL)
    }
  })
  
  output$est_icc_table <- renderTable({
    icc_output() %>% 
      rename("Mean ICC" = MAP, "Lower CrI" = CrI_lower, "Upper CrI" = CrI_upper)
  }, colnames = T
  )
  
  # TODO: make sure plot re-renders (or fades out) when recalculating - power plot does this! 
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
      # create a progress notification pop-up telling the user that the report is rendering
      id <- showNotification(paste0("Preparing report..."), 
                             duration = 10, 
                             closeButton = FALSE)
      
      # remove notification when calculation finishes
      on.exit(removeNotification(id), add = TRUE)
      
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
