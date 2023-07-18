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

df_sample_sizes <- df_ss %>% 
  filter(ICC == 0.05) %>% 
  filter(prev_thresh == 0.05) %>% 
  select(n_clust, prevalence, N_opt) %>% 
  # mutate(N_opt = replace_na(as.character(N_opt), ">1000")) %>% # can use this is we don't care about re-ordering and want it to look good
  pivot_wider(names_from = prevalence, values_from = N_opt) 

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
  
  output$sample_size_table <- renderDT({
    datatable(df_sample_sizes, 
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
              )
              )
    })
  
  # ----------------------------------
  #  User-input table: sample size and proportion drop-out
  # ----------------------------------
  
  # create a reactive value for df_sizes_update
  design_rv <- reactiveValues(df_sizes_update = NULL)

  # Make the editable data frame reactive and dependent on the number of clusters entered by the user
  df_sizes <- eventReactive(input$design_nclust, ignoreNULL=T, ignoreInit=T, {
    # create the data frame with fixed columns and rows based on user input
    # TODO: This needs to be updated to ideal numbers based on final simulations (need to make design_nclust reactive), keep this fixed at default vals (prev==0.1), if prev=8,9,10 fix at 500 sample size
    # store all dfs in list and access list by index number aka input$design_nclust
    data.frame(
      cluster = rep(2:input$design_nclust),
      sample_size = rep(100, input$design_nclust),
      prop_dropout = rep(0.1, input$design_nclust)
    )
  })
  
  # observe when the user specifies n clusters
  observeEvent(input$design_nclust, ignoreNULL=T, ignoreInit=T, {
    print("Number of clusters selected")

    output$text_edit_clusttab <- renderText("The table below now has rows corresponding to the number of clusters in your study.
                                            Please edit the target sample size and expected proportion of participant drop-out for each cluster by double-clicking
                                            and editing the table below. When you are finished click the 'Calculate final sample sizes' button")
    
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
  
  # When 'calculate sample sizes' button is clicked:
  # update the data frame with the user-entered values, calculate the adjusted sample size, and create a final df that is reactive
  df_sizes_final <- eventReactive(input$calc_sizes, {
    
    # require n clusters to be defined
    req(input$design_nclust)
    
    # get the stored (and edited) data frame with sample sizes
    df <- design_rv$df_sizes_update
    
    # calculate adjusted sample size
    df <- df %>% mutate(final_sample_size = ceiling(sample_size/(1-prop_dropout)))
    
    return(df)
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
       return("The final sample sizes are below: ")
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
       return("Based on the values you entered for sample size and taking into account the proportion drop-out,
                                             the final adjusted sample sizes are calculated using the formula: Nadj=n/(1-d) where Nadj is the adjusted sample size,
                                             n is the target sample size, and d is the expected drop-out proportion")
     }
     # if it hasn't been created then display nothing
     else{
       return(NULL)
     }
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
                                 reps = as.numeric(input$param_n_sims)
    )
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
      paste0("The plot shows the mean and lower and upper credible interval based on the parameters selected and the sample sizes you entered in Step 1.")
      
    }
    # if it hasn't been created yet then return nothing
    else{
      return(NULL)     
    }
  })
  
  # TODO seems like the plot is not re-loading when params are changed, but working for button click
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

  # initialize empty data frame
  # df_a <- data.frame(
  #   cluster = integer(),
  #   n_deletions = integer(),
  #   sample_size = integer()
  # )
  # 
  # # render the initial table ()
  # output$editable_deltab <- renderDT({
  #   datatable(df_a, 
  #             editable = list(
  #               target = 'cell',
  #               disable = list(
  #                 columns = c(0)
  #               )
  #             ),
  #             # rownames = FALSE,
  #             # colnames = c(), # add colnames
  #             options = list(dom = 'rt',
  #                            autoWidth = TRUE, pageLength = 20)) 
  #   
  #   
  # })
  
  # create a reactive value for df_analysis_update
  analysis_rv <- reactiveValues(df_analysis_update = NULL)
  
  # Make the editable data frame reactive and dependent on the deletion and sample sizes entered by the user
  df_deletions <- eventReactive(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
    # TODO this also needs to be updated to ideal numbers based on the final simulations?
    # create the data frame with fixed columns and rows based on user input
    data.frame(
      cluster = c(rep(2:input$analysis_nclust)),
      n_deletions = c(rep(5, input$analysis_nclust)),
      sample_size = c(rep(100, input$analysis_nclust))
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
 
  # When 'Estimate prevalence' button is clicked:
  # Calculate prevalence using DRpower ::get_prevalence() with the user-entered deletions and sample sizes
  prev_output <- eventReactive(input$est_prev, {
    
    # require the updated data frame to have been created to make sure there is a data frame to get values from
    req(analysis_rv$df_analysis_update)
    
    # create a progress notification pop-up telling the user that prevalence is being estimated
    id <- showNotification(paste0("Estimating prevalence..."), 
                           duration = 10, 
                           closeButton = FALSE)
    
    # remove notification when calculation finishes
    on.exit(removeNotification(id), add = TRUE)
    
    df <- analysis_rv$df_analysis_update
    
    DRpower::get_prevalence(n = df$n_deletions, 
                            N = df$sample_size)
    
  })
  
  # If user clicks 'estimate prevalence' button before selecting clusters and entering sample sizes, an error message will pop-up
  observeEvent(input$est_prev, {
    print("Estimate prevalence button clicked")
    
      # display error message if the user has not entered the deletions and sample sizes, require the reactiveVal 'df_analysis_update' to have been created
      if(is.null(analysis_rv$df_analysis_update)){
        # TODO debugging
        print("estimate prev is NULL")
        print("error should have popped up")
        
        show_alert(
          title = "Error!",
          text = "You have not selected the number of clusters or entered the values for your study. Please select the number of clusters from the drop-down menu and enter the values in the table.",
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
      return("The estimated prevalence value is below: ")
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
      paste0("The table and plot show the mean and lower and upper credible interval. There is a ",
             ceiling(prev_output()$prob_above_threshold*100),
             "% probability that the ",
             "pfhrp2 prevalence is above the 5% threshold.")
    }
    else{
      return(NULL)
    }
  })
  
  output$est_prev_table <- renderTable({
    prev_output() %>% 
      rename("Mean prevalence" = MAP, "Lower CrI" = CrI_lower, "Upper CrI" = CrI_upper, "Probability above threshold" = prob_above_threshold)
  }, colnames = T
  )

  # TODO: make sure plot re-renders (or fades out) when recalculating - power plot does this! 
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
      return("The estimated ICC value is below: ")
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
      return("The table and plot show the mean and lower and upper credible interval")
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
