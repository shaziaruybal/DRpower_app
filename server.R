######### DRpower App: Server ####################################################################
# Authors: Shazia Ruybal-Pesántez (sruybal@imperial.ac.uk)
##################################################################################################

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinyBS)
library(DRpower)
library(kableExtra)

set.seed(10)

df_ss <- DRpower::df_ss

function(input, output, session) {
  
  ##################################################
  # TESTING
  ################################################## 
  
  # output$test_table <- renderTable({
  #   head(df_ss)
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

    # icc <- as.numeric(input$ss_icc)
    # prev <- as.numeric(input$ss_prev)/100

    df_ss %>%
    filter(ICC == as.numeric(input$ss_icc)) %>%
    filter(prev_thresh == as.numeric(input$ss_prev)/100) %>%
    filter(prior_ICC_shape2 == 9) %>% # TODO fixed at 9 (check this when final final table is ready)
    select(n_clust, prevalence, N_opt) %>%
    pivot_wider(names_from = prevalence, values_from = N_opt)
  })

  # render explanatory text for the sample sizes table that should appear when the user selects ICC and prev
  output$text_ss <- renderText({
    # require the user inputs to render the text
    req(input$ss_icc, input$ss_prev)

    "Columns give the assumed true prevalence of pfhrp2/3 deletions in the province. 10% is highlighted as the suggested default. Rows give the number of clusters (e.g., health facilities) within the province. Scroll the table to view all suggested values. Note that if a particular cell is blank, the target sample size is >2000."

  })

  output$sample_size_table <- renderDT({
    datatable(df_sample_sizes(),
              colnames = c("Number of clusters", "1%", "2%", "3%", "4%", "5%", "6%", "7%", "8%", "9", "10%", "11%", "12%", "13%", "14%", "15%", "16%", "17%", "18%", "19%", "20%"),
              rownames = FALSE,
              extensions = c("Buttons", "FixedHeader"),
              # extensions = c("Buttons", "FixedHeader", "FixedColumns"),
              options = list(# autoWidth = T,
                             pageLength = 20,
                             fixedHeader = TRUE,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")),
                             # fixedColumns = list(leftColumns = 1),
                             scrollX = '500px',
                             dom = 'tB',
                             buttons = c('copy', 'csv', 'excel')
              )) %>%
      formatStyle("0.1",
                  backgroundColor = "lavender", # thistle, lavender
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
    # getting target sizes to pre-populate the table from fixed defaults of ICC=0.05 and prev_thresh=0.05 
    df_targets <- df_ss %>% 
        filter(ICC == 0.05) %>% 
        filter(prev_thresh == 0.05) %>% 
        filter(prior_ICC_shape2==9) %>% # TODO fixed at 9 (check this when final final table is ready)
        select(n_clust, prevalence, N_opt) %>% 
        pivot_wider(names_from = prevalence, values_from = N_opt) 
    
    # get the target sample sizes from table with fixed prev of 10%, fix it at 500 if nclust is 2 or 3 (because NA)
    if(input$design_nclust==2 | input$design_nclust==3 | input$design_nclust==4){
      target_size <- 500
    }
    else{
      target_size <- df_targets %>% filter(n_clust == input$design_nclust) %>% select(`0.1`) %>% as.integer()
    }

    # create the data frame with fixed columns and rows based on user input and target sample sizes as defaults
    data.frame(
      cluster = rep(1:input$design_nclust),
      target_sample_size = rep(target_size, input$design_nclust),
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
                  columns = c(0)
                )
              ),
              rownames = FALSE, 
              colnames = c("Cluster", "Target sample size", "% drop-out"),
              extensions = c("FixedHeader"),
              # extensions = c("FixedHeader", "FixedColumns"),
              options = list(dom = 'rt',
                             # autoWidth = TRUE,
                             pageLength=20,
                             # fixedHeader = T,
                             # columnDefs = list(list(className = "dt-center",
                             #                        targets = "_all")),
                             # fixedColumns = list(leftColumns = c(1)),
                             scrollX = '400px'))
  })

  # observe when table is edited and update the data frame with the user entered values
  observeEvent(input$editable_clusttab_cell_edit, {
    print("Editable design table has been edited")
    
    # get the latest updated data frame
    df <- design_rv$df_sizes_update

    # iterate over each cell edit event, make sure the values are numeric
    for (i in seq_along(input$editable_clusttab_cell_edit$row)) {
      row <- input$editable_clusttab_cell_edit$row[i]
      col <- input$editable_clusttab_cell_edit$col[i]+1
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
  
  # Create a reactiveVal that counts how many times the calculate final sample sizes button has been clicked
  # - this is useful if the user clicks the button without having selected n_clust, because error message will pop-up and the counter can be reset to 0
  design_rv <- reactiveValues(calc_sizes_click = NULL)
  
  # When 'calculate sample sizes' button is clicked:
  # update the data frame with the user-entered values, calculate the adjusted sample size, and create a final df that is reactive
  df_sizes_final <- eventReactive(input$calc_sizes, {
    
    # require n clusters to be defined and the button click to be > 0 (if 0 then the user has clicked without select n_clust)
    req(input$design_nclust, design_rv$calc_sizes_click > 0)
    
    # get the stored (and edited) data frame with sample sizes
    df <- design_rv$df_sizes_update
    
    # check that sample size values are numeric and that no value is NA (and if so show pop-up error message)
    if(is.numeric(df$target_sample_size) && !any(is.na(df$target_sample_size))){
      
      # calculate adjusted sample size
      df <- df %>% mutate(adj_sample_size = ceiling(target_sample_size/(1-(percent_dropout/100))))
    
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
       
       # Reset the button click count to 0 so that a new click (with no error - ie the user has chosen n_clust) will trigger the results box to render
       design_rv$calc_sizes_click <- input$calc_sizes
       design_rv$calc_sizes_click <- 0
       print(design_rv$calc_sizes_click)
     }
     else{
       # return(NULL)
       # Save the current number of clicks to a reactiveVal, we can use clicks>0 to ensure that the results box only renders when the user clicks again
       design_rv$calc_sizes_click <- input$calc_sizes
       print(design_rv$calc_sizes_click)
     }
   })     
     
   # The results box, text and plots are displayed once the calculate final sample sizes button is clicked 
   output$final_sizes_results <- renderUI({
     
     # require n clusters to be defined, calculate sizes button to be clicked and df_sizes_final() to be created
     req(input$design_nclust, input$calc_sizes, df_sizes_final())
     
     box(width = 5, 
         background = "purple",
         title = "Adjusted sample sizes",
         p("Based on the values you entered for sample size (n) and taking into account the proportion drop-out (d), the adjusted sample size is calculated using the formula n_adj = n/(1-d). This still refers to confirmed malaria positive cases. Scroll the table to view."),
         br(),
         DTOutput("final_sizes_table")
     )
   })
  
  # render the edited table
  output$final_sizes_table <- renderDT({
    datatable(df_sizes_final(), 
              colnames = c("Cluster", "Target sample size", "% drop-out", "Adjusted sample size"),
              extensions = c("FixedHeader"),
              # extensions = c("FixedHeader", "FixedColumns"),
              rownames = F,
              options = list(dom = 'rt',
                             # width=4,
                             pageLength=20,
                             fixedHeader = T,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")),
                             # fixedColumns = list(leftColumns = c(1)),
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
      
    DRpower::get_power_threshold(N = df_sizes_final()$target_sample_size, 
                                 prevalence = as.numeric(input$param_prev)/100, # make sure to convert to proportion for appropriate calculation
                                 ICC = as.numeric(input$param_icc),
                                 reps = as.numeric(input$param_n_sims))
  })
  
  # If user clicks 'estimate power' button before entering sample sizes, an error message will pop-up
  observeEvent(input$est_pow, {
      print("Estimate power button has been clicked")
      
      # error message pops up if the user has not entered the sample sizes (check that calculate sizes button has been clicked)
      if(input$calc_sizes || is.null(df_sizes_final())){
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
    
  
  # The results box, text and plots are displayed once the estimate power button is clicked 
  output$est_power_results <- renderUI({
    
    # require estimate power button click
    req(input$est_pow, power_output())
    
    box(width = 5, 
        background = "purple",
        title = "Estimated power",
        p("The plot shows the mean and lower and upper 95% confidence interval based on cluster sizes and parameters chosen above."),
        br(),
        renderTable(power_output() %>%
                      rename("Power" = power, "Lower 95%CI" = lower, "Upper 95%CI" = upper),
                    digits = 1,
                    colnames = T,
                    align = "c"),
        br(),
        plotOutput("est_power_plot")
    )
  })

  output$est_power_plot <- renderPlot({
    
    # require power_output() to exist
    req(power_output())
    
    ggplot(power_output()) +
      geom_segment(aes(x = " ", xend = " ",y = lower, yend = upper), color = "black", linewidth = 1) +
      geom_point(aes(x = " ", y = power),
                 size = 4,
                 shape = 21,
                 fill = "skyblue3") +
      geom_hline(yintercept = 80, color = "darkgrey", linetype = "dashed") +
      geom_text(aes(x= " ", y = 82.5, label = "80% threshold"), color = "darkgrey") +
      scale_y_continuous(labels = scales::percent_format(1, scale = 1), limits = c(0, 100)) +
      labs(x = "",
           y = "Estimated power") +
      theme_light() +
      theme(text = element_text(size = 16))
  })
  
  # ----------------------------------
  #  Save results and render downloadable design report
  # ----------------------------------
  
  # Store a reactive value that checks whether the summary data is complete or not (T/F)
  design_rv <- reactiveValues(design_data_ready = FALSE)
  
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
          solidHeader = T,
          # background = "purple",
          collapsible = TRUE,
          title = "Data summary",
          h4("Final cluster sizes:"),
          renderTable(df_sizes_final(), digits = 0),
          br(), br(),
          h4("Parameters for power calculation:"),
          p("ICC: ", input$param_icc),
          p("Prevalence: ", ceiling(as.numeric(input$param_prev)), "%"), 
          p("Number of simulations: ", input$param_n_sims),
          br(), br(),
          h4("Power estimates:"),
          renderTable(power_output())
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
    
      filename = paste0("PfHRP2_Planner_Design_Report_", Sys.Date(), ".pdf"),
      # filename = paste0("PfHRP2_Planner_Design_Report_", Sys.Date(), ".html"),
      content = function(file) {
        # create a progress notification pop-up telling the user that the report is rendering
        id <- showNotification(paste0("Preparing report..."), 
                               duration = 10, 
                               closeButton = FALSE)
        
        # remove notification when calculation finishes
        on.exit(removeNotification(id), add = TRUE)
        
        tempReport <- file.path(tempdir(), "template_design_report_pdf.Rmd")
        file.copy("template_design_report_pdf.Rmd", tempReport, overwrite = TRUE)
        
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
    
    print("number of analysis clusters selected and initial df created")
    
    # create the data frame with fixed columns and rows based on user input
    data.frame(
      cluster = c(rep(1:input$analysis_nclust)),
      n_deletions = c(rep(NA, input$analysis_nclust)),
      sample_size = c(rep(NA, input$analysis_nclust))
    )
  })  
  
  # When the user selects the number of clusters, we store the initial values in df_sizes_update() so we can keep track of any user edits to the table
  observeEvent(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
    print("Number of final clusters selected - saving initial df as reactiveVal")
    
    analysis_rv$df_analysis_update <- df_deletions()
  }) 
  
  # Render editable table
  output$editable_deltab <- renderDT({
    datatable(df_deletions(), 
              editable = list(
                target = 'cell',
                numeric = c(2,3),
                disable = list(
                  columns = c(0)
                )
              ),
              rownames = FALSE,
              colnames = c("Number of clusters", "Number of deletions", "Sample size"), 
              extensions = c("FixedHeader"),
              # extensions = c("FixedHeader", "FixedColumns"),
              caption = "Double-click to edit each cell in the table below and enter your study values.",
              options = list(dom = 'rt',
                             # autoWidth = TRUE, 
                             pageLength = 20,
                             # columnDefs = list(list(className = "dt-center",
                             #                        targets = "_all")),
                             # fixedColumns = list(leftColumns = c(1)),
                             scrollX = '400px')) 
  })
  
  # observe when table is edited and update the data frame with the user entered values
  observeEvent(input$editable_deltab_cell_edit, {
    print("Editable analysis table has been edited")
    
    # get the latest updated data frame
    df <- analysis_rv$df_analysis_update
    
    # iterate over each cell edit event
    for (i in seq_along(input$editable_deltab_cell_edit$row)) {
      row <- input$editable_deltab_cell_edit$row[i]
      col <- input$editable_deltab_cell_edit$col[i]+1
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
      print(df)

      DRpower::get_prevalence(n = df$n_deletions,
                              N = df$sample_size,
                              prev_thresh = as.numeric(input$analysis_prevthresh)/100) # make sure we convert input prev_thresh to proportion for calculation
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
  
  # The results box, text and plots are displayed once the estimate prevalence button is clicked 
  output$est_prev_results <- renderUI({
    req(input$est_prev, prev_output())

    print("est_prev results should print")
    
    box(width = 12,
        background = "purple",
        title = "Prevalence estimates",
        p("The table and the plot below show the maximum a posteriori (MAP) estimate of the prevalence, along with a 95% credible interval (CrI). The MAP estimate can be used as a central estimate of the prevalence, but it should always be reported alongside the CrI to give a measure of uncertainty. "),
        br(),
        renderTable(prev_output() %>% mutate(prob_above_threshold = prob_above_threshold*100) %>% 
                      rename("MAP prevalence estimate (%)" = MAP, "Lower CrI (%)" = CrI_lower, "Upper CrI (%)" = CrI_upper, "Probability above threshold (%)" = prob_above_threshold), 
                    digits = 1,
                    colnames = T,
                    align = "c"),
        br(),
        h4(htmlOutput("est_prev_resulttext")),
        br(),
        plotOutput("est_prev_plot")
    )
  })

  output$est_prev_resulttext <- renderUI({
    # require estimate prevalence button click
    req(prev_output())

    # check if prev_output() has been created, which means the results have been calculated and can be displayed
    if(!is.null(prev_output()) && prev_output()$prob_above_threshold >= 0.95){
      line1 <- paste("RESULT: We estimate that the prevalence of", em("pfhrp2/3"), "deletions is ", ceiling(as.numeric(prev_output()$MAP)), "% (95% CrI: ", ceiling(as.numeric(prev_output()$CrI_lower)), "- ", ceiling(as.numeric(prev_output()$CrI_upper)), "%).")
      line2 <- paste("We reject the hypothesis that the ", em("pfhrp2/3"), "deletion prevalence is below the ", ceiling(as.numeric(input$analysis_prevthresh)), "% threshold. We conclude that prevalence is above the threshold.")

      HTML(paste(line1, line2, sep = "<br/><br/>"))

    }

    else if(!is.null(prev_output()) && prev_output()$prob_above_threshold < 0.95){
      line1 <- paste("RESULT: We estimate that the prevalence of", em("pfhrp2/3"), "deletions is ", ceiling(as.numeric(prev_output()$MAP)), "% (95% CrI: ", ceiling(as.numeric(prev_output()$CrI_lower)), "- ", ceiling(as.numeric(prev_output()$CrI_upper)), "%).")
      line2 <- paste("We accept the hypothesis that the ", em("pfhrp2/3"), "deletion prevalence is below the ", ceiling(as.numeric(input$analysis_prevthresh)), "% threshold. We conclude that prevalence is below the threshold.")

      HTML(paste(line1, line2, sep = "<br/><br/>"))
    }

    else {
      return(NULL)
    }
  })
  
  output$est_prev_plot <- renderPlot({
      # require prev_output() to exist
      req(prev_output())
      
      ggplot(prev_output()) +
        geom_segment(aes(x = " ", xend = " ", y = CrI_lower, yend = CrI_upper),
                     color = "black", linewidth = 1) +
        geom_point(aes(x = " ", y = MAP),
                   size = 3,
                   shape = 21,
                   fill = "skyblue3") +
        # use the user-entered prev_thresh to plot threshold line
        geom_hline(aes(yintercept = as.numeric(input$analysis_prevthresh)), # make sure we convert back to proportion here
                   color = "darkgrey",
                   linetype = "dashed") +
        # use the user-entered prev_thresh to plot threshold line
        geom_text(aes(x= " ", 
                      y = (as.numeric(input$analysis_prevthresh))+2, 
                      label = paste0(ceiling(as.numeric(input$analysis_prevthresh)),"% threshold")), 
                      color = "darkgrey") +
        scale_y_continuous(labels = scales::percent_format(1, scale = 1), limits = c(0,100)) +
        labs(x = "",
             y = "Estimated prevalence") +
        theme_light() +
        theme(text = element_text(size = 16))
    })
  
  # ----------------------------------
  #  Results table/plot: estimated ICC
  # ----------------------------------
  
    # When 'Estimate ICC' button is clicked:
    # Calculate ICC using DRpower::get_ICC() with the user-entered deletions and sample sizes
    icc_output <- eventReactive(input$est_icc, {
      
      # require the updated data frame to have been created to make sure there is a data frame to get values from
      req(analysis_rv$df_analysis_update, prev_output())
      
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
    
  # If user clicks 'estimate ICC' button before selecting clusters and entering sample sizes in step 1, an error message will pop-up
  observeEvent(input$est_icc, {
    print("Estimate ICC button clicked")

    # To make sure the error message pops up as expected, don't show it if est_prev button has been clicked AND power_output() has been created, otherwise show error message 
    if(input$est_prev && !is.null(prev_output())){
      # debugging, remove later
      print("After user clicks the estimate ICC button, this is the edited df and prev_output() (no pop-up error msg needed): ")
      print(analysis_rv$df_analysis_update)
      print(prev_output())
      return(NULL)
      
    }
    else{
      # TODO debugging
      print("ICC error should have popped up")
      
      show_alert(
        title = "Error!",
        text = "You have not selected the number of clusters or entered the values for your study. Please go back to the previous section ('Estimate prevalence') and select the number of clusters from the drop-down menu and enter the values in the table.",
        type = "error"
      )
    }
    
  })
  
  # The results box, text and plots are displayed once the estimate icc button is clicked 
  output$est_icc_results <- renderUI({
    req(input$est_icc, icc_output())
    
    print("est_icc results should print")
    
    box(width = 12, 
        background = "purple",
        title = "ICC estimates",
        p("The table and the plot below show the maximum a posteriori (MAP) estimate of the ICC, along with a 95% credible interval (CrI). For context, an ICC of 0.05 is used by default in the Design tab based on an ", a("analysis of historical studies.", href = "https://mrc-ide.github.io/DRpower/articles/historical_analysis.html")),
        br(),
        renderTable(icc_output() %>% 
                      rename("MAP estimate of ICC" = MAP, "Lower CrI" = CrI_lower, "Upper CrI" = CrI_upper), 
                    digits = 1,
                    colnames = T, 
                    align = "c"),
        br(),
        plotOutput("est_icc_plot")
    )
  })
  
  # NOTE need to divide by 100 to convert to proportion
  output$est_icc_plot <- renderPlot({
    ggplot(icc_output()) +
      geom_segment(aes(x = " ", xend = " ", y = CrI_lower/100, yend = CrI_upper/100), 
                   color = "black", linewidth = 1) +
      geom_point(aes(x = " ", y = MAP/100), 
                 size = 4, 
                 shape = 21,
                 fill = "skyblue3") +
      scale_y_continuous(limits = c(0,1)) +
      labs(x = "",
           y = "Estimated ICC") +
      theme_light() +
      theme(text = element_text(size = 16)) 
  })
  
  # ----------------------------------
  #  Save results and render downloadable analysis report  
  # ----------------------------------

  # Store a reactive value that checks whether the summary data is complete or not (T/F)
  analysis_rv <- reactiveValues(analysis_data_ready = FALSE)
  
  # The save button allows the user to cross-check the assumed parameters entered and check the numbers that will be printed in the report
  # - if the user has not entered the values correctly in the previous tabs, an error message will pop-up and the analysis_data_ready reactive val will be set to FALSE
  # - if it passes all validation checks (ie user has entered everything), analysis_data_ready will be set to TRUE 
  observeEvent(input$save_analysis_data, {
    print("Save analysis data button has been clicked")
    
    # If all conditions are not met - ie the user has gone through the entire Estimate Prevalence and ICC tabs, set analysis_data_ready as FALSE
    if (input$analysis_prevthresh=="" || input$analysis_nclust=="" || input$est_prev==0 || input$est_icc==0 || is.null(prev_output()) || is.null(icc_output())) {
      print("error should pop up when save results is clicked")
      show_alert(
        title = "Error!",
        text = "The summary cannot be displayed because you haven't completed Steps 1 and/or 2. Please go back to 'Estimate prevalence' and 'Estimate ICC' and follow all the steps.",
        type = "error"
      )
      
      analysis_rv$analysis_data_ready <- FALSE
      print("analysis data is not ready")
      print(analysis_rv$analysis_data_ready)
    }
    
    # If all conditions have been met, set analysis_data_ready to TRUE
    else {
      analysis_rv$analysis_data_ready <- TRUE
      print("analysis data is ready for download")
      print(analysis_rv$analysis_data_ready)
    }
    
  })
  
  # Display a summary of the assumed parameters and data once the save button is clicked 
  output$text_analysis_summary <- renderUI({
    
    req(input$save_analysis_data)
    
    if (analysis_rv$analysis_data_ready==TRUE) {
      print("text analysis summary should print")
      
      box(width = 12, 
          # background = "purple", 
          collapsible = TRUE,
          title = "Data summary",
          h4("Final study values:"),
          renderTable(analysis_rv$df_analysis_update, digits = 0),
          br(), br(),
          h4("Parameters for calculations:"),
          p("Prevalence threshold: ", ceiling(as.numeric(input$analysis_prevthresh)), "%"), 
          br(), br(),
          h4("Prevalence estimates:"),
          renderTable(prev_output()),
          br(), br(),
          h4("ICC estimates:"),
          renderTable(icc_output())
      )
    }
  })
  
  # Render the download button only if the user has clicked on the save button and the data is ready to be downloaded (ie analysis_data_ready==TRUE)
  output$analysis_download <- renderUI({
    req(input$save_analysis_data)
    
    if(analysis_rv$analysis_data_ready==TRUE){
      print("download button shown because everything has been entered")
      
      box(width = 12,
          title = "Click below to download your analysis report.",
          em("This creates a pdf summary of the assumed parameters and your results, with standardised text to minimise mistakes."),
          br(), br(),
          downloadButton("analysis_report", "Download analysis report", icon("download")))
    }
    
  })
  
  # The downloadHandler() for the design report will be triggered if the downloadButton() is clicked 
  output$analysis_report <- downloadHandler(
    filename = paste0("PfHRP2_Planner_Analysis_Report_", Sys.Date(), ".pdf"),
    content = function(file) {
      # create a progress notification pop-up telling the user that the report is rendering
      id <- showNotification(paste0("Preparing report..."), 
                             duration = 10, 
                             closeButton = FALSE)
      
      # remove notification when calculation finishes
      on.exit(removeNotification(id), add = TRUE)
      
      tempReport <- file.path(tempdir(), "template_analysis_report_pdf.Rmd")
      file.copy("template_analysis_report_pdf.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(analysis_prevthresh = input$analysis_prevthresh,
                     analysis_nclusters = input$analysis_nclust,
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
