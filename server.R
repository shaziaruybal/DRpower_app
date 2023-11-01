######### DRpower App: Server ####################################################################
# Authors: Shazia Ruybal-Pesántez (sruybal@imperial.ac.uk)
##################################################################################################

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinyBS)
library(DRpower)
library(kableExtra)
library(shinyvalidate)

set.seed(10)

df_ss <- DRpower::df_ss

function(input, output, session) {
  
  ##################################################
  # TESTING
  ################################################## 
  
  # output$landing_page <- renderUI({
  #   includeHTML("landing_page.html")
  # })

  ##################################################
  # EXPLORE
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
  
  ##################################################
  # NUMERIC INPUT VALIDATIONS
  ##################################################
  
  # 1. Create an InputValidator object
  iv <- InputValidator$new()
  
  # 2. Add validation rules
  iv$add_rule("param_prev", sv_between(6, 100)) # validate within range 
  iv$add_rule("param_prev", sv_integer()) # validate integer
  iv$add_rule("param_icc", sv_between(0, 1)) # validate within range 
  iv$add_rule("param_n_sims", sv_between(100, 10000)) # validate within range 
  iv$add_rule("param_n_sims", sv_integer()) # validate integer

  # 3. Start displaying errors in the UI
  iv$enable()

  ##################################################
  # DESIGN
  ##################################################

  # ----------------------------------
  #  Dynamic UI that controls how user enters sample sizes
  # ----------------------------------
  
  # render the dynamic UI based on the user choice, if manual enter then the editable table is displayed, if not the user-uploaded .csv table is displayed
  output$enter_sizes_dynamicUI <- renderUI({
    
    if(input$design_table_choice=="manual"){
      fluidPage(
        selectInput(
          inputId = "design_nclust",
          label = strong("Select number of clusters: "),
          width = "40%",
          choices = c("", seq(2, 20)),
        ),
        htmlOutput("text_edit_clusttab"),
        DTOutput("editable_clusttab"),
        br(),
        bsAlert("error_noclusters"), # this creates an error message if user clicks calculate without choosing number of clusters
        actionButton(inputId = "add_row_design",
                     label = "Add row",
                     icon("circle-plus")),
        actionButton(inputId = "delete_row_design",
                     label = "Delete row",
                     icon("circle-minus")),
        bsTooltip(id = "delete_row_design",
                  title = "Select the row you want to delete by clicking it once, this should highlight the row in blue. Then click button.",
                  placement = "right"),
        br(), br(),
        actionButton(
          inputId = "calc_sizes",
          label = "Calculate adjusted sample sizes",
          icon = icon("clipboard-check")),
        helpText(em("If you update these values, make sure you remember to recalculate your adjusted sample sizes and estimate power below"))
      )
    }
    
    
    else if(input$design_table_choice=="upload"){
      fluidPage(
        p("Please use the ", a(href="design_template.csv", "template provided", download=NA, target="_blank"), "and ensure your file matches exactly."),
        fileInput(inputId = "uploaded_design_table",
                  label = "Upload your sample size table (.csv):",
                  multiple = FALSE,
                  accept = ".csv"),
        br(),
        textOutput("design_upload_status"),
        # strong("Check your uploaded file below. If everything looks OK, click 'Calculate adjusted sample sizes' button."),
        renderDT(design_rv$df_sizes_uploaded,
                 rownames = FALSE,
                 colnames = c("Cluster", "Target sample size", "% drop-out"),
                 selection = "none",
                 options = list(dom = 'rt',
                                pageLength=20,
                                columnDefs = list(list(className = "dt-center",
                                                       targets = "_all")),
                                scrollX = '400px')
        ),
        br(),
        actionButton(
          inputId = "calc_sizes",
          label = "Calculate adjusted sample sizes",
          icon = icon("clipboard-check"))
      )
    }
  })
  
  # ----------------------------------
  #  Set up reactiveVals for design tab
  # ----------------------------------
  
  design_rv <- reactiveValues(
                              # this is the data frame that will be created when the user selects n clusters, and if they update any values in the table and/or add/delete rows etc
                              df_sizes_update = NULL,
                              # this is the reactiveVal where the uploaded data frame will be stored
                              df_sizes_uploaded = NULL,
                              # Create a reactiveVal that counts how many times the calculate final sample sizes button has been clicked
                              # - this is useful if the user clicks the button without having selected n_clust, because error message will pop-up and the counter can be reset to 0
                              # calc_sizes_click = NULL,
                              # Store a reactive value that checks whether the summary data is complete or not (T/F)
                              design_data_ready = FALSE
                              )
  
  # ----------------------------------
  #  User-uploaded table: sample size and proportion drop-out
  # ----------------------------------
  
  # If user uploads their own design sample size table, we save the dataframe as the reactiveVal "df_sizes_uploaded"
  observeEvent(input$uploaded_design_table, {
    # require the user to have selected upload option
    req(input$design_table_choice=="upload")
    
    print("dataset uploaded")
    
    # Validation check
    tryCatch(
      {
        df <- read.csv(input$uploaded_design_table$datapath)
        
        if(!any(is.na(df$cluster)) && is.numeric(df$percent_dropout) && !any(is.na(df$percent_dropout)) && is.numeric(df$target_sample_size) && !any(is.na(df$target_sample_size))){
        
          print("uploaded data looks OK")
          
          design_rv$df_sizes_uploaded <- df
        }
        
        else{
          show_alert(
            title = "Error!",
            text = "The dataset you uploaded is not in the correct format. Please use the template provided and only edit the appropriate cells.",
            type = "error"
          )
          
          print("uploaded data does not look OK")
          design_rv$df_sizes_uploaded <- NULL
        }
      },
      # in theory this shouldn't be needed because the fileInput requires only .csv files (it only allows you to select .csv from your local files)
      error = function(err){
        show_alert(
          title = "Error!",
          text = "Invalid file type. Please upload a .csv file.",
          type = "error"
        )
      }
    )
    
  })

  output$design_upload_status <- renderText({
    if (is.null(design_rv$df_sizes_uploaded)) {
      "Please upload a correctly-formatted CSV file."
    } else {
      paste("File uploaded:", input$uploaded_design_table$name)
    }
  })
  
  # ----------------------------------
  #  User-input table: sample size and proportion drop-out
  # ----------------------------------
  
  # observe when the user specifies n clusters
  observeEvent(input$design_nclust, ignoreNULL=T, ignoreInit=T, {
    # require the user to have selected manual enter
    req(input$design_table_choice=="manual")
    
    # Only perform the following if the user has selected n clusters (otherwise the blank option is default upon initialization, and will likely remain if users choose to upload)
    if(input$design_nclust!=""){
      
    print("Number of clusters selected")
    output$text_edit_clusttab <- renderUI(HTML(paste("Please edit the target sample size and expected proportion of participant drop-out for each cluster by ", strong("double-clicking"), " and editing each cell in the table below. You can also edit the cluster number to your own cluster or site names if you wish. When you are finished click the 'Calculate adjusted sample sizes' button. ")))
    
    # getting target sizes to pre-populate the table from fixed defaults of ICC=0.05 and prev_thresh=0.05 
    df_targets <- df_ss %>% 
      filter(ICC == 0.05) %>% 
      filter(prev_thresh == 0.05) %>% 
      filter(prior_ICC_shape2==9) %>% # TODO fixed at 9 (check this when final final table is ready)
      select(n_clust, prevalence, N_opt) %>%
      pivot_wider(names_from = prevalence, values_from = N_opt) 
    
    # get the target sample sizes from table with fixed prev of 10%, fix it at 500 if nclust is 2, 3 or 4 (because NA)
    if(input$design_nclust==2 | input$design_nclust==3 | input$design_nclust==4){
      target_size <- 500
    }
    else{
      target_size <- df_targets %>% filter(n_clust == input$design_nclust) %>% select(`0.1`) %>% as.integer()
    }
    
    # create the starting data frame with fixed columns and rows based on user input and target sample sizes as defaults
    df_sizes <- data.frame(
                cluster = rep(1:input$design_nclust),
                target_sample_size = rep(target_size, input$design_nclust),
                percent_dropout = rep(10, input$design_nclust)
                )
    
    print("After user selects N clusters, this is the df:")
    print(df_sizes)
    
    # when df_sizes is created, store the initial values in df_sizes_update()
    design_rv$df_sizes_update <- df_sizes
    }
    
    else{
      print("input$design_clust==''")
      # be explicit here, but this should happen anyways
      design_rv$df_sizes_update <- NULL
    }
  })
  
  # render editable table
  output$editable_clusttab <- renderDT({
    if(input$design_table_choice=="manual"){
    datatable(design_rv$df_sizes_update, 
              editable = list(
                target = 'cell',
                numeric = c(2,3) #,
                # disable = list(
                #   columns = c(0)
                # )
              ),
              rownames = FALSE, 
              colnames = c("Cluster", "Target sample size", "% drop-out"),
              # selection = "none", # uncomment if you want to disable row selection when clicking (it was annoying before but now we need for delete row)
              # extensions = c("FixedHeader"),
              # extensions = c("FixedHeader", "FixedColumns"),
              options = list(dom = 'rt',
                             # autoWidth = TRUE,
                             pageLength=20,
                             # fixedHeader = T,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")),
                             # fixedColumns = list(leftColumns = c(1)),
                             scrollX = '400px'))
    }
  })

  # observe when table is edited and update the data frame with the user entered values
  observeEvent(input$editable_clusttab_cell_edit, {
    # Require user to have selected 'manual'
    req(input$design_table_choice=="manual")
    
    print("Editable design table has been edited")
    
    # get the latest updated data frame
    df <- design_rv$df_sizes_update

    # iterate over each cell edit event, make sure the values are numeric
    for (i in seq_along(input$editable_clusttab_cell_edit$row)) {
      
      print("original col index:")
      print(input$editable_deltab_cell_edit$col[i])
      
      row <- input$editable_clusttab_cell_edit$row[i]
      col <- input$editable_clusttab_cell_edit$col[i]+1
      
      print("col index + 1:")
      print(col)
      
      value <- input$editable_clusttab_cell_edit$value[i]

      # make sure edited value for sample_size (col index 2) and dropout (col index 3) is numeric
      if (col==2 || col==3){
        print("Value:")
        print(value)
        print(str(value))
        
        value <- as.numeric(value)
      }
      
      else {
        print("Value:")
        print(value)
        print(str(value))
        
        value <- value
      }
      
      # update the corresponding cell in the new data frame
      df[row, col] <- value
    }

    # assign the updated data frame to df_sizes_update
    design_rv$df_sizes_update <- df
    
  })
  
  # Observe if add row button has been clicked, and if so add a row to the edited table (see: https://stackoverflow.com/questions/52427281/add-and-delete-rows-of-dt-datatable-in-r-shiny)
  observeEvent(input$add_row_design, {
    # Require user to have selected 'manual'
    req(input$design_table_choice=="manual")
    
    print("add row button clicked")
    
    # get the latest updated data frame
    df <- design_rv$df_sizes_update
    row_num <- nrow(df)
    
    new_df <- df %>% add_row(cluster = row_num+1, 
                             target_sample_size = NA,
                             percent_dropout = NA)
    
    print(new_df)
    
    # assign the updated data frame to df_sizes_update
    design_rv$df_sizes_update <- new_df
  })
  
  # Observe if delete row button has been clicked, and if so add a row to the edited table
  observeEvent(input$delete_row_design, {
    # Require user to have selected 'manual'
    req(input$design_table_choice=="manual")
    
    print("delete row button clicked")
    
    # get the latest updated data frame
    df <- design_rv$df_sizes_update
    
    # check if rows are selected
    if(!is.null(input$editable_clusttab_rows_selected)){
      # if they are, delete them from the data frame
      df <- df[-as.numeric(input$editable_clusttab_rows_selected),]
    }
    
    print(df)
    
    # assign the updated data frame to df_analysis_update
    design_rv$df_sizes_update <- df
  })
  
  
  # ----------------------------------
  #  Calculate adjusted sample sizes
  # ----------------------------------
  
  # When 'calculate sample sizes' button is clicked:
  observeEvent(input$calc_sizes, {
    print("Calculate final sample sizes values button clicked")
    
    req(input$design_table_choice)
    
    # If user has selected manual but has not entered data, error message should pop-up
    if(input$design_table_choice=="manual" && is.null(design_rv$df_sizes_update)){
        show_alert(
          title = "Error!",
          text = "Make sure you have filled in the table. You should only enter integers in your table for sample size and drop-out and make sure you have filled in all the cells. Please go back and enter the values again.",
          type = "error"
        )
    }
    
    # If user has selected 'upload' option but hasn't uploaded a file (or the data is not correct), error message should pop-up
    else if(input$design_table_choice=="upload" && is.null(design_rv$df_sizes_uploaded)){
       show_alert(
        title = "Error!",
        text = "Make sure you have uploaded the correct file type (.csv) and in the correct format (see template for an example). You should only enter integers for sample size and drop-out.",
        type = "error"
       )
    }
    else {
      print("no error needed")
      # return(NULL)
    }
      
  })
  
  # When 'calculate sample sizes' button is clicked:
  # update the data frame with the user-entered values or the uploaded data, check dfs are inputted correctly, calculate the adjusted sample size, and create a final df that is reactive
  df_sizes_final <- eventReactive(input$calc_sizes, {

    # If the user has selected "manual entry" and the design_rv$df_sizes_update data frame exists, get the stored (and edited) data frame with sample sizes
    if(input$design_table_choice=="manual" && !is.null(design_rv$df_sizes_update)){
      df <- design_rv$df_sizes_update
      print("df_sizes_final is based on the manual entry table")
    }
    # If the user has selected "upload" and the design_rv$df_sizes_uploaded data frame exists, get theuploaded data frame with sample sizes
    else if(input$design_table_choice=="upload" && !is.null(design_rv$df_sizes_uploaded)){
      df <- design_rv$df_sizes_uploaded
      print("df_sizes_final is based on the uploaded table")
    }
    else{
      print("data not correct so return NULL")
      return(NULL)
    }
    
    # double check that sample size values are numeric and that no value is NA (and if so show pop-up error message)
    if(!any(is.na(df$cluster)) && is.numeric(df$percent_dropout) && !any(is.na(df$percent_dropout)) && is.numeric(df$target_sample_size) && !any(is.na(df$target_sample_size))){

      # calculate adjusted sample size
      df <- df %>% mutate(adj_sample_size = ceiling(target_sample_size/(1-(percent_dropout/100))))

      return(df)
    }
    else{
      cat("The df that gives errors is:", df)
      show_alert(
        title = "Error!",
        text = "Make sure you have only entered integers in your table and/or make sure you have filled in all the cells. Please go back and enter the values again or upload your file again if you selected to upload your own.",
        type = "error"
      )
    }
  }) 
     
   # The results box, text and plots are displayed once the calculate final sample sizes button is clicked 
   output$final_sizes_results <- renderUI({
     
     # require n clusters to be defined, calculate sizes button to be clicked and df_sizes_final() to be created
     req(input$calc_sizes, df_sizes_final())
     
     box(width = 5, 
         collapsible = T,
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
              # extensions = c("FixedHeader"),
              # extensions = c("FixedHeader", "FixedColumns"),
              rownames = F,
              options = list(dom = 'rt',
                             # width=4,
                             pageLength=20,
                             # fixedHeader = T,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")),
                             # fixedColumns = list(leftColumns = c(1)),
                             # Custom JS code to edit the header background and text color, see: https://stackoverflow.com/questions/63119369/background-color-in-datatable-rowname-header-top-left-area
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#f9f9f9', 'color': '#55529e'});",
                               "}"),
                             scrollX = '400px'
                             )
              ) %>% DT::formatStyle(columns = names(df_sizes_final()), backgroundColor = "#f9f9f9") 
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
    
    # this tryCatch will make sure an error message pops up if there is an error in the power calculation (eg the user enters negative values)  
    tryCatch({
      DRpower::get_power_threshold(N = df_sizes_final()$target_sample_size, 
                                   prevalence = as.numeric(input$param_prev)/100, # make sure to convert to proportion for appropriate calculation
                                   ICC = as.numeric(input$param_icc),
                                   reps = as.numeric(input$param_n_sims))
    }, error = function(err){
      show_alert(
        title = "Error!",
        text = "Power cannot be estimated because there is an error in the values you entered. Please make sure you have entered only positive integers.",
        type = "error"
      )
    })
  })
  
  # If user clicks 'estimate power' button before entering sample sizes, an error message will pop-up
  observeEvent(input$est_pow, {
      print("Estimate power button has been clicked")
    
    # To make sure the error message pops up as expected, don't show it if calc_sizes button has been clicked AND df_sizes_final() has been created, otherwise show error message 
    if(input$calc_sizes && !is.null(df_sizes_final())){
      print("button has been clicked and df_sizes_final() has been calculated (so no pop-up error needed):")
      print(df_sizes_final())
      return(NULL)
    }
    else{
      # TODO debugging
      print("calculate sizes is NULL")
      print("error should have popped up")
      show_alert(
        title = "Error!",
        text = "You have not entered the sample sizes correctly. Please go back to Step 1 and choose the number of clusters and enter the values in the table, and then click the 'Calculate final sample sizes' button.",
        type = "error"
      )
    }
    })
    
  # The results box, text and plots are displayed once the estimate power button is clicked 
  output$est_power_results <- renderUI({
    
    # require estimate power button click
    req(input$est_pow, power_output())
    
    box(width = 5, 
        collapsible = T,
        background = "purple",
        title = "Estimated power",
        p("The plot shows the mean and lower and upper 95% confidence interval based on cluster sizes and parameters chosen above."),
        br(),
        renderTable(power_output() %>%
                      rename("Power" = power, "Lower 95%CI" = lower, "Upper 95%CI" = upper),
                    digits = 2,
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
                 fill = "mediumpurple") +
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
  # ---------------------------------
  
  # The save button allows the user to cross-check the assumed parameters entered and check the numbers that will be printed in the report
  # - if the user has not entered the values correctly in the previous tab, an error message will pop-up and the design_data_ready reactive val will be set to FALSE
  # - if it passes all validation checks (ie user has entered everything), design_data_ready will be set to TRUE 
  observeEvent(input$save_design_data, {
    print("Save design data button has been clicked")

    # If all conditions are not met - ie the user has gone through the entire Step 2 Final cluster sizes tab, set design_data_ready as FALSE
    if (input$est_pow==0 || is.null(df_sizes_final()) || is.null(power_output())) {
      print("error should pop up when save results is clicked")
      show_alert(
        title = "Error!",
        text = "The summary cannot be displayed because you haven't completed the previous steps. Please go back to 'Final cluster sizes' and follow all the steps.",
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
          renderTable(power_output(), digits = 2)
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
          em("This creates an html summary of the assumed parameters and your results with standardised text to minimise mistakes. Note that you can convert this to a pdf if preferred by going to file/print."),
          br(), br(),
          downloadButton("design_report", "Download design report", icon("download")))
    }

  })
  
  # The downloadHandler() for the design report will be triggered if the downloadButton() is clicked 
  output$design_report <- downloadHandler(
    
      # filename = paste0("PfHRP2_Planner_Design_Report_", Sys.Date(), ".pdf"),
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
  #  Dynamic UI that controls how user enters deletions and final sample sizes
  # ----------------------------------
  
  # render the dynamic UI based on the user choice, if manual enter then the editable table is displayed, if not the user-uploaded .csv table is displayed
  output$enter_deletions_dynamicUI <- renderUI({
    
    if(input$analysis_table_choice=="manual"){
      fluidPage(
        selectInput(
          inputId = "analysis_nclust",
          label = strong("Select final number of clusters: "),
          width = "40%",
          choices = c("", seq(2, 20)),
        ),
        DTOutput("editable_deltab"),
        br(),
        bsAlert("error_nodeletions"), # this creates an error message if user clicks estimate prevalence without entering deletions/sample sizes
        actionButton(inputId = "add_row_analysis",
                     label = "Add row",
                     icon("circle-plus")),
        actionButton(inputId = "delete_row_analysis",
                     label = "Delete row",
                     icon("circle-minus")),
        bsTooltip(id = "delete_row_analysis",
                  title = "Select the row you want to delete by clicking it once, this should highlight the row in blue. Then click button.",
                  placement = "right"),
        br(), br(),
        actionButton(inputId = "est_prev",
                     label = "Estimate prevalence",
                     icon("clipboard-check")),
        helpText(em("If you update these values, make sure you remember to recalculate prevalence"))
      )
    }
    
    else if(input$analysis_table_choice=="upload"){
      fluidPage(
        p("Please use the ", a(href="analysis_template.csv", "template provided", download=NA, target="_blank"), "and ensure your file matches exactly."),
        fileInput(inputId = "uploaded_analysis_table",
                  label = "Upload your final study table (.csv):",
                  multiple = FALSE,
                  accept = ".csv"),
        br(),
        strong("Check your uploaded file below. If everything looks OK, click the 'Estimate prevalence' button."),
        renderDT(df_deletions_uploaded(),
                 rownames = FALSE,
                 colnames = c("Cluster", "Number of deletions", "Sample size"),
                 selection = "none",
                 options = list(dom = 'rt',
                                pageLength=20,
                                columnDefs = list(list(className = "dt-center",
                                                       targets = "_all")),
                                scrollX = '400px')
        ),
        br(),
        actionButton(inputId = "est_prev",
                     label = "Estimate prevalence",
                     icon("clipboard-check"))
      )
    }
    
  })
  
  # ----------------------------------
  #  User-uploaded table: number of deletions and final sample sizes
  # ----------------------------------
  
  # If user uploads their own analysis deletions/final sample sizes table, we create a new reactiveVal "df_deletions_uploaded"
  df_deletions_uploaded <- reactive({
    req(input$uploaded_analysis_table)
    
    # Validation check
    tryCatch(
      {
        read.csv(input$uploaded_analysis_table$datapath)
      },
      # in theory this shouldn't be needed because the fileInput requires only .csv files (it only allows you to select .csv from your local files)
      error = function(err){
        show_alert(
          title = "Error!",
          text = "Invalid file. Please upload a .csv file.",
          type = "error"
        )
      }
    )
    
  })
  
  # ----------------------------------
  #  User-input table: number of deletions and final sample sizes
  # ----------------------------------

  # create a reactive value for df_analysis_update
  analysis_rv <- reactiveValues(df_analysis_update = NULL)

  # Make the editable data frame reactive and dependent on the deletion and sample sizes entered by the user
  # df_deletions <- eventReactive(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
  #   
  #   print("number of analysis clusters selected and initial df created")
  #   
  #   # create the data frame with fixed columns and rows based on user input
  #   data.frame(
  #     cluster = c(rep(1:input$analysis_nclust)),
  #     n_deletions = c(rep(NA, input$analysis_nclust)),
  #     sample_size = c(rep(NA, input$analysis_nclust))
  #   )
  # })  
  
  # When the user selects the number of clusters, we store the initial values in df_analysis_update() so we can keep track of any user edits to the table
  observeEvent(input$analysis_nclust, ignoreNULL=T, ignoreInit=T, {
    req(input$analysis_table_choice=="manual", input$analysis_nclust!="")
    
    print("Number of final clusters selected - saving initial df as reactiveVal")
    
    print("number of analysis clusters selected and initial df created")
    
    # create the data frame with fixed columns and rows based on user input
    df_deletions <- data.frame(
      cluster = c(rep(1:input$analysis_nclust)),
      n_deletions = c(rep(NA, input$analysis_nclust)),
      sample_size = c(rep(NA, input$analysis_nclust))
    )
    
    # assign the initial data frame 'df_deletions' to df_analysis_update
    if(input$analysis_table_choice=="manual"){
    analysis_rv$df_analysis_update <- df_deletions
    }
  }) 
  
  # Render editable table
  output$editable_deltab <- renderDT({
    if(input$analysis_table_choice=="manual"){
    datatable(analysis_rv$df_analysis_update, 
              editable = list(
                target = 'cell',
                numeric = c(2,3) #,
                # disable = list(
                #   columns = c(0)
                # )
              ),
              rownames = FALSE,
              colnames = c("Cluster", "Number of deletions", "Sample size"), 
              # selection = "none", # uncomment if you want to disable row selection when clicking (it was annoying before but now we need for delete row)
              # extensions = c("FixedHeader"),
              # extensions = c("FixedHeader", "FixedColumns"),
              caption = htmltools::tags$caption(htmltools::tags$span("Double-click ", style="font-weight:bold; color:black"), htmltools::tags$span("to edit each cell in the table below and enter your study values.")),
              # caption = "Double-click to edit each cell in the table below and enter your study values.",
              options = list(dom = 'rt',
                             # autoWidth = TRUE, 
                             pageLength = 20,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all")),
                             # fixedColumns = list(leftColumns = c(1)),
                             scrollX = '400px')) 
    }
  })
  
  # observe when table is edited and update the data frame with the user entered values
  observeEvent(input$editable_deltab_cell_edit, {
    req(input$analysis_table_choice=="manual")
    
    print("Editable analysis table has been edited")
    
    # get the latest updated data frame
    df <- analysis_rv$df_analysis_update

    # iterate over each cell edit event
    for (i in seq_along(input$editable_deltab_cell_edit$row)) {
      print("original col index:")
      print(input$editable_deltab_cell_edit$col[i])
      
      row <- input$editable_deltab_cell_edit$row[i]
      col <- input$editable_deltab_cell_edit$col[i]+1
      
      print("col index + 1:")
      print(col)
      
      value <- input$editable_deltab_cell_edit$value[i]
      
      # make sure edited value for n_del (col index 2) and sample_size (col index 3) is numeric
      if (col==2 || col==3){
        print("Value:")
        print(value)
        print(str(value))
        
        value <- as.numeric(value)
      }

      else {
        print("Value:")
        print(value)
        print(str(value))
     
        value <- value
      }
      
      # update the corresponding cell in the new data frame
      df[row, col] <- value
    }
    
    # assign the updated data frame to df_analysis_update
    analysis_rv$df_analysis_update <- df
    
  })
  
  # Observe if add row button has been clicked, and if so add a row to the edited table (see: https://stackoverflow.com/questions/52427281/add-and-delete-rows-of-dt-datatable-in-r-shiny)
  observeEvent(input$add_row_analysis, {
    req(input$analysis_table_choice=="manual")
    
    print("add row button clicked")

    # get the latest updated data frame
    df <- analysis_rv$df_analysis_update
    row_num <- nrow(df)

    new_df <- df %>% add_row(cluster = row_num+1, 
                             n_deletions = NA,
                             sample_size = NA)

    print(new_df)
    
    # assign the updated data frame to df_analysis_update
    analysis_rv$df_analysis_update <- new_df
  })
  
  # Observe if delete row button has been clicked, and if so add a row to the edited table
  observeEvent(input$delete_row_analysis, {
    req(input$analysis_table_choice=="manual")
    
    print("delete row button clicked")

    # get the latest updated data frame
    df <- analysis_rv$df_analysis_update

    # check if rows are selected
    if(!is.null(input$editable_deltab_rows_selected)){
      # if they are, delete them from the data frame
      df <- df[-as.numeric(input$editable_deltab_rows_selected),]
    }

    print(df)

    # assign the updated data frame to df_analysis_update
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
    
    # get the stored and edited data frame with sample sizes, or the uploaded data frame
    if(input$analysis_table_choice=="manual"){
      df <- analysis_rv$df_analysis_update
    }
    else if(input$analysis_table_choice=="upload"){
      df <- df_deletions_uploaded()
    }
    
    # check that values are numeric and that no value is NA (and if so show pop-up error message)
    if(is.numeric(df$n_deletions) && !any(is.na(df$n_deletions)) && is.numeric(df$sample_size) && !any(is.na(df$sample_size))){
      print(str(df))
      print(df)

      # this tryCatch will make sure an error message pops up if there is an error in the power calculation (eg the user enters negative values, or number of deletions is larger than sample size)  
      tryCatch({
      DRpower::get_prevalence(n = df$n_deletions,
                              N = df$sample_size,
                              prev_thresh = 0.05) # HARD CODING 5% THRESHOLD
      }, error = function(err){
        show_alert(
          title = "Error!",
          text = "Prevalence cannot be estimated because there is an error in the values you entered. Please make sure you have entered only positive integers. The number of deletions should always be less than or equal to the total sample size.",
          type = "error"
        )
      })
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
      if(is.null(analysis_rv$df_analysis_update)){
        # TODO debugging
        print("estimate prev is NULL")
        print("error should have popped up")
        
        show_alert(
          title = "Error!",
          text = "You have not selected the number of clusters and/or entered the values for your study. Please select the number of clusters from the drop-down menu and enter the values in the table.",
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
    
    box(width = 6,
        collapsible = T,
        background = "purple",
        title = "Prevalence estimates",
        p("The table and the plot below show the maximum a posteriori (MAP) estimate of the prevalence, along with a 95% credible interval (CrI). The MAP estimate can be used as a central estimate of the prevalence, but it should always be reported alongside the CrI to give a measure of uncertainty. "),
        br(),
        renderTable(prev_output() %>% mutate(prob_above_threshold = prob_above_threshold*100) %>% 
                      rename("Prevalence estimate (%)" = MAP, "Lower CrI (%)" = CrI_lower, "Upper CrI (%)" = CrI_upper, "Probability above threshold (%)" = prob_above_threshold), 
                    digits = 2,
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
      line1 <- paste("RESULT: We estimate that the prevalence of", em("pfhrp2/3"), "deletions is ", round(as.numeric(prev_output()$MAP), 2), "% (95% CrI: ", round(as.numeric(prev_output()$CrI_lower), 2), "- ", round(as.numeric(prev_output()$CrI_upper), 2), "%).")
      line2 <- paste("We conclude that the ", em("pfhrp2/3"), "deletion prevalence is above the 5% threshold (probability above threshold = ", round(as.numeric(prev_output()$prob_above_threshold)*100, 2), "%).")

      HTML(paste(line1, line2, sep = "<br/><br/>"))

    }

    else if(!is.null(prev_output()) && prev_output()$prob_above_threshold < 0.95){
      line1 <- paste("RESULT: We estimate that the prevalence of", em("pfhrp2/3"), "deletions is ", round(as.numeric(prev_output()$MAP), 2), "% (95% CrI: ", round(as.numeric(prev_output()$CrI_lower), 2), "- ", round(as.numeric(prev_output()$CrI_upper), 2), "%).")
      line2 <- paste("We conclude that the ", em("pfhrp2/3"), "deletion prevalence is below the 5% threshold (probability above threshold = ", round(as.numeric(prev_output()$prob_above_threshold)*100, 2), "%).")

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
                   size = 4,
                   shape = 21,
                   fill = "mediumpurple") +
        geom_hline(aes(yintercept = 5), 
                   color = "darkgrey",
                   linetype = "dashed") +
        geom_text(aes(x= " ", 
                      y = 7, 
                      label = "5% threshold"), 
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
      
      # get the stored and edited data frame with sample sizes, or the uploaded data frame
      if(input$analysis_table_choice=="manual"){
        df <- analysis_rv$df_analysis_update
      }
      else if(input$analysis_table_choice=="upload"){
        df <- df_deletions_uploaded()
      }
      
      DRpower::get_ICC(n = df$n_deletions,
                       N = df$sample_size)
      
    })
    
  # If user clicks 'estimate ICC' button before selecting clusters and entering sample sizes in step 1, an error message will pop-up
  observeEvent(input$est_icc, {
    print("Estimate ICC button clicked")

    # To make sure the error message pops up as expected, don't show it if est_prev button has been clicked AND power_output() has been created, otherwise show error message 
    if(input$est_prev && !is.null(analysis_rv$df_analysis_update) && !is.null(prev_output())){
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
        text = "You have not selected the number of clusters or entered the values for your study. Please go back to Step 1 ('Enter the values specific to your study') and select the number of clusters from the drop-down menu and enter the values in the table.",
        type = "error"
      )
    }
    
  })
  
  # The results box, text and plots are displayed once the estimate icc button is clicked 
  output$est_icc_results <- renderUI({
    req(input$est_icc, icc_output())
    
    print("est_icc results should print")
    print(icc_output())
    
    box(width = 6,
        collapsible = T,
        background = "purple",
        title = "ICC estimates",
        p("The table and the plot below show the maximum a posteriori (MAP) estimate of the ICC, along with a 95% credible interval (CrI). For context, an ICC of 0.05 is used by default in the Design tab based on an ", a("analysis of historical studies.", target = "_blank", href = "https://mrc-ide.github.io/DRpower/articles/historical_analysis.html")),
        br(),
        renderTable(icc_output() %>% 
                      rename("Estimate of ICC" = MAP, "Lower CrI" = CrI_lower, "Upper CrI" = CrI_upper), 
                    digits = 2,
                    colnames = T, 
                    align = "c"),
        br(),
        plotOutput("est_icc_plot")
    )
  })
  
  output$est_icc_plot <- renderPlot({
    ggplot(icc_output()) +
      geom_segment(aes(x = " ", xend = " ", y = CrI_lower, yend = CrI_upper), 
                   color = "black", linewidth = 1) +
      geom_point(aes(x = " ", y = MAP), 
                 size = 4, 
                 shape = 21,
                 fill = "mediumpurple") +
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
    if (input$analysis_nclust=="" || input$est_prev==0 || input$est_icc==0 || is.null(prev_output()) || is.null(icc_output())) {
      print("error should pop up when save results is clicked")
      show_alert(
        title = "Error!",
        text = "The summary cannot be displayed because you haven't completed Steps 1 and/or 2. Please go back to 'Estimate prevalence and ICC' and follow all the steps.",
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
          if(input$analysis_table_choice=="manual"){
            req(analysis_rv$df_analysis_update)
            renderTable(analysis_rv$df_analysis_update, digits = 0)
          }
          else if(input$analysis_table_choice=="upload"){
            req(df_deletions_uploaded())
            renderTable(df_deletions_uploaded(), digits = 0)
          },
          br(), br(),
          h4("Prevalence estimates:"),
          renderTable(prev_output() %>% mutate(prob_above_threshold = prob_above_threshold*100), 
                      digits = 2),
          br(), br(),
          h4("ICC estimates:"),
          renderTable(icc_output(), digits = 2)
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
          em("This creates an html summary of the assumed parameters and your results with standardised text to minimise mistakes. Note that you can convert this to a pdf if preferred by going to file/print."),
          br(), br(),
          downloadButton("analysis_report", "Download analysis report", icon("download")))
    }
    
  })
  
  # The downloadHandler() for the design report will be triggered if the downloadButton() is clicked 
  output$analysis_report <- downloadHandler(
    # filename = paste0("PfHRP2_Planner_Analysis_Report_", Sys.Date(), ".pdf"),
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
      
      # select the correct study data based on manual entry vs uploaded
      if(input$analysis_table_choice=="manual"){
        study_data <- analysis_rv$df_analysis_update
      }
      else if(input$analysis_table_choice=="upload"){
        study_data <- df_deletions_uploaded()
      }
      
      params <- list(analysis_nclusters = input$analysis_nclust,
                     analysis_study_data = study_data,
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
