#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shiny.fluent)
library(shiny.blueprint)
library(shinydashboard)

dashboardPage(#theme = "flatly",
  dashboardHeader(title = "pfhrp2 Planner"),
  dashboardSidebar(width = 150,
                   # size = "thin",
                   sidebarMenu(
                    # menuItem(tabName = "test_tab",
                    #          text = "TESTING"),
                    menuItem(tabName = "about", 
                             text = " About",
                             icon = icon("home")
                    ),
                    menuItem(tabName = "design", 
                             text = " Design", 
                             icon = icon("paintbrush")
                    ),
                    menuItem(tabName = "analysis",
                             text = " Analysis",
                             icon = icon("chart-line")
                    )
  )),
  dashboardBody(
    tabItems(
      # ----------------------------------
      # TESTING
      # tabItem(tabName = "test_tab",
      #         actionButton("test_button", "Test me"),
      #         plotOutput("test_plot"),
      #         # DTOutput("editable_table")
      #         downloadButton("design_report", "Download design report", icon("download"))
      #         ),
      # ----------------------------------
      # Tab 1 - About
      tabItem(
        tabName = "about",
        fluidRow(
          column(width = 12, style='padding:20px;',
                 Callout(
                   title = "How to use this tool",
                   "Some intro text here and the aims of this tool. Basic instructions for how to use are found below. ",
                   "For more detailed information, see the DRpower R package website"
                 )
          )
        )
      ),
      # ----------------------------------
      # Tab 2 - Design
      tabItem(
        tabName = "design",
        fluidRow(
          mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Sample size tables",
                                 br(),
                                 box(width = 10, 
                                     title = "Sample sizes to achieve a given target power", 
                                     TooltipHost(content = "TEST: Click and select the row with the number of clusters you intend to use for your study",
                                                 delay = 0,
                                                 Text(htmltools::em("TEST: Interact with the table below. Note to self: only one table below for 80% ATM"))
                                     ),
                                     br(),
                                     selectInput(
                                       inputId = "user_pow",
                                       label = "Target power: ", 
                                       width = "auto",
                                       choices = c(seq(0.0, 1.0, by = 0.1)),
                                       selected = 0.5
                                     ),
                                     DTOutput("sample_size_table"),
                                     textOutput("table_NA")
                                 )
                        ),
                        tabPanel("Final cluster sizes",
                                 tags$head(tags$script(src = "message-handler.js")),
                                 br(),
                                 fluidRow(
                                   box(width = 7,
                                       title = "1. Enter the values specific to your study",
                                       # TooltipHost(content = "Select the number of clusters in your study so that you can populate the table with your sample sizes and the estimated proportion of study participant drop-out.",
                                       #             delay = 0,
                                       #             Text(htmltools::em("Select the number of clusters in your study"))
                                       # ),
                                       selectInput(
                                         inputId = "design_nclust",
                                         label = strong("Select number of clusters: "),
                                         width = "auto",
                                         choices = c(1, 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20), 
                                         selected = 10,
                                       ),
                                       textOutput("text_edit_clusttab"), 
                                       DTOutput("editable_clusttab"),
                                       br(),
                                       actionButton(
                                         inputId = "calc_sizes",
                                         label = "Calculate final sample sizes")
                                   ),
                                   box(width = 5, 
                                       background = "purple",
                                       title = textOutput("title_finalsizesbox"),
                                       p(textOutput("text_finalsizesbox")),
                                       DTOutput("final_sizes_table")
                                       ),
                                 ),
                                 fluidRow(
                                   box(
                                     width = 7,
                                     title = "2. Estimate power", 
                                     TooltipHost(content = "Select the parameters.",
                                                 delay = 0,
                                                 Text(htmltools::em("Select the parameters"))
                                     ),
                                     selectInput("param_prev",
                                                 label = "Select prevalence:",
                                                 choices = c(0.06, 0.07, 0.08, 0.09, 0.10), 
                                                 selected = "0.06",
                                                 width = "200px",
                                     ),
                                     selectInput("param_icc", 
                                                 label = "Select intra-cluster correlation:",
                                                 choices = c(0.01, 0.05, 0.10), 
                                                 selected = "0.1",
                                                 width = "200px",
                                     ),
                                     selectInput("param_n_sims", 
                                                 label = "Select the number of simulations:",
                                                 choices = c(seq(100, 1000, by=100)), 
                                                 selected = "100",
                                                 width = "250px",
                                     ),
                                     actionButton(inputId = "est_pow",
                                                  label = "Estimate power",
                                                  icon = icon("clipboard-check"))
                                   ),
                                   box(width = 5,
                                       background = "purple",
                                       title = textOutput("title_powbox"),
                                       p(textOutput("text_powbox")),
                                       br(),
                                       plotOutput("est_power_plot")
                                   ),
                                 )
                        ),
                        tabPanel(title = "Generate report",
                                 br(),
                                 Callout(
                                   title = "Download the design phase report",
                                   "Click the button below to generate report. This creates a pdf or similar with standardised text describing ",
                                   "the assumptions set previously. This provides text that can be copied over directly into a paper/report to minimise mistakes",
                                   br(), br(),
                                   # downloadButton("design_report", "Download design report", icon("download"))
                                 ),
                                 downloadButton("design_report", "Download design report", icon("download"))
                        )
            )
          )
        )
      ),
      # ----------------------------------
      # Tab 3 - Analysis
      tabItem(
        tabName = "analysis",
        fluidRow(
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel(
                title = "Estimate prevalence",
                fluidRow(
                  column(width = 12, style='padding:20px;',
                         Callout(
                           title = "Estimate prevalence",
                           "To estimate the prevalence of *pfhrp2* deletions, enter the final values obtained from the study in the table below. ",
                           "These parameters are used for inference and to produce various estimates of the prevalence, including simple mean, ",
                           "posterior mean and median, the credible interval (CrI)."
                         )
                  ),
                  box(width = 10,
                      title = "1. Enter the values specific to your study",
                      br(),
                      selectInput(
                        inputId = "analysis_nclust",
                        label = strong("Select final number of clusters: "),
                        width = "auto",
                        choices = c(1, 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20), 
                        selected = 10,
                      ),
                      DTOutput("editable_deltab"),
                      br(),
                      actionButton(inputId = "est_prev", 
                                   label = "Estimate prevalence", 
                                   icon("clipboard-check")),
                  )
                ),
                box(width = 10, background = "purple",
                    title = textOutput("title_prevbox"),
                    p(textOutput("text_prevbox")),
                    br(),
                    tableOutput("est_prev_table"),
                    br(),
                    plotOutput("est_prev_plot")
                ),
              ),
              tabPanel(
                title= "Estimate ICC",
                fluidRow(
                column(width = 12, style='padding:20px;',
                       Callout(
                         title = "Estimate ICC",
                         "Supplementary analyses include calculating ICC.... bla bla ",
                         br(),
                         actionButton(inputId = "est_icc",
                                      label = "Estimate ICC",
                                      icon("clipboard-check"))
                       )
                ),
                ),
                box(width = 10, background = "purple",
                      title = textOutput("title_iccbox"),
                      p(textOutput("text_iccbox")),
                      br(),
                      DTOutput("est_icc_table"),
                      br(),
                      plotOutput("est_icc_plot")
                ),
              ),
              tabPanel(
                title = "Generate report",
                br(),
                Callout(
                  title = "Download the analysis phase report",
                  "Click the button below to generate report. This creates a pdf or similar with standardised text describing ",
                  "the assumptions set previously. This provides text that can be copied over directly into a paper/report to minimise mistakes",
                  br(), br(),
                ),
                downloadButton("analysis_report", "Download analysis report", icon("download"))
              )
            )
          )
        )
      )
    )
  ), skin = "purple"
)