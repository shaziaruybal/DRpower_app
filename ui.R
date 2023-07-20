######### DRpower App: UI ########################################################################
# Authors: Shazia Ruybal-Pesántez (sruybal@imperial.ac.uk)
##################################################################################################

library(shiny)
library(DT)
library(shiny.fluent)
library(shiny.blueprint)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)

dashboardPage(#theme = "flatly",
  skin = "purple",
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
                   br(),
                   "This tool is designed to help researchers conducting ", em("Plasmodium pfhrp"), "2/3 gene deletion studies. It can be used in two ways:",
                   br(), br(),
                   "1.	In the design phase (before data have been collected) to help guide the appropriate number of clusters and a sample size per cluster.",
                   br(),
                   "2.	In the analysis phase (once data are available) to estimate prevalence of deletions and determine if they are above a set threshold.",
                   br(), br(),
                   "The ideal plan would be to perform both steps, i.e., using this app before a study has started to choose sample sizes and then returning to the app once data are available. However, it is valid to analyse data even if sample sizes were chosen using a different method (see [FAQs](LINK)).",
                   br(), br(), 
                   "For those wanting more background information on the method, or who want to perform more advanced analyses, please take a look at the [DRpower R package](LINK) that underpins this app.",
                   br(), br(),
                   "This tool was developed by Shazia Ruybal-Pesántez and Bob Verity, Malaria Modelling Group, Imperial College London, in collaboration with the World Health Organisation (WHO).",
                   br(), br(), br(),
                   em("Most recent update X August 2023.")
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
            br(),
            tabsetPanel(type = "tabs",
                        tabPanel("Sample size tables",
                                 br(),
                                 Callout(
                                   title = "Step 1. Consult sample size tables",
                                   br(),
                                   "The table below gives the number of confirmed malaria positive samples required ", em("per cluster "), "in order for study power to be 80% or higher. You can use these numbers as a general guide when scoping out a study plan, before moving to more tailored sample sizes in the next step.",
                                   br(),
                                   "In general, it is recommended to focus efforts on recruiting more clusters, rather than obtaining large numbers of samples from just a few clusters. Not only will the overall study sample size be lower, but this will also make results more robust to variation within a province. "
                                 ),
                                 br(),
                                 box(width = 12, 
                                     title = "Sample sizes required to achieve a target power of 80%", 
                                     "Minimum sample size depends on many factors including the degree of intra-cluster correlation, the prevalence threshold that we are testing against, and the true prevalence in the province. For help choosing these values, see [here](LINK TO FAQs).",
                                     br(),
                                     # TooltipHost(content = "A high value implies a high variation in the prevalence of deletions between clusters. A value of 5% is suggested by default based on an analysis of historical studies.",
                                     #             delay = 0,
                                     #             Text(htmltools::em("Intra-cluster correlation"))
                                     # ),
                                     br(),
                                     selectInput(
                                       inputId = "ss_icc",
                                       label = strong("Select the intra-cluster correlation: "),
                                       width = "auto",
                                       choices = c("", 0.00, 0.01, 0.02, 0.05, 0.10, 0.20), 
                                       selected = NULL,
                                     ),
                                     selectInput(
                                       inputId = "ss_prev",
                                       label = strong("Select the prevalence threshold: "),
                                       width = "auto",
                                       choices = c("", 0.05, 0.08, 0.10), 
                                       selected = NULL,
                                     ),
                                     br(),
                                     DTOutput("sample_size_table")
                                 )
                        ),
                        tabPanel("Final cluster sizes",
                                 helpText("Some text here to describe the sample size tab"),
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
                                         choices = c("", 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20), 
                                         selected = NULL,
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
                                       bsAlert("error_noclusters"), # this creates an error message if user clicks calculate without choosing number of clusters
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
                                       bsAlert("error_nosizes"), # this creates an error message if user clicks estimate power without entering sample sizes
                                       title = textOutput("title_powbox"),
                                       p(textOutput("text_powbox")),
                                       br(),
                                       plotOutput("est_power_plot")
                                   ),
                                 )
                        ),
                        tabPanel(title = "Generate report",
                                 br(),
                                 # Callout(
                                 #   title = "Download the design phase report",
                                 #   "Click the button below to generate report. This creates a pdf or similar with standardised text describing ",
                                 #   "the assumptions set previously. This provides text that can be copied over directly into a paper/report to minimise mistakes",
                                 #   # br(), br(),
                                 #   # actionButton(inputId = "design_report", 
                                 #   #              label = "Download design report", 
                                 #   #              icon = icon("download"))
                                 # ),
                                 box(width = 12,
                                     # background = "navy", #Valid colors are: blue, light-blue, navy, olive.
                                     title = "Download the design phase report",
                                     p("Click the button below to generate report. This creates a pdf or similar with standardised text describing the assumptions set previously. This provides text that can be copied over directly into a paper/report to minimise mistakes"),
                                     br(),
                                     downloadButton("design_report", "Download design report", icon("download"))
                                 ),
                                 # downloadButton("design_report", "Download design report", icon("download"))
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
                  box(width = 12,
                      title = "1. Enter the values specific to your study",
                      br(),
                      selectInput(
                        inputId = "analysis_prevthresh",
                        label = strong("Select prevalence threshold: "),
                        width = "auto",
                        choices = c("", 0.05, 0.08, 0.10), 
                        selected = NULL
                      ),
                      selectInput(
                        inputId = "analysis_nclust",
                        label = strong("Select final number of clusters: "),
                        width = "auto",
                        choices = c("", 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20), 
                        selected = NULL
                      ),
                      DTOutput("editable_deltab"),
                      br(),
                      actionButton(inputId = "est_prev", 
                                   label = "Estimate prevalence", 
                                   icon("clipboard-check")),
                  )
                ),
                box(width = 12, background = "purple",
                    bsAlert("error_nodeletions"), # this creates an error message if user clicks estimate prevalence without entering deletions/sample sizes
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
                box(width = 12, background = "purple",
                      title = textOutput("title_iccbox"),
                      p(textOutput("text_iccbox")),
                      br(),
                      tableOutput("est_icc_table"),
                      br(),
                      plotOutput("est_icc_plot")
                ),
              ),
              tabPanel(
                title = "Generate report",
                br(),
                # Callout(
                #   title = "Download the analysis phase report",
                #   "Click the button below to generate report. This creates a pdf or similar with standardised text describing ",
                #   "the assumptions set previously. This provides text that can be copied over directly into a paper/report to minimise mistakes",
                #   br(), br(),
                # ),
                box(width = 12,
                    # background = "navy", #Valid colors are: blue, light-blue, navy, olive.
                    title = "Download the analysis phase report",
                    p("Click the button below to generate report. This creates a pdf or similar with standardised text describing ",
                      "the assumptions set previously. This provides text that can be copied over directly into a paper/report to minimise mistakes",
                    ),
                    br(),
                    downloadButton("analysis_report", "Download analysis report", icon("download"))
                )
              )
            )
          )
        )
      )
    )
  )
)