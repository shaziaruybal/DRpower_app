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
                    ),
                    menuItem(tabName = "faq",
                             text = " FAQ",
                             icon = icon("info-circle")
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
                                   # br(),
                                   "The table below gives the number of confirmed malaria positive samples required ", em("per cluster "), "in order for study power to be 80% or higher. You can use these numbers as a general guide when scoping out a study plan, before moving to more tailored sample sizes in the next step.",
                                   br(), br(),
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
                                     helpText(em("A high ICC value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on an analysis of historical studies.")),
                                     br(),
                                     selectInput(
                                       inputId = "ss_prev",
                                       label = strong("Select the prevalence threshold (%): "),
                                       width = "auto",
                                       choices = c("", 5, 8, 10), 
                                       selected = NULL,
                                     ),
                                     helpText(em("The prevalence value that we are comparing against in our hypothesis test (5% by default).")),
                                     br(),
                                     textOutput("text_ss"),
                                     DTOutput("sample_size_table")
                                 )
                        ),
                        tabPanel("Final cluster sizes",
                                 br(),
                                 Callout(
                                   title = "Step 2. Refine your cluster sizes",
                                   # br(),
                                   "Sample size tables assume you will collect the same number of samples in every cluster, but this may not be possible in practice. Here, you can enter your final target sample size in each cluster and then estimate power directly.",
                                   br(), br(),
                                   "When choosing sample sizes, remember this is the number of ", em("confirmed malaria positive "), "individuals. Check with local teams to see how many cases can realistically be recruited within the study period based on local incidence trends. You can also use this table to account for drop-out, which can occur for many reasons from failure of lab samples to participants withdrawing consent. Local staff and technicians may be able to advise on sensible values for assumed drop-out."
                                 ),
                                 br(),
                                 fluidRow(
                                   box(width = 7,
                                       title = "1. Enter sample sizes specific to your study",
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
                                       br(),
                                       DTOutput("final_sizes_table")
                                       ),
                                 ),
                                 fluidRow(
                                   box(
                                     width = 7,
                                     title = "2. Estimate power", 
                                     p("Using the sample sizes above, you can estimate the power of your study by simulation."),
                                     # TooltipHost(content = "Select the parameters.",
                                     #             delay = 0,
                                     #             Text(htmltools::em("Select the parameters"))
                                     # ),
                                     selectInput("param_prev",
                                                 label = "Select the prevalence",
                                                 choices = c(0.06, 0.07, 0.08, 0.09, 0.10), 
                                                 selected = "0.06",
                                                 width = "200px",
                                     ),
                                     helpText(em("This is the assumed true prevalence of pfhrp2/3 deletions in the province. A value of 10% is used by default (see [here](LINK) for help choosing this value).")),
                                     br(),
                                     selectInput("param_icc", 
                                                 label = "Select the intra-cluster correlation",
                                                 choices = c(0.01, 0.05, 0.10), 
                                                 selected = "0.1",
                                                 width = "200px",
                                     ),
                                     helpText(em("A high value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on an analysis of historical studies.")),
                                     br(),
                                     selectInput("param_n_sims", 
                                                 label = "Select the number of simulations",
                                                 choices = c(seq(100, 1000, by=100)), 
                                                 selected = "100",
                                                 width = "250px",
                                     ),
                                     helpText(em("A larger number of simulations will produce more accurate estimates of the power but will take longer to compute. We recommend 100 simulations for exploration and 1000 simulations for final analysis.")),
                                     br(),
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
                                 Callout(
                                   title = "Download the design phase report",
                                   "Click the button below to generate report. Click the button below to generate a report based on the information you entered in the previous tab. This creates a pdf with standardised text to minimise mistakes.",
                                   br()
                                 ),                                 
                                 downloadButton("design_report", "Download design report", icon("download")),
                                 box(width = 12,
                                     # background = "navy", #Valid colors are: blue, light-blue, navy, olive.
                                     title = "Download the design phase report",
                                     p("Click the button below to generate a report based on the information you entered in the previous tab. This creates a pdf with standardised text to minimise mistakes."),
                                     br(),
                                     # downloadButton("design_report", "Download design report", icon("download"))
                                 ),
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
                           title = "Step 1. Estimate prevalence and compare against a threshold",
                           "Here, you can enter your observed counts of pfhrp2/3 deletions in each cluster and use the DRpower model to estimate the prevalence of deletions along with a 95% credible interval (CrI). You can also compare prevalence against a threshold to work out the probability of being above this threshold.",
                           br(), br(),
                           "If your intention is to make a binary decision as to whether prevalence is above or below the threshold (i.e., a hypothesis test) then it is worth being clear about your analysis plan ", strong("before "), "you see the result. For example, we recommend accepting that prevalence is above the threshold if the probability of this outcome is 0.95 or higher (the power calculations in the Design tab assume this value). You should not change your criteria for accepting/rejecting a hypothesis once you have seen the result, as this introduces bias. "
                         )
                  ),
                  box(width = 12,
                      title = "1. Enter the values specific to your study",
                      p("Enter the raw number of observed pfhrp2/3 deletion counts, and the number of confirmed malaria cases per cluster. Also set the prevalence threshold against which we want to compare."),
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
                         title = "Step 2. Analysis of intra-cluster correlation (ICC)",
                         "Although the prevalence of pfhrp2/3 deletions is usually the main focus of our analysis, the intra-cluster correlation is an extremely valuable supplementary analysis. Reporting this value not only contextualises the prevalence estimates, but it also provides valuable information to future studies to assist with design.",
                         br(), br(),
                         em("The raw data for this analysis are taken from the previous tab, and there are no additional parameters needed."),
                         br(), br(),
                         actionButton(inputId = "est_icc",
                                      label = " Estimate ICC",
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
                Callout(
                  title = "Download the analysis phase report",
                  "Click the button below to generate a report based on the information you entered in the previous tabs. This creates a pdf with standardised text to minimise mistakes. ",
                  br(), br(),
                ),
                downloadButton("analysis_report", "Download analysis report", icon("download")),
                box(width = 12,
                    # background = "navy", #Valid colors are: blue, light-blue, navy, olive.
                    title = "Download the analysis phase report",
                    p("Click the button below to generate a report based on the information you entered in the previous tabs. This creates a pdf with standardised text to minimise mistakes. "),
                    br(),
                    # downloadButton("analysis_report", "Download analysis report", icon("download"))
                )
              )
            )
          )
        )
      ),
      # ----------------------------------
      # Tab 4 - FAQ
      tabItem(
        tabName = "faq",
        fluidRow(
          column(width = 12, style='padding:20px;',
                 Callout(
                   title = "Frequently Asked Questions",
                   br(),
                   strong("1. What is statistical power?"),
                   br(),br(),
                   "Statistical power is defined as the probability of correctly rejecting the null hypothesis. In simple terms, it is the probably of finding something interesting if it is really there.",
                   br(), br(),
                   "For example, imagine that the true prevalence of pfhrp2/3 deletions in your province is 10%, and that you design a study that has 50% power to detect a prevalence over 5%. This means you are just as likely to (correctly) conclude that prevalence is above 5% as you are to reach the opposite conclusion.",
                   br(), br(),
                   "Studies should generally aim for high power because conducting studies that have a low chance of success can be a waste of resources and also raises ethical issues. That being said, we cannot aim for 100% power because this would involve sampling the entire population. As a general rule of thumb we tend to aim for power of around 80%, which is what is 
                   assumed in this app.",
                   br(), br(),
                   strong("2. Why do I have to choose a prevalence value? Isn’t this the thing I’m trying to estimate?"),
                   br(), br(),
                   "This can be one of the most confusing things about power analysis! The best way to think about this is to make a distinction between the ", 
                   em("true prevalence in the province"), 
                   ", i.e., the prevalence of pfhrp2/3 deletions if we were able to survey every single individual, and the ",
                   em("prevalence in the sample"), 
                   ". The prevalence in the sample is only an estimate of the true prevalence and will tend to vary around the true value by random chance. For example, we might get “lucky” and find a lot of people with the deletion, in which case our sample prevalence will be higher than the true prevalence, or we might get “unlucky” and see the opposite effect.",
                   br(), br(),
                   "Imagine that the true prevalence in our province is 6%. It would only take a small amount of bad luck for the sample prevalence to be less than 5%, meaning we would come to the wrong conclusion that prevalence was below the 5% threshold. On the other hand, if the prevalence in our province is 20% then we would have to be extremely unlucky for the sample prevalence to dip this low. This means that our chance of coming to the correct conclusion is highest when the true prevalence is a long way from the threshold. For this reason, we cannot perform power analysis without first fixing how strong our effect size is.",
                   br(), br(),
                   strong("3. How should I decide what “true prevalence” value to assume?"),
                   br(), br(),
                   "This is a tricky question to answer, as it depends on the details of your study area your specific objectives. We can ask instead; what prevalence level do you really care about detecting, i.e., what is relevant for control purposes? If the prevalence of pfhrp2/3 deletions was 5.1% then would you want to know so that you can immediately switch RDTs? What if the prevalence was 5.001%?",
                   br(), br(),
                   "In reality, we should remember that the 5% level was chosen based on an argument that this is ",
                   em("roughly "),
                   "the level at which missed cases due to deletions match missed cases due to loss of sensitivity in alternative RDTs. We should treat this number as a useful guide, not a value to slavishly follow. We should also keep in mind that the closer our assumed prevalence is to the 5% threshold, the larger our sample size will be, up to values that are completely unrealistic for any control programme. There is a balance to be struck between sensitivity to detect a given effect size, and pragmatic arguments based on logistics, budget, and ethical considerations. Here, we opt for an assumed 10% prevalence as the default, as this gives a reasonable level of sensitivity while also leading to realistic sample sizes."

                 )
          )
        )
      )
    ),
    tags$style(HTML("
    .footer {
      background-color: #242d31; 
      color: white;
      text-align: center;
      position: absolute;
      bottom: 0;
      width: 100%;
      font-style: italic;
    }"
    )),
    fluidRow(
      column(12, class = "footer", HTML("<span style='font-size:12px;background-color: black;'><p>Developed by Shazia Ruybal-Pesántez and Bob Verity</span>")))
      # this option has hyperlinks to github if needed
      # column(12, class = "footer", HTML("<span style='font-size:12px;background-color: black;'><p>Developed by <a href='https://www.github.com/shaziaruybal' style='color:#535394;'>Shazia Ruybal-Pesántez</a> and <a href='https://www.github.com/bobverity' style='color:#535394;'>Bob Verity</a></span>")))
    # this option uses dashboardFooter() from shinydashboardPlus package - would be better to not have to use it if possible as formatting not ideal
    # dashboardFooter(left = HTML("<span style='font-size:12px;background-color: black;'><p>Developed by <a href='https://www.github.com/shaziaruybal' style='color:#535394;'>Shazia Ruybal-Pesántez</a> and <a href='https://www.github.com/bobverity' style='color:#535394;'>Bob Verity</a></span>"),
    #                 right = HTML("<span style='font-size:12px;'>© 2023</span>"))
  )
)