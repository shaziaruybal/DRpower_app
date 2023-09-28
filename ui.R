######### DRpower App: UI ########################################################################
# Authors: Shazia Ruybal-Pesántez (sruybal@imperial.ac.uk)
##################################################################################################

library(shiny)
library(DT)
library(shiny.blueprint)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)

# This is a custom function that calls upon the goToTab JS code (lines 79-88) to navigate to a tab by its 'tabname'
shinyLink <- function(to, label) {
  tags$a(
    class = c("shiny__link", "inline-link"), 
    href = "#",
    onclick = sprintf("goToTab('%s'); return false;", to),
    label
  )
}


dashboardPage(#theme = "flatly",
  skin = "purple",
  # dashboardHeader(title = HTML(paste(em("pfhrp2/3"), " Planner"))),
  dashboardHeader(title = "pfhrp2/3 Planner"),
  dashboardSidebar(width = 150,
                   # size = "thin",
                   sidebarMenu(
                    # menuItem(tabName = "test_tab",
                    #          text = "TESTING"),
                    menuItem(tabName = "home", 
                             text = " Home",
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
                   )
  ),
  dashboardBody(
    useShinyjs(),
    # The CSS code below creates a custom callout box with the same styling as the shiny.blueprint::Callout box but without funcitonality JS/HTML issues
    tags$style(
      HTML("
        .custom-callout {
          border: 1px solid #f0f0f0;
          background-color: rgb(218, 224, 231);
          padding: 15px;
          margin-bottom: 10px;
        }
        .callout-title{
          font-size: 17px;
          font-weight: 600;
          margin-bottom: 10px;
        }
        
        .inline-link {
          display: inline;
          margin-right: 0px; /* Add spacing between inline links */
        }
      ")
    ),
    # The JavaScript code below runs the function that triggers a click "event" switching to the resepctive tabName when the link is clicked 
    tags$script('
      function goToTab(tabName) {
        var tab = document.querySelector("a[data-value=" + tabName + "]");
        if (tab) {
          tab.click()
          // var tabUrl = tab.getAttribute("href");
          // window.open(tab, "_blank");
        }
      }
    '),
    tabItems(
      # ----------------------------------
      # TESTING
      # tabItem(tabName = "test_tab",
      #         h2("This is a test to see if we can hyperlink to the FAQ tab!"),
      #         br(), br(),
      #         # Custom Shiny Dashboard box with callout styling
              # div(class = "custom-callout",
              #     "This tool is designed to help researchers conducting ", em("Plasmodium pfhrp2/3"), " gene deletion studies. It can be used in two ways:",
              #     br(), br(),
              #     "1.	In the ", shinyLink(to = "design", label = "design phase"), "(before data have been collected) to help guide the appropriate number of clusters and a sample size per cluster.",
              #     br(),
              #     "2.	In the ", shinyLink(to = "analysis", label = "analysis phase"), "(once data are available) to estimate prevalence of deletions and determine if they are above a set threshold.",
              #     br(), br(),
              #     "The ideal plan would be to perform both steps, i.e., using this app before a study has started to choose target sample sizes and then returning to the app once data are available. However, it is valid to analyse data even if sample sizes were chosen using a different method (see ",shinyLink(to = "faq", label = "FAQs)"),
              #     br(), br(),
              #     "For those wanting more background information on the method, or who want to perform more advanced analyses, please take a look at the ",
              #     a("DRpower R package ", href='https://mrc-ide.github.io/DRpower/'),
              #     "that underpins this app.",
              #     br(), br(),
              #     "This tool was developed by Shazia Ruybal-Pesántez and Bob Verity, Malaria Modelling Group, Imperial College London, in collaboration with the World Health Organisation (WHO).",
              #     br(), br(), br(),
              #     em("Most recent update X August 2023.")
              # ),
      #         br(),
      #         br(),
      #         box("This is a test shinydashboard::box to see if the JS hyperlinks work (working)",
      #             div(
      #               tags$a("Go to FAQ page", href = "#", onclick = "goToTab('faq')")
      #             ),
      #             div(
      #               tags$a("Go to Design page", href = "#", onclick = "goToTab('design')")
      #             ),
      #             div(
      #               tags$a("Go to Analysis page", href = "#", onclick = "goToTab('analysis')")
      #             ),
      #             div(
      #               tags$a("Go back to About", href = "#", onclick = "goToTab('about')")
      #             )
      #         ),
              
              # fluidRow(
              #   shinydashboard::box(width = 12,
              #       background = "purple",
              #       tableOutput("test_table"))
              # ),
      # ),
      # ----------------------------------
      # Tab 1 - Home
      tabItem(
        tabName = "home",
        fluidRow(
          column(width = 12, 
                 style='padding:20px;',
                 div(class = "custom-callout",
                     p(class = "callout-title", "How to use this tool"),
                     "This tool is designed to help researchers conducting ", em("Plasmodium pfhrp2/3"), " gene deletion studies. It can be used in two ways:",
                     br(), br(),
                     "1.	In the ", shinyLink(to = "design", label = "design phase"), "(before data have been collected) to help guide the appropriate number of clusters and a sample size per cluster.",
                     br(),
                     "2.	In the ", shinyLink(to = "analysis", label = "analysis phase"), "(once data are available) to estimate prevalence of deletions and determine if they are above a set threshold.",
                     br(), br(),
                     "The ideal plan would be to perform both steps, i.e., using this app before a study has started to choose target sample sizes and then returning to the app once data are available. However, it is valid to analyse data even if sample sizes were chosen using a different method (see ",shinyLink(to = "faq", label = "FAQs"), ").",
                     br(), br(),
                     "For those wanting more background information on the method, or who want to perform more advanced analyses, please take a look at the ",
                     a("DRpower R package ", target = "_blank", href='https://mrc-ide.github.io/DRpower/'),
                     "that underpins this app.",
                     br(), br(),
                     "This tool was developed by Shazia Ruybal-Pesántez and Bob Verity, Malaria Modelling Group, Imperial College London, in collaboration with the Global Malaria Programme, World Health Organisation (WHO).",
                     br(), br(), br(),
                     em("Most recent update 18 August 2023.")
                 ),
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
                                 div(class = "custom-callout",
                                    p(class = "callout-title", "Step 1. Consult sample size tables"),
                                    "The table below gives the number of confirmed malaria positive samples required ", em("per cluster "), "in order for study power to be 80% or higher. You can use these numbers as a general guide when scoping out a study plan, before moving to more tailored sample sizes in the next step.",
                                    br(), br(),
                                    "In general, it is recommended to focus efforts on recruiting more clusters, rather than obtaining large numbers of samples from just a few clusters. Not only will the overall study sample size be lower, but this will also make results more robust to variation within a province."
                                 ),
                                 br(),
                                 box(width = 12, 
                                     title = "Sample sizes required to achieve a target power of 80%", 
                                     "Minimum sample size depends on many factors including the degree of intra-cluster correlation, the prevalence threshold that we are testing against, and the true prevalence in the province. For help choosing these values, see ",
                                     shinyLink(to = "faq", label = "here. "), 
                                     br(),
                                     br(),
                                     selectInput(
                                       inputId = "ss_icc",
                                       label = strong("Select the intra-cluster correlation (ICC): "),
                                       width = "40%",
                                       choices = c("", 0.00, 0.01, 0.02, 0.05, 0.10, 0.20), 
                                       selected = 0.05,
                                     ),
                                     helpText(em("A high ICC value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on an "),
                                              em(a("analysis of historical studies.", target = "_blank", href = "https://mrc-ide.github.io/DRpower/articles/historical_analysis.html"))),
                                     br(), 
                                     selectInput(
                                       inputId = "ss_prev",
                                       label = strong("Select the prevalence threshold (%): "),
                                       width = "40%",
                                       choices = c("", 5, 8, 10), 
                                       selected = 5,
                                     ),
                                     helpText(em("The prevalence value that we are comparing against in our hypothesis test (5% by default, see ", em(shinyLink(to = "faq", label = "here")), ").")),
                                     br(),
                                     tagAppendAttributes(textOutput("text_ss"), style="white-space:pre-wrap;"),
                                     DTOutput("sample_size_table")
                                 )
                        ),
                        tabPanel("Final cluster sizes",
                                 br(),
                                 div(class = "custom-callout",
                                     p(class = "callout-title", "Step 2. Refine your cluster sizes"),
                                     "Sample size tables assume you will collect the same number of samples in every cluster, but this may not be possible in practice. Here, you can enter your final target sample size in each cluster and then estimate power directly.",
                                     br(), br(),
                                     "When choosing sample sizes, remember this is the number of ", em("confirmed malaria positive "), "individuals. Check with local teams to see how many cases can realistically be recruited within the study period based on local incidence trends. You can also use this table to account for drop-out, which can occur for many reasons from participants withdrawing consent to failure of lab samples. Local staff and technicians may be able to advise on sensible values for assumed drop-out."
                                 ),
                                 br(),
                                 fluidRow(
                                   box(width = 7,
                                       title = "1. Enter sample sizes specific to your study",
                                       selectInput(
                                         inputId = "design_nclust",
                                         label = strong("Select number of clusters: "),
                                         width = "40%",
                                         choices = c("", 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20), 
                                         selected = NULL,
                                       ),
                                       textOutput("text_edit_clusttab"), 
                                       DTOutput("editable_clusttab"),
                                       br(),
                                       bsAlert("error_noclusters"), # this creates an error message if user clicks calculate without choosing number of clusters
                                       actionButton(
                                         inputId = "calc_sizes",
                                         label = "Calculate adjusted sample sizes",
                                         icon = icon("clipboard-check")),
                                       helpText(em("If you update these values, make sure you remember to recalculate your adjusted sample sizes and estimate power below"))
                                   ),
                                   uiOutput("final_sizes_results"),
                                 ),
                                 fluidRow(
                                   box(
                                     width = 7,
                                     title = "2. Estimate power", 
                                     p("Using the target sample sizes above, you can estimate the power of your study by simulation."),
                                     selectInput("param_prev",
                                                 label = "Select the prevalence (%)",
                                                 choices = c(6, 7, 8, 9, 10), 
                                                 selected = "10",
                                                 width = "40%",
                                     ),
                                     helpText(em("This is the assumed true prevalence of pfhrp2/3 deletions in the province. A value of 10% is used by default (see "),
                                              em(shinyLink(to = "faq", label = "here")), 
                                              em("for help choosing this value).")),
                                     br(),
                                     selectInput("param_icc", 
                                                 label = "Select the intra-cluster correlation",
                                                 choices = c(0.01, 0.05, 0.10), 
                                                 selected = "0.05",
                                                 width = "40%",
                                     ),
                                     helpText(em("A high value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on an ", em(a("analysis of historical studies.", target = "_blank", href = "https://mrc-ide.github.io/DRpower/articles/historical_analysis.html")))),
                                     br(),
                                     selectInput("param_n_sims", 
                                                 label = "Select the number of simulations",
                                                 choices = c(seq(100, 1000, by=100)), 
                                                 selected = "100",
                                                 width = "40%",
                                     ),
                                     helpText(em("A larger number of simulations will produce more accurate estimates of the power but will take longer to compute. We recommend 100 simulations for exploration and 1000 simulations for final analysis.")),
                                     br(),
                                     bsAlert("error_nosizes"), # this creates an error message if user clicks estimate power without entering sample sizes
                                     actionButton(inputId = "est_pow",
                                                  label = "Estimate power",
                                                  icon = icon("clipboard-check")),
                                     helpText(em("If you update any of the values in Step 1, make sure you remember to recalculate power"))
                                   ),
                                   uiOutput("est_power_results"),
                                 )
                        ),
                        tabPanel(title = "Generate report",
                                 br(),
                                 box(width = 12,
                                     title = "Check and save your parameters and results",
                                     p("Click the button below to display a summary of the information you entered in the previous tab. If everything looks as expected, click on the download button to download your report PDF."),
                                     br(),
                                     actionButton("save_design_data", " Save results", icon("floppy-disk"))
                                 ),
                                 uiOutput("text_design_summary"),
                                 uiOutput("design_download"),
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
                  column(width = 12, 
                         style='padding:20px;',
                         div(class = "custom-callout",
                             p(class = "callout-title", "Step 1. Estimate prevalence and compare against a threshold"),
                             "Here, you can enter your observed counts of ", em("pfhrp2/3"), " deletions in each cluster and use the DRpower model to estimate the prevalence of deletions along with a 95% credible interval (CrI). You can also compare prevalence against a threshold to work out the probability of being above this threshold.",
                             br(), br(),
                             "If your intention is to make a binary decision as to whether prevalence is above or below the threshold (i.e., a hypothesis test) then it is worth being clear about your analysis plan ", strong("before "), "you see the result. For example, we recommend accepting that prevalence is above the threshold if the probability of this outcome is 0.95 or higher (the power calculations in the Design tab assume this value). You should not change your criteria for accepting/rejecting a hypothesis once you have seen the result, as this introduces bias. "
                         )
                  ),
                  box(width = 12,
                      title = "1. Enter the values specific to your study",
                      p("Set the prevalence threshold against which we want to compare. Select the final number of clusters in your study and enter the raw number of observed ", em("pfhrp2/3"), " deletion counts, and the number of confirmed malaria cases per cluster."),
                      br(),
                      selectInput(
                        inputId = "analysis_prevthresh",
                        label = strong("Select prevalence threshold (%): "),
                        width = "40%",
                        choices = c("", 5, 8, 10),
                        selected = 5
                      ),
                      selectInput(
                        inputId = "analysis_nclust",
                        label = strong("Select final number of clusters: "),
                        width = "40%",
                        choices = c("", 2, 3, 4, 5, 6,7, 8, 9, 10, 15, 20), 
                        selected = NULL
                      ),
                      DTOutput("editable_deltab"),
                      br(),
                      bsAlert("error_nodeletions"), # this creates an error message if user clicks estimate prevalence without entering deletions/sample sizes
                      actionButton(inputId = "est_prev", 
                                   label = "Estimate prevalence", 
                                   icon("clipboard-check")),
                      helpText(em("If you update these values, make sure you remember to recalculate prevalence"))
                  )
                ),
                uiOutput("est_prev_results"),
              ),
              tabPanel(
                title= "Estimate ICC",
                fluidRow(
                column(width = 12, 
                       style='padding:20px;',
                       div(class = "custom-callout",
                           p(class = "callout-title", "Step 2. Analysis of intra-cluster correlation (ICC)"),
                           "Although the prevalence of ", em("pfhrp2/3"), " deletions is usually the main focus of our analysis, the intra-cluster correlation is an extremely valuable supplementary analysis. Reporting this value not only contextualises the prevalence estimates, but it also provides valuable information to assist with the design of future studies.",
                           br(), br(),
                           em("The raw data for this analysis are taken from the previous tab, and there are no additional parameters needed."),
                           br(), br(),
                           actionButton(inputId = "est_icc",
                                        label = " Estimate ICC",
                                        icon("clipboard-check")),
                           helpText(em("If you update any of the values in Step 1, make sure you remember to recalculate ICC"))
                       )
                ),
                ),
                uiOutput("est_icc_results"),
              ),
              tabPanel(
                title = "Generate report",
                br(),
                box(width = 12,
                    title = "Check and save your parameters and results",
                    p("Click the button below to display a summary of the information you entered in the previous tab. If everything looks as expected, click on the download button to download your report PDF."),
                    br(),
                    actionButton("save_analysis_data", " Save results", icon("floppy-disk"))
                ),
                uiOutput("text_analysis_summary"),
                uiOutput("analysis_download"),
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
                   "the level at which missed cases due to deletions match missed cases due to loss of sensitivity in alternative RDTs. We should treat this number as a useful guide, not a value to slavishly follow. We should also keep in mind that the closer our assumed prevalence is to the 5% threshold, the larger our sample size will need to be, up to values that are completely unrealistic for any control programme. There is a balance to be struck between sensitivity to detect a given effect size, and pragmatic arguments based on logistics, budget, and ethical considerations. Here, we opt for an assumed 10% prevalence as the default, as this gives a reasonable level of sensitivity while also leading to realistic sample sizes.",
                   br(), br(),
                   em("For more information on the statical methods used in the back-end of this app or if you want to do more advanced analyses, please visit the "),
                   em(a("DRpower R package website.", target = "_blank", href='https://mrc-ide.github.io/DRpower/'))
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
  )
)
