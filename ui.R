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
                    menuItem(tabName = "explore", 
                             text = " Explore", 
                             icon = icon("table")
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
    # TODO: move this CSS code into a .css file to clean up UI code 
    tags$style(
      HTML("
        /* # The CSS code below creates a custom callout box with the same styling as the shiny.blueprint::Callout box but without funcitonality JS/HTML issues*/
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
        
        /* Add spacing between inline links */
        .inline-link {
          display: inline;
          margin-right: 0px; 
        }
        
        /* Change the color of the tooltip boxes */
        .tooltip > .tooltip-inner {
                width: 400px;
                color: white;
                background-color: #666666;
        }
        
        /* Change the width and color of the landing page line */
        hr {
          opacity: 1; 
          border-top: 2.5px solid #605ca3;
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
      #         uiOutput("landing_page")
      # ),
      # ----------------------------------
      # Tab 1 - Home
      tabItem(
        tabName = "home",
        fluidRow(
          column(width = 12,
                 style='padding-left:30px;padding-right:30px', 
                 img(src = "img/icl_mrc_logo.png", height = "40%", width = "40%"),
                 h1("Welcome to the ", em("pfhrp2/3"), "Planner", style = "color: #605ca3"),
                 hr(),
                 h3("How to use this tool", style = "color: #605ca3"),
                 p("This tool is designed to help researchers conducting ", em("Plasmodium falciparum hrp2/3"), " gene deletion studies. It can be used in two ways:"),
                 "1.	In the ", strong(shinyLink(to = "design", label = "design phase")), "(before data have been collected) to help guide the appropriate number of health facilities and a sample size per health facility.",
                 br(),
                 "2.	In the ", strong(shinyLink(to = "analysis", label = "analysis phase")), "(once data are available) to estimate prevalence of deletions and determine if they are above a set threshold.",
                 br(), br(),
                 p("The ideal plan would be to perform both steps, i.e., using this app before a study has started to choose target sample sizes and then returning to the app once data are available. However, it is valid to analyse data even if sample sizes were chosen using a different method (see ",shinyLink(to = "faq", label = "FAQs"), ")."),
                 p("For those wanting more background information on the method, or who want to perform more advanced analyses, please take a look at the ",
                    a("DRpower R package ", target = "_blank", href='https://mrc-ide.github.io/DRpower/'), "that underpins this app."),
                 h3("Our framework", style = "color: #605ca3"),
                 img(src = "img/design_phase_v3.png", height = "75%", width = "75%"),
                 img(src = "img/analysis_phase_v4.png", height = "75%", width = "75%"),
                 br(), br(),
                 h3("Now go ahead and start ", strong(shinyLink(to = "explore", label = "exploring!")), style = "color: #605ca3"),
                 br(), 
                 h4("Acknowledgments", style = "color: #605ca3"),
                 p("This tool was developed by Shazia Ruybal-Pesántez and Bob Verity, Malaria Modelling Group, Imperial College London, in collaboration with the Global Malaria Programme, World Health Organisation (WHO)."),
                 br(), 
                 h4("How to reference", style = "color: #605ca3"),
                 p("Verity B, Ruybal-Pesántez S (2023). DRpower and pfhrp2/3 planner app: Study design and analysis for pfhrp2/3 deletion prevalence studies. R package version 1.0.2 and R Shiny app version 1.0.0."),
                 br(), 
                 h5(em("Data privacy disclaimer"), style = "color: #605ca3"),
                 p(em("This web application does not store any data within the application itself. Data is temporarily stored on our the Shiny server during your active session and/or when you save your results for export into the downloadable report. Please note that any refresh or reload of the page will result in the loss of data, as it is not stored beyond the duration of your session.")),
                 br(), 
                 br(),
                 p(em("Most recent update XX May 2024.")),
                 br()
          )
        )
      ),
      # ----------------------------------
      # Tab 2 - Explore
      tabItem(
        tabName = "explore",
        fluidRow(
            column(width = 12, 
                   style='padding:20px;', 
                   div(class = "custom-callout",
                       p(class = "callout-title", "How many samples? How many health facilities?"),
                       "The table below gives the number of confirmed malaria positive samples required ", em("per health facility "), "in order for study power to be 80% or higher. You can use these numbers as a general guide when scoping out a study plan, before moving to more tailored sample sizes in the ",
                       shinyLink(to = "design", label = "Design"), " step.",
                       br(), 
                   ),
                   box(width = 12,
                       collapsible = T,
                       title = "Sample sizes required to achieve a target power of 80%",
                       selectInput(
                         inputId = "ss_icc",
                         label = strong("Select the intra-cluster correlation (ICC): "),
                         width = "40%",
                         choices = c("", 0.00, 0.01, 0.02, 0.05, 0.10, 0.20),
                         selected = 0.05,
                       ),
                       bsTooltip("ss_icc", 
                                 # TODO: the <a> href doesn't work for some reason 
                                 # title = "<p>A high ICC value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on <p><a href='https://mrc-ide.github.io/DRpower/articles/historical_analysis.html;'>an analysis of historical studies.</a>",
                                 title = "A high ICC value implies a high variation in the prevalence of deletions between health facilities (i.e., clusters). A value of 0.05 is suggested by default based on an analysis of historical studies (see the DRpower website).",
                                 placement = "right",
                                 options = list(container = "body", 
                                                html = T)),
                       # helpText(em("A high ICC value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on an "),
                       #          em(a("analysis of historical studies.", target = "_blank", href = "https://mrc-ide.github.io/DRpower/articles/historical_analysis.html"))),
                       # br(),
                       selectInput(
                         inputId = "ss_prev",
                         label = strong("Select the prevalence threshold (%): "),
                         width = "40%",
                         choices = c("", 5, 8, 10),
                         selected = 5,
                       ),
                       bsTooltip("ss_prev", 
                                 title = "The prevalence value that we are comparing against in our hypothesis test (5% by default, see FAQs)",
                                 placement = "right",
                                 options = list(container = "body", 
                                                html = T)),
                       # helpText(em("The prevalence value that we are comparing against in our hypothesis test (5% by default, see ", em(shinyLink(to = "faq", label = "here")), ").")),
                       # br(),
                   ),
                   box(width = 12,
                       tagAppendAttributes(textOutput("text_ss"), style="white-space:pre-wrap;"),
                       DTOutput("sample_size_table")
                   )
            ),
        )
      ),
      # ----------------------------------
      # Tab 3 - Design
      tabItem(
        tabName = "design",
        fluidRow(
          column(width = 12,
                 style='padding:20px;', 
            tabsetPanel(type = "tabs",
                        tabPanel("Final health facility sizes",
                                 br(),
                                 div(class = "custom-callout",
                                     p(class = "callout-title", "Refine your health facility sizes"),
                                     "Sample size tables assume you will collect the same number of samples in every health facility, but this may not be possible in practice. Here, you can enter your final target sample size in each health facility and then estimate power directly. Generally, surveys will focus on health facilities but the 'cluster' could be different in specific situations. ",
                                     br(), br(),
                                     "When choosing sample sizes, remember this is the number of ", em("confirmed malaria positive "), "individuals. Check with local teams to see how many cases can realistically be recruited within the study period based on local incidence trends. You can also use this table to account for drop-out, which can occur for many reasons from participants withdrawing consent to failure of lab samples. Local staff and technicians may be able to advise on sensible values for assumed drop-out."
                                 ),
                                 br(),
                                 fluidRow(
                                   box(width = 7,
                                       collapsible = T,
                                       title = "1. Enter sample sizes specific to your study",
                                       radioButtons(inputId = "design_table_choice", 
                                                    label = "Choose one:",
                                                    choiceNames = list("Enter values manually","Upload a .csv file"),
                                                    choiceValues = list("manual", "upload")),
                                       # the dynamic UI below either shows the editable table or a file upload option
                                       uiOutput("enter_sizes_dynamicUI"),
                                       ),
                                   uiOutput("final_sizes_results"),
                                 ),
                                 fluidRow(
                                   box(
                                     width = 7,
                                     collapsible = T,
                                     title = "2. Estimate power", 
                                     # p("Using the target sample sizes above, you can estimate the power of your study by simulation."),
                                     numericInput("param_prev",
                                                  label = "Select the prevalence (%)",
                                                  value = 10,
                                                  min = 6,
                                                  max = 100,
                                                  step = 1,
                                                  width = "40%"
                                     ),
                                     bsTooltip("param_prev", 
                                               title = "This is the assumed true prevalence of pfhrp2/3 deletions in the province. A value of 10% is used by default (see FAQs for help choosing this value).",
                                               placement = "right",
                                               options = list(container = "body", 
                                                              html = T)),
                                     # helpText(em("This is the assumed true prevalence of pfhrp2/3 deletions in the province. A value of 10% is used by default (see "),
                                     #          em(shinyLink(to = "faq", label = "here")), 
                                     #          em("for help choosing this value).")),
                                     # br(),
                                     numericInput("param_icc",
                                                  label = "Select the intra-cluster correlation",
                                                  value = 0.05,
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.01,
                                                  width = "40%"
                                     ),
                                     bsTooltip("param_icc", 
                                               title = "A high value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on an analysis of historical studies (see DRpower package website).",
                                               placement = "right",
                                               options = list(container = "body", 
                                                              html = T)),
                                     # helpText(em("A high value implies a high variation in the prevalence of deletions between clusters. A value of 0.05 is suggested by default based on an ", em(a("analysis of historical studies.", target = "_blank", href = "https://mrc-ide.github.io/DRpower/articles/historical_analysis.html")))),
                                     # br(),
                                     numericInput("param_n_sims", 
                                                  label = "Select the number of simulations",
                                                  value = 100,
                                                  min = 100,
                                                  max = 10000,
                                                  step = 100, 
                                                  width = "40%"
                                     ),
                                     bsTooltip("param_n_sims", 
                                               title = "A larger number of simulations will produce more accurate estimates of the power but will take longer to compute. We recommend 100 simulations for exploration and 1000 simulations for final analysis.",
                                               placement = "right",
                                               options = list(container = "body", 
                                                              html = T)),
                                     # helpText(em("A larger number of simulations will produce more accurate estimates of the power but will take longer to compute. We recommend 100 simulations for exploration and 1000 simulations for final analysis.")),
                                     # br(),
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
      # Tab 4 - Analysis
      tabItem(
        tabName = "analysis",
        fluidRow(
          column(width = 12,
                 style='padding:20px;', 
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     title = "Estimate prevalence and ICC",
                     br(),
                     div(class = "custom-callout",
                         p(class = "callout-title", "Estimate prevalence and compare against the 5% threshold"),
                         "Here, you can enter your observed counts of ", em("pfhrp2/3"), " deletions in each health facility and use the DRpower model to estimate the prevalence of deletions along with a 95% credible interval (CrI). You can also compare prevalence against the ", a("WHO recommended 5% threshold ", target = "_blank", href = "https://iris.who.int/handle/10665/331197"), "to work out the probability of being above this threshold. Generally, surveys will focus on health facilities but the 'cluster' could be different in specific situations. ",
                         br(), br(),
                         "If your intention is to make a binary decision as to whether prevalence is above or below the threshold (i.e., a hypothesis test) then it is worth being clear about your analysis plan ", strong("before "), "you see the result. For example, we recommend accepting that prevalence is above the 5% threshold if the probability of this outcome is 0.95 or higher (the power calculations in the Design tab assume this value). You should not change your criteria for accepting/rejecting a hypothesis once you have seen the result."
                     ),
                     br(),
                     fluidRow(
                       box(width = 6,
                           collapsible = T,
                           title = "Step 1. Analyse prevalence of deletions compared with the 5% threshold",
                           p("Select the final number of health facilities in your study and enter the raw number of observed ", em("pfhrp2/3"), " deletion counts, and the number of confirmed malaria cases per cluster."),
                           br(),
                           radioButtons(inputId = "analysis_table_choice", 
                                        label = "Choose one:",
                                        choiceNames = list("Enter values manually","Upload a .csv file"),
                                        choiceValues = list("manual", "upload")),
                           # the dynamic UI below either shows the editable table or a file upload option
                           uiOutput("enter_deletions_dynamicUI"),
                           # selectInput(
                           #   inputId = "analysis_nclust",
                           #   label = strong("Select final number of clusters: "),
                           #   width = "40%",
                           #   choices = c("", seq(2, 20)), 
                           #   selected = NULL
                           # ),
                           # DTOutput("editable_deltab"),
                           # br(), 
                           # bsAlert("error_nodeletions"), # this creates an error message if user clicks estimate prevalence without entering deletions/sample sizes
                           # actionButton(inputId = "add_row_analysis", 
                           #              label = "Add row",
                           #              icon("circle-plus")),
                           # actionButton(inputId = "delete_row_analysis", 
                           #              label = "Delete row",
                           #              icon("circle-minus")),
                           # bsTooltip(id = "delete_row_analysis",
                           #           title = "Select the row you want to delete by clicking it once, this should highlight the row in blue. Then click button.",
                           #           placement = "right"),
                           # br(), br(),
                           # actionButton(inputId = "est_prev", 
                           #              label = "Estimate prevalence", 
                           #              icon("clipboard-check")),
                           # helpText(em("If you update these values, make sure you remember to recalculate prevalence"))
                       ),
                       uiOutput("est_prev_results"),
                     ),
                   fluidRow(
                     box(width = 6, 
                         collapsible = T,
                         title = "Step 2. Analyse intra-cluster correlation (ICC)",
                         "Although the prevalence of ", em("pfhrp2/3"), " deletions is usually the main focus of our analysis, the ICC is an extremely valuable supplementary analysis. Reporting this value not only contextualises the prevalence estimates, but it also provides valuable information to assist with the design of future studies.",
                         br(), br(),
                         em("The raw data for this analysis are taken from Step 1, and there are no additional parameters needed."),
                         br(), br(),
                         actionButton(inputId = "est_icc",
                                      label = " Estimate ICC",
                                      icon("clipboard-check")),
                         helpText(em("If you update any of the values in Step 1, make sure you remember to recalculate ICC"))
                     ),
                     uiOutput("est_icc_results"),
                   ),
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
    # Tab 5 - FAQ
    tabItem(
        tabName = "faq",
        fluidRow(
          column(width = 12, style='padding:20px;', 
                 Callout(
                   title = "Frequently Asked Questions",
                   br(),
                   strong("1. What is statistical power?"),
                   br(),br(),
                   "Statistical power is defined as the probability of correctly rejecting the null hypothesis. In simple terms, it is the probability of finding something interesting if it is really there.",
                   br(), br(),
                   "For example, imagine that the true prevalence of pfhrp2/3 deletions in your province is 10%, and that you design a study that has 50% power to detect a prevalence over 5%. This means you are just as likely to (correctly) conclude that prevalence is above 5% as you are to reach the opposite conclusion.",
                   br(), br(),
                   "Studies should generally aim for high power because conducting studies that have a low chance of success can be a waste of resources and also raises ethical issues. That being said, we cannot aim for 100% power because this would involve sampling the entire population. As a general rule of thumb we tend to aim for power of around 80%, which is what is 
                   assumed in this app.",
                   br(), br(),
                   strong("2. Why do I have to choose a prevalence value? Isn’t this the thing I’m trying to estimate?"),
                   br(), br(),
                   "This can be one of the most confusing things about power analysis! The best way to think about this is to make a distinction between the ", 
                   em("true prevalence in the domain (e.g. province, district, etc)"), 
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
                   strong("4. What is intra-cluster correlation (ICC)?"),
                   br(), br(),
                   "Intra-cluster correlation refers to the variation between health facilities or clusters, i.e. how ", em("overdispersed"), " they are.", 
                   br(), br(),
                   "Imagine we are undertaking a survey to estimate whether the ", em("pfhrp2/3"), " gene deletion prevalence is above the 5% threshold at the province-level and we sample from multiple health facilities (i.e. clusters). Due to many factors, there may be differences in the cluster-level prevalences and this variation will impact the estimation of overall province-level prevalence. ",
                   "High ICC would mean there is a lot of variation in the prevalence of gene deletions between health facilities, which will in turn lead to higher uncertainty in our domain-level estimate and require larger sample sizes. The Bayesian model in DRpower takes into account ICC to more robustly estimate a 'mean' domain-level prevalence. ",
                   br(), br(),
                   "Based on a ", a("historical analysis ", target = "_blank", href='https://mrc-ide.github.io/DRpower/articles/historical_analysis.html'), "of ", em("pfhrp2/3"), " studies, we estimated the ICC using the Bayesian model and found 0.05 to be a realistic value that holds true for most studies, which is what is assumed in this app. ",
                   "For more details on the statistical concepts relating to ICC, see the ", a("DRpower R package website.", target = "_blank", href='https://mrc-ide.github.io/DRpower/articles/rationale2_issue.html'),
                   br(), br(),
                   strong("5. What do my sample size numbers actually represent?"),
                   br(), br(),
                   "In these calculations the sample size number represents the number of ", em("confirmed malaria cases, "), "which may be lower than the number of ", em("suspected "), "cases (see schematic below).",
                   img(src = "img/sample_sizes_diagram.png", height = "100%", width = "100%"),
                   br(), br(), br(),
                   strong("5. What if I want to perform even more bespoke and/or advanced analyses?"),
                   br(), br(),
                   "We recommend the DRpower R package for more advanced users. For more information on the statical methods used in the back-end of this app or if you want to do more advanced analyses, please visit the ",
                   a("DRpower R package website.", target = "_blank", href='https://mrc-ide.github.io/DRpower/')
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
      column(12, class = "footer", HTML("<span style='font-size:12px;background-color: black;'><p>DRpower pfhrp2/3 planner app version 1.0.0. Developed by Shazia Ruybal-Pesántez and Bob Verity.</span>")))
      # this option has hyperlinks to github if needed
      # column(12, class = "footer", HTML("<span style='font-size:12px;background-color: black;'><p>Developed by <a href='https://www.github.com/shaziaruybal' style='color:#535394;'>Shazia Ruybal-Pesántez</a> and <a href='https://www.github.com/bobverity' style='color:#535394;'>Bob Verity</a></span>")))
  )
)