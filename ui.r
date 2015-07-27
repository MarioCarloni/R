library(shiny)

shinyUI(
  navbarPage("Menu", # Create navbar
             tabPanel("Tool", # First navbar tab, "Tool"
                      fluidPage( # Create page layout
                        titlePanel("Business Intelligence Calculator"), # Title
                        
                        h4("", # Header containg clickable link with text
                           a("Raising the Bar with Analytics: MIT Sloan Management Review", 
                             href="http://sloanreview.mit.edu/article/raising-the-bar-with-analytics/",
                             target="_blank")),
                        
                        tags$head( # Change background to white
                          tags$style("body {background-color: white ; }")),
           
                        # Create filter sidebar with text, specify filter types
                        sidebarLayout(
                          sidebarPanel(
                            
                            "The Analytics Benefit Calculator (ABC) can help you make a decision on a specific BI tool investment. Filter this sidebar panel to automatically update cost, revenue, or profit forecasts.",
                            
                            br(),
                            br(),
                            
                            textInput("discountRate", # Create text input
                                      "Required Rate of Return:", # Title
                                      value = .05), # Default value
                            
                            textInput("revBegin",
                                      "Starting Annual Revenue:",
                                      value = format(100000,scientific = FALSE)),
                            
                            sliderInput("revGrowthPerc", # Create slider input
                                        "Rate of ordinary revenue growth per year",
                                        min = 0, # Min
                                        max = 3, # Max
                                        step = .05, # Intervals
                                        value = .25), # Default value
                            
                            textInput("revGrowth",
                                      "Additional annual revenue from new BI projects (today's dollars):",
                                      value = format(1000,scientific = FALSE)),
                            
                            sliderInput("revGrowthPercNew",
                                        "Additional revenue growth per year due to enhanced BI capabilities",
                                        min = 0,
                                        max = .5,
                                        step = .01,
                                        value = .05),
                            
                            textInput("costBegin",
                                      "Starting Annual Costs",
                                      value = format(300000,scientific = FALSE)),
                            
                            sliderInput("costGrowthPerc",
                                        "Rate of cost growth per year",
                                        min = 0,
                                        max = .5,
                                        step = .01,
                                        value = .03),
                            
                            textInput("costProject",
                                      "Estimate total inital BI project cost",
                                      value = format(100000,scientific = FALSE)),
                            
                            textInput("savingYear",
                                      "Starting Year Savings for unnecessary software, etc.",
                                      value = 50000),
                            
                            textInput("costGrowth",
                                      "Additional Annual Costs for Software License, Hardware, Talent, etc.",
                                      value = 20000),
                            
                            sliderInput("costRedPerc",
                                        "Reduction in cost growth through efficiencies",
                                        min = 0,
                                        max = .5,
                                        step = .01,
                                        value = .015)
                            ),
                      
                          # Show a plot of the generated distribution
                          mainPanel( # Output window
                            fluidRow( # Create row layout
                              
                              column(6, # Create "column" of  grid width 6 
                                     sliderInput("years",
                                                 "Number of years:",
                                                 min = 1,
                                                 max = 10,
                                                 value = 8,
                                                 animate = T)), # Enable cycling through steps
                              
                              column(6, # 2 columns of same width will evenly distribute
                                     "Use the date slider to analyze sales and profits, or click the play button just below the slider to see animated years",
                                     br(), # Add spacing for text underneath
                                     br(),
                                     "And use the Summary, Chart, and Data table tabs to navigate to various outputs.")),
                            
                            tabsetPanel(type = "tabs", # Create tab panels for various outputs
                                       
                                        tabPanel("Summary", # Create text summary output tab
                                                 textOutput("npv"), # See output$npv in server.r
                                                 textOutput("payback"), # See output$npv in server.r
                                                 textOutput("paybackDiscounted"),
                                                 verbatimTextOutput("summarize")),
                                        
                                        tabPanel("Charts",
                                                 column(6,
                                                        selectInput("chartType",
                                                                    "Chart Display:",
                                                                    c("Revenue", # See "if (input$chartType == "Revenue"){" in server.r
                                                                      "Cost",
                                                                      "Profit"))),
                                                 plotOutput("costPlot"), # See output$costPlot in server.r
                                                 plotOutput("cumCostPlot")),
                                       
                                        tabPanel("Data",
                                                 dataTableOutput(outputId="forecast")) # See forecast() in server.r
                                        )
                            )
                          )
                        )
                      ),
             
             tabPanel("Documentation",
                      fluidPage(
                       
                        h2("Required Rate of Return"),
                        h4("The minimum annual percentage earned by an investment that will induce individuals and companies to put money into a particular security or project."),
                        h5("",
                           a("Investopedia: Required Rate of Return",
                             href = "http://www.investopedia.com/terms/r/requiredrateofreturn.asp",
                             target = "_blank")),
                      
                        br(),
                       
                        h2("Starting Annual Revenue"),
                        h4("Existing revenue from sales"),
                       
                        br(),
                       
                        h2("Rate of Ordinary Revenue Growth"),
                        h4("The percentage by which revenues would increase, without BI project influence"),
                       
                        br(),
                       
                        h2("Additional annual revenue from new BI projects (today's dollars)"),
                        h4("Additional revenue per year you expect from BI projects"),
                      
                        br(),
                        
                        h2("Additional revenue growth per year due to enhanced BI capabilities"),
                        h4("New projects obtained through BI capabilities."),
                       
                        br(),
                        
                        h2("Starting Annual Costs"),
                        h4("Project costs, without BI project influence"),
                       
                        br(),
                       
                        h2("Rate of Cost Growth per Year"),
                        h4("Percent growth rate that costs would gradually rise"),
                        
                        br(),
                        
                        h2("Estimate total inital BI project cost"),
                        h4("Initial total cost of BI project"),
                        
                        br(),
                        
                        h2("Starting Year Savings for unnecessary software, etc."),
                        h4("Estimated annual savings from"),
                        
                        br(),
                        
                        h2("Additional Annual Costs for Software License, Hardware, Talent, etc."),
                        h4("Estimated BI project-associated costs"),
                        
                        br(),
                        
                        h2("Reduction in cost growth through efficiencies"),
                        h4("The decrease in need for additional resources to perform BI tasks")
                        )
                        
               )
             )
  )

