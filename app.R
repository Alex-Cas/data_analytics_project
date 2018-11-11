library(shiny)
library(shinydashboard)
source('data_access.R')

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    
    menuItem("All users", tabName = "allUsers",
             icon = icon("users")),
    menuItem("Specific User", tabName="singleUser",
             icon = icon("user"))
  )),
  
  dashboardBody(

    tabItems(
      tabItem("allUsers", 
              
              checkboxGroupInput("typeSelection_All", "Type filter:",
                                 TypeList,selected = TypeList),
              
              h3("All users"),
              plotOutput("modeFrequencyPie_All"),
              plotOutput("dayUsageBar_All"),
              plotOutput("dayUsageRatio_All"),
              plotOutput("reasonPerStatus")
              
              
        ),
      tabItem("singleUser", 
              
              selectInput("selectedPerson", "Pick a user:",
                          choices=d.uniqueUsers),
              checkboxGroupInput("typeSelection", "Type filter:",
                                 TypeList ,selected = TypeList),
              h3(textOutput("username")),
              plotOutput("modeFrequencyPie"),
              plotOutput("dayUsageBar"),
              plotOutput("dayUsageRatio"),
              plotOutput("trend")
              
      )
    )
    
  )
  
)

server <- function(input, output) {
  

  output$username <- renderText({
    input$selectedPerson
  })
  
  observe({print(input$typeSelection)})
  
  # Single user
  
  output$modeFrequencyPie <- renderPlot({
    d.getModeFrequencyPie(input$selectedPerson)
  })
  
  output$dayUsageBar <- renderPlot({
    d.getDayUsageBar(input$selectedPerson ,input$typeSelection)
  })
  
  output$dayUsageRatio <- renderPlot({
    d.getDayUsageRatio(input$selectedPerson)
  })
  
  output$trend <- renderPlot({
    d.getTrend(input$selectedPerson, input$typeSelection)
  })
  
  
  
  
  # All users
  
  output$modeFrequencyPie_All <- renderPlot({
    d.getModeFrequencyPie_All()
  })
  
  output$dayUsageBar_All <- renderPlot({
    d.getDayUsageBar_All(input$typeSelection_All)
  })
  
  output$dayUsageRatio_All <- renderPlot({
    d.getDayUsageRatio_All()
  })
  
  output$reasonPerStatus <- renderPlot({
    d.getReasonPerStatus()
  })
  
  
}


shinyApp(ui, server)