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
              
              fluidRow(
                
                
                
                # Dynamic valueBoxes
                valueBoxOutput("AgeAverage")
                
                
              ),
              
              checkboxGroupInput("typeSelection_All", "Type filter:",
                                 TypeList,selected = TypeList),
              
              h3("All users"),
              tabBox(
                title = "Garphs Tabs",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("Tab1", plotOutput("modeFrequencyPie_All")),
                tabPanel("Tab2", plotOutput("dayUsageBar_All")),
                tabPanel("Tab3", plotOutput("reasonPerStatus")),
                tabPanel("Tab4", leafletOutput("mapSmokeAll")),
                tabPanel("Cigs - Alcohol", plotOutput("cigsAlcohol"))
                
              )
              
              
              #NplotOutput("modeFrequencyPie_All"),
              #plotOutput("dayUsageBar_All"),
              #plotOutput("reasonPerStatus"),
              #leafletOutput("mapSmokeAll")
              
   
              
              
        ),
      tabItem("singleUser", 
              fluidRow(
                
                
                
                # Dynamic valueBoxes
                valueBoxOutput("Age"),
                valueBoxOutput("Gender"),
                valueBoxOutput("Started"),
                valueBoxOutput("Family")
                
                
              ),
              
              selectInput("selectedPerson", "Pick a user:",
                          choices=d.uniqueUsers),
              checkboxGroupInput("typeSelection", "Type filter:",
                                 TypeList ,selected = TypeList),
              h3(textOutput("username")),
              tabBox(
                title = "Garphs Tabs",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset2", height = "250px",
                tabPanel("Tab1",plotOutput("modeFrequencyPie")),
                tabPanel("Tab2",plotOutput("dayUsageBar")),
                tabPanel("Tab3",plotOutput("trend")),
                tabPanel("Tab4",leafletOutput("mapSmokeUser"))
                )
              
             
              
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
  
  output$mapSmokeUser <- renderLeaflet({
    mapUser(input$selectedPerson,input$typeSelection)
  })
  output$trend <- renderPlot({
    d.getTrend(input$selectedPerson, input$typeSelection)
  })
  
  
  output$Age <- renderValueBox({
    valueBox(
      getAge(input$selectedPerson), "Years", icon = icon("list"),
      color = "purple"
    )})
  
  output$Gender <- renderValueBox({
    valueBox(
      getGender(input$selectedPerson), "Gender" ,icon = icon("trangender"),
      color = "purple"
    )
  })
  
  output$Family <- renderValueBox({
    valueBox(
      getFamily(input$selectedPerson), "Family Status" ,icon = icon("users"),
      color = "purple"
    )
  })
  
  output$Started <- renderValueBox({
    valueBox(
      getStarted(input$selectedPerson), "Started Date" ,icon = icon("clock"),
      color = "purple"
    )
  })
  
  
  # All users
  
  output$modeFrequencyPie_All <- renderPlot({
    d.getModeFrequencyPie_All()
  })
  
  output$dayUsageBar_All <- renderPlot({
    d.getDayUsageBar_All(input$typeSelection_All)
  })
  
  output$reasonPerStatus <- renderPlot({
    d.getReasonPerStatus()
  })

  

  
  output$mapSmokeAll <- renderLeaflet({
    mapAll(input$typeSelection_All)
  })
  
  output$AgeAverage <- renderValueBox({
    valueBox(
      AgeAverage(), "Age Average", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$cigsAlcohol <- renderPlot({
    d.getCigsAlcohol()
  })
  

  
}


shinyApp(ui, server)