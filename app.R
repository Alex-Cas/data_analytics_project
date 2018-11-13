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
                h3("All users"),
                
                
                # Dynamic valueBoxes
                valueBoxOutput("AgeAverage")
                
                
              ),
              box(
                title = "Type Filter", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,height  = "20%",
                checkboxGroupInput("typeSelection_All", "Type filter:",
                                   TypeList,selected = TypeList)
              ),
              
              tabBox(
                title = "Garphs Tabs",
                
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height  = "40%",
                  tabPanel("Mode Frequency", plotOutput("modeFrequencyPie_All")),
                  tabPanel("Usage per Hour", plotOutput("dayUsageBar_All")),
                  tabPanel("Reasons per Status", plotOutput("reasonPerStatus")),
                  tabPanel("Localisation Usage", leafletOutput("mapSmokeAll")),
                  tabPanel("Correlations", plotOutput("Cor")),
                  tabPanel("Cigs - Alcohol", plotOutput("cigsAlcohol"))
                  )
                
              
              
              
              
              
              #NplotOutput("modeFrequencyPie_All"),
              #plotOutput("dayUsageBar_All"),
              #plotOutput("reasonPerStatus"),
              #leafletOutput("mapSmokeAll")
              
   
              
              
        ),
      tabItem("singleUser", 
              
              h3(textOutput("username")),
              fluidRow(
                
                
                
                # Dynamic valueBoxes
                valueBoxOutput("Age"),
                valueBoxOutput("Gender"),
                valueBoxOutput("Started"),
                valueBoxOutput("Family")
                
                
              ),
              
              selectInput("selectedPerson", "Pick a user:",
                          choices=d.uniqueUsers),
              
              box(
                title = "Type Filter", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,height  = "20%",
              checkboxGroupInput("typeSelection", "Type filter:",
                                 TypeList ,selected = TypeList)),
              
              
                
              tabBox(
                title = "Garphs Tabs",height  = "40%",
                
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset2", 
                tabPanel("Mode Frequency",plotOutput("modeFrequencyPie")),
                tabPanel("Usage per Hour",plotOutput("dayUsageBar")),
                tabPanel("Usage per Day",plotOutput("trend")),
                tabPanel("Localisation Usage",leafletOutput("mapSmokeUser"))
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
      getGender(input$selectedPerson), "Gender" ,icon = icon(tolower(getGender(input$selectedPerson))),
      color = "blue"
    )
  })
  
  output$Family <- renderValueBox({
    valueBox(
      getFamily(input$selectedPerson), "Family Status" ,icon = icon("users"),
      color = "green"
    )
  })
  
  output$Started <- renderValueBox({
    valueBox(
      getStarted(input$selectedPerson), "Started Date" ,icon = icon("calendar"),
      color = "orange"
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
  
  output$Cor<- renderPlot({CorMatrix()})

    output$cigsAlcohol <- renderPlot({
    d.getCigsAlcohol()
  })
  

  
}


shinyApp(ui, server)