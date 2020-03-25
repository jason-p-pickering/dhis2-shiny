library(shiny)
library(shinyjs)
source("./utils.R")

shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  fetchDemoData <- function() {
    if (!user_input$authenticated) {
      return(NULL)
    } else {
      getDemoData()
    }
    
  }
  
  demo_data <- reactive({
    fetchDemoData()
  })
  
  user_input <- reactiveValues(authenticated = FALSE, status = "")
  
  observeEvent(input$login_button, {
    is_logged_in <- FALSE
    user_input$authenticated <- DHISLogin(input$server, input$user_name, input$password)
    if (user_input$authenticated) {
      #user_input$user_orgunit<-getOption("organisationUnit")
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error")
    }
  })
  
  output$demo_table <- DT::renderDataTable({
    
    d <- demo_data()
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      DT::datatable(d)
      
    } else
    {
      NULL
    }
  })
  
  output$pivot <- renderRpivotTable({
    d<-demo_data()
    
    if (!inherits(d,"error") & !is.null(d)){
      
      if ( is.null(d) ) {return(NULL)} 
      
      demoPivot(d)
      
    } else {
      NULL
    }
  })
  
  output$uiLogin <- renderUI({
    wellPanel(fluidRow(
      h4(
        "Welcome to the DHIS2 Shiny Demo. Please login with admin/district:"
      )
    ),
    fluidRow(
      textInput("user_name", "Username: ", width = "600px"),
      passwordInput("password", "Password:", width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })
  
  output$demo_plot <- renderPlot({ 
    
    d<-demo_data()
    
    if (!inherits(d,"error") & !is.null(d)){

      demoPlot(d)

      
    } else {
      NULL
    }
    
  },height = 600,width = 800)
  
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(fluidRow(
        column(
          width = 2,
          offset = 5,
          br(),
          br(),
          br(),
          br(),
          uiOutput("uiLogin"),
          uiOutput("pass")
        )
      ))
    } else {
      
      fluidPage(tags$head(
        tags$style(
          ".shiny-notification {
        position: fixed;
        top: 10%;
        left: 33%;
        right: 33%;}"
        )
      ),
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "side-panel",
          width = 2),
        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel("Data", DT::dataTableOutput("demo_table")),
          tabPanel("Pivot",rpivotTableOutput({"pivot"})),
          tabPanel("Plot",plotOutput("demo_plot"))
        ))
      ))
    }
    
  })
  
})