#


library(shiny)
ActionLinkIndex <- NULL
YearCount <- 0
firstCall <- TRUE
savedYear <- 2018

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Test Application"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("graph", "Select Buttons:",
                   c("Test1", 
                     "Test2")
      )
    ),
    mainPanel(
      uiOutput("theUI")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  library(tidyr)
  library(dplyr)
  
  ########################################################
  # Data Creation                                        #
  ########################################################
  
  gReportTable2016 <- data.frame(NAME=c("A", "B", "C"), CH=c(0,1,1), CO=c(0,0,0), M=c(1,0,1))
  gReportTable2018 <- data.frame(NAME=c("ABC"), CH=0, CO=0, M=1)
  gReportTable2017 <- data.frame(NAME=c("DEF", "GHI"), CH=c(0,1), CO=c(0,0), M=c(1,1))
  
  ########################################################
  # Observe Events                                       #
  ########################################################
  
  observeEvent(input$theYear, {
    cat("********************** Entered observeEvent(input$theYear) ********************\n")
    savedYear <- input$theYear
    if(input$theYear=="2016") theTable <<- gReportTable2016  
    if(input$theYear=="2017") theTable <<- gReportTable2017 
    if(input$theYear=="2018") theTable <<- gReportTable2018
    if(firstCall==TRUE){
      firstCall <<- FALSE
    } else{
      for(i in ActionLinkIndex:1){
        cat("     ******************** Destroying", paste0("AL", i), "**********************\n")
        t.observers.new[[i]]$destroy()
      }      
    }
    ActionLinkIndex <<- NULL
    
    tempCount <- YearCount + 1
    assign("YearCount", tempCount, pos=1)
    cat("********************** Leaving observeEvent(input$theYear) ********************\n")
  }, ignoreInit=TRUE)
  create.observers.new <- function(number.of.observers, html.ID, in.data){
    trigger.modal.debug <- function(){
      showModal(modalDialog(
        renderUI({
          tagList(
            h4("Print something")
            
          )
        }),
        title = "Blank Modal Window",
        easyClose = TRUE
      ))}
    number.of.observers <- dim(theTable)[[1]]
    IDs <- seq_len(number.of.observers)  
    t.out <- lapply(IDs, function(i){
      cat("Creating observer:", paste0("AL", i), "\n")
      observeEvent(input[[paste0(html.ID, i)]], trigger.modal.debug(), ignoreNULL=TRUE, suspended=FALSE)
    })
    t.out
  }
  
  ########################################################
  # Create UIs                                           #
  ########################################################
  output$theYearList <- renderUI({
    first.year <- 2016
    last.year <- 2018
    year.list <- c(first.year:last.year)
    t.out <- selectInput("theYear", "Year:", 
                         year.list, selected = savedYear 
    )
    t.out   
  })
  output$testReport <- renderUI({
    f.NewRow <- function(the.data){
      the.rows <- c(1:dim(the.data)[[1]])
      t.out <- vector("list", length(the.rows))
      t.out <- lapply(the.rows, function(i){
        t.out[[i]] <- fluidRow(
          f.details(the.data[i,])
        )
      })
      t.out
    }
    f.details <- function(data){
      setValue <- function(data, index){
        index <- index + 1
        t.out <- list(actionLink(paste0("AL", index), label = paste0("Name:", data$NAME)))
        
        ActionLinkIndex <<- index
        t.out
      }
      
      if(is.null(ActionLinkIndex))
        ActionLinkIndex <<- 0

      t.out <- setValue(data, ActionLinkIndex)
      
      t.out
    }
    
    if(!is.null(input$theYear)){
      t.out <- list(fluidRow(
        f.NewRow(theTable) 
      ))
      t.1 <- create.observers.new(ActionLinkIndex, "AL", theTable) #the.data)
      t.observers.new <<- t.1
    } else t.out <- NULL
    t.out
  })
  output$theUI <- renderUI({
    tagList(
      uiOutput("theYearList"),
      uiOutput("testReport")
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

