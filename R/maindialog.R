#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


DIALOG_env <-   new.env(parent = emptyenv())
DIALOG_env$MAINACTIONS <- list()
DIALOG_env$FILES <- "No File"
DIALOG_env$FILE <- ""
DIALOG_env$PERIOD <- ""
DIALOG_env$COUNTRIES <- list("FR","UK")
DIALOG_env$ACTION <- ""
DIALOG_env$COUNTRY <- ""

#' getDialog  get a value from the dialog system
#'
#' @param value The name of the value to retrieve
#'              If NULL then the dialog env is returned 
#'
#' @return either a value or an Env
#' @export
#'
getDialog <- function(value=NULL) {
  if (! is.null(value)) { 
    result <- get(value,envir = DIALOG_env)
  } else result <- DIALOG_env  
  return(result)
}

#' setDialog
#'
#' @param dialog  A name corresponding to a variable in the dialog Env 
#' @param value   The value to assign to that variable
#'
#' @return Nothing
#' @export
#'
setDialog <-  function(dialog,value=NULL){
  if (! is.null(value)) { 
    assign(dialog ,value, DIALOG_env)
  } else DIALOG_env <- dialog 
}




#' updatefilelist  Used internally by maindialog to refresh file list
#'
#' @return The list of file extracted from datasource for the given criteria
#'         criteria are read from dialog values
#' @export
#'
updatefilelist <- function() {
  action <- getDialog("ACTION")
  country <- getDialog("COUNTRY")
  status <- ""
  if (action=="GETNEWFILE") {
    result <- searchNewFiles(country,country)
  } else {
    if (action=="IMPORT") {
      status <- "NEW"
    }
    if (action=="CHECK") {
      status <- "IMPORTED"
    }
    if (action=="VALIDATE") {
      status <- "CHECKED"
    }
    
    result <-  getFileList(country,status=status)
    result <- result$FileName
  }
  if (is.null(result)| length(result) == 0) result <- "No Files"
  setDialog("FILES",result)
}


# 

# 
#' maindialog Run a dialog to drive the project
#'
#' @return Run the application (return a shiny R object to print() )
#' @export
#' @import shiny
#'
maindialog <- function() {
  
  # Define UI for application 
  # @import shiny
  ui <- fluidPage(
    
    # Application title
    titlePanel("STRAP Flow control"),
    hr(),
    helpText("Main menu to run scripts for data management"),
    selectInput("country",label="Select the country :",getDialog("COUNTRIES")),
    # "Select the correct period :",
    #  numericInput("year", "Enter the year :",2022,2000,2023),
    #  numericInput("month", "Enter the month :",01,1,12),
    #    fileInput("dataset", label = "File to import"),
    #    textOutput("summary"),
    #    textOutput("nbrecords"),
    
    br(),
    selectInput("action",
                label = "Select an action to lauch",
                choices = c("Get new files"="GETNEWFILE","Import data"="IMPORT","Check data"="CHECK",
                            "Run validation process"="VALIDATE")),
    
    selectInput("filelist",label="Select the file to process :",choices=getDialog("FILES")),
    #    textOutput("selected_event"),
    helpText("After selecting country and action, pick a file name in the list and click submit to run."),
    helpText("Pressing Reset button restore datasource list in it original version cancelling all action."),
    helpText("To save datasource list and quit, use OK Button."),
    fluidRow(align="center",
             splitLayout(       
               actionButton('reset',"Reset"),
               actionButton('submit',"Submit"),
               actionButton('ok',"OK")
             )
    )
    
  )
  
  # Define server logic it is run for each new visitor, it initialise the interface 
  server <- function(input, output, session) {
    #  output$text <- renderText({ "text"; input$txt })
    # This part is run once at beginning 
    r <- reactiveValues()
    r$submitrun = FALSE
    r$doreset = FALSE
    
    setDialog("EXIT",FALSE)
    
    # output$summary <- renderPrint({
    #   # this part is run at beginning and every time something change, it update the graphic interface
    #   input$dataset
    # })
    
    # output$selected_event <- renderText({
    #   paste("You have selected this",input$action)
    # })
    
    observeEvent(input$submit, {
      # this part is run when the associated value changes
      r$submitrun <- TRUE
    })
    
    observeEvent(input$reset, {
      r$doreset <- TRUE
    })
    
    observeEvent(input$action, {
      setDialog("ACTION",input$action)
      setDialog("COUNTRY",input$country)
      setDialog("FILE",input$filelist)
      # after any change we update the file list
      do.call(updatefilelist,list())
      result <- getDialog()
      updateSelectInput(session,"filelist",choices=result$FILES)
    })
    
    observeEvent(input$country, {
      setDialog("COUNTRY",input$country)
    })

    observeEvent(input$filelist, {
      setDialog("FILE",input$filelist)
    })

     observeEvent(input$okmod, {
      # if ok remove the dialog
      if (! is.null(input$period)) {
        setDialog("PERIOD",input$period)
        removeModal()
      } # else back to the showModal()
    })
    
    observe({
      # this part is run everytime something change
      if(r$submitrun){
        if (input$action=="IMPORT") {
          # we open a modal dialog to input file characteristic before import
          filename <- input$filelist 
          output$filename <- renderText({filename })
          showModal(modalDialog(
            title = "File parameters",
            
            textInput("period", "Choose period for that data set",
                      placeholder = 'Try "yyyy-mm" or "yyyywxx"'
            ),
            "current file : ",
            textOutput("filename"),
            footer=tagList(modalButton("cancel"),
                           actionButton("okmod","OK")
            )
          )) # end of modal dialog
        }
        # we run the selected action by calling a R function called mainaction
        do.call("mainaction",list("action"=input$action,"country"=input$country)) 
        r$submitrun <- FALSE
      }
      if (input$ok) {
        # we press OK then we save the source list to validate all process done during session
        saveSourceList()
        setDialog("EXIT",TRUE)
        stopApp()
      }
      if (r$doreset) {
        # empty string passed to reopen current datasource list 
        # a confirm dialog could be usefull !
        openSourceList("")
        setDialog("FILES","No File")
        updateSelectInput(session,"filelist",choices=getDialog("FILES"))
        r$doreset <- FALSE
      }
      # # after any change we update the file list done in observe event but should be done after other action
      # do.call(updatefilelist,list())
      # result <- getDialog()
      # updateSelectInput(session,"filelist",choices=result$FILES)
      
    })
  }
  
  
  # open the source file dataset 
  openSourceList(pathToFile("SOURCES","datasources.xlsx"))

  runApp(shinyApp(ui = ui, server = server))
}


