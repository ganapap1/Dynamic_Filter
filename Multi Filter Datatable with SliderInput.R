#################################################################################
#Loading required libraries
#################################################################################
library(shiny)
library(shinydashboard)  # for Dashboard
library(shinydashboardPlus)
library(shinyWidgets)    # for sendSweetAlert function
library(shinyalert)      # for alert message very nice format
library(DT)              # for using %>% which works as a pipe in R code
library(shinyjs)         # for DT:: datatable output and render
library(dplyr)           # select functions are covered in the library it is used while selecting and deleting a row

#################################################################################
#Actionbutton style function height 30px and width 155px
#################################################################################
styleButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:3px;
                        height:30px;
                        width:155px;
                        font-size: 13px;"
}

#################################################################################
#Actionbutton style function 50px and width 150px
#################################################################################
styleTallButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:2px;
                        height:50px;
                        width:150px;
                        font-size: 13px;"
}

#################################################################################
# header which is part of ui 
#################################################################################
header <- dashboardHeaderPlus(
  title = NULL,
  tags$li(
    a(
      splitLayout(
        cellWidths = c("50%", "50%"),
        actionButton(inputId = "mFileImport", label = "Upload Dataset..!",style=styleButtonBlue()),
        actionButton(inputId = "mFileModify", label = "Modify Dataset..!",style=styleButtonBlue())
      ),
      href = NULL,
      style = "cursor: pointer;height:15px;"
    ),
    class = "dropdown"
    
  )
)

#################################################################################
# rightsidebar and sidebar which is part of ui 
#################################################################################
rightsidebar <- rightSidebar()

sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    align='left',
    actionButton(inputId = "mdummy",label = HTML(
      paste(
        '<p text-align ="center"><h4 id="heading"><b><i>',
        "D Y N A M I C",
        " :: ",
        "M U L T I   F I L T E R",
        '</i></b></p><h5>'
      )
    ), style=
      "white-space: normal;
                        text-align:left;
                        color: #ffffff; 
                        background-color:black;
                        border-color: black;
                       # border-width:3px;
                        height:550px;
                        width:20px;
                        font-size: 15px;"
    )
  )#sidebar menu
)



#################################################################################
# dashboard body and sidebar which is part of ui 
#################################################################################
body <- dashboardBody(
  useShinyalert(),
  shinyjs::useShinyjs(),

  box(
    width = 12,
    height='1075px',
    align = "center",
    tags$head(
      tags$style(HTML("
                      .box { height: 90vh; overflow-y: auto; }
                      " )
      )
    ),
    box  (
      id = "slidebarbox206",
      width = 6,
      height ='500px',
      align ='center',
      title = 'Play with Slider & PickerInput & Filter Data',
      status = "warning",
      solidHeader = TRUE,
      collapsible = FALSE,
      awesomeCheckbox(inputId = "mRedesignSliders",label = "Impact ALL Filters",value = FALSE),
      uiOutput(outputId = "muimultisliderplay"),
      box(width = 12,height = 2),
      uiOutput(outputId = "multislidertext")
    ),#box closrue slider input
    

    box(
      id = "slidebarbox207",
      width = 6,
      height ='500px',
      title = HTML(paste('Dataset Filtered',downloadButton("mDownloaddtdfBtn", "Download CSV", style = styleButtonBlue()))),
      status = "warning",
      solidHeader = TRUE,
      collapsible = FALSE,
      HTML('<h6>'),
      DT::dataTableOutput('tblmultifilter', height = 400),
      HTML('<h5>')
    ), #box closure

    box(
      id = "slidebarbox208",
      width = 12,
      height ='500px',
      title = 'Section of Numeric and / or Character Variables',
      status = "primary",
      solidHeader = TRUE,
      collapsible = FALSE,
      br(),
      column(width = 6,
             selectInput(inputId = 'mNumVarFilter',
                         label = "Select Numeric Variable to Filter",
                         choices = NULL,selected = NULL,
                         multiple = TRUE
             )
             ),
 
      column(width = 6,
             selectInput(inputId = 'mChrVarFilter',
                         label = "Select Character Variable to Filter",
                         choices = NULL,selected = NULL,
                         multiple = TRUE)
      ),
      column(width = 12,
        br(),
        # Horizontal line ----
        #tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
        splitLayout(cellWidths = c("50%","50%"),
                    actionButton(inputId = 'mselectAllNum',label = "Select ALL Numeric fields",style = styleTallButtonBlue()),
                    actionButton(inputId = 'mselectAllChar',label = "Select ALL Character fields",style = styleTallButtonBlue())
        )
      )
    )#box closure
  )#column closure
) # dashboardBody closure


ui <- dashboardPagePlus(
  shinyjs::useShinyjs(),
  header = header,
  sidebar = sidebar,
  body = body,
  rightsidebar = rightsidebar
)



#################################################################################
# server function starts here 
#################################################################################

server <- function(input, output, session) {
  ##this is to hide right side bar
  shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  inserted <- c()
  slidercolrange <- -2
  
  vmy <- reactiveValues(mydata=NULL,data_1=NULL)
  
  #################################################################################
  # File dataset upload code starts here
  #################################################################################
  
  observeEvent(input$mFileImport,{
    showModal(
      modalDialog(
        size = 's',
        column(   
          width = 12,
          offset = 0,
          align = "center",
          fluidRow(
            box(
              width = 12,
              height = 375,
              align = "center",
              title = "Uplaod Dataset",
              status = "warning",
              solidHeader = TRUE,
              collapsible = FALSE,
              # Input: Select a file ----
              fileInput("file",
                        label = "Select: csv, xls, xlsx, rds ",
                        multiple = FALSE,
                        accept = c("text/csv/txt/Excel",
                                   "text/comma-separated-values,text/plain/excel",
                                   ".csv",".xls",".xlsx",".rds")),
              
              # Horizontal line ----
              #tags$hr(),
              column(
                width = 5,
                offset = 1,
                align = "left",
                fluidRow(
                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header", TRUE),
                  
                  # Input: Select separator ----
                  radioButtons("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ",")
                )
              ),
              column(
                width = 5,
                offset = 0,
                align = "left",
                fluidRow(
                  br(),
                  br(),
                  # Input: Select quotes ----
                  radioButtons("quote", "Quote",
                               choices = c(None = "",
                                           "Double Qot." = '"',
                                           "Single Qot." = "'"),
                               selected = '"')
                )
              )
            ), #box closure
            HTML("click outside this box to exit")
          )# fluidRow closure
          
        ), #column
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$file,{
    ext <- tools::file_ext(input$file$name)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    if (ext == "rds"){
      vmy$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv"){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop where you got:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        
        vmy$mydata <- as.data.frame(read.csv(input$file$datapath,
                                             header = input$header,
                                             sep = input$sep,
                                             quote = input$quote)
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    else{
      shinyalert("Oops!", "valid files are only csv, excel or rds", type = "error")
      return()
    }
    removeModal()
    mnumericcolname <-  names(dplyr::select_if(vmy$mydata,is.numeric)) 
    for (i in mnumericcolname){
      vmy$mydata[i] <- round(vmy$mydata[i],2)
    }
    vmy$mydata <- na.omit(vmy$mydata)
    vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
    row.names(vmy$mydata) <- 1:nrow(vmy$mydata)
    
    fnupdatevarchoice()
    fncreatedftype()
    updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
    disable("mRedesignSliders")
  })
  
  #################################################################################
  # File dataset upload code ENDS here
  #################################################################################
  
  
  
  fnupdatevarchoice <- function(){
    updateSelectInput(session,inputId = "mNumVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.numeric)) )
    updateSelectInput(session,inputId = "mChrVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.character)) )
  }
  
  
  observeEvent(input$mselectAllNum,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    if (length(names(dplyr::select_if(vmy$mydata,is.numeric)))==0){
      shinyalert("Oops!", "There is no NUMERIC field in this dataset ...!", type = "error")
      return()
    }
    updateSelectInput(session,inputId = "mNumVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.numeric)),selected = names(dplyr::select_if(vmy$mydata,is.numeric)) )
  })
  
  
  observeEvent(input$mselectAllChar,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    if (length(names(dplyr::select_if(vmy$mydata,is.character)))==0){
      shinyalert("Oops!", "There is no CHARACTER field in this dataset ...!", type = "error")
      return()
    }
    updateSelectInput(session,inputId = "mChrVarFilter",choices = names(dplyr::select_if(vmy$mydata,is.character)),selected =  names(dplyr::select_if(vmy$mydata,is.character)) )
  })
  
  
  #################################################################################
  # File dataset MODIFY code starts here
  #################################################################################
  
  fncreatedftype <- function(){
    vmy$df_types <- data.frame("col_types" = unlist(lapply(vmy$mydata, typeof)))
    vmy$df_types$Var_name <- rownames(vmy$df_types)
    row.names(vmy$df_types) <- NULL
    vmy$df_types <-vmy$df_types %>% dplyr::select(-col_types, everything())
  }

  
  observeEvent(input$mFileModify,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    
    showModal(
      modalDialog(
        size = 's',
        column(
          width = 12,
          offset = 0,
          align = "center",
          fluidRow(
            box(
              width = 12,
              height = 425,
              align = "center",
              title = "Data Structure",
              status = "warning",
              solidHeader = TRUE,
              collapsible = FALSE,
              DT::dataTableOutput("dt",height = 260),
              tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
              useShinyjs(),
              extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
              textOutput("mselectedvariable"),
              br(),
              actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable",style = styleTallButtonBlue())
            ) #box closure
          )# fluidRow closure
          
        ), #column
        easyClose = TRUE
      )
    )
  })
  

  output$dt <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "single", target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(dom = 't',ordering=FALSE, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",  
                                   "}")
                                 
                  ) 
                  
    )
    
  })
  
  
  output$mselectedvariable <-  renderText({
    if(length(input$dt_cell_clicked) != 0){
      clicked_list <- input$dt_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],'\n',"Type:",vmy$df_types[clicked_list$row,2]))
      
    }
  })
  #################################################################################
  # File dataset MODIFY code ENDS here
  #################################################################################
  
  
  
  
  
  #################################################################################
  # File dataset DELETE VARIABLE code starts here
  #################################################################################
  
  ### delete selected column
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    temp <- select(vmy$mydata,-vmy$df_types[input$dt_cell_clicked$row,1])
    vmy$mydata <- temp
    removeModal()
    temp2 <- subset(vmy$df_types, Var_name!=vmy$df_types[input$dt_cell_clicked$row,1] )
    vmy$df_types <- temp2
    fnupdatevarchoice()
  })
  
  #################################################################################
  # File dataset DELETE VARIABLE code ENDS here
  #################################################################################
  
  
  
  
  output$tblmultifilter <- DT::renderDataTable({
    dtdftemp <- vmy$data_1()
    vmy$dtdf <- dtdftemp
    DT::datatable(vmy$dtdf,
                  class ='cell-border stripe compact white-space: nowrap', 
                  escape= FALSE,
                  rownames = TRUE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(
                    lengthMenu = list(c(15, 25, 50,-1), c('15', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = FALSE,
                    ordering = FALSE
                  )
    ) 
  })
  
  my.styleallrows <- function(.) formatStyle(., columns=0, target= 'row',color = 'black', 
                                             backgroundColor = '#ffffed',
                                             fontWeight ='normal',lineHeight='75%')
  my.styleonecolumn <- function(.) formatStyle(., columns=c("var_name"), target= 'cell',color = 'black', 
                                               backgroundColor = '#ffffed',
                                               fontWeight ='bold',lineHeight='70%')
  
  observeEvent(input$mNumVarFilter,{
    disable("mRedesignSliders")
    updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
    enable("mRedesignSliders")
  }) 
  
  observeEvent(input$mChrVarFilter,{
    disable("mRedesignSliders")
    updateAwesomeCheckbox(session,inputId = 'mRedesignSliders',value = FALSE)
    enable("mRedesignSliders")
  }) 
  
  
  #######- above multi table datatable end
  
  output$muimultisliderplay <- renderUI({
    tryCatch({
      if (length(input$mNumVarFilter) != 0){
        slider_options <- input$mNumVarFilter
      }
      else{
        return()
      }
      
      # First, create a list of sliders each with a different name
      sliders <- lapply(1:length(slider_options), function(i) {
        if (slidercolrange==12){
          slidercolrange <- 1
        }
        else{
          slidercolrange <- slidercolrange ++ 4
        }
        inputName1A <- slider_options[i]
        
        if (input$mRedesignSliders == TRUE){
          column(slidercolrange+4,sliderInput(inputId = inputName1A, label = inputName1A, 
                                              min=min(vmy$data_1()[,inputName1A],na.rm = TRUE), 
                                              max=max(vmy$data_1()[,inputName1A],na.rm = TRUE), 
                                              value=c(min(vmy$data_1()[[inputName1A]],na.rm = TRUE),max(vmy$data_1()[[inputName1A]],na.rm = TRUE)),
                                              width = "600px"))
        }
        else{
          column(slidercolrange+4,sliderInput(inputId = inputName1A, label = inputName1A,
                                              min=min(vmy$mydata[,inputName1A],na.rm = TRUE),
                                              max=max(vmy$mydata[,inputName1A],na.rm = TRUE),
                                              value=c(min(vmy$mydata[[inputName1A]],na.rm = TRUE),max(vmy$mydata[[inputName1A]],na.rm = TRUE)),
                                              width = "600px"))
        }  
        
        
      })
      # Create a tagList of sliders (this is important)
      do.call(tagList, sliders)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  })
  
  
  
  output$multislidertext <- renderUI({
    tryCatch({
      if (length(input$mChrVarFilter) != 0){
        slider_optionsTXT <- input$mChrVarFilter
      }
      else{
        return()
      }
      
      # First, create a list of sliders each with a different name
      sliders <- lapply(1:length(slider_optionsTXT), function(i) {
        if (slidercolrange==12){
          slidercolrange <- 1
        }
        else{
          slidercolrange <- slidercolrange ++ 4
        }
        inputName1ATXT <- slider_optionsTXT[i]
        
        if (input$mRedesignSliders == TRUE){
          mchoice <- as.list(unlist(t(distinct(vmy$data_1()[inputName1ATXT]))))
        }
        else{
          mchoice <- as.list(unlist(t(distinct(vmy$mydata[inputName1ATXT]))))
        }
        
        column(slidercolrange+4,pickerInput(inputId = inputName1ATXT, label = inputName1ATXT, choices =mchoice,selected =mchoice ,multiple = TRUE,
                                            options = list('actions-box' = TRUE), width = "200px")) 
        
      })
      # Create a tagList of sliders (this is important)
      do.call(tagList, sliders)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  })
  
  
  vmy$data_1 <-reactive({
    tryCatch({ 
      data_ <- vmy$mydata
      slider_options <- slider_options <- input$mNumVarFilter
      # this is how you fetch the input variables from ui 
      for(i in slider_options) {
        
        xxtt<-as.double(eval(parse(text=paste0("input$",i))))
        data_ <- data_[data_[[i]] <= xxtt[2] &                       
                         data_[[i]] >= xxtt[1],]
        
      }
      
      slider_optionsTXT <-  input$mChrVarFilter 
      # this is how you fetch the input variables from ui component component character fields
      for(i in slider_optionsTXT) {
        
        xxttTXT<-eval(parse(text=paste0("input$",i)))
        data_ <- data_[data_[[i]] %in%  xxttTXT,]
      }
      data_
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    return(data_)
  })
  
  output$mDownloaddtdfBtn<- downloadHandler(
    filename = function() {
      paste("multimodeldf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vmy$data_1()[input[["tblmultifilter_rows_all"]],2:17 ]), file, row.names = FALSE)
    }
  )
  
  
 
} #server closure
shinyApp(ui, server)


