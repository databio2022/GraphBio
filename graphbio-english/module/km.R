#!/user/bin/Rscript
#surv km curve
#shiny module

library(ggplot2)
library(survival)
library(survminer)

kmUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="Survial Curves",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包ggplot2、survival和survminer。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2、survival及survminer包。")
        ),
        box(width=4,
          # Input: Select a file ----
          actionBttn(
             inputId = ns("rune"),
             label = "run example",
             style = "fill", 
              color = "warning",
              size = "sm"
          ),  
          tags$hr(),                
          tags$h5("Upload a csv or comma-separated file"),
          actionBttn(
             inputId = ns("show"),
             label = "view example file",
             style = "fill", 
              color = "primary",
              size = "sm"
          ),
          tags$br(),
          tags$br(),
          fileInput(ns("file1"),NULL,
                    multiple = FALSE,
                    accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
          pickerInput(
               inputId = ns("color"),
               label = "Select Colors", 
               choices = paste0("color", 1),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "Figure Width", value = 8),
          numericInput(ns("h"), label = "Figure Height", value = 8),
          downloadBttn(
            outputId = ns("pdf"),
            label="Download PDF Figure",
            style = "fill",
            color = "success",
            size='sm'
          )
        )
    )
  )
}

kmServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=lung[,c(5,2,3)]
        modalDialog(
          span('First column represents class,second column represents survival time，third column represents survival status.'),
          tags$hr(),
          renderTable(d[1:20,],rownames=FALSE),
          easyClose=TRUE,
          footer = tagList(
            modalButton("Close")
          )
        )
      }
      
      # Show modal when button is clicked.
      observeEvent(input$show, {
        showModal(dataModal())
      })
      #init
      output$plot <- renderPlot({
        NULL
      })

      # The user's data, parsed into a data frame
      vals=reactiveValues()
      plot <- reactive({
          d=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE)
          fit <- survfit(Surv(time, status) ~ sex, data = lung)
          if(input$color == "color1"){
                p=ggsurvplot(fit,pval = TRUE)
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
        d=lung
        fit <- survfit(Surv(time, status) ~ sex, data = lung)
        if(input$color == "color1"){
              p=ggsurvplot(fit,pval = TRUE)
        }
        p
      })

      # Example
      observeEvent(input$rune, {
        output$plot <- renderPlot({  
              plote()
        })
      })

      # inputfile1
      observeEvent(input$file1, {
        output$plot <- renderPlot({  
              plot()
        })
      })
      #download pdf figure
      output$pdf <- downloadHandler(
        filename="survplot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)
          dev.off()
        }
      )
    }
  )    
}
