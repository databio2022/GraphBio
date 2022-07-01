#!/user/bin/Rscript
#roc curve
#shiny module

library(pROC)

rocUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="ROC Curves",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包pROC。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和pROC包。")),
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

rocServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=aSAH[,c("outcome","s100b")]
        names(d)=c("observe","predict")
        modalDialog(
          span('First column represents observed values, second column represents predicted values.'),
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
      plota <- reactive({
          d=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE)
          rocobj=roc(d[,1], d[,2],ci=TRUE)
          x=as.character(rocobj$ci)
          textlabel=paste0("AUC: ",round(as.numeric(x[2]),3),"(",round(as.numeric(x[1]),3),"-",round(as.numeric(x[3]),3),")")
          if(input$color == "color1"){
            plot(smooth(rocobj), col="red")
            text(0.28, 0.5, labels=textlabel)
          }
          vals$rocobj=rocobj
          vals$label=textlabel
      })

      #example
      plote <- reactive({
          d=aSAH[,c("outcome","s100b")]
          rocobj=roc(d[,1], d[,2],ci=TRUE)
          x=as.character(rocobj$ci)
          textlabel=paste0("AUC: ",round(as.numeric(x[2]),3),"(",round(as.numeric(x[1]),3),"-",round(as.numeric(x[3]),3),")")
          if(input$color == "color1"){
            plot(smooth(rocobj), col="red")
            text(0.28, 0.5, labels=textlabel)
          }
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
              plota()
        })
      })
      #download pdf figure
      output$pdf <- downloadHandler(
        filename="rocplot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(plot(smooth(vals$rocobj), col="red"))
          text(0.28, 0.5, labels=vals$label)          
          dev.off()
        }
      )
    }
  )    
}
