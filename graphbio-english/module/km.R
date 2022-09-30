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
        box(title="Survival Curves",solidHeader=TRUE,status='primary',background = "white",
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
          tags$h5("Upload a csv file(also support txt,xls,xlsx)"),
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
               choices = paste0("color", 1:3),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "Figure Width", value = 8),
          numericInput(ns("h"), label = "Figure Height", value = 8),
          numericInput(ns("ppi"), label = "Figure Resolution", value = 72),
                  dropdownButton(
                    downloadBttn(
                      outputId = ns("pdf"),
                      label="PDF figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("png"),
                      label="PNG figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("jpeg"),
                      label="JPEG figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("tiff"),
                      label="TIFF figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    circle=FALSE,
                    label="Download Figure",
                    status="success"
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
          span('First column represents class,second column represents survival time，third column represents survival status.Only support two groups comparsion.'),
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
        if(file_ext(input$file1$datapath) == "csv"){
          d=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE)
        }else if(file_ext(input$file1$datapath) == "txt"){
          d=read.table(input$file1$datapath,header=T,sep="\t",comment.char="",quote="",check.names=FALSE)
        }else if(file_ext(input$file1$datapath) == "xls"){
          d=readxl::read_xls(input$file1$datapath)
          d=as.data.frame(d)
        }else if(file_ext(input$file1$datapath) == "xlsx"){
          d=readxl::read_xlsx(input$file1$datapath)
          d=as.data.frame(d)
        }
          fit <- survfit(Surv(time, status) ~ sex, data = lung)
          if(input$color == "color1"){
                p=ggsurvplot(fit,pval = TRUE)
          }else if(input$color == "color2"){
                p=ggsurvplot(fit,pval = TRUE,palette="npg")
          }else if(input$color == "color3"){
                p=ggsurvplot(fit,pval = TRUE,palette="jco")
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
        }else if(input$color == "color2"){
                p=ggsurvplot(fit,pval = TRUE,palette="npg")
        }else if(input$color == "color3"){
                p=ggsurvplot(fit,pval = TRUE,palette="jco")
        }
        vals$p=p
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
      output$png <- downloadHandler(
        filename="survplot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="survplot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="survplot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
