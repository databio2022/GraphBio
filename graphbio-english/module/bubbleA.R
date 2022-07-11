#!/user/bin/Rscript
#venn plot
#up to 5 groups
#shiny module

library(r2d3)

bubbleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="Dot Plot",solidHeader=TRUE,status='primary',background = "white",
             d3Output(ns("plot"),height=1000) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8),
                    #tags$hr(),
                    #tags$h6("该工具使用了R包r2d3。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和r2d3包。")),
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
          #pickerInput(
          #     inputId = ns("color"),
          #     label = "选择颜色", 
          #     choices = paste0("color", 1:3),
          #     multiple = FALSE,
          #     selected = "color1"
          #  ),
          downloadBttn(
            outputId = ns("pdf"),
            label="Download HTML File",
            style = "fill",
            color = "success",
            size='sm'
          )
        #  tags$hr(),
        #  tags$h5("如何从下载的html文件生成svg矢量图，请关注我们视频号，查看教程！")
        )
    )
  )
}

bubbleServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/go_term_bubble.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        d[is.na(d)] <- ""
        modalDialog(
          span('1. The figure can show all siginificant GO terms.The Circles are bigger,then significance is higher.'),
          tags$br(),
          span('2. The example file format：first column represents GO terms, second column represents -log10(FDR) and requires a decreasing order,third column represents text lables to be presented on the Figure.
          The table headers must keep the same with example file.'),
          tags$hr(),
          renderTable(d[1:15,],rownames=FALSE),
          easyClose=TRUE,
          size="l",
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
      output$plot <- renderD3({
        NULL
      })

      # The user's data, parsed into a data frame
      vals=reactiveValues()
      plot <- reactive({
        if(file_ext(input$file1$datapath) == "csv"){
          d=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        }else if(file_ext(input$file1$datapath) == "txt"){
          d=read.table(input$file1$datapath,header=T,sep="\t",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        }else if(file_ext(input$file1$datapath) == "xls"){
          d=readxl::read_xls(input$file1$datapath)
          d=as.data.frame(d)
        }else if(file_ext(input$file1$datapath) == "xlsx"){
          d=readxl::read_xlsx(input$file1$datapath)
          d=as.data.frame(d)
        }
        d3=r2d3(data = d, d3_version = 3, script = "./js/d3js/my_bubble.js")
        vals$p=d3
        d3
      })

      #example
      plote <- reactive({
        r2d3(data = read.csv("./www/go_term_bubble.csv"), d3_version = 3, script = "./js/d3js/my_bubble.js")
      })

      # Example
      observeEvent(input$rune, {
        output$plot <- renderD3({  
              plote()
        })
      })

      # inputfile1
      observeEvent(input$file1, {
        output$plot <- renderD3({  
              plot()
        })
      })
      #download pdf figure
      output$pdf <- downloadHandler(
        filename="bubbleA.html",
        content = function(file){
            save_d3_html(vals$p,file)
        }
      )
    }
  )    
}
