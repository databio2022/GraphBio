#!/user/bin/Rscript
#roc curve
#shiny module

library(ggcorrplot)

corrmUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="ROC曲线",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8,
                    tags$hr(),
                    tags$h6("该工具使用了R包ggcorrplot。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggcorrplot包。")
            ),
        box(width=4,
          # Input: Select a file ----
          actionBttn(
             inputId = ns("rune"),
             label = "运行例子",
             style = "fill", 
              color = "warning",
              size = "sm"
          ),  
          tags$hr(),                
          tags$h5("上传文件(csv格式或逗号分隔txt文件)"),
          actionBttn(
             inputId = ns("show"),
             label = "查看示例文件",
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
          numericInput(ns("labelsize"), label = "相关系数字体大小", value = 2.5),
          pickerInput(
               inputId = ns("color"),
               label = "选择颜色", 
               choices = paste0("color", 1),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "下载图片宽度", value = 8),
          numericInput(ns("h"), label = "下载图片高度", value = 8),
          downloadBttn(
            outputId = ns("pdf"),
            label="下载PDF图片",
            style = "fill",
            color = "success",
            size='sm'
          )
        )
    )
  )
}

corrmServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/heatmap_test.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE)
        modalDialog(
          span('基因表达矩阵，行为基因，列为样本'),
          span('注意：上传文件为逗号分隔文件（可用excel保存为csv格式，不是csv UTF-8!)!'),
          tags$hr(),
          renderTable(d[1:10,],rownames=TRUE),
          easyClose=TRUE,
          size="l",
          footer = tagList(
            modalButton("关闭")
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
          d=read.table(input$file1$datapath,header=T,sep=",",row.names=1,comment.char="",quote="",check.names=FALSE)
          corr <- round(cor(d), 2) # data frame
          if(input$color == "color1"){
            p=ggcorrplot(corr, hc.order = TRUE, type = "lower",
               outline.col = "white", colors = c("#6D9EC1", "white", "#E46726"),lab = T,lab_size = input$labelsize)
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          d=read.table("./www/heatmap_test.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE)
          corr <- round(cor(d), 2) # data frame
          if(input$color == "color1"){
            p=ggcorrplot(corr, hc.order = TRUE, type = "lower",
               outline.col = "white", colors = c("#6D9EC1", "white", "#E46726"),lab = T,lab_size = input$labelsize)
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
              plota()
        })
      })
      #download pdf figure
      output$pdf <- downloadHandler(
        filename="corrMplot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)     
          dev.off()
        }
      )
    }
  )    
}
