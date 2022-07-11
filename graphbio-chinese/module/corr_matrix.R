#!/user/bin/Rscript
#roc curve
#shiny module

library(ggcorrplot)

corrmUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="相关性热图",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包ggcorrplot。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggcorrplot包。")
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
          tags$h5("上传文件(支持csv、txt、xls、xlsx)"),
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
               choices = paste0("color", 1:2),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "下载图片宽度", value = 8),
          numericInput(ns("h"), label = "下载图片高度", value = 8),
          numericInput(ns("ppi"), label = "图像分辨率", value = 72),
                  dropdownButton(
                    downloadBttn(
                      outputId = ns("pdf"),
                      label="PDF图片",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("png"),
                      label="PNG图片",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("jpeg"),
                      label="JPEG图片",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("tiff"),
                      label="TIFF图片",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    circle=FALSE,
                    label="下载图片",
                    status="success"
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
        if(file_ext(input$file1$datapath) == "csv"){
          d=read.table(input$file1$datapath,header=T,sep=",",row.names=1,comment.char="",quote="",check.names=FALSE)
        }else if(file_ext(input$file1$datapath) == "txt"){
          d=read.table(input$file1$datapath,header=T,sep="\t",row.names=1,comment.char="",quote="",check.names=FALSE)
        }else if(file_ext(input$file1$datapath) == "xls"){
                d=readxl::read_xls(input$file1$datapath)
                d=as.data.frame(d)
                rownames(d)=d[,1]
                d=d[,-1]
            }else if(file_ext(input$file1$datapath) == "xlsx"){
                d=readxl::read_xlsx(input$file1$datapath)
                d=as.data.frame(d)
                rownames(d)=d[,1]
                d=d[,-1]
            }
          corr <- round(cor(d), 2) # data frame
          if(input$color == "color1"){
            p=ggcorrplot(corr, hc.order = TRUE, type = "lower",
               outline.col = "white", colors = c("#6D9EC1", "white", "#E46726"),lab = T,lab_size = input$labelsize)
          }else if(input$color == "color2"){
            p=ggcorrplot(corr, hc.order = TRUE, type = "lower",
               outline.col = "white", lab = T,lab_size = input$labelsize)
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
          }else if(input$color == "color2"){
            p=ggcorrplot(corr, hc.order = TRUE, type = "lower",
               outline.col = "white", lab = T,lab_size = input$labelsize)
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
      output$png <- downloadHandler(
        filename="corrMplot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="corrMplot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="corrMplot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
