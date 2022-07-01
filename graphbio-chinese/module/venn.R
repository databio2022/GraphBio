#!/user/bin/Rscript
#venn plot
#up to 5 groups
#shiny module

library(ggvenn)
library(VennDiagram)

vennUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="韦恩图",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8,
                    tags$hr(),
                    tags$h6("该工具使用了R包ggvenn(画<=4组)和VennDiagram(画5组)。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggvenn(画<=4组)和VennDiagram(画5组)包。参考文献："),
                    tags$h6("VennDiagram: a package for the generation of highly-customizable Venn and Euler diagrams in R")),
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
          fileInput(ns("vennfile1"),NULL,
                    multiple = FALSE,
                    accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
          pickerInput(
               inputId = ns("color"),
               label = "选择颜色", 
               choices = paste0("color", 1:3),
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

vennServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/venn_example.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        d[is.na(d)] <- ""
        modalDialog(
          span('注意：上传文件为逗号分隔文件（可用excel保存为csv格式，不是csv UTF-8!),目前最多支持4组交集!'),
          tags$hr(),
          renderTable(d,rownames=FALSE),
          easyClose=TRUE,
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
      plot <- reactive({
        d=read.table(input$vennfile1$datapath,
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        if(is.numeric(d[,1])){
          group=names(d)
          mergegene=list()
          for(i in 1:length(group)){
            mergegene[[group[i]]]=d[!is.na(d[,i]),i]
          }
          if(input$color == "color1"){
            p=ggvenn(
              mergegene,
              fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
              stroke_size = 0.5, set_name_size = 4
            )
          }else if(input$color == "color2"){
            p=ggvenn(
              mergegene,
              fill_color = c("blue", "yellow", "green", "red"),
              stroke_size = 0.5, set_name_size = 4
            )
          }else if (input$color == "color3"){
            p=ggvenn(
              mergegene,
              fill_color = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
              stroke_size = 0.5, set_name_size = 4
            )
          }
          vals$p=p
          p
        }else if(is.character(d[,1])){
          group=names(d)
          mergegene=list()
          for(i in 1:length(group)){
            mergegene[[group[i]]]=d[d[,i]!="",i]
          }
          if(input$color == "color1"){
            p=ggvenn(
              mergegene,
              fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
              stroke_size = 0.5, set_name_size = 4
            )
          }else if(input$color == "color2"){
            p=ggvenn(
              mergegene,
              fill_color = c("blue", "yellow", "green", "red"),
              stroke_size = 0.5, set_name_size = 4
            )
          }else if (input$color == "color3"){
            p=ggvenn(
              mergegene,
              fill_color = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
              stroke_size = 0.5, set_name_size = 4
            )
          }
          vals$p=p
          p          
        }
      })

      #example
      plote <- reactive({
        d=read.table("./www/venn_example.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        group=names(d)
        mergegene=list()
        for(i in 1:length(group)){
          mergegene[[group[i]]]=d[!is.na(d[,i]),i]
        }
        if(input$color == "color1"){
          p=ggvenn(
            mergegene,
            fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
            stroke_size = 0.5, set_name_size = 4
          )
        }else if(input$color == "color2"){
          p=ggvenn(
            mergegene,
            fill_color = c("blue", "yellow", "green", "red"),
            stroke_size = 0.5, set_name_size = 4
          )
        }else if (input$color == "color3"){
          p=ggvenn(
            mergegene,
            fill_color = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3"),
            stroke_size = 0.5, set_name_size = 4
          )
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
      observeEvent(input$vennfile1, {
        output$plot <- renderPlot({  
              plot()
        })
      })
      #download pdf figure
      output$pdf <- downloadHandler(
        filename="venn.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h)
          print(vals$p)
          dev.off()
        }
      )
    }
  )    
}
