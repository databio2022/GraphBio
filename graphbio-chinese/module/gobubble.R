#!/user/bin/Rscript
#roc curve
#shiny module

library(ggplot2)

gobubbleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="气泡图",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包ggplot2。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2包。")
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
          pickerInput(
               inputId = ns("color"),
               label = "选择颜色", 
               choices = paste0("color", 1:3),
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

gobubbleServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/go_bubble_example.csv",header=TRUE,sep=",",check.names=FALSE)
        names(d)=c("godesc","FDR","gene_ratio","gene_number")
        modalDialog(
          span('GO或KEGG富集分析结果。第一列为go名称，第2列为FDR，第3列为基因比例，第4列为基因数目。'),
          span('注意：上传文件为逗号分隔文件（可用excel保存为csv格式，不是csv UTF-8!)!'),
          tags$hr(),
          renderTable(d,rownames=FALSE),
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
          names(d)=c("godesc","FDR","gene_ratio","gene_number")
          # Most basic bubble plot
          space=(max(d$gene_ratio)-min(d$gene_ratio))/nrow(d)
          if(input$color == "color1"){
            p=ggplot(d,aes(x=gene_ratio, y=godesc, size=gene_number, color=FDR)) +
                scale_color_gradient(high="#21908CFF",low="orange")+
                geom_point(alpha=0.8)+theme_bw(base_size=12)+ylab("")+
                scale_size(range=c(5,10))+xlim(min(d$gene_ratio)-space,max(d$gene_ratio)+space)
          }else if (input$color == "color2") {
            p=ggplot(d,aes(x=gene_ratio, y=godesc, size=gene_number, color=FDR)) +
                scale_color_gradient(high="blue",low="red")+
                geom_point(alpha=0.8)+theme_bw(base_size=12)+ylab("")+
                scale_size(range=c(5,10))+xlim(min(d$gene_ratio)-space,max(d$gene_ratio)+space)             
          }else if (input$color == "color3") {
            p=ggplot(d,aes(x=gene_ratio, y=godesc, size=gene_number, color=FDR)) +
                scale_color_gradient(high="navy",low="firebrick3")+
                geom_point(alpha=0.8)+theme_bw(base_size=12)+ylab("")+
                scale_size(range=c(5,10))+xlim(min(d$gene_ratio)-space,max(d$gene_ratio)+space)             
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          d=read.table("./www/go_bubble_example.csv",header=TRUE,sep=",",check.names=FALSE)
          names(d)=c("godesc","FDR","gene_ratio","gene_number")
          # Most basic bubble plot
          space=(max(d$gene_ratio)-min(d$gene_ratio))/nrow(d)
          if(input$color == "color1"){
            p=ggplot(d,aes(x=gene_ratio, y=godesc, size=gene_number, color=FDR)) +
                scale_color_gradient(high="#21908CFF",low="orange")+
                geom_point(alpha=0.8)+theme_bw(base_size=12)+ylab("")+
                scale_size(range=c(5,10))+xlim(min(d$gene_ratio)-space,max(d$gene_ratio)+space)
          }else if (input$color == "color2") {
            p=ggplot(d,aes(x=gene_ratio, y=godesc, size=gene_number, color=FDR)) +
                scale_color_gradient(high="blue",low="red")+
                geom_point(alpha=0.8)+theme_bw(base_size=12)+ylab("")+
                scale_size(range=c(5,10))+xlim(min(d$gene_ratio)-space,max(d$gene_ratio)+space)             
          }else if (input$color == "color3") {
            p=ggplot(d,aes(x=gene_ratio, y=godesc, size=gene_number, color=FDR)) +
                scale_color_gradient(high="navy",low="firebrick3")+
                geom_point(alpha=0.8)+theme_bw(base_size=12)+ylab("")+
                scale_size(range=c(5,10))+xlim(min(d$gene_ratio)-space,max(d$gene_ratio)+space)             
          }
          vas$p=p
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
        filename="gobubble_plot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)     
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="gobubble_plot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="gobubble_plot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="gobubble_plot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      
    }
  )    
}
