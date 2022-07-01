#!/user/bin/Rscript
#ma plot
#shiny module

library(ggplot2)
library(ggpubr)
library(ggrepel)

maUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="MA图",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8,
                    tags$hr(),
                    tags$h6("该工具使用了R包ggplot2。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2包。")),
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

maServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/ma_example.csv",header=T,sep=",",comment.char="",quote="",check.names=FALSE,row.names=1)
        names(d)=c("log2FoldChange","padj","meanfpkm","label")
        modalDialog(
          span('第1列为基因ID，第2列为log2倍数，第3列为校正p值，第4列为平均表达值，第5列为需要标记的基因名字,若不需要标记，此列留空即可。'),
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
          dm=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE,row.names=1)
          names(dm)=c("log2FoldChange","padj","meanfpkm","label")
          dm1=dm[!is.na(dm$padj),]
          dm1$ma_class="none"
          dm1[dm1$log2FoldChange > 0 & dm1$padj <= 0.05,]$ma_class="up"
          dm1[dm1$log2FoldChange < 0 & dm1$padj <= 0.05,]$ma_class="down"
          dm1$ma_class=factor(dm1$ma_class,levels = c("down","none","up"))
          up_num=nrow(dm1[dm1$ma_class == "up",])
          down_num=nrow(dm1[dm1$ma_class == "down",])
          xval=ceiling(max(abs(dm1$log2FoldChange)))
          if(input$color == "color1"){
            p=ggplot(data = dm1, aes(x = log2(meanfpkm), y = log2FoldChange, colour = ma_class,label=label)) + 
              geom_point(data = dm1[dm1$ma_class=="none",],color="#E7B800")+
              geom_point(data = dm1[dm1$ma_class=="up",],color="#FC4E07")+
              geom_point(data = dm1[dm1$ma_class=="down",],color="#00AFBB")+
              geom_hline(yintercept = 0,linetype=2)+
              theme_pubr(base_size = 12,border=TRUE)+geom_label_repel(
                fontface="bold",
                color="purple",
                box.padding=unit(1, "lines"),
                point.padding=unit(0.5, "lines"),
                segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = dm1[dm1$label != "",], color = "purple")+
                annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          dm=read.table("./www/ma_example.csv",header=T,sep=",",comment.char="",quote="",check.names=FALSE,row.names=1)
          names(dm)=c("log2FoldChange","padj","meanfpkm","label")
          dm1=dm[!is.na(dm$padj),]
          dm1$ma_class="none"
          dm1[dm1$log2FoldChange > 0 & dm1$padj <= 0.05,]$ma_class="up"
          dm1[dm1$log2FoldChange < 0 & dm1$padj <= 0.05,]$ma_class="down"
          dm1$ma_class=factor(dm1$ma_class,levels = c("down","none","up"))
          up_num=nrow(dm1[dm1$ma_class == "up",])
          down_num=nrow(dm1[dm1$ma_class == "down",])
          xval=ceiling(max(abs(dm1$log2FoldChange)))
          if(input$color == "color1"){
            p=ggplot(data = dm1, aes(x = log2(meanfpkm), y = log2FoldChange, colour = ma_class,label=label)) + 
              geom_point(data = dm1[dm1$ma_class=="none",],color="#E7B800")+
              geom_point(data = dm1[dm1$ma_class=="up",],color="#FC4E07")+
              geom_point(data = dm1[dm1$ma_class=="down",],color="#00AFBB")+
              geom_hline(yintercept = 0,linetype=2)+
              theme_pubr(base_size = 12,border=TRUE)+geom_label_repel(
                fontface="bold",
                color="purple",
                box.padding=unit(1, "lines"),
                point.padding=unit(0.5, "lines"),
                segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = dm1[dm1$label != "",], color = "purple")+
                annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
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
        filename="maplot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)      
          dev.off()
        }
      )
    }
  )    
}
