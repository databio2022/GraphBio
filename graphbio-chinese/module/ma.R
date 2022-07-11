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
            width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包ggplot2。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2包。")),
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
               choices = paste0("color", 1),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("ps"), label = "点大小", value = 2),
          numericInput(ns("ls"), label = "标签大小", value = 3),
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
        if(file_ext(input$file1$datapath) == "csv"){
          dm=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE,row.names=1)
        }else if(file_ext(input$file1$datapath) == "txt"){
          dm=read.table(input$file1$datapath,header=T,sep="\t",comment.char="",quote="",check.names=FALSE,row.names=1)
        }else if(file_ext(input$file1$datapath) == "xls"){
                dm=readxl::read_xls(input$file1$datapath)
                dm=as.data.frame(dm)
                rownames(dm)=dm[,1]
                dm=dm[,-1]
            }else if(file_ext(input$file1$datapath) == "xlsx"){
                dm=readxl::read_xlsx(input$file1$datapath)
                dm=as.data.frame(d)
                rownames(dm)=dm[,1]
                dm=dm[,-1]
            }
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
          }else if(input$color == "color2"){
            p=ggplot(data = dm1, aes(x = log2(meanfpkm), y = log2FoldChange, colour = ma_class,label=label)) + 
              geom_point(data = dm1[dm1$ma_class=="none",],color="#4DBBD599",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="up",],color="#E64B3599",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="down",],color="#00A08799",size=input$ps)+
              geom_hline(yintercept = 0,linetype=2)+
              theme_pubr(base_size = 12,border=TRUE)+geom_label_repel(
                size = input$ls,
                fontface="bold",
                color="purple",
                box.padding=unit(1, "lines"),
                point.padding=unit(0.5, "lines"),
                segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = dm1[dm1$label != "",], color = "purple")+
                annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
          }else if(input$color == "color3"){
            p=ggplot(data = dm1, aes(x = log2(meanfpkm), y = log2FoldChange, colour = ma_class,label=label)) + 
              geom_point(data = dm1[dm1$ma_class=="none",],color="#86868699",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="up",],color="#CD534C99",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="down",],color="#0073C299",size=input$ps)+
              geom_hline(yintercept = 0,linetype=2)+
              theme_pubr(base_size = 12,border=TRUE)+geom_label_repel(
                size = input$ls,
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
              geom_point(data = dm1[dm1$ma_class=="none",],color="#E7B800",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="up",],color="#FC4E07",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="down",],color="#00AFBB",size=input$ps)+
              geom_hline(yintercept = 0,linetype=2)+
              theme_pubr(base_size = 12,border=TRUE)+geom_label_repel(
                size = input$ls,
                fontface="bold",
                color="purple",
                box.padding=unit(1, "lines"),
                point.padding=unit(0.5, "lines"),
                segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = dm1[dm1$label != "",], color = "purple")+
                annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
          }else if(input$color == "color2"){
            p=ggplot(data = dm1, aes(x = log2(meanfpkm), y = log2FoldChange, colour = ma_class,label=label)) + 
              geom_point(data = dm1[dm1$ma_class=="none",],color="#4DBBD599",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="up",],color="#E64B3599",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="down",],color="#00A08799",size=input$ps)+
              geom_hline(yintercept = 0,linetype=2)+
              theme_pubr(base_size = 12,border=TRUE)+geom_label_repel(
                size = input$ls,
                fontface="bold",
                color="purple",
                box.padding=unit(1, "lines"),
                point.padding=unit(0.5, "lines"),
                segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = dm1[dm1$label != "",], color = "purple")+
                annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
          }else if(input$color == "color3"){
            p=ggplot(data = dm1, aes(x = log2(meanfpkm), y = log2FoldChange, colour = ma_class,label=label)) + 
              geom_point(data = dm1[dm1$ma_class=="none",],color="#86868699",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="up",],color="#CD534C99",size=input$ps)+
              geom_point(data = dm1[dm1$ma_class=="down",],color="#0073C299",size=input$ps)+
              geom_hline(yintercept = 0,linetype=2)+
              theme_pubr(base_size = 12,border=TRUE)+geom_label_repel(
                size = input$ls,
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
      output$png <- downloadHandler(
        filename="maplot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="maplot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="maplot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
