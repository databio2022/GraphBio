#!/user/bin/Rscript
#volcano plot
#shiny module

library(ggplot2)
library(ggpubr)
library(ggrepel)

volcanoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="火山图",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8
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
          numericInput(ns("pval"), label = "显著性阈值(P值 或者 FDR)", value = 0.05),
          numericInput(ns("fc"), label = "log2倍数变化", value = 1),
          pickerInput(
               inputId = ns("color"),
               label = "选择颜色", 
               choices = paste0("color", 1:3),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("ps"), label = "点大小", value = 2),
          numericInput(ns("ls"), label = "标签大小", value = 4),
          numericInput(ns("w"), label = "下载图片宽度", value = 8),
          numericInput(ns("h"), label = "下载图片高度", value = 9),
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

volcanoServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/volcano_example.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
        d=d[1:10,]
        modalDialog(
          span('第1列为基因ID，第2列是log2倍数变化值，第3列为显著性阈值（P值或FDR值），第4列为需要标记的基因名字（可选，若不需要标记，此列可以忽略）。
            注意：上传文件为逗号分隔文件（可用excel保存为csv格式，不是csv UTF-8!)，例子只显示前10行！'),
          tags$hr(),
          renderTable(d,rownames=TRUE),
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
        if(file_ext(input$file1$datapath) == "csv"){
          d=read.table(input$file1$datapath,header=TRUE,row.names=1,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
        }else if(file_ext(input$file1$datapath) == "txt"){
          d=read.table(input$file1$datapath,header=TRUE,row.names=1,sep="\t",check.names=FALSE,quote="",comment.char="",fill=TRUE)
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
          if(ncol(d) == 3){
              names(d)[1]=c("log2FC")
              names(d)[3]=c("label")
              d=d[!is.na(d[,2]),]
              d$log10=-log10(d[,2])  # col 2 fdr
              d$class="none"
              d[d[,2] <= input$pval & d[,1] >= input$fc,]$class="UP"
              d[d[,2] <= input$pval & d[,1] <= -input$fc,]$class="DOWN"
              up_num=nrow(d[d$class == "UP",])
              down_num=nrow(d[d$class == "DOWN",])
              d$class <- as.factor(d$class) # col 3 class
              xval=ceiling(max(abs(d[,1])))
              if(input$color == "color1"){
                colors <- c("UP"="#FC4E07", "none"="#E7B800", "DOWN"="#00AFBB")
              }else if(input$color == "color2"){
                colors <- c("UP"="#E64B3599", "none"="#4DBBD599", "DOWN"="#00A08799")
              }else if(input$color == "color3"){
                colors <- c("UP"="#CD534C99", "none"="#86868699", "DOWN"="#0073C299")
              }
              
              p=ggplot(data=d, aes(x=log2FC,y=log10,color=class,label=label)) + 
                  geom_point(data = d[d$class=="UP",],aes(y=log10,color="UP"),size=input$ps)+
                  geom_point(data = d[d$class=="none",],aes(y=log10,color="none"),size=input$ps)+
                  geom_point(data = d[d$class=="DOWN",],aes(y=log10,color="DOWN"),size=input$ps)+
                  scale_color_manual(values = colors)+
                  ylab(paste0("-log10 ",names(d)[2]))+xlab("Log2FoldChange")+
                  theme_pubr(base_size = 12,border=TRUE)+geom_hline(yintercept=-log10(input$pval), linetype="dashed", 
                      color = "black", size=0.5)+geom_vline(xintercept=c(-input$fc,input$fc), linetype="dashed", 
                      color = "black", size=0.5)+geom_label_repel(
                      size=input$ls,
                      fontface="bold",
                      color="purple",
                      box.padding=unit(1, "lines"),
                      point.padding=unit(0.5, "lines"),
                      segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = d[d$label!="",], color = "purple")+
                      annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                      annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
          }else{
              names(d)[1]=c("log2FC")
              d=d[!is.na(d[,2]),]
              d$log10=-log10(d[,2])  # col 2 fdr
              d$class="none"
              d[d[,2] <= input$pval & d[,1] >= input$fc,]$class="UP"
              d[d[,2] <= input$pval & d[,1] <= -input$fc,]$class="DOWN"
              up_num=nrow(d[d$class == "UP",])
              down_num=nrow(d[d$class == "DOWN",])
              d$class <- as.factor(d$class) # col 3 class
              xval=ceiling(max(abs(d[,1])))
              if(input$color == "color1"){
                colors <- c("UP"="#FC4E07", "none"="#E7B800", "DOWN"="#00AFBB")
              }else if(input$color == "color2"){
                colors <- c("UP"="#E64B3599", "none"="#4DBBD599", "DOWN"="#00A08799")
              }else if(input$color == "color3"){
                colors <- c("UP"="#CD534C99", "none"="#86868699", "DOWN"="#0073C299")
              }
              p=ggplot(data=d, aes(x=log2FC,y=log10,color=class)) + 
                  geom_point(data = d[d$class=="UP",],aes(y=log10,color="UP"),size=input$ps)+
                  geom_point(data = d[d$class=="none",],aes(y=log10,color="none"),size=input$ps)+
                  geom_point(data = d[d$class=="DOWN",],aes(y=log10,color="DOWN"),size=input$ps)+
                  scale_color_manual(values = colors)+
                  ylab(paste0("-log10 ",names(d)[2]))+xlab("Log2FoldChange")+
                  theme_pubr(base_size = 12,border=TRUE)+geom_hline(yintercept=-log10(input$pval), linetype="dashed", 
                      color = "black", size=0.5)+geom_vline(xintercept=c(-input$fc,input$fc), linetype="dashed", 
                      color = "black", size=0.5)+
                      annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                      annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")

          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          d=read.table("./www/volcano_example.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
          names(d)[1]=c("log2FC")
          names(d)[3]=c("label")
          d=d[!is.na(d[,2]),]
          d$log10=-log10(d[,2])  # col 2 fdr
          d$class="none"
          d[d[,2] <= input$pval & d[,1] >= input$fc,]$class="UP"
          d[d[,2] <= input$pval & d[,1] <= -input$fc,]$class="DOWN"
          up_num=nrow(d[d$class == "UP",])
          down_num=nrow(d[d$class == "DOWN",])
          d$class <- as.factor(d$class) # col 3 class
          xval=ceiling(max(abs(d[,1])))
              if(input$color == "color1"){
                colors <- c("UP"="#FC4E07", "none"="#E7B800", "DOWN"="#00AFBB")
              }else if(input$color == "color2"){
                colors <- c("UP"="#E64B3599", "none"="#4DBBD599", "DOWN"="#00A08799")
              }else if(input$color == "color3"){
                colors <- c("UP"="#CD534C99", "none"="#86868699", "DOWN"="#0073C299")
              }
          p=ggplot(data=d, aes(x=log2FC,y=log10,color=class,label=label)) + 
              geom_point(data = d[d$class=="UP",],aes(y=log10,color="UP"),size=input$ps)+
              geom_point(data = d[d$class=="none",],aes(y=log10,color="none"),size=input$ps)+
              geom_point(data = d[d$class=="DOWN",],aes(y=log10,color="DOWN"),size=input$ps)+
              scale_color_manual(values = colors)+
              ylab(paste0("-log10 ",names(d)[2]))+xlab("Log2FoldChange")+
              theme_pubr(base_size = 12,border=TRUE)+geom_hline(yintercept=-log10(input$pval), linetype="dashed", 
                  color = "black", size=0.5)+geom_vline(xintercept=c(-input$fc,input$fc), linetype="dashed", 
                  color = "black", size=0.5)+geom_label_repel(
                  size=input$ls,
                  fontface="bold",
                  color="purple",
                  box.padding=unit(1, "lines"),
                  point.padding=unit(0.5, "lines"),
                  segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = d[d$label!="",], color = "purple")+
                  annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                  annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
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
        filename="volcano.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h)
          print(vals$p)
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="volcano.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="volcano.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="volcano.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
