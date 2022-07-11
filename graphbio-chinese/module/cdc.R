#!/user/bin/Rscript
#cumlative distribution curve
#shiny module

library(ggplot2)
library(RColorBrewer)

cdcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="累积分布曲线",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8),
                    #tags$hr(),
                    #tags$h6("该工具使用了R包ggplot2。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2包。")),
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
          materialSwitch(
             inputId = ns("value"),
             label = "是否进行log2处理", 
             value = FALSE,
             status = "warning"
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

cdcServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/cdc_example.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        d[is.na(d)] <- ""
        modalDialog(
          span('支持2组或2组以上的曲线绘制。注意：上传文件为逗号分隔文件（可用excel保存为csv格式，不是csv UTF-8!)!'),
          tags$hr(),
          renderTable(d[1:20,],rownames=FALSE),
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
          if(ncol(d) == 2){
            x=d[!is.na(d[,1]),1]
            y=d[!is.na(d[,2]),2]
            text=paste0('Kolmogorov Smirnov test pvalue=',ks.test(x,y)$p.value)
            if(input$value){
              d=data.frame(value=c(log2(x+0.0001),log2(y+0.0001)),group=rep(names(d),c(length(x),length(y))))
            }else{
              d=data.frame(value=c(x,y),group=rep(names(d),c(length(x),length(y))))
            }
            if(input$color == "color1"){
                p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_manual(values = c("#00AFBB", "#E7B800"))+
                    annotate(geom = 'text', label = text, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)
            }else if(input$color == "color2"){
                p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_manual(values = c("#00AFBB", "#FC4E07"))+
                    annotate(geom = 'text', label = text, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)
            }else if (input$color == "color3"){
                p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_manual(values = c("#6495ED", "#FF4500"))+
                    annotate(geom = 'text', label = text, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)
            }
            vals$p=p
            p
          }else{
            if(input$value){
              ab=NULL
              ab1=NULL
              for(i in 1:ncol(d)){
                x1=d[!is.na(d[,i]),i]
                ab=append(ab,x1)
                ab1=append(ab1,length(x1))
              }
              d=data.frame(value=log2(ab+0.0001),group=rep(names(d),ab1))
            }else{
                ab=NULL
                ab1=NULL
                for(i in 1:ncol(d)){
                  x1=d[!is.na(d[,i]),i]
                  ab=append(ab,x1)
                  ab1=append(ab1,length(x1))
                }
                d=data.frame(value=ab,group=rep(names(d),ab1))
            }
            if(input$color == "color1"){
                p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')
            }else if(input$color == "color2"){
                p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_brewer(palette="Set2")
            }else if (input$color == "color3"){
                p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_brewer(palette="Set1")
            }
            vals$p=p
            p            
          }
      })

      #example
      plote <- reactive({
        d=read.table("./www/cdc_example.csv",header=T,sep=",",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        x=d[!is.na(d[,1]),1]
        y=d[!is.na(d[,2]),2]
        text=paste0("Kolmogorov Smirnov test pvalue=", ks.test(x,y)$p.value)
        if(input$value){
          d=data.frame(value=c(log2(x+0.0001),log2(y+0.0001)),group=rep(names(d),c(length(x),length(y))))
        }else{
          d=data.frame(value=c(x,y),group=rep(names(d),c(length(x),length(y))))
        }
        if(input$color == "color1"){
              p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_manual(values = c("#00AFBB", "#E7B800"))+
                  annotate(geom = 'text', label = text, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)
        }else if(input$color == "color2"){
              p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_manual(values = c("#00AFBB", "#FC4E07"))+
                  annotate(geom = 'text', label = text, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)
        }else if (input$color == "color3"){
              p=ggplot(d, aes(value, colour = group)) + stat_ecdf(geom="line",size=1)+theme_pubr(border=TRUE)+ylab("Cumulative fraction")+xlab('Value')+scale_color_manual(values = c("#6495ED", "#FF4500"))+
                  annotate(geom = 'text', label = text, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)
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
        filename="cdcplot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h)
          print(vals$p)
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="cdcplot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="cdcplot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="cdcplot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
