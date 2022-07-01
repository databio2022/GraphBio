#!/user/bin/Rscript
#stack bar percent visualization
#shiny module

library(ggpubr)
library(ggplot2)
library(dplyr)
library(RColorBrewer) 

stackbarUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="堆叠柱状图",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=800) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),
            width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包geomnet。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和geomnet包。")),
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
               choices = paste0("color", seq(3)),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "下载图片宽度", value = 6),
          numericInput(ns("h"), label = "下载图片高度", value = 6),
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

stackbarServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=mtcars[,c("cyl","carb")]
        colnames(d)=c("sample","type")
        d$type=paste0("type",d$type)
        d$sample=paste0("sample",d$sample)
        modalDialog(
          span('文件格式：第1列为sample，第2列为type，一个sample一般包含多个类型。'),
          tags$hr(),
          renderTable(d[1:20,],rownames=FALSE),
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
          d=read.table(input$file1$datapath,header=TRUE,sep=",",comment.char = "",check.names=FALSE)
          colnames(d)=c("sample","type")
          d=d %>% group_by(sample,type) %>% summarise(n=n()) %>% mutate(freq=n/sum(n)) %>% select(-n)
          if(input$color == "color1"){
              p=ggplot(d, aes(sample, freq, fill=type))+
                geom_bar(stat="identity",position="fill")+
                scale_fill_brewer(palette="Paired")+
                ggtitle("")+
                theme_pubr(border=TRUE)+
                ylab("Frequency")+
                xlab("")+theme(legend.position = "right")
          }else if(input$color == "color2"){
              p=ggplot(d, aes(sample, freq, fill=type))+
                geom_bar(stat="identity",position="fill")+
                ggtitle("")+
                theme_pubr(border=TRUE)+
                ylab("Frequency")+
                xlab("")+theme(legend.position = "right")
          }else if(input$color == "color3"){
              p=ggplot(d, aes(sample, freq, fill=type))+
                geom_bar(stat="identity",position="fill")+
                scale_fill_brewer(palette="Set2")+
                ggtitle("")+
                theme_pubr(border=TRUE)+
                ylab("Frequency")+
                xlab("")+theme(legend.position = "right")
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          d=mtcars[,c("cyl","carb")]
          colnames(d)=c("sample","type")
          x=d %>% group_by(sample,type) %>% summarise(n=n()) %>% mutate(freq=n/sum(n)) %>% select(-n)
          x$type=paste0("type",x$type)
          x$sample=paste0("sample",x$sample)

          if(input$color == "color1"){
              p=ggplot(x, aes(sample, freq, fill=type))+
                geom_bar(stat="identity",position="fill")+
                scale_fill_brewer(palette="Paired")+
                ggtitle("")+
                theme_pubr(border=TRUE)+
                ylab("Frequency")+
                xlab("")+theme(legend.position = "right")
          }else if(input$color == "color2"){
              p=ggplot(x, aes(sample, freq, fill=type))+
                geom_bar(stat="identity",position="fill")+
                ggtitle("")+
                theme_pubr(border=TRUE)+
                ylab("Frequency")+
                xlab("")+theme(legend.position = "right")
          }else if(input$color == "color3"){
              p=ggplot(x, aes(sample, freq, fill=type))+
                geom_bar(stat="identity",position="fill")+
                scale_fill_brewer(palette="Set2")+
                ggtitle("")+
                theme_pubr(border=TRUE)+
                ylab("Frequency")+
                xlab("")+theme(legend.position = "right")
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
        filename="stack_bar.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)     
          dev.off()
        }
      )
    }
  )    
}
