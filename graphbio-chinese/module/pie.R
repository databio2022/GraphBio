#!/user/bin/Rscript
#correlation scatter plot
#shiny module

library(ggplot2)
library(RColorBrewer)

pieUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="饼图",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8,
                    tags$hr(),
                    tags$h6("该工具使用了R包ggplot2。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2包。")
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

pieServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/pie_example.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        d[is.na(d)] <- ""
        modalDialog(
          span('color1支持最多8类，color2支持12类，color3支持9类。'),
          span('注意：上传文件为逗号分隔文件（可用excel保存为csv格式，不是csv UTF-8!)!'),
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
          d=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE)
          x=table(d[,1])
          x=as.data.frame(x)
          x$percent=round(x$Freq/sum(x$Freq) * 100,2)
          x$Var1=as.factor(x$Var1)

          # Compute the position of labels
          d <- x %>% 
            arrange(desc(Var1)) %>%
            mutate(ypos = cumsum(percent)- 0.5*percent)

          labels=as.factor(paste(d$Var1,paste0(d$percent,"%")))
          if(input$color == "color1"){
              p=ggplot(d, aes(x="", y=percent, fill=Var1)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                scale_fill_brewer(palette="Set2")+
                geom_text(aes(y = ypos, label = labels, size=6)) +
                theme_void()+
                theme(legend.position="none")
          }else if(input$color == "color2"){
              p=ggplot(d, aes(x="", y=percent, fill=Var1)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                scale_fill_brewer(palette="Set3")+
                geom_text(aes(y = ypos, label = labels, size=6)) +
                theme_void()+
                theme(legend.position="none")
          }else if (input$color == "color3"){
              p=ggplot(d, aes(x="", y=percent, fill=Var1)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                scale_fill_brewer(palette="Set1")+
                geom_text(aes(y = ypos, label = labels, size=6)) +
                theme_void()+
                theme(legend.position="none")
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          d=read.table("./www/pie_example.csv",header=T,sep=",",comment.char="",quote="",check.names=FALSE)
          x=table(d[,1])
          x=as.data.frame(x)
          x$percent=round(x$Freq/sum(x$Freq) * 100,2)
          x$Var1=as.factor(x$Var1)

          # Compute the position of labels
          d <- x %>% 
            arrange(desc(Var1)) %>%
            mutate(ypos = cumsum(percent)- 0.5*percent)

          labels=as.factor(paste(d$Var1,paste0(d$percent,"%")))
        if(input$color == "color1"){
              p=ggplot(d, aes(x="", y=percent, fill=Var1)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                scale_fill_brewer(palette="Set2")+
                geom_text(aes(y = ypos, label = labels, size=6)) +
                theme_void()+
                theme(legend.position="none")
        }else if(input$color == "color2"){
              p=ggplot(d, aes(x="", y=percent, fill=Var1)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                scale_fill_brewer(palette="Set3")+
                geom_text(aes(y = ypos, label = labels, size=6)) +
                theme_void()+
                theme(legend.position="none")
        }else if (input$color == "color3"){
              p=ggplot(d, aes(x="", y=percent, fill=Var1)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                scale_fill_brewer(palette="Set1")+
                geom_text(aes(y = ypos, label = labels, size=6)) +
                theme_void()+
                theme(legend.position="none")
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
              plot()
        })
      })
      #download pdf figure
      output$pdf <- downloadHandler(
        filename="pieplot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h)
          print(vals$p)
          dev.off()
        }
      )
    }
  )    
}
