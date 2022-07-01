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
        box(title="Volcano Plot",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8
                    #tags$hr(),
                    #tags$h6("该工具使用了R包ggplot2。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2包。")
            ),
        box(width=4,
          # Input: Select a file ----
          actionBttn(
             inputId = ns("rune"),
             label = "run example",
             style = "fill", 
              color = "warning",
              size = "sm"
          ),  
          tags$hr(),                
          tags$h5("Upload a csv or comma-separated file"),
          actionBttn(
             inputId = ns("show"),
             label = "view example file",
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
          numericInput(ns("pval"), label = "P value or FDR", value = 0.05),
          numericInput(ns("fc"), label = "log2FC", value = 1),
          pickerInput(
               inputId = ns("color"),
               label = "Select Colors", 
               choices = paste0("color", 1),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "Figure Width", value = 8),
          numericInput(ns("h"), label = "Figure Height", value = 9),
          downloadBttn(
            outputId = ns("pdf"),
            label="Download PDF Figure",
            style = "fill",
            color = "success",
            size='sm'
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
          span('First column represents gene ID, second column represents log2FC, third column represents pvalue or fdr,fourth column represents genes to be labeled on the figure and is optional.'),
          tags$hr(),
          renderTable(d,rownames=TRUE),
          easyClose=TRUE,
          footer = tagList(
            modalButton("Close")
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
          d=read.table(input$file1$datapath,header=TRUE,row.names=1,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
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
              colors <- c("UP"="#FC4E07", "none"="#E7B800", "DOWN"="#00AFBB")
              p=ggplot(data=d, aes(x=log2FC,y=log10,color=class,label=label)) + 
                  geom_point(data = d[d$class=="UP",],aes(y=log10,color="UP"))+
                  geom_point(data = d[d$class=="none",],aes(y=log10,color="none"))+
                  geom_point(data = d[d$class=="DOWN",],aes(y=log10,color="DOWN"))+
                  scale_color_manual(values = colors)+
                  ylab(paste0("-log10 ",names(d)[2]))+xlab("Log2FoldChange")+
                  theme_pubr(base_size = 12,border=TRUE)+geom_hline(yintercept=-log10(input$pval), linetype="dashed", 
                      color = "black", size=0.5)+geom_vline(xintercept=c(-input$fc,input$fc), linetype="dashed", 
                      color = "black", size=0.5)+geom_label_repel(
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
              colors <- c("UP"="#FC4E07", "none"="#E7B800", "DOWN"="#00AFBB")
              p=ggplot(data=d, aes(x=log2FC,y=log10,color=class)) + 
                  geom_point(data = d[d$class=="UP",],aes(y=log10,color="UP"))+
                  geom_point(data = d[d$class=="none",],aes(y=log10,color="none"))+
                  geom_point(data = d[d$class=="DOWN",],aes(y=log10,color="DOWN"))+
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
          colors <- c("UP"="#FC4E07", "none"="#E7B800", "DOWN"="#00AFBB")
          p=ggplot(data=d, aes(x=log2FC,y=log10,color=class,label=label)) + 
              geom_point(data = d[d$class=="UP",],aes(y=log10,color="UP"))+
              geom_point(data = d[d$class=="none",],aes(y=log10,color="none"))+
              geom_point(data = d[d$class=="DOWN",],aes(y=log10,color="DOWN"))+
              scale_color_manual(values = colors)+
              ylab(paste0("-log10 ",names(d)[2]))+xlab("Log2FoldChange")+
              theme_pubr(base_size = 12,border=TRUE)+geom_hline(yintercept=-log10(input$pval), linetype="dashed", 
                  color = "black", size=0.5)+geom_vline(xintercept=c(-input$fc,input$fc), linetype="dashed", 
                  color = "black", size=0.5)+geom_label_repel(
                  fontface="bold",
                  color="purple",
                  box.padding=unit(1, "lines"),
                  point.padding=unit(0.5, "lines"),
                  segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = d[d$label!="",], color = "purple")+
                  annotate(geom = 'text', label = paste0('UP_Number: ', up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5)+
                  annotate(geom = 'text', label = paste0('DOWN_Number: ', down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+labs(color = "class")
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
    }
  )    
}
