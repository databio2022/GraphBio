#!/user/bin/Rscript
#cumlative distribution curve
#shiny module

library(ggplot2)
library(RColorBrewer)
library(GOplot)

chordUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="GOplot(chord plot)",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=800,height=800) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包GOplot。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和GOplot包。参考文献："),
                  #  tags$h6("GOplot: an R package for visually combining expression data with functional analysis")),
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
          tags$h5("Upload a csv file(also support txt,xls,xlsx)"),
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
          pickerInput(
               inputId = ns("color"),
               label = "Select Colors", 
               choices = paste0("color", 1:2),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "Figure Width", value = 12),
          numericInput(ns("h"), label = "Figure Height", value = 12),
          numericInput(ns("ppi"), label = "Figure Resolution", value = 72),
                  dropdownButton(
                    downloadBttn(
                      outputId = ns("pdf"),
                      label="PDF figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("png"),
                      label="PNG figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("jpeg"),
                      label="JPEG figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = ns("tiff"),
                      label="TIFF figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    circle=FALSE,
                    label="Download Figure",
                    status="success"
                  )
        )
    )
  )
}

chordServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/chord_example.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        modalDialog(
          span('Each row reprsents a gene，each column represents a GO term, value 0 or 1 represents the relationship of gene and GO term. The last column represents logFC and is optional.'),
          tags$hr(),
          renderTable(d[1:10,c(1,2,3,4,ncol(d))],rownames=FALSE),
          size="l",
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

        if(file_ext(input$file1$datapath) == "csv"){
          d=read.table(input$file1$datapath,header=T,row.names=1,sep=",",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        }else if(file_ext(input$file1$datapath) == "txt"){
          d=read.table(input$file1$datapath,header=T,row.names=1,sep="\t",comment.char="",quote="",check.names=FALSE,fill=TRUE)
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
          if(names(d)[length(names(d))] == "logFC"){            
            if(input$color == "color1"){
                p=GOChord(d,ribbon.col=brewer.pal(length(names(d))-1,"Set2"),nlfc = 1)
            }else if(input$color == "color2"){
                p=GOChord(d,ribbon.col=brewer.pal(length(names(d))-1,"Set3"),nlfc = 1)
            }
            vals$p=p
            p
          }else{
            if(input$color == "color1"){
                p=GOChord(d,ribbon.col=brewer.pal(length(names(d)),"Set2"),nlfc = 0)
            }else if(input$color == "color2"){
                p=GOChord(d,ribbon.col=brewer.pal(length(names(d)),"Set3"),nlfc = 0)
            }
            vals$p=p
            p
          }
      })

      #example
      plote <- reactive({
        d=read.table("./www/chord_example.csv",header=T,row.names=1,sep=",",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        if(input$color == "color1"){
              p=GOChord(d,ribbon.col=brewer.pal(length(names(d))-1,"Set2"),nlfc = 1)
        }else if(input$color == "color2"){
              p=GOChord(d,ribbon.col=brewer.pal(length(names(d))-1,"Set3"),nlfc = 1)
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
        filename="chordplot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h)
          print(vals$p)
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="chordplot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="chordplot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="chordplot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
