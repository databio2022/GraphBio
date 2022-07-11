#!/user/bin/Rscript
#correlation scatter plot
#shiny module

library(ggpubr)

corrscatterUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="Scatter Plot",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包ggpubr。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggpubr包。")),
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
               choices = paste0("color", 1:3),
               multiple = FALSE,
               selected = "color1"
            ),
          tags$strong("Regression Line"),
          switchInput(
             inputId = ns("line"),
             value = TRUE
          ),  
          numericInput(ns("w"), label = "Figure Width", value = 8),
          numericInput(ns("h"), label = "Figure Height", value = 8),
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

corrscatterServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/corr_scatter.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )
        d[is.na(d)] <- ""
        modalDialog(
          span('Pearson Correlation Analysis'),
          tags$hr(),
          renderTable(d,rownames=FALSE),
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
          d=read.table(input$file1$datapath,
            header = TRUE,
            sep=",",
            check.names=FALSE,
            quote = "",
            comment.char="",
            fill=TRUE
            )
        }else if(file_ext(input$file1$datapath) == "txt"){
          d=read.table(input$file1$datapath,
            header = TRUE,
            sep="\t",
            check.names=FALSE,
            quote = "",
            comment.char="",
            fill=TRUE
            )
        }else if(file_ext(input$file1$datapath) == "xls"){
          d=readxl::read_xls(input$file1$datapath)
          d=as.data.frame(d)
        }else if(file_ext(input$file1$datapath) == "xlsx"){
          d=readxl::read_xlsx(input$file1$datapath)
          d=as.data.frame(d)
        }
        if(is.numeric(d[,1])){
          if(input$color == "color1"){
            if(input$line){
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )
            }else{
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "none",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )              
            }
          }else if(input$color == "color2"){
            if(input$line){
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )
            }else{
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "none",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )              
            }
          }else if (input$color == "color3"){
            if(input$line){
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )
            }else{
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "none",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )              
            }
          }
          vals$p=p
          p
        }
      })

      #example
      plote <- reactive({
        d=read.table("./www/corr_scatter.csv",
          header = TRUE,
          sep=",",
          check.names=FALSE,
          quote = "",
          comment.char="",
          fill=TRUE
          )

        if(input$color == "color1"){
            if(input$line){
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )
            }else{
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "none",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )              
            }
        }else if(input$color == "color2"){
            if(input$line){
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )
            }else{
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "none",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )              
            }
        }else if (input$color == "color3"){
            if(input$line){
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )
            }else{
              p=ggscatter(d, x = names(d)[1], y = names(d)[2],
                 color = "#00AFBB", shape = 19, size = 3, # Points color, shape and size
                 add = "none",  # Add regressin line
                 add.params = list(color = "#00AFBB", fill = "#FC4E07"), # Customize reg. line
                 conf.int = TRUE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
                 )              
            }
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
        filename="corr_scatter.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h)
          print(vals$p)
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="corr_scatter.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="corr_scatter.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="corr_scatter.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
