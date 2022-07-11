#!/user/bin/Rscript
#volcano plot
#shiny module

library(factoextra)
library(tools)

textclusterUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="Text Cluster",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包factoextra。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和factoextra包。")
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
          numericInput(ns("cluster_num"), label = "Class Numbers", value = 10),
          #pickerInput(
          #     inputId = ns("type"),
          #     label = "选择图类型", 
          #     choices = c("default","circle"),
          #     multiple = FALSE,
          #     selected = "default"
          #  ),
          numericInput(ns("w"), label = "Figure Width", value = 12),
          numericInput(ns("h"), label = "Figure Height", value = 10),
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

textclusterServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/go_term.csv",header=FALSE,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
        d=d[1:10,]
        modalDialog(
          span("Support up to 100 sentences."),
          tags$hr(),
          renderTable(d,rownames=FALSE,colnames=FALSE),
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
        if(!is.null(input$file1$datapath)){
          if(file_ext(input$file1$datapath) == "csv"){
            d=read.table(input$file1$datapath,row.names=1,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
          }else if(file_ext(input$file1$datapath) == "txt"){
            d=read.table(input$file1$datapath,row.names=1,sep="\t",check.names=FALSE,quote="",comment.char="",fill=TRUE)
          }else if(file_ext(input$file1$datapath) == "xls"){
          d=readxl::read_xls(input$file1$datapath,col_names = FALSE)
          d=as.data.frame(d)
        }else if(file_ext(input$file1$datapath) == "xlsx"){
          d=readxl::read_xlsx(input$file1$datapath,col_names = FALSE)
          d=as.data.frame(d)
        }
          tmpfile=paste0(md5sum(input$file1$datapath)[[1]],".csv")
          cmd=paste(paste("./miniconda3/envs/bert-as-service/bin/python ./py_scripts/bert_vec_get.py",input$file1$datapath),tmpfile)
          system(cmd)
          d1=read.table(paste0("./tmp/",tmpfile),header=FALSE,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
          system(paste0("rm -f ./tmp/",tmpfile))
          rownames(d1)=rownames(d)
          d <- dist(d1, method = "euclidean")
          hc1 <- hclust(d, method = "complete" )
          p=fviz_dend(hc1, k = input$cluster_num,                
                    cex = 0.5,                 # label size
                    k_colors = "jco",
                    color_labels_by_k = TRUE,  # color labels by groups
                    ggtheme = theme_void(),     # Change theme
                    rect_border = "jco",
                    rect_fill = TRUE,
                    rect = TRUE,
                    type="rectangle",
                    horiz=FALSE,ylim=c(-80,80)
          )
          vals$p=p
          p
        }
      })

      #example
      plote <- reactive({
          d1=read.table("./www/text_cluster_example.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE,quote="",comment.char="",fill=TRUE)
          d <- dist(d1, method = "euclidean")
          hc1 <- hclust(d, method = "complete" )
          p=fviz_dend(hc1, k = input$cluster_num,                
                    cex = 0.5,                 # label size
                    k_colors = "jco",
                    color_labels_by_k = TRUE,  # color labels by groups
                    ggtheme = theme_void(),     # Change theme
                    rect_border = "jco",
                    rect_fill = TRUE,
                    rect = TRUE,
                    type="rectangle",
                    horiz=FALSE,ylim=c(-80,80)
          )
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
        filename="text_cluster.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h)
          print(vals$p)
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="text_cluster.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="text_cluster.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="text_cluster.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
