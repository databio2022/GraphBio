#!/user/bin/Rscript
#roc curve
#shiny module

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggpubr)

pcaUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="PCA Analysis",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包FactoMineR和factoextra。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和FactoMineR及factoextra包。")),
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
          tags$h5("Upload a sample metadata file"),
          actionBttn(
             inputId = ns("show1"),
             label = "view example file",
             style = "fill", 
              color = "primary",
              size = "sm"
          ),
          tags$br(),
          tags$br(),
          fileInput(ns("file2"),NULL,
                    multiple = FALSE,
                    accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
          numericInput(ns("varnum"), label = "Top n highest variable features", value = 1000),
          pickerInput(
               inputId = ns("color"),
               label = "Select Colors", 
               choices = paste0("color", 1),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "Figure Width", value = 8),
          numericInput(ns("h"), label = "Figure Height", value = 8),
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

pcaServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/heatmap_test.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE)
        modalDialog(
          renderTable(d[1:10,],rownames=TRUE),
          easyClose=TRUE,
          size="l",
          footer = tagList(
            modalButton("Close")
          )
        )
      }
      dataModalx <- function(failed = FALSE) {
        d=read.table("./www/group_info.csv",header=TRUE,sep=",",check.names=FALSE)
        names(d)=c("sample","group")
        modalDialog(
          span('The samples in metadata file must keep the same order with that of gene expression file.'),
          tags$hr(),
          renderTable(d[1:10,],rownames=FALSE),
          easyClose=TRUE,
          size="l",
          footer = tagList(
            modalButton("Close")
          )
        )
      }
      
      # Show modal when button is clicked.
      observeEvent(input$show, {
        showModal(dataModal())
      })
      observeEvent(input$show1, {
        showModal(dataModalx())
      })
      #init
      output$plot <- renderPlot({
        NULL
      })

      # The user's data, parsed into a data frame
      vals=reactiveValues()
      plota <- reactive({
            dm=read.table(input$file1$datapath,header=T,sep=",",row.names=1,comment.char="",quote="",check.names=FALSE)
            if(nrow(dm) > input$varnum){
                sel = order(apply(dm, 1, var), decreasing=TRUE)[1:input$varnum] #choose top 1000 variable
                dm2000=dm[sel,]
                data.pca <- PCA(as.data.frame(t(dm2000)), graph = FALSE)
            }else{
                dm2000=dm
                data.pca <- PCA(as.data.frame(t(dm2000)), graph = FALSE)            
            }
            if(!is.null(input$file2$datapath)){
                groupinfo=read.table(input$file2$datapath,header=TRUE,sep=",")
                names(groupinfo)=c("sample","group")
                if(input$color == "color1"){
                  p=fviz_pca_ind(data.pca,
                               geom.ind = "point", # show points only (nbut not "text")
                               col.ind = groupinfo$group, # color by groups,note that expression matrix rows match group
                               addEllipses = TRUE, # Concentration ellipses
                               ellipse.type = "confidence", # narrow ellipses
                               legend.title = "Groups"
                               )+theme_pubr(base_size = 12,border=TRUE)
                }
            }else{
                if(input$color == "color1"){
                  p=fviz_pca_ind(data.pca,
                               geom.ind = "point", # show points only (nbut not "text")
                               addEllipses = FALSE, # Concentration ellipses
                               legend.title = "Groups"
                               )+theme_pubr(base_size = 12,border=TRUE)
                }              
            }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          dm=read.table("./www/heatmap_test.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE)
          if(nrow(dm) > input$varnum){
              sel = order(apply(dm, 1, var), decreasing=TRUE)[1:input$varnum] #choose top 1000 variable
              dm2000=dm[sel,]
              data.pca <- PCA(as.data.frame(t(dm2000)), graph = FALSE)
          }else{
              dm2000=dm
              data.pca <- PCA(as.data.frame(t(dm2000)), graph = FALSE)            
          }

          groupinfo=read.table("./www/group_info.csv",header=TRUE,sep=",")
          names(groupinfo)=c("sample","group")
          if(input$color == "color1"){
            p=fviz_pca_ind(data.pca,
                         geom.ind = "point", # show points only (nbut not "text")
                         col.ind = groupinfo$group, # color by groups,note that expression matrix rows match group
                         addEllipses = TRUE, # Concentration ellipses
                         ellipse.type = "confidence", # narrow ellipses
                         legend.title = "Groups"
                         )+theme_pubr(base_size = 12,border=TRUE)
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
        filename="pca.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)     
          dev.off()
        }
      )
    }
  )    
}
