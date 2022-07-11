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
        box(title="主成分分析",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包FactoMineR和factoextra。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和FactoMineR及factoextra包。")
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
          tags$h5("上传表达矩阵文件(支持csv、txt、xls、xlsx)"),
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
          tags$h5("上传分组信息文件(支持csv、txt、xls、xlsx)"),
          actionBttn(
             inputId = ns("show1"),
             label = "查看示例文件",
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
          numericInput(ns("varnum"), label = "排名前N个最大变化的基因", value = 1000),
          pickerInput(
               inputId = ns("color"),
               label = "选择颜色", 
               choices = paste0("color", 1),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("ps"), label = "Point Size", value = 3),
          tags$strong("Highlight组"),
          switchInput(
             inputId = ns("cls"),
             value = TRUE
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

pcaServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/heatmap_test.csv",header=TRUE,row.names=1,sep=",",check.names=FALSE)
        modalDialog(
          span('基因表达矩阵，行为基因，列为样本'),
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
      dataModalx <- function(failed = FALSE) {
        d=read.table("./www/group_info.csv",header=TRUE,sep=",",check.names=FALSE)
        names(d)=c("sample","group")
        modalDialog(
          span('实验分组信息。样本顺序应与表达矩阵中的列顺序保持一致。支持两组或更多组。'),
          span('注意：上传文件为逗号分隔文件（可用excel保存为csv格式，不是csv UTF-8!)!'),
          tags$hr(),
          renderTable(d[1:10,],rownames=FALSE),
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
        if(file_ext(input$file1$datapath) == "csv"){
            dm=read.table(input$file1$datapath,header=T,sep=",",row.names=1,comment.char="",quote="",check.names=FALSE)
         }else if(file_ext(input$file1$datapath) == "txt"){
            dm=read.table(input$file1$datapath,header=T,sep="\t",row.names=1,comment.char="",quote="",check.names=FALSE)
         }else if(file_ext(input$file1$datapath) == "xls"){
                dm=readxl::read_xls(input$file1$datapath)
                dm=as.data.frame(dm)
                rownames(dm)=dm[,1]
                dm=dm[,-1]
            }else if(file_ext(input$file1$datapath) == "xlsx"){
                dm=readxl::read_xlsx(input$file1$datapath)
                dm=as.data.frame(dm)
                rownames(dm)=dm[,1]
                dm=dm[,-1]
            }
            if(nrow(dm) > input$varnum){
                sel = order(apply(dm, 1, var), decreasing=TRUE)[1:input$varnum] #choose top 1000 variable
                dm2000=dm[sel,]
                data.pca <- PCA(as.data.frame(t(dm2000)), graph = FALSE)
            }else{
                dm2000=dm
                data.pca <- PCA(as.data.frame(t(dm2000)), graph = FALSE)            
            }
            if(!is.null(input$file2$datapath)){
              if(file_ext(input$file2$datapath) == "csv"){
                groupinfo=read.table(input$file2$datapath,header=TRUE,sep=",")
              }else if(file_ext(input$file2$datapath) == "txt"){
                groupinfo=read.table(input$file2$datapath,header=TRUE,sep="\t")
              }else if(file_ext(input$file2$datapath) == "xls"){
                groupinfo=read.table(input$file2$datapath)
                groupinfo=as.data.frame(groupinfo)
              }else if(file_ext(input$file2$datapath) == "xlsx"){
                groupinfo=read.table(input$file2$datapath)
                groupinfo=as.data.frame(groupinfo)
              }
                names(groupinfo)=c("sample","group")
                if(input$color == "color1"){
                  p=fviz_pca_ind(data.pca,
                               geom.ind = "point", # show points only (nbut not "text")
                               col.ind = groupinfo$group, # color by groups,note that expression matrix rows match group
                               addEllipses = input$cls, # Concentration ellipses
                               ellipse.type = "confidence", # narrow ellipses
                               legend.title = "Groups",
                               pointsize=input$ps
                               )+theme_pubr(base_size = 12,border=TRUE)
                }else if(input$color == "color2"){
                    p=fviz_pca_ind(data.pca,
                                 geom.ind = "point", # show points only (nbut not "text")
                                 col.ind = groupinfo$group, # color by groups,note that expression matrix rows match group
                                 addEllipses = input$cls, # Concentration ellipses
                                 ellipse.type = "confidence", # narrow ellipses
                                 legend.title = "Groups",
                                 pointsize=input$ps,
                                 palette="npg"
                                 )+theme_pubr(base_size = 12,border=TRUE)
                  }else if(input$color == "color3"){
                    p=fviz_pca_ind(data.pca,
                                 geom.ind = "point", # show points only (nbut not "text")
                                 col.ind = groupinfo$group, # color by groups,note that expression matrix rows match group
                                 addEllipses = input$cls, # Concentration ellipses
                                 ellipse.type = "confidence", # narrow ellipses
                                 legend.title = "Groups",
                                 pointsize=input$ps,
                                 palette="jco"
                                 )+theme_pubr(base_size = 12,border=TRUE)
                  }
            }else{
                if(input$color == "color1"){
                  p=fviz_pca_ind(data.pca,
                               geom.ind = "point", # show points only (nbut not "text")
                               addEllipses = FALSE, # Concentration ellipses
                               legend.title = "Groups",
                               pointsize=input$ps
                               )+theme_pubr(base_size = 12,border=TRUE)
                }else if(input$color == "color2"){
                  p=fviz_pca_ind(data.pca,
                               geom.ind = "point", # show points only (nbut not "text")
                               addEllipses = FALSE, # Concentration ellipses
                               legend.title = "Groups",
                               pointsize=input$ps,
                               palette="npg"
                               )+theme_pubr(base_size = 12,border=TRUE)
                }else if(input$color == "color3"){
                  p=fviz_pca_ind(data.pca,
                               geom.ind = "point", # show points only (nbut not "text")
                               addEllipses = FALSE, # Concentration ellipses
                               legend.title = "Groups",
                               pointsize=input$ps,
                               palette="jco"
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
                         addEllipses = input$cls, # Concentration ellipses
                         ellipse.type = "confidence", # narrow ellipses
                         legend.title = "Groups",
                         pointsize=input$ps
                         )+theme_pubr(base_size = 12,border=TRUE)
          }else if(input$color == "color2"){
            p=fviz_pca_ind(data.pca,
                         geom.ind = "point", # show points only (nbut not "text")
                         col.ind = groupinfo$group, # color by groups,note that expression matrix rows match group
                         addEllipses = input$cls, # Concentration ellipses
                         ellipse.type = "confidence", # narrow ellipses
                         legend.title = "Groups",
                         pointsize=input$ps,
                         palette="npg"
                         )+theme_pubr(base_size = 12,border=TRUE)
          }else if(input$color == "color3"){
            p=fviz_pca_ind(data.pca,
                         geom.ind = "point", # show points only (nbut not "text")
                         col.ind = groupinfo$group, # color by groups,note that expression matrix rows match group
                         addEllipses = input$cls, # Concentration ellipses
                         ellipse.type = "confidence", # narrow ellipses
                         legend.title = "Groups",
                         pointsize=input$ps,
                         palette="jco"
                         )+theme_pubr(base_size = 12,border=TRUE)
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
        filename="pca.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)     
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="pca.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="pca.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="pca.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
