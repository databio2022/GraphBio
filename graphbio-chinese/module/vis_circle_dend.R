#!/user/bin/Rscript
#network visualization
#shiny module

library(ggraph)
library(igraph)
library(ggplot2)
library(RColorBrewer) 

circledendUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="circle dendogram",solidHeader=TRUE,status='primary',background = "white",
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
          tags$h5("上传边信息文件(支持csv、txt、xls、xlsx)"),
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
          tags$h5("上传节点信息文件(支持csv、txt、xls、xlsx)"),
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
          numericInput(ns("pointsize"), label = "点大小", value = 10),
          numericInput(ns("labelsize"), label = "标签字体大小", value = 3),
          pickerInput(
               inputId = ns("color"),
               label = "选择颜色", 
               choices = paste0("color", seq(3)),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "下载图片宽度", value = 10),
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

circledendServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/circle_dend_edge_example.csv",header=TRUE,sep=",",check.names=FALSE)
        modalDialog(
          span('边信息。示例图中心节点统一为root节点，然后引申6个组，每个组5个基因。'),
          tags$hr(),
          renderTable(d[1:20,],rownames=FALSE),
          easyClose=TRUE,
          size="l",
          footer = tagList(
            modalButton("关闭")
          )
        )
      }

      dataModal1 <- function(failed = FALSE) {
        d=read.table("./www/circle_dend_node_example.csv",header=TRUE,sep=",",check.names=FALSE)
        modalDialog(
          span('节点信息。示例给出了每个节点（基因）的log2FC值，以及对应的分组信息。'),
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
      observeEvent(input$show1, {
        showModal(dataModal1())
      })

      #init
      output$plot <- renderPlot({
        NULL
      })

      # The user's data, parsed into a data frame
      vals=reactiveValues()
      plota <- reactive({
          # create a data frame giving the hierarchical structure of your individuals
          if(file_ext(input$file1$datapath) == "csv"){
            edges=read.table(input$file1$datapath,header=TRUE,sep=",",check.names=FALSE)
          }else if(file_ext(input$file1$datapath) == "txt"){
            edges=read.table(input$file1$datapath,header=TRUE,sep="\t",check.names=FALSE)
          }else if(file_ext(input$file1$datapath) == "xls"){
          edges=readxl::read_xls(input$file1$datapath)
          edges=as.data.frame(edges)
        }else if(file_ext(input$file1$datapath) == "xlsx"){
          edges=readxl::read_xlsx(input$file1$datapath)
          edges=as.data.frame(edges)
        }
          # create a vertices data.frame. One line per object of our hierarchy
          if(file_ext(input$file2$datapath) == "csv"){
            vertices = read.table(input$file2$datapath,header=TRUE,sep=",",check.names=FALSE)
          }else if(file_ext(input$file2$datapath) == "txt"){
            vertices = read.table(input$file2$datapath,header=TRUE,sep="\t",check.names=FALSE)
          }else if(file_ext(input$file2$datapath) == "xls"){
          vertices=readxl::read_xls(input$file2$datapath)
          vertices=as.data.frame(vertices)
        }else if(file_ext(input$file2$datapath) == "xlsx"){
          vertices=readxl::read_xlsx(input$file2$datapath)
          vertices=as.data.frame(vertices)
        }
          colnames(vertices)=c("name","log2FC","group")
          # Create a graph object
          mygraph <- graph_from_data_frame( edges, vertices=vertices ) 
          groupnum=length(unique(vertices$group))-1

          if(input$color == "color1"){
            p=ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
              geom_edge_diagonal(colour="grey") +
              geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = -((-node_angle(x, y)+90)%%180)+90, hjust='outward', colour=group), size=input$labelsize) +
              geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=log2FC)) +
              scale_size_continuous(range = c(0.1,input$pointsize) ) +
              scale_colour_manual(values= brewer.pal(groupnum,"Paired")) +
              theme_void()+
              theme(
                plot.margin=unit(c(3,3,3,3),"cm"),
              )+
              expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
          }else if(input$color == "color2"){
            p=ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
              geom_edge_diagonal(colour="grey") +
              geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = -((-node_angle(x, y)+90)%%180)+90, hjust='outward', colour=group), size=input$labelsize) +
              geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=log2FC)) +
              scale_size_continuous(range = c(0.1,input$pointsize) ) +
              scale_colour_manual(values= brewer.pal(groupnum,"Dark2")) +
              theme_void()+
              theme(
                plot.margin=unit(c(3,3,3,3),"cm"),
              )+
              expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
          }else if(input$color == "color3"){
            p=ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
              geom_edge_diagonal(colour="grey") +
              geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = -((-node_angle(x, y)+90)%%180)+90, hjust='outward', colour=group), size=input$labelsize) +
              geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=log2FC)) +
              scale_size_continuous(range = c(0.1,input$pointsize) ) +
              theme_void()+
              theme(
                plot.margin=unit(c(3,3,3,3),"cm"),
              )+
              expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          # create a data frame giving the hierarchical structure of your individuals
          edges=read.table("./www/circle_dend_edge_example.csv",header=TRUE,sep=",",check.names=FALSE)
          # create a vertices data.frame. One line per object of our hierarchy
          vertices = read.table("./www/circle_dend_node_example.csv",header=TRUE,sep=",",check.names=FALSE)
          colnames(vertices)=c("name","log2FC","group")
          # Create a graph object
          mygraph <- graph_from_data_frame( edges, vertices=vertices ) 
          groupnum=length(unique(vertices$group))-1

          if(input$color == "color1"){
            p=ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
              geom_edge_diagonal(colour="grey") +
              geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = -((-node_angle(x, y)+90)%%180)+90, hjust='outward', colour=group), size=input$labelsize) +
              geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=log2FC)) +
              scale_size_continuous(range = c(0.1,input$pointsize) ) +
              scale_colour_manual(values= brewer.pal(groupnum,"Paired")) +
              theme_void()+
              theme(
                plot.margin=unit(c(3,3,3,3),"cm"),
              )+
              expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
          }else if(input$color == "color2"){
            p=ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
              geom_edge_diagonal(colour="grey") +
              geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = -((-node_angle(x, y)+90)%%180)+90, hjust='outward', colour=group), size=input$labelsize) +
              geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=log2FC)) +
              scale_size_continuous(range = c(0.1,input$pointsize) ) +
              scale_colour_manual(values= brewer.pal(groupnum,"Dark2")) +
              theme_void()+
              theme(
                plot.margin=unit(c(3,3,3,3),"cm"),
              )+
              expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
          }else if(input$color == "color3"){
            p=ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
              geom_edge_diagonal(colour="grey") +
              geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = -((-node_angle(x, y)+90)%%180)+90, hjust='outward', colour=group), size=input$labelsize) +
              geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=log2FC)) +
              scale_size_continuous(range = c(0.1,input$pointsize) ) +
              theme_void()+
              theme(
                plot.margin=unit(c(3,3,3,3),"cm"),
              )+
              expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
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


      # inputfile2
      observeEvent(input$file2, {
        output$plot <- renderPlot({  
              plota()
        })
      })

      #download pdf figure
      output$pdf <- downloadHandler(
        filename="circle_dend.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)     
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="circle_dend.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="circle_dend.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="circle_dend.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
