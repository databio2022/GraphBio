#!/user/bin/Rscript
#network visualization
#shiny module

library(ggplot2)
library(geomnet)

netUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="网络图",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=800) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),
            width=8
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包geomnet。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和geomnet包。")
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
          numericInput(ns("pointsize"), label = "点大小", value = 12),
          numericInput(ns("labelsize"), label = "标签字体大小", value = 3),
          pickerInput(
               inputId = ns("color"),
               label = "选择颜色", 
               choices = paste0("color", 1),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "下载图片宽度", value = 10),
          numericInput(ns("h"), label = "下载图片高度", value = 10),
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

netServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/corr_net.csv",header=TRUE,sep=",",check.names=FALSE)
        modalDialog(
          span('基因互作关系。第1列为目标基因，第2列为相关基因，第3列为互作系数。例子显示了geneA与100个基因的相关性，值为Pearson相关系数。'),
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
      #init
      output$plot <- renderPlot({
        NULL
      })

      # The user's data, parsed into a data frame
      vals=reactiveValues()
      plota <- reactive({
        if(file_ext(input$file1$datapath) == "csv"){
          d=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE)
        }else if(file_ext(input$file1$datapath) == "txt"){
          d=read.table(input$file1$datapath,header=T,sep="\t",comment.char="",quote="",check.names=FALSE)
        }else if(file_ext(input$file1$datapath) == "xls"){
          d=readxl::read_xls(input$file1$datapath)
          d=as.data.frame(d)
        }else if(file_ext(input$file1$datapath) == "xlsx"){
          d=readxl::read_xlsx(input$file1$datapath)
          d=as.data.frame(d)
        }
          names(d)=c("gene1","gene2","value")
          d1=d[,c(2,1,3)]
          names(d1)=c("gene1","gene2","value")
          mtop=rbind(d,d1)
          if(input$color == "color1"){
              p=ggplot(mtop, aes(from_id = gene1, to_id = gene2)) + 
                  geom_net(aes(colour = value), size=input$pointsize,labelon=TRUE,labelcolour="black",fontsize=input$labelsize,show.legend=TRUE,ealpha=0.15,linewidth=0.15)+
                  scale_colour_gradient2(low="#00AFBB",mid="#E7B800",high="#FC4E07")+
                  xlim(c(-0.1,1.1))+ylim(c(-0.1,1.1))+theme_net()
          }else if(input$color == "color2"){
              p=ggplot(mtop, aes(from_id = gene1, to_id = gene2)) + 
                  geom_net(aes(colour = value), size=input$pointsize,labelon=TRUE,labelcolour="black",fontsize=input$labelsize,show.legend=TRUE,ealpha=0.15,linewidth=0.15)+
                  scale_colour_gradient2(low="#0073C299",mid="#EFC00099",high="#CD534C99")+
                  xlim(c(-0.1,1.1))+ylim(c(-0.1,1.1))+theme_net()
          }else if(input$color == "color3"){
              p=ggplot(mtop, aes(from_id = gene1, to_id = gene2)) + 
                  geom_net(aes(colour = value), size=input$pointsize,labelon=TRUE,labelcolour="black",fontsize=input$labelsize,show.legend=TRUE,ealpha=0.15,linewidth=0.15)+
                  scale_colour_gradient2(low="#00A08799",mid="#3C548899",high="#DC000099")+
                  xlim(c(-0.1,1.1))+ylim(c(-0.1,1.1))+theme_net()
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          d<-read.table("./www/corr_net.csv",header = T,sep=",",check.names=FALSE)
          names(d)=c("gene1","gene2","value")
          d1=d[,c(2,1,3)]
          names(d1)=c("gene1","gene2","value")
          mtop=rbind(d,d1)
          if(input$color == "color1"){
              p=ggplot(mtop, aes(from_id = gene1, to_id = gene2)) + 
                  geom_net(aes(colour = value), size=input$pointsize,labelon=TRUE,labelcolour="black",fontsize=input$labelsize,show.legend=TRUE,ealpha=0.15,linewidth=0.15)+
                  scale_colour_gradient2(low="#00AFBB",mid="#E7B800",high="#FC4E07")+
                  xlim(c(-0.1,1.1))+ylim(c(-0.1,1.1))+theme_net()
          }else if(input$color == "color2"){
              p=ggplot(mtop, aes(from_id = gene1, to_id = gene2)) + 
                  geom_net(aes(colour = value), size=input$pointsize,labelon=TRUE,labelcolour="black",fontsize=input$labelsize,show.legend=TRUE,ealpha=0.15,linewidth=0.15)+
                  scale_colour_gradient2(low="#0073C299",mid="#EFC00099",high="#CD534C99")+
                  xlim(c(-0.1,1.1))+ylim(c(-0.1,1.1))+theme_net()
          }else if(input$color == "color3"){
              p=ggplot(mtop, aes(from_id = gene1, to_id = gene2)) + 
                  geom_net(aes(colour = value), size=input$pointsize,labelon=TRUE,labelcolour="black",fontsize=input$labelsize,show.legend=TRUE,ealpha=0.15,linewidth=0.15)+
                  scale_colour_gradient2(low="#00A08799",mid="#3C548899",high="#DC000099")+
                  xlim(c(-0.1,1.1))+ylim(c(-0.1,1.1))+theme_net()
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
        filename="network_plot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)     
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="network_plot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="network_plot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="network_plot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
