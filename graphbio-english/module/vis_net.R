#!/user/bin/Rscript
#network visualization
#shiny module

library(ggplot2)
library(geomnet)

netUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="Network Plot",solidHeader=TRUE,status='primary',background = "white",
            plotOutput(ns("plot"),height=800) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),
            width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包geomnet。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和geomnet包。")),
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
          numericInput(ns("pointsize"), label = "Point Size", value = 12),
          numericInput(ns("labelsize"), label = "Label Size", value = 3),
          pickerInput(
               inputId = ns("color"),
               label = "Select Colors", 
               choices = paste0("color", 1:3),
               multiple = FALSE,
               selected = "color1"
            ),
          numericInput(ns("w"), label = "Figure Width", value = 10),
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

netServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/corr_net.csv",header=TRUE,sep=",",check.names=FALSE)
        modalDialog(
          span('Gene co-expression network。First column represents target gene,second column represents correlated genes with target gene, third column represents correlation coefficients.'),
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
