#!/user/bin/Rscript
#sixiangxian plot
#shiny module

library(ggplot2)
library(ggpubr)
library(ggrepel)

sixiangxianUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        box(title="Four Quadrant Diagrams",solidHeader=TRUE,status='primary',background = "white",
            column(12, align="center", plotOutput(ns("plot"),width=600,height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5)),
            width=8),
                  #  tags$hr(),
                  #  tags$h6("该工具使用了R包ggplot2。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和ggplot2包。")),
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
          tags$h5("Upload a csv file(also supprot txt,xls,xlsx)"),
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
          numericInput(ns("fc1"), label = "First log2FC (Y axis)", value = 0),
          numericInput(ns("fc2"), label = "Second log2FC (X axis)", value = 0),
          numericInput(ns("ps"), label = "Point Size", value = 1),
          sliderTextInput(
             inputId = ns("tm"),
             label = "Transparency", 
             choices = seq(from = 0,
                 to = 1,
                 by = 0.1),
             selected = 1,
             grid = TRUE
          ),
          pickerInput(
               inputId = ns("color"),
               label = "Select Colors", 
               choices = paste0("color", 1:2),
               multiple = FALSE,
               selected = "color1"
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

sixiangxianServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      #modal
      dataModal <- function(failed = FALSE) {
        d=read.table("./www/sixiangxian_example.csv",header=T,sep=",",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        names(d)=c("geneid","log2FoldChange","padj_a","diff.log2.fc","padj_b","label")
        modalDialog(
          span('First column represents gene ID, second column represents log2FC,third column represents padj,fourth column represents log2FC from the other omics data, 
            fifth column represents padj from the other omics data,sixth column represents genes to be labeled on the figure.'),
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
          do=read.table(input$file1$datapath,header=T,sep=",",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        }else if(file_ext(input$file1$datapath) == "txt"){
          do=read.table(input$file1$datapath,header=T,sep="\t",comment.char="",quote="",check.names=FALSE,fill=TRUE)
        }else if(file_ext(input$file1$datapath) == "xls"){
          do=readxl::read_xls(input$file1$datapath)
          do=as.data.frame(do)
        }else if(file_ext(input$file1$datapath) == "xlsx"){
          do=readxl::read_xlsx(input$file1$datapath)
          do=as.data.frame(do)
        }
          names(do)=c("geneid","log2FoldChange","padj_a","diff.log2.fc","padj_b","label")
          group="none"
          da=data.frame(do,group)

          da[da$log2FoldChange > input$fc1 & da$diff.log2.fc > input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hyper-up"
          da[da$log2FoldChange < -input$fc1 & da$diff.log2.fc > input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hyper-down"
          da[da$log2FoldChange < -input$fc1 & da$diff.log2.fc < -input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hypo-down"
          da[da$log2FoldChange > input$fc1 & da$diff.log2.fc < -input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hypo-up"

          da$group=factor(da$group,levels = c("none", "Hyper-up" ,"Hypo-up", "Hypo-down","Hyper-down"))

          x=table(da$group)
          s1=paste("Hyper-up",x["Hyper-up"])
          s2=paste("Hypo-up",x["Hypo-up"])
          s3=paste("Hypo-down",x["Hypo-down"])
          s4=paste("Hyper-down",x["Hyper-down"])

          if(input$color == "color1"){
            p=ggplot(data=da, aes(x=diff.log2.fc,y=log2FoldChange,color=group,label=label)) + geom_point(data = da[da$group=="none",],color="gray",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-up",],color="#fc8d62",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-up",],color="#e78ac3",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-down",],color="#66c2a5",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-down",],color="#8da0cb",size=input$ps,alpha=input$tm)+
                    theme_bw()+geom_hline(yintercept = 0,linetype=1)+geom_hline(yintercept = c(-input$fc1,input$fc1),linetype=2)+
                    geom_vline(xintercept = c(-input$fc2,input$fc2),linetype=2)+geom_vline(xintercept = 0,linetype=1)+
                    xlim(c(-max(abs(da$diff.log2.fc)),max(abs(da$diff.log2.fc))))+ylim(c(-max(abs(da$log2FoldChange)),max(abs(da$log2FoldChange))))+
                    theme(
                    panel.grid=element_blank(),
                    axis.title = element_text(size = 16),
                    axis.text = element_text(size = 14))+ annotate(geom = 'text', label = s1, x = Inf, y = Inf, hjust = 1.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s2, x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s3, x = -Inf, y = -Inf, hjust = -0.2, vjust = -0.7)+
                    annotate(geom = 'text', label = s4, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)+geom_label_repel(
                    fontface="bold",
                    color="purple",
                    box.padding=unit(1, "lines"),
                    point.padding=unit(0.5, "lines"),
                    segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = da[da$label!="",], color = "purple")+labs(color="class")
          }else if(input$color == "color2"){
            p=ggplot(data=da, aes(x=diff.log2.fc,y=log2FoldChange,color=group,label=label)) + geom_point(data = da[da$group=="none",],color="gray",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-up",],color="red",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-up",],color="green",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-down",],color="orange",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-down",],color="blue",size=input$ps,alpha=input$tm)+
                    theme_bw()+geom_hline(yintercept = 0,linetype=1)+geom_hline(yintercept = c(-input$fc1,input$fc1),linetype=2)+
                    geom_vline(xintercept = c(-input$fc2,input$fc2),linetype=2)+geom_vline(xintercept = 0,linetype=1)+
                    xlim(c(-max(abs(da$diff.log2.fc)),max(abs(da$diff.log2.fc))))+ylim(c(-max(abs(da$log2FoldChange)),max(abs(da$log2FoldChange))))+
                    theme(
                    panel.grid=element_blank(),
                    axis.title = element_text(size = 16),
                    axis.text = element_text(size = 14))+ annotate(geom = 'text', label = s1, x = Inf, y = Inf, hjust = 1.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s2, x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s3, x = -Inf, y = -Inf, hjust = -0.2, vjust = -0.7)+
                    annotate(geom = 'text', label = s4, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)+geom_label_repel(
                    fontface="bold",
                    color="purple",
                    box.padding=unit(1, "lines"),
                    point.padding=unit(0.5, "lines"),
                    segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = da[da$label!="",], color = "purple")+labs(color="class")
          }
          vals$p=p
          p
      })

      #example
      plote <- reactive({
          do=read.table("./www/sixiangxian_example.csv",header=T,sep=",",comment.char="",quote="",check.names=FALSE,fill=TRUE)
          names(do)=c("geneid","log2FoldChange","padj_a","diff.log2.fc","padj_b","label")
          group="none"
          da=data.frame(do,group)

          da[da$log2FoldChange > input$fc1 & da$diff.log2.fc > input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hyper-up"
          da[da$log2FoldChange < -input$fc1 & da$diff.log2.fc > input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hyper-down"
          da[da$log2FoldChange < -input$fc1 & da$diff.log2.fc < -input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hypo-down"
          da[da$log2FoldChange > input$fc1 & da$diff.log2.fc < -input$fc2 & da$padj_a <= 0.05 & da$padj_b <= 0.05,]$group="Hypo-up"

          da$group=factor(da$group,levels = c("none", "Hyper-up" ,"Hypo-up", "Hypo-down","Hyper-down"))

          x=table(da$group)
          s1=paste("Hyper-up",x["Hyper-up"])
          s2=paste("Hypo-up",x["Hypo-up"])
          s3=paste("Hypo-down",x["Hypo-down"])
          s4=paste("Hyper-down",x["Hyper-down"])

          if(input$color == "color1"){
            p=ggplot(data=da, aes(x=diff.log2.fc,y=log2FoldChange,color=group,label=label)) + geom_point(data = da[da$group=="none",],color="gray",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-up",],color="#fc8d62",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-up",],color="#e78ac3",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-down",],color="#66c2a5",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-down",],color="#8da0cb",size=input$ps,alpha=input$tm)+
                    theme_bw()+geom_hline(yintercept = 0,linetype=1)+geom_hline(yintercept = c(-input$fc1,input$fc1),linetype=2)+
                    geom_vline(xintercept = c(-input$fc2,input$fc2),linetype=2)+geom_vline(xintercept = 0,linetype=1)+
                    xlim(c(-max(abs(da$diff.log2.fc)),max(abs(da$diff.log2.fc))))+ylim(c(-max(abs(da$log2FoldChange)),max(abs(da$log2FoldChange))))+
                    theme(
                    panel.grid=element_blank(),
                    axis.title = element_text(size = 16),
                    axis.text = element_text(size = 14))+ annotate(geom = 'text', label = s1, x = Inf, y = Inf, hjust = 1.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s2, x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s3, x = -Inf, y = -Inf, hjust = -0.2, vjust = -0.7)+
                    annotate(geom = 'text', label = s4, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)+geom_label_repel(
                    fontface="bold",
                    color="purple",
                    box.padding=unit(1, "lines"),
                    point.padding=unit(0.5, "lines"),
                    segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = da[da$label!="",], color = "purple")+labs(color="class")
          }else if(input$color == "color2"){
            p=ggplot(data=da, aes(x=diff.log2.fc,y=log2FoldChange,color=group,label=label)) + geom_point(data = da[da$group=="none",],color="gray",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-up",],color="red",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-up",],color="green",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hypo-down",],color="orange",size=input$ps,alpha=input$tm)+
                    geom_point(data = da[da$group=="Hyper-down",],color="blue",size=input$ps,alpha=input$tm)+
                    theme_bw()+geom_hline(yintercept = 0,linetype=1)+geom_hline(yintercept = c(-input$fc1,input$fc1),linetype=2)+
                    geom_vline(xintercept = c(-input$fc2,input$fc2),linetype=2)+geom_vline(xintercept = 0,linetype=1)+
                    xlim(c(-max(abs(da$diff.log2.fc)),max(abs(da$diff.log2.fc))))+ylim(c(-max(abs(da$log2FoldChange)),max(abs(da$log2FoldChange))))+
                    theme(
                    panel.grid=element_blank(),
                    axis.title = element_text(size = 16),
                    axis.text = element_text(size = 14))+ annotate(geom = 'text', label = s1, x = Inf, y = Inf, hjust = 1.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s2, x = -Inf, y = Inf, hjust = -0.2, vjust = 1.5)+
                    annotate(geom = 'text', label = s3, x = -Inf, y = -Inf, hjust = -0.2, vjust = -0.7)+
                    annotate(geom = 'text', label = s4, x = Inf, y = -Inf, hjust = 1.2, vjust = -0.7)+geom_label_repel(
                    fontface="bold",
                    color="purple",
                    box.padding=unit(1, "lines"),
                    point.padding=unit(0.5, "lines"),
                    segment.colour = "purple",segment.size = 0.5,segment.alpha = 0.5,max.overlaps = Inf)+geom_point(data = da[da$label!="",], color = "purple")+labs(color="class")
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
        filename="sixiangxian_plot.pdf",
        content = function(file){
          pdf(file,width=input$w,height=input$h,onefile=FALSE)
          print(vals$p)      
          dev.off()
        }
      )
      output$png <- downloadHandler(
        filename="sixiangxian_plot.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="sixiangxian_plot.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="sixiangxian_plot.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )

    }
  )    
}
