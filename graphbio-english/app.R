##GraphBio##
#heatmap
#add venn plot
#add volcano plot
#add text cluster
#add bubbleA 
#add correlation scatter plot
#add pie plot
#add ecdf
#add surv curve
#add ma plot
#add sixiangxian plot
#add PCA plot
#add CORR MATRIX plot
#add GO bubble plot
#add network plot
#version 2.2.5
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinycssloaders)
library(shinyFiles)
library(DT)
library(shinyjs)

library(pheatmap)
library(viridis)
library(grid)
library(ggplotify)
library(waiter)

source("module/venn.R",encoding = "utf-8")
source("module/volcano.R",encoding = "utf-8")
source("module/text_cluster.R",encoding = "utf-8")
source("module/bubbleA.R",encoding = "utf-8")
source("module/corr_scatter.R",encoding = "utf-8")
source("module/pie.R",encoding = "utf-8")
source("module/cdc.R",encoding = "utf-8")
source("module/km.R",encoding = "utf-8")
source("module/roc.R",encoding = "utf-8")
source("module/ma.R",encoding = "utf-8")
source("module/sixiangxian.R",encoding = "utf-8")
source("module/corr_matrix.R",encoding = "utf-8")
source("module/pca.R",encoding = "utf-8")
source("module/gobubble.R",encoding = "utf-8")
source("module/vis_net.R",encoding = "utf-8")
source("module/chord.R",encoding = "utf-8")

example=read.table("./www/heatmap_test.csv",header=TRUE,sep=",",row.names=1)
example1=read.table("./www/group_info.csv",header=TRUE,sep=",")
ui <- dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ...")),
    title = "GraphBio: a shiny web app to easily perform popular visualization analysis for omics data",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "GraphBio",
        color = "primary",
        href = "#",
        image = "GB.png",
        opacity=1
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE
    ),
  ## Sidebar content
   sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      sidebarUserPanel(
        image = "dashboard.svg",
        name = "Welcome to use GraphBio"
      ),
      sidebarMenu(id="sidebar",
        sidebarHeader("Omics data visualization"),
        menuItem("Help Center", tabName = "help", icon = ionicon(name="information-circle")),
        menuItem("Contact Us", tabName = "contact", icon = ionicon(name="call")),
        menuItem("FAQ", tabName = "faq", icon = ionicon(name="help-circle")),
        menuItem("Heatmap", tabName = "heatmap", icon = ionicon(name="arrow-forward"),selected=TRUE),
        menuItem("Venn Diagram", tabName = "venn", icon = ionicon(name="arrow-forward")),
        menuItem("Volcano Plot", tabName = "volcano", icon = ionicon(name="arrow-forward")),
        menuItem("Cluster Analysis", tabName = "cluster", icon = ionicon(name="arrow-forward"),
          menuSubItem("Text Cluster",tabName = "textcluster")
        ),
        menuItem("Dot Plot", tabName = "bubble", icon = ionicon(name="arrow-forward"),
          menuSubItem("Style 1",tabName = "bubbleA"),
          menuSubItem("Style 2",tabName = "bubbleB")
        ),
        menuItem("Correlation Analysis", tabName = "corr", icon = ionicon(name="arrow-forward"),
          menuSubItem("Scatter Plot",tabName = "corrA"),
          menuSubItem("Heatmap",tabName = "corrM")
        ),
        menuItem("Pie Plot", tabName = "pie", icon = ionicon(name="arrow-forward")),
        menuItem("Cumulative Distribution Curve", tabName = "cdc", icon = ionicon(name="arrow-forward")),
        menuItem("Survival Curve", tabName = "km", icon = ionicon(name="arrow-forward")),
        menuItem("ROC Curve", tabName = "roc", icon = ionicon(name="arrow-forward")),
        menuItem("MA Plot", tabName = "ma", icon = ionicon(name="arrow-forward")),
        menuItem("Four Quadrant Diagrams", tabName = "sixx", icon = ionicon(name="arrow-forward")),
        menuItem("PCA Analysis", tabName = "pca", icon = ionicon(name="arrow-forward")),
        menuItem("Network Plot", tabName = "net", icon = ionicon(name="arrow-forward")),
        menuItem("GOplot(chord plot)", tabName = "chord", icon = ionicon(name="arrow-forward"))
      )
    ),
    footer = dashboardFooter(
      left = a(
        href = "http://www.databio1.com",
        target = "_blank", "Shuzhi Biotech, LLC, Guangzhou, Guangdong, China.",a(
        href = "https://beian.miit.gov.cn/",
        target = "_blank", "粤ICP备2021032373号"
      )
      ),
      right = "2022"
    ),
    body  = dashboardBody(
        tags$head(includeScript("./js/baidu_analysis.js")),
        tabItems(
            tabItem(tabName = "heatmap",
            fluidRow(
                box(title="Heatmap",solidHeader=TRUE,status='primary',background = "white",
                    plotOutput("plot",height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8),
                    #tags$hr(),
                    #tags$h6("该工具使用了R包pheatmap。如果在您的研究工作中使用到该工具，请引用该网址(GraphBio: www.graphbio1.com)和pheatmap包。")),
                box(width=4,
                  # Input: Select a file ----               
                  actionBttn(
                     inputId = "rune",
                     label = "run example",
                     style = "fill", 
                      color = "warning",
                      size = "sm",
                  ),  
                  tags$hr(),                
                  tags$h5("Upload a gene expression file (support csv,txt,xls,xlsx)"),
                  actionBttn(
                     inputId = "show",
                     label = "view example file",
                     style = "fill", 
                      color = "primary",
                      size = "sm",
                  ),
                  tags$br(),
                  tags$br(),
                  fileInput("file1",NULL,
                            multiple = FALSE,
                            accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                  tags$h5("Upload a sample metadata file (support csv,txt,xls,xlsx, optional)"),
                  actionBttn(
                     inputId = "show1",
                     label = "view example file",
                     style = "fill", 
                      color = "primary",
                      size = "sm",
                  ),
                  tags$br(),
                  tags$br(),
                  fileInput("file2",NULL,
                            multiple = FALSE,
                            accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                  pickerInput(
                       inputId = "color",
                       label = "Select Colors", 
                       choices = paste0("color", 1:6),
                       multiple = FALSE,
                       selected = "color1"
                    ),
                   awesomeRadio(
                       inputId = "cluster",
                       label = "Select Cluster Methods", 
                       choices = c("complete", "average", "none"),
                       selected = "complete",
                       status = "warning"
                     ),  
                   switchInput(
                      inputId = "rowname",
                      label = "Gene Name",
                      labelWidth = "100px",
                      value = FALSE
                   ),
                   switchInput(
                      inputId = "colname",
                      label = "Sample Name",
                      labelWidth = "100px",
                      value = FALSE
                   ),
                  numericInput("w", label = "Figure Width", value = 9),
                  numericInput("h", label = "Figure Height", value = 6),
                  numericInput("ppi", label = "Figure Resolution", value = 72),
                  dropdownButton(
                    downloadBttn(
                      outputId = "pdf",
                      label="PDF figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = "png",
                      label="PNG figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = "jpeg",
                      label="JPEG figure",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = "tiff",
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
            ),
          tabItem(tabName = "help",
            fluidRow(box(width=12,
            title="Help Center",solidHeader=TRUE,status='primary',background = "white",height="100%",
            tags$h2("Help Center"),
            tags$hr(),
            tags$p("GraphBio includes various of popular data visualization function modules for omics data, and each module follows same design priciple so as to let users use easily. 
            GraphBio supports mutiple common inputfile formats including csv,txt(tab-separated),xls,xlsx, and mutiple common figure formats including pdf,png,jpeg,tiff can be easily downloaded.
            Here, let's take heatmap as an example to demonstrate the abilities of GraphBio.
            First of all, we need to click <heatmap> module on the left panel of GraphBio.Then we can see a parameters settings panel on the right of GraphBio."),
            tags$img(src="x1.png",width="50%",height="50%"),
            tags$p("Before upload a file, we need to click <view example file> button and view content style."),
            tags$img(src="format.png",width="50%",height="50%"),
            tags$p("Subsequently,we can prepare our own file via Excel software
            according to reference example file. The prepared file can be saved as one of four formats(csv,txt,xls,xlsx)."),
            tags$img(src="heatmap1.png",width="50%",height="50%"),
            tags$p("Then upload the file by clicking <Browse...> button.
            When the upload flow is complete, the figure is automaticly made."),
            tags$img(src="show.png",width="50%",height="50%"),
            tags$p("Usually, we possiably want to add a group annotation bar on the figure.Thus, we can upload another sample
            metadata file."),
            tags$img(src="heatmap2.png"),
            tags$img(src="show1.png",width="50%",height="50%"),
            tags$p("Moreover,we provided mutiple popular color presets selections and other necessary cutomization settings,users can freely test these settings.Finally,we prodvied mutiple popular figure formats for users to download."),
            tags$img(src="settings.png",width="50%",height="50%")
            
            )
            )
          ),
          tabItem(tabName = "contact",
            fluidRow(box(width=12,
            title="Contact Us",solidHeader=TRUE,status='primary',background = "white",height=800,
            tags$p("GraphBio is a easy-to-use visualization analysis tool for omics data. It provides 15 popular visualization analysis modules, 
              including heatmap, volcano plots, MA plots, network plots, dot plots, chord plots, pie plots, four quadrant diagrams, venn diagrams, 
              cumulative distribution curves, PCA, survival analysis, ROC analysis, correlation analysis and text cluster analysis. This enables 
              experimental biologists without programming skills to easily perform visualization analysis and get publication-ready plots in shorter time.In the future,
              we will continue to integrate popular visualization analysis methods into GraphBio, and provide more easy-to-use modules to research community, 
              accelerating the pace of scientific research in cloud era."),
            tags$p("If you have any problem while using GraphBio, pleast feel free to contact us (databio@163.com).")
            ))
          ),
          tabItem(tabName = "faq",
            fluidRow(box(width=12,
            title="FAQ",solidHeader=TRUE,status='primary',background = "white",height=800,
            tags$h2("FAQ"),
            tags$hr(),
            tags$p("1. GraphBio limits file uploads to 5MB per file."),
            tags$p("2. When we save file as csv format in Mircosoft Excel，please do not select csv UTF-8 format."),
            tags$p("3. When clicking [run example] button, the generated figure can only be viewed instead of being downloaded."),
            tags$p("4. When clicking [view example file] button, example file only show part data for demonstrating the format of input file."),
            tags$p("5. The downloaded PDF figures can be easily edited using Adobe Illustrator or Adobe Acrobat software.")
              )
            )
          ),
          tabItem(tabName="venn",vennUI("venn")),
          tabItem(tabName="volcano",volcanoUI("volcano")),
          tabItem(tabName="textcluster",textclusterUI("textcluster")),
          tabItem(tabName="bubbleA",bubbleUI("bubbleA")),
          tabItem(tabName="bubbleB",gobubbleUI("bubbleB")),
          tabItem(tabName="corrA",corrscatterUI("corrA")),
          tabItem(tabName="corrM",corrmUI("corrM")),
          tabItem(tabName="pie",pieUI("pie")),
          tabItem(tabName="cdc",cdcUI("cdc")),
          tabItem(tabName="km",kmUI("km")),
          tabItem(tabName="roc",rocUI("roc")),
          tabItem(tabName="ma",maUI("ma")),
          tabItem(tabName="sixx",sixiangxianUI("sixx")),
          tabItem(tabName="pca",pcaUI("pca")),
          tabItem(tabName="net",netUI("net")),
          tabItem(tabName="chord",chordUI("chord"))
        )
    )
)

server <- function(input, output, session) {
      waiter_hide()
      #venn module
      vennServer("venn")
      #volcano module
      volcanoServer("volcano")
      #text cluster module
      textclusterServer("textcluster")
      #bubbleA
      bubbleServer("bubbleA")
      #corrA
      corrscatterServer("corrA")
      #pie
      pieServer("pie")
      #cdc
      cdcServer("cdc")
      #surv
      kmServer("km")
      #roc
      rocServer("roc")
      #ma
      maServer("ma")
      #sixiangxian
      sixiangxianServer("sixx")
      #corr matrix
      corrmServer("corrM")
      #pca
      pcaServer("pca")
      #go bubble
      gobubbleServer("bubbleB")
      #net
      netServer("net")
      #chord
      chordServer("chord")
      #heatmap
      dataModal <- function(failed = FALSE) {
        modalDialog(
          renderTable(example[1:6,1:6],rownames=TRUE),
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

      dataModalx <- function(failed = FALSE) {
        modalDialog(
          span('Note: the samples in metadata file must keep the same order with that of gene expression file. Support two or more groups.'),
          tags$hr(),
          renderTable(example1[1:6,],rownames=FALSE),
          easyClose=TRUE,
          footer = tagList(
            modalButton("Close")
          )
        )
      }
      
      # Show modal when button is clicked.
      observeEvent(input$show1, {
        showModal(dataModalx())
      })

      #init
      output$plot <- renderPlot({
        NULL
      })
      #main content
      #example
      vals=reactiveValues()
      heatmape <- reactive({
            d=example
            d=d[which(apply(d,1,sd) > 0),]
            d=t(scale(t(d)))
            d[d > 1.5]=1.5
            d[d < -1.5]=-1.5
            grp=example1
            annotation_col = data.frame(name=factor(grp$group))
            rownames(annotation_col) = colnames(d)
            #pheatmap(d,scale="none",annotation_col = annotation_col,color=colorRampPalette(viridis(3))(100),show_rownames=F,show_colnames=F,cluster_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
            if(input$cluster=="complete"){
                if(input$color == "color1"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                }else if(input$color == "color2"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                }
                else if(input$color == "color3"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                            }
                else if(input$color == "color4"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                            }
                else if(input$color == "color5"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                            }
                else if(input$color == "color6"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                            }
            }else if(input$cluster=="average"){
                if(input$color == "color1"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                }else if(input$color == "color2"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                }
                else if(input$color == "color3"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                            }
                else if(input$color == "color4"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                            }
                else if(input$color == "color5"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                            }
                else if(input$color == "color6"){
                    p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                            }                     
            }else{
                    if(input$color == "color1"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }else if(input$color == "color2"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }
                    else if(input$color == "color3"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color4"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color5"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color6"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }                   
                }
            vals$p=p
            p
        })

      observeEvent(input$rune, {
        output$plot <- renderPlot({  
              heatmape()
        })
      })
      #input file
     
      heatmap <- reactive({
            req(input$file1)
            if(file_ext(input$file1$datapath) == "csv"){
                d=read.table(input$file1$datapath,header=TRUE,row.names=1,sep=",",quote="",comment.char = "",check.names=FALSE)
            }else if(file_ext(input$file1$datapath) == "txt"){
                d=read.table(input$file1$datapath,header=TRUE,row.names=1,sep="\t",quote="",comment.char = "",check.names=FALSE)
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
            d=d[which(apply(d,1,sd) > 0),]
            d=t(scale(t(d)))
            d[d > 1.5]=1.5
            d[d < -1.5]=-1.5
            vals$d=d
            if(!is.null(input$file2$datapath)){
              if(file_ext(input$file2$datapath) == "csv"){
                grp=read.table(input$file2$datapath,header=TRUE,row.names=1,sep=",",quote="",comment.char = "",check.names=FALSE)
              }else if(file_ext(input$file2$datapath) == "txt"){
                grp=read.table(input$file2$datapath,header=TRUE,row.names=1,sep="\t",quote="",comment.char = "",check.names=FALSE)
              }else if(file_ext(input$file2$datapath) == "xls"){
                grp=readxl::read_xls(input$file2$datapath)
                grp=as.data.frame(grp)
                rownames(grp)=grp[,1]
                grp=grp[,-1]
              }else if(file_ext(input$file2$datapath) == "xlsx"){
                grp=readxl::read_xlsx(input$file2$datapath)
                grp=as.data.frame(grp)
                rownames(grp)=grp[,1]
                grp=grp[,-1]
              }
                annotation_col = data.frame(name=factor(grp$group))
                rownames(annotation_col) = colnames(d)
                #pheatmap(d,scale="none",annotation_col = annotation_col,color=colorRampPalette(viridis(3))(100),show_rownames=F,show_colnames=F,cluster_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                if(input$cluster=="complete"){
                    if(input$color == "color1"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                    }else if(input$color == "color2"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                    }
                    else if(input$color == "color3"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                    else if(input$color == "color4"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                    else if(input$color == "color5"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                    else if(input$color == "color6"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                }else if(input$cluster=="average"){
                    if(input$color == "color1"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }else if(input$color == "color2"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }
                    else if(input$color == "color3"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color4"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color5"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color6"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }                     
                }else{
                    if(input$color == "color1"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }else if(input$color == "color2"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }
                    else if(input$color == "color3"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color4"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color5"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color6"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }                   
                }
                vals$p=p
                p
            }else{
                if(input$cluster=="complete"){
                    if(input$color == "color1"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                    }else if(input$color == "color2"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                    }
                    else if(input$color == "color3"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                    else if(input$color == "color4"){
                        p=pheatmap(d,scale="none",show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                    else if(input$color == "color5"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                    else if(input$color == "color6"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method=input$cluster)
                                }
                }else if(input$cluster=="average"){
                    if(input$color == "color1"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }else if(input$color == "color2"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }
                    else if(input$color == "color3"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color4"){
                        p=pheatmap(d,scale="none",show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color5"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color6"){
                        p=pheatmap(d,scale="none",color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=T,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }                    
                }else{
                    if(input$color == "color1"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(viridis(3))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }else if(input$color == "color2"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("blue", "white", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                    }
                    else if(input$color == "color3"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("green", "black", "red"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color4"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color5"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("#3182bd","#272222","#f2f609"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }
                    else if(input$color == "color6"){
                        p=pheatmap(d,scale="none",annotation_col=annotation_col,color=colorRampPalette(c("navy", "white", "firebrick3"))(1000),show_rownames=input$rowname,show_colnames=input$colname,cluster_cols=F,border_color=NA,clustering_method="average",clustering_distance_cols="correlation",clustering_distance_rows="correlation")
                                }                   
                }
                vals$p=p
                p
            }
        })

        observeEvent(input$file1, {
          output$plot <- renderPlot({  
                heatmap()
          })
        })

        output$pdf <- downloadHandler(
          filename="heatmap.pdf",
          content = function(file){
            pdf(file,width=input$w,height=input$h)
            print(vals$p)
            dev.off()
          }
        )
      output$png <- downloadHandler(
        filename="heatmap.png",
        content = function(file){
          png(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$jpeg <- downloadHandler(
        filename="heatmap.jpeg",
        content = function(file){
          jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )
      output$tiff <- downloadHandler(
        filename="heatmap.tiff",
        content = function(file){
          tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
          print(vals$p)
          dev.off()
        }
      )


}

shinyApp(ui, server)

