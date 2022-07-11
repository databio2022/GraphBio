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
source("module/vis_circle_dend.R",encoding = "utf-8")
source("module/stackbar.R",encoding = "utf-8")

example=read.table("./www/heatmap_test.csv",header=TRUE,sep=",",row.names=1)
example1=read.table("./www/group_info.csv",header=TRUE,sep=",")
ui <- dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ...")),
    title = "在线生信作图GraphBio-广州数智生物科技有限责任公司",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "数智生物",
        color = "primary",
        href = "http://www.databio1.com",
        image = "logo.jpg",
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
        name = "欢迎使用GraphBio"
      ),
      sidebarMenu(id="sidebar",
        sidebarHeader("基因测序数据可视化"),
        menuItem("使用说明", tabName = "help", icon = ionicon(name="information-circle")),
        menuItem("联系我们", tabName = "contact", icon = ionicon(name="call")),
        menuItem("常见问题", tabName = "faq", icon = ionicon(name="help-circle")),
        menuItem("热图", tabName = "heatmap", icon = ionicon(name="arrow-forward"),selected=TRUE),
        menuItem("韦恩图", tabName = "venn", icon = ionicon(name="arrow-forward")),
        menuItem("火山图", tabName = "volcano", icon = ionicon(name="arrow-forward")),
        menuItem("聚类分析", tabName = "cluster", icon = ionicon(name="arrow-forward"),
          menuSubItem("文本聚类",tabName = "textcluster")
        ),
        menuItem("气泡图", tabName = "bubble", icon = ionicon(name="arrow-forward"),
          menuSubItem("样式一",tabName = "bubbleA"),
          menuSubItem("样式二",tabName = "bubbleB")
        ),
        menuItem("相关性分析", tabName = "corr", icon = ionicon(name="arrow-forward"),
          menuSubItem("散点图",tabName = "corrA"),
          menuSubItem("矩阵",tabName = "corrM")
        ),
        menuItem("堆叠柱状图", tabName = "stackbar", icon = ionicon(name="arrow-forward")),
        menuItem("饼图", tabName = "pie", icon = ionicon(name="arrow-forward")),
        menuItem("累积分布曲线", tabName = "cdc", icon = ionicon(name="arrow-forward")),
        menuItem("生存曲线", tabName = "km", icon = ionicon(name="arrow-forward")),
        menuItem("ROC曲线", tabName = "roc", icon = ionicon(name="arrow-forward")),
        menuItem("MA图", tabName = "ma", icon = ionicon(name="arrow-forward")),
        menuItem("四象限图", tabName = "sixx", icon = ionicon(name="arrow-forward")),
        menuItem("主成分分析", tabName = "pca", icon = ionicon(name="arrow-forward")),
        menuItem("网络图", tabName = "xnet", icon = ionicon(name="arrow-forward"),
          menuSubItem("Simple",tabName = "net"),
          menuSubItem("Circle_dend",tabName = "circle_dend")          
        ),
        menuItem("弦(chord)图", tabName = "chord", icon = ionicon(name="arrow-forward"))
      )
    ),
    footer = dashboardFooter(
      left = a(
        href = "http://www.databio1.com",
        target = "_blank", "广州数智生物科技有限责任公司. ",a(
        href = "https://beian.miit.gov.cn/",
        target = "_blank", "粤ICP备2021032373号"
      )
      ),
      right = "2021-2022"   

    ),
    body  = dashboardBody(
        tags$head(includeScript("./js/baidu_analysis.js")),
        tabItems(
            tabItem(tabName = "heatmap",
            fluidRow(
                box(title="热图",solidHeader=TRUE,status='primary',background = "white",align="center",
                    #tags$h4("20220505更新：新增了【堆叠柱状图】"),
                    plotOutput("plot",height=600) %>% withSpinner(color="#0dc5c1",type = 5,size=0.5),width=8
                    ),
                box(width=4,
                  # Input: Select a file ----
                  actionButton(inputId='ab12', label="期刊分析工具JournalAnalysis", 
                          status="success", 
                          onclick ="window.open('http://www.databio1.com/journalanalysis', '_blank')"),
                  actionButton(inputId='ab1', label="加入生信问答社区BioX", 
                          status="success", 
                          onclick ="window.open('http://www.graphbio1.com:8080/biox', '_blank')"),
                  tags$hr(),                 
                  actionBttn(
                     inputId = "rune",
                     label = "运行例子",
                     style = "fill", 
                      color = "warning",
                      size = "sm"
                  ),  
                  tags$hr(),                
                  tags$h5("上传基因表达文件(支持csv、txt、xls、xlsx)"),
                  actionBttn(
                     inputId = "show",
                     label = "查看示例文件",
                     style = "fill", 
                      color = "primary",
                      size = "sm"
                  ),
                  tags$br(),
                  tags$br(),
                  fileInput("file1",NULL,
                            multiple = FALSE,
                            accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                  tags$h5("上传实验分组信息文件(支持csv、txt、xls、xlsx,可选)"),
                  actionBttn(
                     inputId = "show1",
                     label = "查看示例文件",
                     style = "fill", 
                      color = "primary",
                      size = "sm"
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
                       label = "选择颜色", 
                       choices = paste0("color", 1:6),
                       multiple = FALSE,
                       selected = "color1"
                    ),
                   awesomeRadio(
                       inputId = "cluster",
                       label = "选择聚类方法", 
                       choices = c("complete", "average", "none"),
                       selected = "complete",
                       status = "warning"
                     ),  
                   switchInput(
                      inputId = "rowname",
                      label = "基因名称",
                      labelWidth = "80px",
                      value = FALSE
                   ),
                   switchInput(
                      inputId = "colname",
                      label = "样本名称",
                      labelWidth = "80px",
                      value = FALSE
                   ),
                  numericInput("w", label = "下载图片宽度", value = 9),
                  numericInput("h", label = "下载图片高度", value = 6),
                  numericInput("ppi", label = "图像分辨率", value = 72),
                  dropdownButton(
                    downloadBttn(
                      outputId = "pdf",
                      label="PDF图片",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = "png",
                      label="PNG图片",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = "jpeg",
                      label="JPEG图片",
                      style = "fill",
                      color = "success",
                      size='sm',
                      block=TRUE
                    ),
                    downloadBttn(
                      outputId = "tiff",
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
            ),
          tabItem(tabName = "help",
            fluidRow(box(width=12,
            title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
            tags$h2("使用说明"),
            tags$hr(),
            tags$p("GraphBio包含了众多流行的组学数据可视化功能模块，旨在以一种简单高效的方式帮助科研人员快速完成数据可视化和数据探索。GraphBio每个功能模块都遵循了相同的设计模式，便于用户快速使用新的功能模块。
            这里，我们以热图绘制为例来说明GraphBio的使用方法。
              首先，我们在GraphBio左侧栏点击【热图】模块，然后便可看到画图区域和右侧的参数设置面板。"),
            tags$img(src="x1.png",width="50%",height="50%"),
            tags$p("在上传文件之前，我们需要在右侧参数面板栏点击【查看示例文件】，得到需要上传文件的内容格式。"),
            tags$img(src="format.png",width="50%",height="50%"),
            tags$p("从参考示例文件可以看出，待上传文件需要是一个基因表达矩阵（当然其实只要是这样格式的一个数值矩阵即可，并不限制于基因），行为基因，列为样本，然后我们可以用Excel来准备作图数据。"),
            tags$img(src="heatmap1.png",width="50%",height="50%"),
            tags$p("文件另存为csv格式（不是csv UTF8）, 制表符分隔txt，xls，xlsx格式亦可。然后将csv文件上传至GraphBio，一旦上传完成，便会自动生成图片。"),
            tags$img(src="show.png",width="50%",height="50%"),
            tags$p("此外，我们可以对样本增加注释条，这时需准备一个样本注释文件，注意样本的行顺序需与表达文件列顺序保持一致，如下，我们给出了样本所属组信息："),
            tags$img(src="heatmap2.png"),
            tags$p("同样，另存为csv格式，然后上传GraphBio，即可在图形上方增加注释条。"),
            tags$img(src="show1.png",width="50%",height="50%"),
            tags$p("最后，用户可根据需要对图形进行一些自定义调整，比如颜色，标签，聚类方法等，做好后，可以通过点击【下载图片】按钮获得需要的图片格式。"),
            tags$img(src="f1.png",width="50%",height="50%")
            )
            )
          ),
          tabItem(tabName = "contact",
            fluidRow(box(width=12,
            title="联系我们",solidHeader=TRUE,status='primary',background = "white",height=800,
            tags$h2("广州数智生物科技有限责任公司"),
            tags$a(href="http://www.databio1.com", "前往公司官网"),
            tags$hr(),
            tags$p("数智生物是由多位毕业于中山大学的资深生物信息博士成立的一家专注于基因大数据分析和管理的专业生物信息公司。
              团队在基因测序数据分析领域积累了十分丰富的实践经验，
              对WGS、RNA-seq、CHIP-seq、ATAC-seq、HIC、HICHIP、m6A-seq、scRNA-seq等组学数据有着深刻的理解。
              团队成员均来自一线科研团队，
              科研成果曾在Nature、Nature Cell Biology、European Urology、Cell Reports等著名杂志上发表。
              随着生命科学研究步入基因大数据时代，科研人员面临数据处理和分析难的痛点，我们旨在为广大生物医学研究人员提供高质量的软件工具以及整体的基因组学数据分析解决方案，加速生命科学发现与转化应用。
              "),
            tags$h2("GraphBio"),
            tags$hr(),
            tags$p("GraphBio是一款针对基因组学数据可视化的软件，它将提供易用、创新的可视化应用，帮助研究人员更加直观地从数据中获取新的科学洞察。我们在视频号上分享了许多关于GraphBio使用的具体案例，欢迎大家关注。
            除了GraphBio外，我们也正在全力依托于当前的云计算架构，
            打造基因大数据管理和分析平台，我们希望在云时代大幅提升科学研究的效率。如果您有任何建议或需要，欢迎随时与我们联系。"),
            fluidRow(
            box(width=4,background = "white",status='primary',title="微信公众号",solidHeader=TRUE,tags$img(src = "public.jpg", width = "300",height="300",style= "display:block;margin:auto;")),
            box(width=4,background = "white",status='primary',title="视频号",solidHeader=TRUE,tags$img(src = "vedio.png", width = "300",height="300",style= "display:block;margin:auto;")),
            box(width=4,background = "white",status='primary',title="微信",solidHeader=TRUE,tags$img(src = "contact.png", width = "300",height="300",style= "display:block;margin:auto;")))
              )
            )
          ),
          tabItem(tabName = "faq",
            fluidRow(box(width=12,
            title="常见问题",solidHeader=TRUE,status='primary',background = "white",height=800,
            tags$h2("常见问题"),
            tags$hr(),
            tags$p("1. 引用请用，Zhao, T.X. and Wang, Z.L., 2022. GraphBio: a shiny web app to easily perform popular visualization analysis for omics data. bioRxiv."),
            tags$p("2. 画图数据请按照示例文件格式准备即可，上传即自动生成图，一般不会有任何问题，若出现问题，基本99%是文件格式存在问题。"),
            tags$p("3. GraphBio所有应用均有使用教程，已发布在我们的视频号上。请点击左侧栏【联系我们】，关注我们【视频号】，获取最新数据可视化内容。"),
            tags$p("4. 上传文件需为csv格式，在excel另存为时，不要保存为csv UTF-8格式。"),
            tags$p("5. 点击应用运行例子，只提供在线测试预览，不提供下载，所以下载文件是无效的。只有自己上传的文件，下载才有效。"),
            tags$p("6. 示例文件仅展示上传文件所需格式，所以只显示了部分作图数据，并不是生成示例图的全部数据。"),
            tags$p("7. 若需要对下载图中的文字和颜色进行修改，可通过Adobe Illustrator或Adobe Acrobat软件进行编辑。"),
            tags$p("8. 文件内容不能有逗号，单双引号等异常符号，否则会报错。"),
            tags$p("9. 第一列基因名字不能有重复。"),
            tags$p("10. 单个上传文件大小不能超过5MB。")
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
          tabItem(tabName="stackbar",stackbarUI("stackbar")),
          tabItem(tabName="pie",pieUI("pie")),
          tabItem(tabName="cdc",cdcUI("cdc")),
          tabItem(tabName="km",kmUI("km")),
          tabItem(tabName="roc",rocUI("roc")),
          tabItem(tabName="ma",maUI("ma")),
          tabItem(tabName="sixx",sixiangxianUI("sixx")),
          tabItem(tabName="pca",pcaUI("pca")),
          tabItem(tabName="net",netUI("net")),
          tabItem(tabName="circle_dend",circledendUI("circle_dend")),
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
      #stackbar
      stackbarServer("stackbar")
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
      circledendServer("circle_dend")
      #chord
      chordServer("chord")
      #heatmap
      dataModal <- function(failed = FALSE) {
        modalDialog(
          renderTable(example[1:6,1:6],rownames=TRUE),
          easyClose=TRUE,
          footer = tagList(
            modalButton("关闭")
          )
        )
      }
      
      # Show modal when button is clicked.
      observeEvent(input$show, {
        showModal(dataModal())
      })

      dataModalx <- function(failed = FALSE) {
        modalDialog(
          span('注意：样本顺序需与表达矩阵样本顺序保持一致！支持两组或更多组。'),
          tags$hr(),
          renderTable(example1[1:6,],rownames=FALSE),
          easyClose=TRUE,
          footer = tagList(
            modalButton("关闭")
          )
        )
      }
      
      # Show modal when button is clicked.
      observeEvent(input$show1, {
        showModal(dataModalx())
      })

      #init
#      output$plot <- renderImage({
#
#        # Return a list containing the filename and alt text
#        list(src = "./www/qun.png")
#
#      }, deleteFile = FALSE)

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

