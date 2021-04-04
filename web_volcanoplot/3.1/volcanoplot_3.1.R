# 2021/4/1 zhangjun
# 安装需要的R 包

if(!require("tidyverse")){install.packages('tidyverse')}
if(!require("shiny")){install.packages('shiny')}
if(!require("shinydashboard")){install.packages('shinydashboard')}
if(!require("shinythemes")){install.packages('shinythemes')}
if(!require("ggplot2")){install.packages('ggplot2')}
if(!require("ggprism")){install.packages('ggprism')}
if(!require("ggrepel")){install.packages('ggrepel')}
if(!require("DT")){install.packages('DT')}
if(!require("plotly")){install.packages('plotly')}
if(!require("plotly")){install.packages('colourpicker')}


library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(ggprism)
library(tidyverse)
library(ggrepel)
library(DT)
library(plotly)
library(colourpicker)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # theme = shinytheme("darkly"),
    # theme = shinytheme("simplex"),
    # theme = shinytheme("spacelab"),
    # theme = shinytheme("superhero"),
    # theme = shinytheme("united"),
    # theme = shinytheme("yeti"),
    theme = shinytheme("flatly"),
    # theme = 'bootstrap.min.css',
    # Application title
    titlePanel("Volcano Plot in Zhoulab"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            fileInput('data',
                      h3('choose your input data'),
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            tags$strong('Note: Your input file column name must be ',em('" log2FoldChange " , " pvalue " , " baseMean " and " gene_name "'),
                     ' if not, please modify them to be the same name!'),
            hr(),
            numericInput('log2FC',
                        'log2FoldChange:',
                        min = 0,
                        max = 5,
                        value = 1,
                        step = 0.1,
                        width = 400),
            numericInput('pvalue',
                         'Pvalue:',
                         min = 0,
                         max = 1,
                         value = 0.05,
                         step = 0.01,
                         width = 400),
            textAreaInput('gene',
                         'Intrerest gene names:',value = '',
                         rows = 3,cols = 50),
            fluidRow(column(6,numericInput('text_size','Gene size:',
                                           min = 0.5,
                                           max = 20,
                                           step = 0.5,
                                           value = 8,
                                           width = 400)),
                     column(6,numericInput('circle_size','Circle size:',
                                           min = 0.5,
                                           max = 20,
                                           step = 0.1,
                                           value = 5.5,
                                           width = 400))),
            fluidRow(column(6,selectInput('text_col','Gene color:',
                                          choices = c('black'='black','red'='#d44000',
                                                      'orange'='#f37121','green'='#61b15a',
                                                      'blue'='#1f3c88','lightgreen'='#28df99',
                                                      'lightred'='#ff4b5c','grassgreen'='#d3de32',
                                                      'lightblue'='#6886c5','pink'='#fa744f',
                                                      'Ginger'='#f7ea00','rose red'='#ff005c','violet'='#892cdc',
                                                      'tomato red'='#e40017','mustad green'='#d2e603','baby blue'='#5fdde5',
                                                      'royalblue'='#0779e4','lilac'='#efa8e4','dark royalblue'='#342ead',
                                                      'reseda'='#4cd3c2','willow green'='#21bf73','matcha green'='#ccda46',
                                                      'rose pink'='#f54291','yellow'='#ffe837','jade green'='#00bd56',
                                                      'lemon yellow'='#fff200'),
                                          selected = 'black',width = 600)),
                     column(6,selectInput('circle_col','Circle color:',
                                          choices = c('black'='black','red'='#d44000',
                                                      'orange'='#f37121','green'='#61b15a',
                                                      'blue'='#1f3c88','lightgreen'='#28df99',
                                                      'lightred'='#ff4b5c','grassgreen'='#d3de32',
                                                      'lightblue'='#6886c5','pink'='#fa744f',
                                                      'Ginger'='#f7ea00','rose red'='#ff005c','violet'='#892cdc',
                                                      'tomato red'='#e40017','mustad green'='#d2e603','baby blue'='#5fdde5',
                                                      'royalblue'='#0779e4','lilac'='#efa8e4','dark royalblue'='#342ead',
                                                      'reseda'='#4cd3c2','willow green'='#21bf73','matcha green'='#ccda46',
                                                      'rose pink'='#f54291','yellow'='#ffe837','jade green'='#00bd56',
                                                      'lemon yellow'='#fff200'),
                                          selected = '#f37121',width = 600))),
            
            selectInput('col',
                        'The colors palette:',
                        choices = c('flat1' = '#dd2c00_#438a5e_grey',
                                    'flat2' = '#e7305b_#96bb7c_grey',
                                    'flat3' = '#d54062_#519872_grey',
                                    'flat4' = '#be0000_#161d6f_grey',
                                    'flat5' = '#ff9292_#aee1e1_grey',
                                    'flat6' = '#be0000_#f58634_grey',
                                    'flat7' = '#c70039_#1a508b_grey',
                                    'flat8' = '#bb2205_#61b15a_grey',
                                    'lxr1' = '#ff005c_#7868e6_grey',
                                    'lxr2' = '#91091e_#a1cae2_grey',
                                    'lxr3' = '#f88f01_#16c79a_grey',
                                    'lxr4' = '#fbbedf_#bce6eb_grey',
                                    'lxr5' = '#f56a79_#1aa6b7_grey',
                                    'lxr6' = '#ff9a76_#679b9b_grey',
                                    'lxr7' = '#f4e04d_#28df99_grey',
                                    'lxr8' = '#fddb3a_#00bcd4_grey'),
                        selected = 'flat1',
                        width = 400),
            sliderInput('pointsize',
                        'Point Size:',
                        min = 1,
                        max = 20,
                        value = 5,
                        step = 0.5,
                        width = 400),
            numericInput('alpha',
                         'The Point alpha:',
                         min = 0,
                         max = 1,
                         value = 0.5,
                         step = 0.1,
                         width = 400),
             sliderInput('xcut',
                         'X aixs cut range:',
                         min = -30,
                         max = 30,
                         value = c(-30,30),
                         step = 0.5,
                         width = 400),
            sliderInput('ycut',
                         'Y aixs cut range:',
                         min = 0,
                         max = 1000,
                         value = c(0,500),
                         step = 1,
                         width = 400),
            checkboxInput('border',
                        'Show border?',
                        value = TRUE,
                        width = 400),
            numericInput('base_size',
                         'The theme base size:',
                         min = 10,max = 30,value = 16,step = 1,width = 400),
               fluidRow(column(6,numericInput('xdis',
                         'The X axis breaks:',
                         min = 0,max = 50,value = 20,step = 0.5,width = 400)),
                 column(6,numericInput('ydis',
                         'The Y axis breaks:',
                         min = 0,max = 50,value = 50,step = 0.5,width = 400))),
            textInput('title',
                      'Plot title:',
                      value = 'Zhoulab volcano plot',width = 200),
            tags$hr(),
            tags$h3('Download your figure here:'),
        fluidRow(column(4,radioButtons('format',
                        'Export format:',
                        choices = c('PDF'),
                        selected = 'PDF')),
                 column(3,downloadButton('downloadata',
                       'Download data here:'))),
        tags$h3('Save Plot height and width:'),
               fluidRow(column(5,numericInput('height',
                                       'Height',min = 1,max = 100,step = 1,value = 15)),
                 column(5,numericInput('width',
                                       'Width',min = 1,max = 100,step = 1,value = 22)))
        ),
        
################################################################################        
        # Show a plot of the generated distribution
        mainPanel(
            tags$strong("To make things easier,we develop this interactive application to make a nice volcano plot.",
            "Thank you for your support ,learning shiny on this website :", span('https://shiny.rstudio.com/tutorial/ .',style="color:red") , "if you have any advice or suggestions,please connect me .(Jun Zhang)",
            " My email is: 3219030654@stu.cpu.edu.cn !"),
            tags$h3("Hello,welcome to zhoulab plot platform,enjoying your plot!"),
            tags$h4('This is your volcanoplot here:'),
            tabsetPanel(type = "tabs",
                        tabPanel("Volcano Plot",
                                 icon = icon('chart-bar'),
                                 h4(tags$strong('Volcano figure is showing (press submit):')),
                                 plotOutput("volcaplot",height = 550,width = 700),
                                             fluidRow(column(width = 4,
                                                             box(selectInput('up_shape',
                                                             'Up gene shape:',
                                                             choices = c('square'=15,'circle'=16,'upper triangle'=17,'hollow upper triangle'=2,
                                                                         'lower triangle'=6,'rhombic'=18,'star'=11,'cross'=3),
                                                             selected=16,
                                                             width = 150))),
                                                      column(width = 4,
                                                             box(selectInput('nosig_shape',
                                                             'Nosig gene shape:',
                                                             choices = c('square'=15,'circle'=16,'upper triangle'=17,'hollow upper triangle'=2,
                                                                         'lower triangle'=6,'rhombic'=18,'star'=11,'cross'=3),
                                                             selected=16,
                                                             width = 150))),
                                                      column(width = 4,
                                                             box(selectInput('down_shape',
                                                             'Down gene shape:',
                                                              choices = c('square'=15,'circle'=16,'upper triangle'=17,'hollow upper triangle'=2,
                                                                          'lower triangle'=6,'rhombic'=18,'star'=11,'cross'=3),
                                                              selected=16,
                                                             width = 150))),
                                                      ),
                                 fluidRow(column(width = 4,
                                                 box(selectInput('arrow_col',
                                                                 'arrow line color:',
                                                                 choices = c('black'='black','red'='#d44000',
                                                                             'orange'='#f37121','green'='#61b15a',
                                                                             'blue'='#1f3c88','lightgreen'='#28df99',
                                                                             'lightred'='#ff4b5c','grassgreen'='#d3de32',
                                                                             'lightblue'='#6886c5','pink'='#fa744f',
                                                                             'Ginger'='#f7ea00','rose red'='#ff005c','violet'='#892cdc',
                                                                             'tomato red'='#e40017','mustad green'='#d2e603','baby blue'='#5fdde5',
                                                                             'royalblue'='#0779e4','lilac'='#efa8e4','dark royalblue'='#342ead',
                                                                             'reseda'='#4cd3c2','willow green'='#21bf73','matcha green'='#ccda46',
                                                                             'rose pink'='#f54291','yellow'='#ffe837','jade green'='#00bd56',
                                                                             'lemon yellow'='#fff200'),
                                                                 selected = 'black',
                                                                 width = 150))),
                                          column(width = 4,
                                                 box(numericInput('arrow_line',
                                                                  'arrow line width:',
                                                                  width = 150,
                                                                  min = 0.5,max = 15,step = 0.5,value = 2))),
                                          column(width = 4,
                                                 box(actionButton('submit',
                                                                  'Submit',
                                                                  icon = icon('power-off'),
                                                                   width = 128))))),
                        tabPanel("Ploty volcano",
                                 icon = icon('eye'),
                                 h4(tags$strong('This panel shows interactive ploting (press submit):')),
                                 plotlyOutput('plotly_volcano',height = 550,width = 700),
                                 actionButton('plotly_submit',
                                              'plotly Submit',icon = icon('power-off'))),
                        tabPanel("Personalized volcano",
                                 icon = icon('user-secret'),
                                 h4(tags$strong('This panel shows personalized colors (press submit):')),
                                 plotOutput('personalized_volcano',height = 550,width = 700),
                                 fluidRow(column(width = 3,
                                                 box(colourInput("col_up", "Select UP colour:", "red"),width = 150)),
                                          column(width = 3,
                                                 box(colourInput("col_nosig", "Select NoSig colour:", "grey"),width = 150)),
                                          column(width = 3,
                                                 box(colourInput("col_down", "Select Down colour", "blue"),width = 150)),
                                          column(width = 3,
                                                 box(actionButton('personalized_submit',
                                                 'Personalized Submit',icon = icon('power-off')),width = 150)))),
                        tabPanel("Summary", 
                                 icon = icon('chart-pie'),
                                 h3(tags$strong('This panel shows summary of the data columns:')),
                                 verbatimTextOutput("summary")),
                        tabPanel("Table", 
                                 icon = icon('diagnoses'),
                                 h3(tags$strong('This panel shows the all the data you upload:')),
                                 DT::dataTableOutput("table"))),
            tabsetPanel(type = "tabs",
                        tabPanel("BaseMean Plot",
                                 icon = icon('chart-bar'),
                                 h4(tags$strong('BaseMean figure is showing (press submit):')),
                                 plotOutput("baseplot",height = 550,width = 700),
                                 box(actionButton('Submit',
                                                  'Submit',
                                                  icon = icon('power-off'),
                                                  width = 128))),
                        tabPanel("Table", 
                                 icon = icon('diagnoses'),
                                 h3(tags$strong('This panel shows the all significant upregulated and downregulated genes here:')),
                                 DT::dataTableOutput("sumtable")),
                        tabPanel("Filtertype output",
                                 icon = icon('cannabis'),
                                 h3(tags$strong('This panel shows the type you want to download the significant upregulated or downregulated genes or both here:')),
                                 fluidRow(column(5,imageOutput('image3')),
                                          column(5,imageOutput('image4'))),
                                 fluidRow(h2(tags$strong('To make the world better !'))),
                                 radioButtons('filtertable',
                                              label = h3('Choose type you want:'),
                                              choices = c('updown','up','down'),
                                              selected = 'updown',
                                              inline = T)),
                        tabPanel('Download Table',
                                 icon = icon('arrow-alt-circle-down'),
                                 h3(tags$strong('This panel download the data you have choosed berfore:')),
                                 fluidRow(column(5,imageOutput('image1')),
                                          column(5,imageOutput('image2'))),
                                          h2(tags$strong('路漫漫其修远兮，吾将上下而求索!')),
                                 h2(tags$strong('Welcome  to  join  in  Zhoulab!')),
                                 fluidRow(textInput('download_name',
                                                             h4(tags$strong('Download data prefix:')),
                                                             value = "my_output")),
                                 fluidRow(downloadButton('downtable',
                                                'Download Table')))),
            )
)
)

# Define server logic required to draw
server <- function(input, output) {
    
    data <- reactive({
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
    })
################################################################################
    # volcanoplot   
    volcano <- eventReactive(input$submit, {
        infile <- input$data$datapath

        if (is.null(infile))
            return(NULL)

        d <- infile
        type <- str_sub(d,-3)

        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        # res <- read.csv(infile,header = T,sep = ',')
        # assign the up-regulated and down-regulated genes
        res$threshold = factor(
            ifelse(res$pvalue < input$pvalue & abs(res$log2FoldChange) >= input$log2FC,
                   ifelse(res$log2FoldChange>= input$log2FC,'Up','Down'),
                   'NoSig'),levels=c('Up','Down','NoSig'))

        # split colors
        colo <- unlist(strsplit(input$col,split = '_'))

        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[3]

        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = ',')))
        my_gene <- res[which(res$gene_name %in% b),]

        # print shape
        # print(input$up_shape)
        # plot
        p<- ggplot(data=res,aes(x=log2FoldChange,y=-log10(pvalue),color=threshold)) +
            theme_prism(base_size = input$base_size,border = input$border) +
            geom_point(size = input$pointsize ,
                       alpha = input$alpha,
                       aes(shape=threshold)) +
            scale_shape_manual(values = c('Up'=as.numeric(input$up_shape),
                                          'Down'=as.numeric(input$down_shape),
                                          'NoSig'=as.numeric(input$nosig_shape)),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            scale_color_manual(values = c('Up'=colo[1],
                                          'Down'=colo[2],
                                          'NoSig'=colo[3]),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            geom_vline(xintercept=c(-input$log2FC,input$log2FC),
                       lty=2,col="black",size=1.3) +
            geom_hline(yintercept = -log10(input$pvalue),lty=2,
                       col="black",size=1.3) +
            theme(legend.position = c(0.15,0.9),
                  legend.text = element_text(size = 16),
                  legend.background = element_blank()) +
            scale_x_continuous(limits = c(input$xcut[1],input$xcut[2]),
                               breaks = seq(input$xcut[1],input$xcut[2],by = input$xdis)) +
            scale_y_continuous(limits = c(input$ycut[1],input$ycut[2]),
                               breaks = seq(input$ycut[1],input$ycut[2],by = input$ydis)) +
            labs(title = input$title) +
            geom_text_repel(data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(0.03, "npc"),
                                          ends = "last", type = "open"),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=1,stroke=2,
                       data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue)))

        ggsave('volcanoplot.pdf',
               height = input$height,
               width = input$width,
               units = 'cm')
        p
    })

    output$volcaplot <- renderPlot({
        volcano()
    })

################################################################################
    # plotly volcano plotly_submit
    plotly_volcan <- eventReactive(input$plotly_submit, {
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        # res <- read.csv(infile,header = T,sep = ',')
        # assign the up-regulated and down-regulated genes
        res$threshold = factor(
            ifelse(res$pvalue < input$pvalue & abs(res$log2FoldChange) >= input$log2FC,
                   ifelse(res$log2FoldChange>= input$log2FC,'Up','Down'),
                   'NoSig'),levels=c('Up','Down','NoSig'))
        
        # split colors
        colo <- unlist(strsplit(input$col,split = '_'))
        
        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[3]
        
        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = ',')))
        my_gene <- res[which(res$gene_name %in% b),]
        
        # print shape
        # print(input$up_shape)
        # plot
        p<- ggplot(data=res,aes(x=log2FoldChange,y=-log10(pvalue),color=threshold)) +
            theme_prism(base_size = input$base_size,border = input$border) +
            geom_point(size = input$pointsize ,
                       alpha = input$alpha,
                       aes(shape=threshold,
                           text=gene_name)) +
            scale_shape_manual(values = c('Up'=as.numeric(input$up_shape),
                                          'Down'=as.numeric(input$down_shape),
                                          'NoSig'=as.numeric(input$nosig_shape)),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            scale_color_manual(values = c('Up'=colo[1],
                                          'Down'=colo[2],
                                          'NoSig'=colo[3]),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            geom_vline(xintercept=c(-input$log2FC,input$log2FC),
                       lty=2,col="black",size=1.3) +
            geom_hline(yintercept = -log10(input$pvalue),lty=2,
                       col="black",size=1.3) +
            theme(legend.position = c(0.15,0.9),
                  legend.text = element_text(size = 16),
                  legend.background = element_blank()) +
            scale_x_continuous(limits = c(input$xcut[1],input$xcut[2]),
                               breaks = seq(input$xcut[1],input$xcut[2],by = input$xdis)) +
            scale_y_continuous(limits = c(input$ycut[1],input$ycut[2]),
                               breaks = seq(input$ycut[1],input$ycut[2],by = input$ydis)) +
            labs(title = input$title) +
            geom_text_repel(data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(0.03, "npc"),
                                          ends = "last", type = "open"),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=1,stroke=2,
                       data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue)))
        
        p
    })
    
    output$plotly_volcano <- renderPlotly({
        plotly_volcan()
    })

    
################################################################################
    # personalized color plot
    # volcanoplot   
    personalied_volcano <- eventReactive(input$personalized_submit, {
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        # res <- read.csv(infile,header = T,sep = ',')
        # assign the up-regulated and down-regulated genes
        res$threshold = factor(
            ifelse(res$pvalue < input$pvalue & abs(res$log2FoldChange) >= input$log2FC,
                   ifelse(res$log2FoldChange>= input$log2FC,'Up','Down'),
                   'NoSig'),levels=c('Up','Down','NoSig'))
        
        # split colors
        # colo <- unlist(strsplit(input$col,split = '_'))
        
        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[3]
        
        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = ',')))
        my_gene <- res[which(res$gene_name %in% b),]
        
        # print shape
        # print(input$up_shape)
        # plot
        p<- ggplot(data=res,aes(x=log2FoldChange,y=-log10(pvalue),color=threshold)) +
            theme_prism(base_size = input$base_size,border = input$border) +
            geom_point(size = input$pointsize ,
                       alpha = input$alpha,
                       aes(shape=threshold)) +
            scale_shape_manual(values = c('Up'=as.numeric(input$up_shape),
                                          'Down'=as.numeric(input$down_shape),
                                          'NoSig'=as.numeric(input$nosig_shape)),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            scale_color_manual(values = c('Up'=input$col_up,
                                          'Down'=input$col_down,
                                          'NoSig'=input$col_nosig),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            geom_vline(xintercept=c(-input$log2FC,input$log2FC),
                       lty=2,col="black",size=1.3) +
            geom_hline(yintercept = -log10(input$pvalue),lty=2,
                       col="black",size=1.3) +
            theme(legend.position = c(0.15,0.9),
                  legend.text = element_text(size = 16),
                  legend.background = element_blank()) +
            scale_x_continuous(limits = c(input$xcut[1],input$xcut[2]),
                               breaks = seq(input$xcut[1],input$xcut[2],by = input$xdis)) +
            scale_y_continuous(limits = c(input$ycut[1],input$ycut[2]),
                               breaks = seq(input$ycut[1],input$ycut[2],by = input$ydis)) +
            labs(title = input$title) +
            geom_text_repel(data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(0.03, "npc"),
                                          ends = "last", type = "open"),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=1,stroke=2,
                       data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue)))
        
        ggsave('personalize_dvolcanoplot.pdf',
               height = input$height,
               width = input$width,
               units = 'cm')
        p
    })
    
    output$personalized_volcano <- renderPlot({
        personalied_volcano()
    })
################################################################################
    # basemean plot
    basemean <- eventReactive(input$Submit, {
        infile <- input$data$datapath

        if (is.null(infile))
            return(NULL)

        d <- infile
        type <- str_sub(d,-3)

        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')

        # assign the up-regulated and down-regulated genes
        res$threshold = factor(
            ifelse(res$pvalue < input$pvalue & abs(res$log2FoldChange) >= input$log2FC,
                   ifelse(res$log2FoldChange>= input$log2FC,'Up','Down'),
                   'NoSig'),levels=c('Up','Down','NoSig'))

        # split colors
        colo <- unlist(strsplit(input$col,split = '_'))

        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[3]

        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = ',')))
        my_gene <- res[which(res$gene_name %in% b),]

        # plot
        p1<- ggplot(data=res,aes(y=log2FoldChange,x=log2(baseMean+1),color=threshold)) +
            theme_prism(base_size = input$base_size,border = input$border) +
            geom_point(size = input$pointsize ,
                       alpha = input$alpha,
                       aes(shape=threshold)) +
            scale_shape_manual(values = c('Up'=as.numeric(input$up_shape),
                                          'Down'=as.numeric(input$down_shape),
                                          'NoSig'=as.numeric(input$nosig_shape)),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            scale_color_manual(values = c('Up'=colo[1],
                                          'Down'=colo[2],
                                          'NoSig'=colo[3]),
                               labels= c('Up'= up, 'Down'=down,'NoSig'= nosig)) +
            geom_hline(yintercept=c(-input$log2FC,input$log2FC),
                       lty=2,col="black",size=1.3) +
            theme(legend.position = c(0.85,0.9),
                  legend.text = element_text(size = 16),
                  legend.background = element_blank()) +
            scale_y_continuous(limits = c(input$xcut[1],input$xcut[2]),
                               breaks = seq(input$xcut[1],input$xcut[2],by = input$xdis)) +
            scale_x_continuous(limits = c(input$ycut[1],input$ycut[2]),
                               breaks = seq(input$ycut[1],input$ycut[2],by = input$ydis)) +
            labs(title = input$title) +
            geom_text_repel(data = my_gene, aes(y=log2FoldChange,x=log2(baseMean+1), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(0.03, "npc"),
                                          ends = "last", type = "open"),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=1,stroke=2,
                       data = my_gene, aes(y=log2FoldChange,x=log2(baseMean+1)))
        p1

    })
    
    output$baseplot <- renderPlot({
        basemean()
    })
    
################################################################################
    # summary
    output$summary <- renderPrint({
        summary(data())
    })
################################################################################
    # table
    output$table <- DT::renderDataTable({
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        DT::datatable(res)
    })

################################################################################
    # sumtable
    output$sumtable <- renderDataTable({
        
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        res$threshold = factor(
            ifelse(res$pvalue < input$pvalue & abs(res$log2FoldChange) >= input$log2FC,
                   ifelse(res$log2FoldChange>= input$log2FC,'Up','Down'),
                   'NoSig'),levels=c('Up','Down','NoSig'))
        updown <- filter(res,threshold=='Up'|threshold=='Down')
        
        DT::datatable(updown)
        
    })
    
################################################################################
    # downtable download_name
    typeout <- reactive({
        infile <- input$data$datapath

        if (is.null(infile))
        return(NULL)

        d <- infile
        type <- str_sub(d,-3)

        if(type=='csv')
        res <- read.csv(infile,header = T,sep = ',') else
            res <- read.csv(infile,header = T,sep = '\t')

        res$threshold = factor(
        ifelse(res$pvalue < input$pvalue & abs(res$log2FoldChange) >= input$log2FC,
               ifelse(res$log2FoldChange>= input$log2FC,'Up','Down'),
               'NoSig'),levels=c('Up','Down','NoSig'))
        updown <- filter(res,threshold=='Up'|threshold=='Down')
        up <- filter(res,threshold=='Up')
        down <- filter(res,threshold=='Down')
        
        if(input$filtertable=='updown'){
            final_res <- updown
        }else if(input$filtertable=='up'){
            final_res <- up
        }else {
            final_res <- down
        }
        
        })

    
        output$downtable <- downloadHandler(
        filename = function() {
            paste(input$download_name, '.csv',sep = "")
        },
        
        content = function(file) {
        write.csv(typeout(),file,row.names = FALSE)
            })
    
################################################################################    
    # save plot
    output$downloadata <- downloadHandler(
        filename = function() {
            paste('basemeanplot', sep = '.', switch(
                input$format, PDF = 'pdf'))
        },
        
        content = function(file) {
            ggsave(file,
                   height = input$height,
                   width = input$width,
                   limitsize = FALSE,
                   units = 'cm',
                   )})
        
################################################################################
        # image
        output$image1 <- renderImage({
           return(list(src='www/IMG_7316.JPG',
                       width=350,
                       height=250)) 
        },deleteFile = FALSE)
    
        output$image2 <- renderImage({
            return(list(src='www/zhoulab.jpg',
                        width=350,
                        height=250)) 
        },deleteFile = FALSE)
        output$image3 <- renderImage({
            return(list(src='www/shiny.png',
                        width=150,
                        height=150)) 
        },deleteFile = FALSE)
        
        output$image4 <- renderImage({
            return(list(src='www/rstudio.jpg',
                        width=275,
                        height=150)) 
        },deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
