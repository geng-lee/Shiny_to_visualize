library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(ggprism)
library(tidyverse)
library(ggrepel)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    # theme = 'bootstrap.min.css',
    # Application title
    titlePanel("Volcano plot in Zhoulab"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            fileInput('data',
                      h3('choose your input data'),
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            helpText('NOTE: Your input file column name must be "log2FoldChange" , "pvalue" , "baseMean" and "gene_name",
                     if not, please modify them to be the same name!'),
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
            textInput('gene',
                      'Intrerest gene:',value = NA),
            fluidRow(column(6,numericInput('text_size','Gene size:',
                                           min = 0.5,
                                           max = 20,
                                           step = 0.5,
                                           value = 3,
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
                                                      'lightblue'='#6886c5','pink'='#fa744f'),
                                          selected = '#d44000',width = 600)),
                     column(6,selectInput('circle_col','Circle color:',
                                          choices = c('black'='black','red'='#d44000',
                                                      'orange'='#f37121','green'='#61b15a',
                                                      'blue'='#1f3c88','lightgreen'='#28df99',
                                                      'lightred'='#ff4b5c','grassgreen'='#d3de32',
                                                      'lightblue'='#6886c5','pink'='#fa744f'),
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
            "Thank you for your support ,if you have any advice or suggestions,please connect me.",
            "My email is: 3219030654@stu.cpu.edu.cn !"),
            tags$h3("Hello,welcome to zhoulab plot plantform,enjoying your plot!"),
            tags$h4('This is your volcanoplot here:'),
            tabsetPanel(type = "tabs",
                        tabPanel("Volcano Plot",
                                 icon = icon('chart-bar'),
                                 h4(tags$strong('Volcano figure is showing:')),
                                 plotOutput("volcaplot",height = 550,width = 700),
                                 tabsetPanel(fluidRow(box(
                                              selectInput('arrow_col',
                                                          'arrow line color:',
                                                          choices = c('black'='black','red'='#d44000',
                                                                      'orange'='#f37121','green'='#61b15a',
                                                                      'blue'='#1f3c88','lightgreen'='#28df99',
                                                                      'lightred'='#ff4b5c','grassgreen'='#d3de32',
                                                                      'lightblue'='#6886c5','pink'='#fa744f'),
                                                          selected = 'black',
                                                          width = 150)),
                                             box(numericInput('arrow_line',
                                                          'arrow line width:',
                                                          width = 150,
                                                          min = 0.5,max = 15,step = 0.5,value = 2))))),
                        tabPanel("Summary", 
                                 icon = icon('chart-pie'),
                                 verbatimTextOutput("summary")),
                        tabPanel("Table", 
                                 icon = icon('diagnoses'),
                                 tableOutput("table"))),
            tabsetPanel(type = "tabs",
                        tabPanel("BaseMean Plot",
                                 icon = icon('chart-bar'),
                                 h4(tags$strong('BaseMean figure is showing:')),
                                 plotOutput("baseplot",height = 550,width = 700)),
                        tabPanel("Table", 
                                 icon = icon('diagnoses'),
                                 h3(tags$strong('This panel shows the all significant upregulated and downregulated genes here:')),
                                 tableOutput("sumtable")),
                        tabPanel("Filtertype output",
                                 icon = icon('cannabis'),
                                 h3(tags$strong('This panel shows the type you want to download the significant upregulated or downregulated genes or both here:')),
                                 radioButtons('filtertable',
                                              label = h3('Choose type you want:'),
                                              choices = c('updown','up','down'),
                                              selected = 'updown',
                                              inline = T)),
                        tabPanel('Download Table',
                                 icon = icon('arrow-alt-circle-down'),
                                 h3(tags$strong('This panel download the data you have choosed berfore:')),
                                 downloadButton('downtable',
                                                'Download Table'))),
            )
),
)

# Define server logic required to draw a histogram
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
    output$volcaplot <- renderPlot({
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

        # plot
        p<- ggplot(data=res,aes(x=log2FoldChange,y=-log10(pvalue))) +
            theme_prism(base_size = input$base_size,border = input$border) +
            geom_point(size = input$pointsize ,
                       alpha = input$alpha,
                       aes(color=threshold)) +
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


################################################################################
    # basemean plot
    output$baseplot <- renderPlot({
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
        p1<- ggplot(data=res,aes(y=log2FoldChange,x=log2(baseMean+1))) +
            theme_prism(base_size = input$base_size,border = input$border) +
            geom_point(size = input$pointsize ,
                       alpha = input$alpha,
                       aes(color=threshold)) +
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
################################################################################
    # summary
    output$summary <- renderPrint({
        summary(data())
    })
################################################################################
    # table
    output$table <- renderTable({
        
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        head(res,n=20)
        
    })

################################################################################
    # sumtable
    output$sumtable <- renderTable({
        
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
        updown
        
    })
    
################################################################################
    # downtable
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
            paste('my_filter_results', '.csv',sep = "")
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
