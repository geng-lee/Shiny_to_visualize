library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(ggprism)
library(tidyverse)
library(ggrepel)
library(DT)
library(plotly)
library(shinycssloaders)
library(colourpicker)

## Data frame for usernames and passwords
user_key <- data.frame(username = c("zhoulab"),
                       secret = c("2018"))


#  Login page (first UI) ----------------------------------------------------------------------------------

ui1 <- fluidPage(
    
    div(id = "header",
        h1("Welcome To ZhouLab ! Thank you for your support."),
        align = "center"
    ),
    
    div(id = "loginpage",
        
        fluidRow(
            br(),br(),
            h4(em("Please enter your Username and Password :", align = "left")),
            hr(),
        ),
        
        fluidRow(
            column(
                width = 4, 
                # offset = 4,
                wellPanel(
                    textInput(inputId = "user", label = h4(tags$strong("User Name",style='color:#1C8EC7')), placeholder = "User Name",),
                    passwordInput(inputId = "pass", label = h4(tags$strong("Password",style='color:#1C8EC7')), placeholder = "Password"),
                    actionButton(inputId = "login", h4(tags$strong("Login",style='color:#1C8EC7'))),
                    hr(),
                    span(textOutput("loginfail"), style='color:red')
                )
            ),
            column(8,
                   tags$img(src='zhoulab.jpg',width=900,height=550))
        ),
        
        
    )
)


# Page to be accessed (second UI) -----------------------------------------------------------------------
# Define UI for application that draws a histogram
ui2 <- fluidPage(
    # theme = shinytheme("darkly"),
    theme = shinytheme("simplex"),
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

                     fluidRow(column(6,
                                     numericInput('nlog2FC','neg log2FC',min = -10,max = 0,value = -1,step = 0.1)),
                              column(6,
                                     numericInput('plog2FC','pos log2FC',min = -10,max = 0,value = 1,step = 0.1))),
                     numericInput('pvalue',
                                  'Pvalue:',
                                  min = 0,
                                  max = 1,
                                  value = 0.05,
                                  step = 0.01,
                                  width = 400),
                     h5(tags$strong('Setting legend position:')),
                     fluidRow(column(6,numericInput('x_pos','X axis position:',
                                                    min = 0,
                                                    max = 1,
                                                    step = 0.05,
                                                    value = 0.15,
                                                    width = 400)),
                              column(6,numericInput('y_pos','Y axis position:',
                                                    min = 0,
                                                    max = 1,
                                                    step = 0.05,
                                                    value = 0.9,
                                                    width = 400))),
                     textAreaInput('gene',
                                   'Intrerest gene names:',value = '',
                                   rows = 3,cols = 50),
                     selectInput('circle_shape','Circle shape:',
                                 choices = c('hollow circle'=1,'filled circle'=16),
                                 selected = 1),
                     fluidRow(column(4,numericInput('text_size','Gene size:',
                                                    min = 0.5,
                                                    max = 20,
                                                    step = 0.5,
                                                    value = 8,
                                                    width = 400)),
                              
                              column(4,numericInput('circle_size','Circle size:',
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
                                 choices = c('classic' = '#dd2c00_#438a5e_grey',
                                             'pink girl' = '#e7305b_#96bb7c_grey',
                                             'megenta' = '#d54062_#519872_grey',
                                             'AAAS' = '#be0000_#161d6f_grey',
                                             'maid feelings' = '#ff9292_#aee1e1_grey',
                                             'mash' = '#be0000_#f58634_grey',
                                             'captain America' = '#c70039_#1a508b_grey',
                                             'tomato' = '#bb2205_#61b15a_grey',
                                             'blusher' = '#ff005c_#7868e6_grey',
                                             'darkblood' = '#91091e_#a1cae2_grey',
                                             'village' = '#f88f01_#16c79a_grey',
                                             'Macaron' = '#fbbedf_#bce6eb_grey',
                                             'Spring' = '#ff00c8_#54e346_grey',
                                             'Summer' = '#ffa931_#9fe8fa_grey',
                                             'Autumn' = '#ff4301_#12cad6_grey',
                                             'Winter' = '#ffaf87_#a1c45a_grey',
                                             'Black Dress lane' = '#ff9a76_#679b9b_grey',
                                             'Sprite' = '#f4e04d_#28df99_grey',
                                             'bubble gum' = '#fddb3a_#00bcd4_grey'),
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
                                 min = -10,
                                 max = 1000,
                                 value = c(0,500),
                                 step = 1,
                                 width = 400),
                     fluidRow(column(6,selectInput('vol_line_type',
                                                   'The line type:',
                                                   choices = c('twodash','solid','longdash','dotted','dotdash','dashed','blank'),
                                                   selected = 'dashed')),
                              column(6,numericInput('vol_dash_line_size',
                                                    'The dahsed line size:',
                                                    min = 0,max = 10,value = 1.5,step = 0.1))),
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
                                 fluidRow(column(8,withSpinner(type = 5,size = 0.5,
                                             plotOutput("volcaplot",height = 550,width = 700,
                                                                            # dblclick = "plot1_dblclick",
                                                                            brush = brushOpts(
                                                                                id = "plot1_brush",
                                                                                resetOnNew = TRUE
                                                                            )))),
                                          column(4,
                                                 checkboxInput('flip','axis flip',value = FALSE),
                                                 sliderInput('stroke','circle stroke',min = 0,max = 10,value = 1.5,step = 0.1),
                                                 fluidRow(column(6,textAreaInput('gene1','Gene1')),
                                                          column(6,colourInput('gene1col','gene1 col',value = 'pink'))),
                                                 fluidRow(column(6,textAreaInput('gene2','Gene2')),
                                                          column(6,colourInput('gene2col','gene2 col',value = 'orange'))),
                                                 fluidRow(column(6,textAreaInput('gene3','Gene3')),
                                                          column(6,colourInput('gene3col','gene3 col',value = 'purple'))),
                                                 )),
                                 
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
                                                                 width = 150)))),
                                 fluidRow(column(width = 4,
                                                 box(numericInput('arrow_len',
                                                                  'Arrow length:',
                                                                  min = 0,
                                                                  max=2,
                                                                  value = 0.03,
                                                                  step = 0.01,
                                                                  width = 150))),
                                          column(width = 4,
                                                 box(selectInput('arrow_head',
                                                                 'Arrow heads:',
                                                                 choices = c('first'='first','last'='last','both'='both'),
                                                                 selected='last',
                                                                 width = 150))),
                                          column(width = 4,
                                                 box(selectInput('arrow_type',
                                                                 'Arrow type:',
                                                                 choices = c('open'='open','closed'='closed'),
                                                                 selected='open',
                                                                 width = 150)))),
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
                                 withSpinner(type = 5,size = 0.5,plotlyOutput('plotly_volcano',height = 550,width = 700)),
                                 actionButton('plotly_submit',
                                              'plotly Submit',icon = icon('power-off'))),
                        tabPanel("Personalized volcano",
                                 icon = icon('user-secret'),
                                 h4(tags$strong('This panel shows personalized colors (press submit):')),
                                 fluidRow(column(8,withSpinner(type = 5,size = 0.5,plotOutput('personalized_volcano',height = 550,width = 700,
                                                                            brush = brushOpts(
                                                                                id = "plot1_brush",
                                                                                resetOnNew = TRUE)))),
                                          column(4,
                                                 checkboxInput('pflip','axis flip',value = FALSE),
                                                 sliderInput('pstroke','circle stroke',min = 0,max = 10,value = 1.5,step = 0.1),
                                                 fluidRow(column(6,textAreaInput('pgene1','Gene1')),
                                                          column(6,colourInput('pgene1col','gene1 col',value = 'pink'))),
                                                 fluidRow(column(6,textAreaInput('pgene2','Gene2')),
                                                          column(6,colourInput('pgene2col','gene2 col',value = 'orange'))),
                                                 fluidRow(column(6,textAreaInput('pgene3','Gene3')),
                                                          column(6,colourInput('pgene3col','gene3 col',value = 'purple'))),
                                          )),
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
                                 DT::dataTableOutput("table")),
                        tabPanel("manual",
                                 icon = icon('file-pdf'),
                                 tags$img(src='manual.png',width=1000)
                                 )
                        ),
            tabsetPanel(type = "tabs",
                        tabPanel("BaseMean Plot",
                                 icon = icon('chart-bar'),
                                 h4(tags$strong('BaseMean figure is showing (press submit):')),
                                 withSpinner(type = 5,size = 0.5,plotOutput("baseplot",height = 550,width = 700,
                                                                            brush = brushOpts(
                                                                                id = "plot1_brush",
                                                                                resetOnNew = TRUE))),
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
                                 ),
                                 h2(tags$strong('Road without the end,keep running!')),
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
    
    ## Server for login page (UI1) ------------------------------------------------------
    
    observeEvent(input$login, {
        
        x <- match(input$user, user_key$username)
        
        if(is.na(x) | input$pass != user_key$secret[x]){
            
            output$loginfail <- renderText("Username and Password do not match , please try again !")
            
        } else {
            
            removeUI(selector = "#loginpage")
            
            insertUI("#header", "beforeBegin", ui = ui2)
            
            removeUI(selector = "#header")
            
        }
        
    })
    
    ## Server for page after login (UI2) -------------------------------------------------
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
    
    # Single zoomable plot 
    ranges <- reactiveValues(x = NULL, y = NULL)
    
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
        # res$threshold = factor(
        #     ifelse(res$pvalue < input$pvalue & abs(res$log2FoldChange) >= input$plog2FC,
        #            ifelse(res$log2FoldChange>= input$plog2FC,'Up','Down'),
        #            'NoSig'),levels=c('Up','Down','NoSig'))
        res$threshold <- 'type'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange >= input$plog2FC),'threshold'] <- 'Up'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange <= input$nlog2FC),'threshold'] <- 'Down'
        res[which(res$threshold=='type'),'threshold'] <- 'NoSig'
        
        
        # split colors
        colo <- unlist(strsplit(input$col,split = '_'))
        
        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[3]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        
        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = '\n')))
        my_gene <- res[which(res$gene_name %in% b),]
        
        # filter gene1 you want to mark
        a1=print(input$gene1)
        b1=c(unlist(strsplit(a1,split = '\n')))
        my_gene1 <- res[which(res$gene_name %in% b1),]
        
        # filter gene2 you want to mark
        a2=print(input$gene2)
        b2=c(unlist(strsplit(a2,split = '\n')))
        my_gene2 <- res[which(res$gene_name %in% b2),]
        
        # filter gene3 you want to mark
        a3=print(input$gene3)
        b3=c(unlist(strsplit(a3,split = '\n')))
        my_gene3 <- res[which(res$gene_name %in% b3),]
        
        # 
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
            geom_vline(xintercept=c(input$nlog2FC,input$plog2FC),
                       col="black",linetype= input$vol_line_type,size=input$vol_dash_line_size) +
            geom_hline(yintercept = -log10(input$pvalue),
                       col="black",linetype= input$vol_line_type,size=input$vol_dash_line_size) +
            theme(legend.position = c(as.numeric(input$x_pos),as.numeric(input$y_pos)),
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
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=as.numeric(input$circle_shape),stroke=2,
                       data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue))) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = c(0.5,0.5)) +
            geom_text_repel(data = my_gene1, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$gene1col, shape=as.numeric(input$circle_shape),stroke=input$stroke,
                       data = my_gene1, aes(x=log2FoldChange,y=-log10(pvalue))) +
            geom_text_repel(data = my_gene2, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$gene2col, shape=as.numeric(input$circle_shape),stroke=input$stroke,
                       data = my_gene2, aes(x=log2FoldChange,y=-log10(pvalue))) +
            geom_text_repel(data = my_gene3, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$gene3col, shape=as.numeric(input$circle_shape),stroke=input$stroke,
                       data = my_gene3, aes(x=log2FoldChange,y=-log10(pvalue))) 
        
        if(input$flip==FALSE){
            p 
        }else{
            p + coord_flip()
        }
        
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    
    observe({
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
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
        res$threshold <- 'type'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange >= input$plog2FC),'threshold'] <- 'Up'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange <= input$nlog2FC),'threshold'] <- 'Down'
        res[which(res$threshold=='type'),'threshold'] <- 'NoSig'
        
        # split colors
        colo <- unlist(strsplit(input$col,split = '_'))
        
        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[3]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        
        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = '\n')))
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
            geom_vline(xintercept=c(input$nlog2FC,input$plog2FC),
                       col="black",linetype= input$vol_line_type,size=input$vol_dash_line_size) +
            geom_hline(yintercept = -log10(input$pvalue),
                       col="black",linetype= input$vol_line_type,size=input$vol_dash_line_size) +
            theme(legend.position = c(as.numeric(input$x_pos),as.numeric(input$y_pos)),
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
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=as.numeric(input$circle_shape),stroke=2,
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
        res$threshold <- 'type'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange >= input$plog2FC),'threshold'] <- 'Up'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange <= input$nlog2FC),'threshold'] <- 'Down'
        res[which(res$threshold=='type'),'threshold'] <- 'NoSig'
        
        # split colors
        # colo <- unlist(strsplit(input$col,split = '_'))
        
        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[3]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        
        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = '\n')))
        my_gene <- res[which(res$gene_name %in% b),]
        
        # filter gene1 you want to mark
        a1=print(input$pgene1)
        b1=c(unlist(strsplit(a1,split = '\n')))
        my_gene1 <- res[which(res$gene_name %in% b1),]
        
        # filter gene2 you want to mark
        a2=print(input$pgene2)
        b2=c(unlist(strsplit(a2,split = '\n')))
        my_gene2 <- res[which(res$gene_name %in% b2),]
        
        # filter gene3 you want to mark
        a3=print(input$pgene3)
        b3=c(unlist(strsplit(a3,split = '\n')))
        my_gene3 <- res[which(res$gene_name %in% b3),]
        
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
            geom_vline(xintercept=c(input$nlog2FC,input$plog2FC),
                       col="black",linetype= input$vol_line_type,size=input$vol_dash_line_size) +
            geom_hline(yintercept = -log10(input$pvalue),
                       col="black",linetype= input$vol_line_type,size=input$vol_dash_line_size) +
            theme(legend.position = c(as.numeric(input$x_pos),as.numeric(input$y_pos)),
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
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=as.numeric(input$circle_shape),stroke=2,
                       data = my_gene, aes(x=log2FoldChange,y=-log10(pvalue))) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = c(0.5,0.5)) +
            geom_text_repel(data = my_gene1, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$pgene1col, shape=as.numeric(input$circle_shape),stroke=input$pstroke,
                       data = my_gene1, aes(x=log2FoldChange,y=-log10(pvalue))) +
            geom_text_repel(data = my_gene2, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$pgene2col, shape=as.numeric(input$circle_shape),stroke=input$pstroke,
                       data = my_gene2, aes(x=log2FoldChange,y=-log10(pvalue))) +
            geom_text_repel(data = my_gene3, aes(x=log2FoldChange,y=-log10(pvalue), label = gene_name),
                            size = input$text_size,
                            color=input$text_col,
                            box.padding = unit(2, 'cm'),
                            segment.color = input$arrow_col,
                            show.legend = FALSE,
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$pgene3col, shape=as.numeric(input$circle_shape),stroke=input$pstroke,
                       data = my_gene3, aes(x=log2FoldChange,y=-log10(pvalue))) 
        
        if(input$pflip==FALSE){
            p 
        }else{
            p + coord_flip()
        }
        
        
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
        res$threshold <- 'type'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange >= input$plog2FC),'threshold'] <- 'Up'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange <= input$nlog2FC),'threshold'] <- 'Down'
        res[which(res$threshold=='type'),'threshold'] <- 'NoSig'
        
        # split colors
        colo <- unlist(strsplit(input$col,split = '_'))
        
        # add the assigned genes numbers label
        suma <- as.data.frame(table(res$threshold))
        up <- paste(suma$Var1,suma$Freq,sep = '-')[3]
        down <- paste(suma$Var1,suma$Freq,sep = '-')[1]
        nosig <- paste(suma$Var1,suma$Freq,sep = '-')[2]
        
        # filter genes you want to mark
        a=print(input$gene)
        b=c(unlist(strsplit(a,split = '\n')))
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
            geom_hline(yintercept=c(input$nlog2FC,input$plog2FC),
                       col="black",linetype= input$vol_line_type,size=input$vol_dash_line_size) +
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
                            arrow = arrow(length = unit(as.numeric(input$arrow_len), "npc"),
                                          ends = input$arrow_head, type = input$arrow_type),
                            fontface='italic',
                            segment.size=input$arrow_line ,
                            segment.alpha= 0.8) +
            geom_point(size=input$circle_size,colour=input$circle_col, shape=as.numeric(input$circle_shape),stroke=2,
                       data = my_gene, aes(y=log2FoldChange,x=log2(baseMean+1))) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = c(0.5,0.5))
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
        
        res$threshold <- 'type'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange >= input$plog2FC),'threshold'] <- 'Up'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange <= input$nlog2FC),'threshold'] <- 'Down'
        res[which(res$threshold=='type'),'threshold'] <- 'NoSig'
        
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
        
        res$threshold <- 'type'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange >= input$plog2FC),'threshold'] <- 'Up'
        res[which(res$pvalue < input$pvalue & res$log2FoldChange <= input$nlog2FC),'threshold'] <- 'Down'
        res[which(res$threshold=='type'),'threshold'] <- 'NoSig'
        
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
            paste('my-plot', sep = '.', switch(
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
shinyApp(ui = ui1, server = server)