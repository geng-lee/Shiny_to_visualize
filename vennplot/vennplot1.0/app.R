library(shinyWidgets)
library(shiny)
library(stringr)
library(shinythemes)
library(VennDiagram)
library(ggplot2)
library(ggplotify)
library(grid)
library(myplot)
library(grDevices)


# windowsFonts(Times=windowsFont("TT Times New Roman"))

ui <- navbarPage(
    theme = shinytheme('slate'),
    title = tags$strong('ZhouLab VennPlot'),
    
    tabPanel('vennplot',icon = icon('cc-mastercard'),
             sidebarLayout(
                 sidebarPanel(
                     fileInput('data',
                               h4('Upload overlap data'),
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                     
                     radioGroupButtons(
                         inputId = "set_number",label = "Choose set number",size = 'normal',
                         choices = c("2 group"=2, "3 group"=3, "4 group"=4,"5 group"=5),
                         checkIcon = list(
                             yes = tags$i(class = "fa fa-check-square", 
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-square-o", 
                                         style = "color: steelblue")),
                         individual = T,selected = 2),
                     
                     fluidRow(column(6,textInput('fill','fill color',value = 'red_blue')),
                              column(6,numericInput('alpha','color alpha',min = 0,max = 1,value = 0.3,step = 0.1))),
                     fluidRow(column(4,textInput('border_col','border color',value = 'black',placeholder = 'transparent')),
                              column(4,numericInput('lwd','boeder width',min = 0,max = 10,value = 1)),
                              column(4,textInput('border_line','border line',value = 'solid',placeholder = 'dashed/solid')),
                              ),
                     
                     fluidRow(column(6,textInput('label_col','label col',value = 'black')),
                              column(6,sliderInput('cex','label size',min = 0,max = 20,value = 2)),
                     ),
                     hr(style="border-color: grey"),
                     
                     fluidRow(column(6,selectInput('print_mode','label style',choices = c('raw','percent'),
                                                   selected = 'raw')),
                              column(6,numericInput('sigdigs','sig digts',min = 0,max = 15,value = 3,step = 1)),
                     ),
                     
                     hr(style="border-color: grey"),
                     fluidRow(column(6,textInput('cat_pos','cat pos (0-12 o’clock)',value = '180_180')),
                              column(6,textInput('cat_dist','cat dist',value = '0.07_0.07')),
                              
                              column(6,textInput('cat_col','cat col',value = 'black')),
                              column(6,sliderInput('cat_cex','cat cex',min = 0,max = 20,value = 2,step = 0.5)),
                     ),
                     hr(style="border-color: grey"),
                     fluidRow(column(6,textInput('main','plot title',value = 'Overlap plot')),
                              column(6,numericInput('margin','plot margin',min = 0,max = 1,value = 0.02,step = 0.01))
                     ),
                     
                     tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #fea82f}")),
                     tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #fea82f}")),
                     
                     fluidRow(column(6,sliderInput('main_cex','title cex',min = 0,max = 20,value = 3.5,step = 0.5)),
                              column(6,sliderInput('main_height','title height',min = 0,max = 5,value = 1.1,step = 0.1))
                              ),
                 ),
                 #####################################################################################################################
                 mainPanel(
                     tabsetPanel(
                         tabPanel('Two group Venn',icon = icon('flickr'),
                                  
                                  fluidRow(column(6,plotOutput('two_vennplot',height = 550,width = 550)),
                                           column(6,numericInput('height2',
                                                                          'Height',min = 1,max = 100,step = 1,value = 10),
                                                    numericInput('width2',
                                                                          'Width',min = 1,max = 100,step = 1,value = 10),
                                                    downloadButton('down_two_venn','Download plot here'),
                                                    
                                                    downloadButton('down_two_venn_list','Download  list  here'),
                                                    ),
                                                    
                                           ),
                                  
                                  hr(style="border-color: #ffc2b4"),
                                  
                                  fluidRow(column(2,switchInput(inputId = "euler_d",'euler.d',value = TRUE)),
                                           column(2,switchInput(inputId = "scaled",'scaled',value = TRUE)),
                                           column(2,switchInput(inputId = "inverted",'inverted',value = FALSE)),
                                           ),
                                  
                                  fluidRow(column(2,switchInput(inputId = "ext_text",'extext',value = TRUE)),
                                           column(2,numericInput('ext_pos','ext pos(0-12 o’clock)',min = -360,max = 360,value = 0,step = 1)),
                                           column(2,numericInput('ext_line_lwd','ext line lwd',min = 0,max = 20,value = 1,step = 0.5)),
                                           column(2,textInput('ext_line_lty','ext line type	',value = 'solid',placeholder = 'dashed/solid')),
                                           column(2,numericInput('ext_dist','ext total length',min = -5,max = 5,value = 0,step = 0.05)),
                                           column(2,numericInput('ext_length','ext line length',min = 0,max = 5,value = 1,step = 0.05)),
                                  )
                                  )
                     )
                 )
             )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # -------------------------------------------------------------------------------------two vennplot
    # output$two_vennplot <- renderPlot({
        
    two_plot <- reactive({
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        # to list
        n1=colnames(res)[1]
        n2=colnames(res)[2]
        dat_list <- list(n1=res[,1],n2=res[,2])
        names(dat_list) <- c(n1,n2)
        # -----------------------------------------
        # fill color split
        input_fill_color <- unlist(strsplit(input$fill,split = '_'))
        fill <- c(input_fill_color[1],input_fill_color[2])
        
        # cat.pos split cat_pos
        input_cat_pos <- unlist(strsplit(input$cat_pos,split = '_'))
        cat_ps <- c(as.numeric(input_cat_pos[1]),as.numeric(input_cat_pos[2]))
        
        # cat.pos split cat_pos
        input_cat_dist <- unlist(strsplit(input$cat_dist,split = '_'))
        cat_dis <- c(as.numeric(input_cat_dist[1]),as.numeric(input_cat_dist[2]))
        
        venn.plot = venn.diagram(
            x = dat_list,
            na = 'remove',
            
            filename = NULL,
            fill = fill,
            alpha = input$alpha,
            col = input$border_col,
            lwd = input$lwd,
            lty = input$border_line,
            label.col = input$label_col,
            cex = input$cex,
            # fontfamily = "arial",
            # fontface = "plain",
            print.mode = input$print_mode,
            sigdigs =input$sigdigs ,
            cat.pos = cat_ps,
            cat.dist = cat_dis,
            
            cat.col = input$cat_col,
            cat.cex = input$cat_cex,
            # cat.fontfamily = "arial",
            # cat.fontface = "plain",
            
            main = input$main,
            main.cex = input$main_cex,
            main.pos = c(0.5,input$main_height),
            # main.fontfamily = "arial",
            # main.fontface = "plain",
            
            margin =input$margin,
            
            # ------------------ext_text ext_pos ext_line_lwd ext_line_lty ext_dist ext_length
            euler.d = input$euler_d,
            scaled = input$scaled,
            inverted = input$inverted,
            
            ext.text = input$ext_text,
            ext.pos = input$ext_pos,
            ext.line.lwd = input$ext_line_lwd,
            ext.line.lty = input$ext_line_lty,    
            ext.dist = input$ext_dist,    
            ext.length = input$ext_length,    
        )
        
        
    })
    
    output$two_vennplot <- renderPlot({
        grid.newpage()
        grid.draw(two_plot())
    })
    

    # download two venn plot
    output$down_two_venn <- downloadHandler(
        
        filename = function() {
            paste('two_group_vennplot','pdf',sep = '.')
            },

        
        content = function(file) {
     
            pdf(file,height = input$height2,width = input$width2)
            
            grid.newpage()
            grid.draw(two_plot())
            dev.off()
            })
    
    # download two venn list
    twovenn_list <- reactive({
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        # to list
        n1=colnames(res)[1]
        n2=colnames(res)[2]
        dat_list <- list(n1=na.omit(res[,1]),n2=na.omit(res[,2]))
        names(dat_list) <- c(n1,n2)
        
        inter <- get.venn.partitions(dat_list)
        
        for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
        inter <- inter[,c(1,2,6)]
        
    })
    
    output$down_two_venn_list <- downloadHandler(
        
        filename = function() {
            paste('two_group_vennlist','csv',sep = '.')
        },
        
        
        content = function(file) {
            write.csv(twovenn_list(),file,row.names = F)
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
