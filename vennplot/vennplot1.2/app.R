library(shinyWidgets)
library(shiny)
library(stringr)
library(shinythemes)
library(VennDiagram)
library(ggplot2)
library(ggplotify)
library(grid)
library(colourpicker)
library(grDevices)


# windowsFonts(Times=windowsFont("TT Times New Roman"))

ui <- navbarPage(
    theme = shinytheme('slate'),
    title = tags$strong('ZhouLab VennPlot'),
    
    tabPanel('vennplot',icon = icon('cc-mastercard'),
             sidebarLayout(
                 sidebarPanel(h4(p('Venn plot to intersect diffrent groups elements,2021/04/25 by JunZhang.')),
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
                     
                     fluidRow(column(6,numericInput('rotation_degree','rotation degree',min = 0,max = 360,value = 0,step = 1)),
                              column(6,colourInput('color','select color name here',value = '#CF68E6')),
                              ),
                     
                     fluidRow(column(6,textInput(inputId = 'fill',label = 'fill color',
                                                 value = 'red_blue',
                                                 )),
                              column(6,sliderInput('alpha','color alpha',min = 0,max = 1,value = 0.3,step = 0.1))),
                     fluidRow(column(4,textInput('border_col','border color',value = 'black',placeholder = 'transparent')),
                              column(4,numericInput('lwd','boeder width',min = 0,max = 10,value = 1)),
                              column(4,textInput('border_line','border line',value = 'solid',placeholder = 'dashed/solid')),
                              ),
                     
                     fluidRow(column(6,textInput('label_col','label col',value = 'black')),
                              column(6,sliderInput('cex','label size',min = 0,max = 20,value = 2,step = 0.5)),
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
                              column(6,sliderInput('cat_cex','cat cex',min = 0,max = 20,value = 4,step = 0.5)),
                     ),
                     hr(style="border-color: grey"),
                     fluidRow(column(6,textInput('main','plot title',value = 'Overlap plot')),
                              column(6,numericInput('margin','plot margin',min = 0,max = 1,value = 0.02,step = 0.01))
                     ),
                     tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #289672}")),
                     tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #81b214}")),
                     tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #f67280}")),
                
                     
                     tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #fea82f}")),
                     tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #fea82f}")),
                     
                     fluidRow(column(6,sliderInput('main_cex','title cex',min = 0,max = 20,value = 3.5,step = 0.5)),
                              column(6,sliderInput('main_height','title height',min = 0,max = 5,value = 1.1,step = 0.1))
                              ),
                 ),
                 #####################################################################################################################
                 mainPanel(
                     tabsetPanel(
                         tabPanel('Hint',icon = icon('laugh-beam'),
                                  
                                  fluidRow(column(6,tags$img(src='hint1.png',width=500,height=500)),
                                           column(6,tags$img(src='hint2.png',width=500,height=500)),
                                           ),
                                  hr(style="border-color: #ffc2b4"),
                                  fluidRow(column(6,tags$img(src='hint3.png',width=500,height=500)),
                                           column(6,tags$img(src='hint4.png',width=500,height=500)),
                                  )),
                                  
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
                                  
                                  fluidRow(column(2,switchInput(inputId = "euler_d",'euler.d',value = TRUE,onStatus = "success",offStatus = "danger")),
                                           column(2,switchInput(inputId = "scaled",'scaled',value = TRUE,onStatus = "success",offStatus = "danger")),
                                           column(2,switchInput(inputId = "inverted",'inverted',value = FALSE,onStatus = "success",offStatus = "danger")),
                                           ),
                                  
                                  fluidRow(column(2,switchInput(inputId = "ext_text",'extext',value = TRUE,onStatus = "success",offStatus = "danger")),
                                           column(2,numericInput('ext_pos','ext pos(0-12 o’clock)',min = -360,max = 360,value = 0,step = 1)),
                                           column(2,numericInput('ext_line_lwd','ext line lwd',min = 0,max = 20,value = 1,step = 0.5)),
                                           column(2,textInput('ext_line_lty','ext line type	',value = 'solid',placeholder = 'dashed/solid')),
                                           column(2,numericInput('ext_dist','ext total length',min = -5,max = 5,value = 0,step = 0.05)),
                                           column(2,numericInput('ext_length','ext line length',min = 0,max = 5,value = 1,step = 0.05)),
                                  )
                                  ),
                         tabPanel('Three group Venn',icon = icon('flickr'),
                                  
                                  fluidRow(column(6,plotOutput('three_vennplot',height = 550,width = 550)),
                                           column(6,
                                                  radioGroupButtons("rotation","rotation",
                                                                    choices = c("1", "2", "3"), individual = TRUE,selected = '1',
                                                                    checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))
                                                  ),
                                                  numericInput('height3',
                                                                 'Height',min = 1,max = 100,step = 1,value = 10),
                                                  numericInput('width3',
                                                               'Width',min = 1,max = 100,step = 1,value = 10),
                                                  downloadButton('down_three_venn','Download plot here'),
                                                  
                                                  downloadButton('down_three_venn_list','Download  list  here'),
                                           ),
                                           
                                  ),
                                  
                                  hr(style="border-color: #ffc2b4"),
                                  
                                  fluidRow(column(2,switchInput(inputId = "euler_d3",'euler.d',value = TRUE,onStatus = "success",offStatus = "danger")),
                                           column(2,switchInput(inputId = "scaled3",'scaled',value = TRUE,onStatus = "success",offStatus = "danger")),
                                           column(2,switchInput(inputId = "reverse",'reverse',value = FALSE,onStatus = "success",offStatus = "danger")),
                                           
                                  ),
                         ),
                         tabPanel('Four group Venn',icon = icon('flickr'),
                                  
                                  fluidRow(column(6,plotOutput('four_vennplot',height = 550,width = 550)),
                                           column(6,
                                                  numericInput('height4',
                                                               'Height',min = 1,max = 100,step = 1,value = 10),
                                                  numericInput('width4',
                                                               'Width',min = 1,max = 100,step = 1,value = 10),
                                                  downloadButton('down_four_venn','Download plot here'),
                                                  
                                                  downloadButton('down_four_venn_list','Download  list  here'),
                                           ),
                                           
                                  ),
                                  
                                  hr(style="border-color: #ffc2b4"),
                         ),
                         tabPanel('Five group Venn',icon = icon('flickr'),
                                  
                                  fluidRow(column(6,plotOutput('five_vennplot',height = 550,width = 550)),
                                           column(6,
                                                  numericInput('height5',
                                                               'Height',min = 1,max = 100,step = 1,value = 10),
                                                  numericInput('width5',
                                                               'Width',min = 1,max = 100,step = 1,value = 10),
                                                  downloadButton('down_five_venn','Download plot here'),
                                                  
                                                  downloadButton('down_five_venn_list','Download  list  here'),
                                           ),
                                           
                                  ),
                                  
                                  hr(style="border-color: #ffc2b4"),
                         )
                     )
                 )
             )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    {observe({
        fil_c <- input$set_number
        
        
        if(fil_c == '2'){
            updateTextInput(session ,'fill',value = paste('red_blue'))
        }else if(fil_c == '3'){
            updateTextInput(session ,'fill',value = paste('red_blue_green'))
        }else if(fil_c == '4'){
            updateTextInput(session ,'fill',value = paste('red_blue_green_purple'))
        }else if(fil_c == '5'){
            updateTextInput(session ,'fill',value = paste('red_blue_green_purple_orange'))
        }
        
        if(fil_c == '2'){
            updateTextInput(session ,'cat_pos',value = paste('180_180'))
        }else if(fil_c == '3'){
            updateTextInput(session ,'cat_pos',value = paste('360_360_180'))
        }else if(fil_c == '4'){
            updateTextInput(session ,'cat_pos',value = paste('360_360_360_360'))
        }else if(fil_c == '5'){
            updateTextInput(session ,'cat_pos',value = paste('360_360_180_180_360'))
        }
        
        if(fil_c == '2'){
            updateTextInput(session ,'cat_dist',value = paste('0.07_0.07'))
        }else if(fil_c == '3'){
            updateTextInput(session ,'cat_dist',value = paste('0.07_0.07_0.07'))
        }else if(fil_c == '4'){
            updateTextInput(session ,'cat_dist',value = paste('0.25_0.25_0.11_0.1'))
        }else if(fil_c == '5'){
            updateTextInput(session ,'cat_dist',value = paste('0.25_0.25_0.15_0.22_0.25'))
        }
        
    })
    }
    
    
    
    
    # -------------------------------------------------------------------------------------two vennplot
    # output$two_vennplot <- renderPlot({
        
    {two_plot <- reactive({
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
            
            rotation.degree	= input$rotation_degree,
            
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
    # -------------------------------------------------------------------------------------three vennplot
    # output$three_vennplot <- renderPlot({
    
    {three_plot <- reactive({
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
        n3=colnames(res)[3]
        dat_list <- list(n1=res[,1],n2=res[,2],n3=res[,3])
        names(dat_list) <- c(n1,n2,n3)
        # -----------------------------------------
        # fill color split
        input_fill_color <- unlist(strsplit(input$fill,split = '_'))
        fill <- c(input_fill_color[1],input_fill_color[2],input_fill_color[3])
        
        # cat.pos split cat_pos
        input_cat_pos <- unlist(strsplit(input$cat_pos,split = '_'))
        cat_ps <- c(as.numeric(input_cat_pos[1]),as.numeric(input_cat_pos[2]),as.numeric(input_cat_pos[3]))
        
        # cat.pos split cat_pos
        input_cat_dist <- unlist(strsplit(input$cat_dist,split = '_'))
        cat_dis <- c(as.numeric(input_cat_dist[1]),as.numeric(input_cat_dist[2]),as.numeric(input_cat_dist[3]))
        
        venn.plot = venn.diagram(
            x = dat_list,
            na = 'remove',
            
            rotation.degree	= input$rotation_degree,
            
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
            euler.d = input$euler_d3,
            scaled = input$scaled3,
            reverse = input$reverse ,
            rotation = as.numeric(input$rotation),
        )
        
        
    })
    
    output$three_vennplot <- renderPlot({
        grid.newpage()
        grid.draw(three_plot())
    })
    
    
    # download three venn plot
    output$down_three_venn <- downloadHandler(
        
        filename = function() {
            paste('three_group_vennplot','pdf',sep = '.')
        },
        
        
        content = function(file) {
            
            pdf(file,height = input$height2,width = input$width2)
            
            grid.newpage()
            grid.draw(three_plot())
            dev.off()
        })
    
    # download three venn list
    threevenn_list <- reactive({
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
        n3=colnames(res)[3]
        dat_list <- list(n1=na.omit(res[,1]),n2=na.omit(res[,2]),n3=na.omit(res[,3]))
        names(dat_list) <- c(n1,n2,n3)
        
        inter <- get.venn.partitions(dat_list)
        
        for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
        inter <- inter[,c(1,2,3,7)]
        
    })
    
    output$down_three_venn_list <- downloadHandler(
        
        filename = function() {
            paste('three_group_vennlist','csv',sep = '.')
        },
        
        
        content = function(file) {
            write.csv(threevenn_list(),file,row.names = F)
    })
    }
    
    # -------------------------------------------------------------------------------------four vennplot
    
    {four_plot <- reactive({
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
        n3=colnames(res)[3]
        n4=colnames(res)[4]
        dat_list <- list(n1=res[,1],n2=res[,2],n3=res[,3],n4=res[,4])
        names(dat_list) <- c(n1,n2,n3,n4)
        # -----------------------------------------
        # fill color split
        input_fill_color <- unlist(strsplit(input$fill,split = '_'))
        fill <- c(input_fill_color[1],input_fill_color[2],input_fill_color[3],input_fill_color[4])
        
        # cat.pos split cat_pos
        input_cat_pos <- unlist(strsplit(input$cat_pos,split = '_'))
        cat_ps <- c(as.numeric(input_cat_pos[1]),as.numeric(input_cat_pos[2]),as.numeric(input_cat_pos[3]),as.numeric(input_cat_pos[4]))
        
        # cat.pos split cat_pos
        input_cat_dist <- unlist(strsplit(input$cat_dist,split = '_'))
        cat_dis <- c(as.numeric(input_cat_dist[1]),as.numeric(input_cat_dist[2]),as.numeric(input_cat_dist[3]),as.numeric(input_cat_dist[4]))
        
        venn.plot = venn.diagram(
            x = dat_list,
            na = 'remove',
            
            rotation.degree	= input$rotation_degree,
            
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
            
            # ------------------
            
        )
        
        
    })
    
    output$four_vennplot <- renderPlot({
        grid.newpage()
        grid.draw(four_plot())
    })
    
    
    # download three venn plot
    output$down_four_venn <- downloadHandler(
        
        filename = function() {
            paste('four_group_vennplot','pdf',sep = '.')
        },
        
        
        content = function(file) {
            
            pdf(file,height = input$height4,width = input$width4)
            
            grid.newpage()
            grid.draw(four_plot())
            dev.off()
        })
    
    # download three venn list
    fourvenn_list <- reactive({
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
        n3=colnames(res)[3]
        n4=colnames(res)[4]
        dat_list <- list(n1=na.omit(res[,1]),n2=na.omit(res[,2]),n3=na.omit(res[,3]),n4=na.omit(res[,4]))
        names(dat_list) <- c(n1,n2,n3,n4)
        
        inter <- get.venn.partitions(dat_list)
        
        for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
        inter <- inter[,c(1,2,3,4,8)]
        
    })
    
    output$down_four_venn_list <- downloadHandler(
        
        filename = function() {
            paste('four_group_vennlist','csv',sep = '.')
        },
        
        
        content = function(file) {
            write.csv(fourvenn_list(),file,row.names = F)
        })
    }
    
    # -------------------------------------------------------------------------------------five vennplot
    
    {five_plot <- reactive({
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
        n3=colnames(res)[3]
        n4=colnames(res)[4]
        n5=colnames(res)[5]
        dat_list <- list(n1=res[,1],n2=res[,2],n3=res[,3],n4=res[,4],n5=res[,5])
        names(dat_list) <- c(n1,n2,n3,n4,n5)
        # -----------------------------------------
        # fill color split
        input_fill_color <- unlist(strsplit(input$fill,split = '_'))
        fill <- c(input_fill_color[1],input_fill_color[2],input_fill_color[3],input_fill_color[4],input_fill_color[5])
        
        # cat.pos split cat_pos
        input_cat_pos <- unlist(strsplit(input$cat_pos,split = '_'))
        cat_ps <- c(as.numeric(input_cat_pos[1]),as.numeric(input_cat_pos[2]),as.numeric(input_cat_pos[3]),
                    as.numeric(input_cat_pos[4]),as.numeric(input_cat_pos[5]))
        
        # cat.pos split cat_pos
        input_cat_dist <- unlist(strsplit(input$cat_dist,split = '_'))
        cat_dis <- c(as.numeric(input_cat_dist[1]),as.numeric(input_cat_dist[2]),as.numeric(input_cat_dist[3]),
                     as.numeric(input_cat_dist[4]),as.numeric(input_cat_dist[5]))
        
        venn.plot = venn.diagram(
            x = dat_list,
            na = 'remove',
            
            rotation.degree	= input$rotation_degree,
            
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
            
            margin =input$margin
            
            # ------------------
            
        )
        
        
    })
    
    output$five_vennplot <- renderPlot({
        grid.newpage()
        grid.draw(five_plot())
        
    })
    
    
    # download three venn plot
    output$down_five_venn <- downloadHandler(
        
        filename = function() {
            paste('five_group_vennplot','pdf',sep = '.')
        },
        
        
        content = function(file) {
            
            pdf(file,height = input$height5,width = input$width5)
            
            grid.newpage()
            grid.draw(five_plot())
            dev.off()
        })
    
    # download three venn list
    fivevenn_list <- reactive({
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
        n3=colnames(res)[3]
        n4=colnames(res)[4]
        n5=colnames(res)[5]
        dat_list <- list(n1=na.omit(res[,1]),n2=na.omit(res[,2]),n3=na.omit(res[,3]),n4=na.omit(res[,4]),n5=na.omit(res[,5]))
        names(dat_list) <- c(n1,n2,n3,n4,n5)
        
        inter <- get.venn.partitions(dat_list)
        
        for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ')
        inter <- inter[,c(1,2,3,4,5,9)]
        
    })
    
    output$down_five_venn_list <- downloadHandler(
        
        filename = function() {
            paste('five_group_vennlist','csv',sep = '.')
        },
        
        
        content = function(file) {
            write.csv(fivevenn_list(),file,row.names = F)
        })
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
