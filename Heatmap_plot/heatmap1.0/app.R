library(shinythemes)
library(shiny)
library(DT)
library(stringr)
library(tidyverse)
library(ggplot2)
library(ggplotify)
library(reshape2)
library(shinydashboard)
library(colourpicker)
library(pheatmap)
# Define UI for application that draws a histogram
ui <- fluidPage(
                theme = shinytheme("simplex"),
    fluidRow(
        column(3,
               wellPanel(fileInput('heatmap_data',
                                   h4('Upload your data:'),
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         fluidRow(column(6,
                                         radioButtons('cluster_rows',label = tags$strong('cluster rows:'),
                                                      choices = c('TRUE'=TRUE,'FALSE'=FALSE),
                                                      selected = TRUE)),
                                  column(6,
                                         radioButtons('cluster_cols',label = tags$strong('cluster cols:'),
                                                      choices = c('TRUE'=TRUE,'FALSE'=FALSE),
                                                      selected = TRUE))),
                         selectInput('clustering_method','clustering method:',
                                     choices = c("ward.D","ward.D2","single","complete", "average","mcquitty","median","centroid"),
                                     selected = "complete"),
                         fluidRow(column(6,
                                         numericInput('treeheight_row','treeheight row:',
                                                      min = 0,max = 250,step = 1,value = 50)),
                                  column(6,
                                         numericInput('treeheight_col','treeheight col:',
                                                      min = 0,max = 250,step = 1,value = 50))),
                         fluidRow(column(6,
                                         selectInput('clustering_distance_rows','clustering distance rows method:',
                                                     choices = c("euclidean","maximum","manhattan","canberra", "binary","minkowski","correlation"),
                                                     selected = "euclidean")),
                                  column(6,
                                         selectInput('clustering_distance_cols','clustering distance cols method:',
                                                     choices = c("euclidean","maximum","manhattan","canberra", "binary","minkowski","correlation"),
                                                     selected = "euclidean"))),
                         fluidRow(column(6,
                                         radioButtons('show_rownames',label = tags$strong('show row names:'),
                                                      choices = c('TRUE'=TRUE,'FALSE'=FALSE),
                                                      selected = TRUE,inline = T)),
                                  column(6,
                                         radioButtons('show_colnames',label = tags$strong('show col names:'),
                                                      choices = c('TRUE'=TRUE,'FALSE'=FALSE),
                                                      selected = TRUE,inline = T))),
                         radioButtons('heat_color_choose',h4('heatmap color choose:'),
                                      choices = c('palette'='palette','personalized'='personalized'),
                                      selected = 'palette',inline = T),
                         selectInput('heatmap_col',
                                     'The colors palette:',
                                     choices = c('classic' = '#dd2c00_#438a5e_white',
                                                 'default' = 'firebrick3_navy_white',
                                                 'pink girl' = '#e7305b_#96bb7c_white',
                                                 'megenta' = '#d54062_#519872_white',
                                                 'AAAS' = '#be0000_#161d6f_white',
                                                 'maid feelings' = '#ff9292_#aee1e1_white',
                                                 'mash' = '#be0000_#f58634_white',
                                                 'captain America' = '#c70039_#1a508b_white',
                                                 'tomato' = '#bb2205_#61b15a_white',
                                                 'blusher' = '#ff005c_#7868e6_white',
                                                 'darkblood' = '#91091e_#a1cae2_white',
                                                 'village' = '#f88f01_#16c79a_white',
                                                 'Macaron' = '#fbbedf_#bce6eb_white',
                                                 'Spring' = '#ff00c8_#54e346_white',
                                                 'Summer' = '#ffa931_#9fe8fa_white',
                                                 'Autumn' = '#ff4301_#12cad6_white',
                                                 'Winter' = '#ffaf87_#a1c45a_white',
                                                 'Black Dress lane' = '#ff9a76_#679b9b_white',
                                                 'Sprite' = '#f4e04d_#28df99_white',
                                                 'bubble gum' = '#fddb3a_#00bcd4_white'),
                                     selected = 'firebrick3_navy_white',
                                     width = 400),
                         h5(tags$strong('worked when rows or cols are clustered:',style="color:#1687a7")),
                         fluidRow(column(6,
                                         numericInput('cutree_rows','cutree rows:',
                                                      min = 0,max = 500,step = 1,value = NA)),
                                  column(6,
                                         numericInput('cutree_cols','cutree cols:',
                                                      min = 0,max = 500,step = 1,value = NA))),
                         h5(tags$strong('worked when rows or cols are NOT clustered:',style="color:#1687a7")),
                         fluidRow(column(6,
                                         textInput('gaps_row','gaps row:',value = '')),
                                  column(6,
                                         textInput('gaps_col','gaps col:',value = ''))),
                         checkboxInput('annotation_names_row',tags$strong('Show Annotation names row'),value = TRUE),
                         checkboxInput('annotation_names_col',tags$strong('Show Annotation names col'),value = TRUE),
                         fluidRow(column(6,
                                         numericInput('fontsize_row','fontsize row:',
                                                      min = 0,max = 50,step = 1,value = 10)),
                                  column(6,
                                         numericInput('fontsize_col','fontsize col:',
                                                      min = 0,max = 50,step = 1,value = 10))),
                         fluidRow(column(4,radioButtons('format',
                                                        'Export format:',
                                                        choices = c('PDF'),
                                                        selected = 'PDF')),
                                  column(3,downloadButton('downloadata',
                                                          'Download data here:'))),
               )),
        column(6,
               tabsetPanel(
                   tabPanel('heatmap plot:',
                            icon = icon('chess-board'),
                            fluidRow(plotOutput('heatmap_plot',height = 750,width = 650)),
                            fluidRow(column(width = 3,
                                            box(colourInput("col_1", "Select first colour:", "red"),width = 150)),
                                     column(width = 3,
                                            box(colourInput("col_2", "Select second colour:", "white"),width = 150)),
                                     column(width = 3,
                                            box(colourInput("col_3", "Select third colour", "blue"),width = 150)),
                                     column(width = 3,
                                            actionButton('heat_submit',
                                                         'submit',
                                                         icon = icon('power-off'),
                                                         width = 128))),
                            # fluidRow(column(width = 6,
                            #                 numericInput("width", "Plot width:", min = 1,max = 100,step = 1,value = NA)),
                            #          column(width = 6,
                            #                 numericInput("height", "Plot height:", min = 1,max = 100,step = 1,value = NA)),
                            #          ),
                            fluidRow(column(6,
                                            radioButtons('legend',label = tags$strong('show legend:'),
                                                         choices = c('TRUE'='TRUE','FALSE'='FALSE'),
                                                         selected = 'TRUE',inline = T)),
                                     column(6,
                                            radioButtons('annotation_legend',label = tags$strong('show annotation legend:'),
                                                         choices = c('TRUE'='TRUE','FALSE'='FALSE'),
                                                         selected = 'TRUE',inline = T)),
                            ),
                            checkboxInput('choose_annocol',h5(tags$strong('Choose to change annotation color:')),value = FALSE),
                            # change textinput fontsize
                            tags$style("#script_anno_col {font-size:20px;}"),
                            fluidRow(textAreaInput('script_anno_col',h5(tags$strong('Script to Change the annotation colors:',style="color:#1687a7",p('(You only need change the column and group names and colors in the list!)',style="color:red"))),
                                                   rows = 5,cols = 100,width="1000px",
                                                   
                                                   value = "anno_color <- list(type=c(control='#23C0F5',treat='#F5C96A'),cellline=c(A549='#4B97FA',H226='#FA99F2'),time=c('#F04AAE','#FF3845'))")),
                            fluidRow(column(4,numericInput('heatmap_height',
                                                           'Height',min = 1,max = 100,step = 1,value = 15)),
                                     column(4,numericInput('heatmap_width',
                                                           'Width',min = 1,max = 100,step = 1,value = 22)),
                                     
                            ),
                            h4(tags$strong('The heatmap plot is saved in the script directory,you can change the plot width and height.',style="color:#EDC203")),
                            
                            
                   ),
                   tabPanel('table upload:',
                            icon = icon('align-justify'),
                            dataTableOutput('heatmap_table')),
                   tabPanel('Column annotation table:',
                            icon = icon('align-justify'),
                            br(),
                            p('Example:'),
                            tags$img(src='column_annotation.png'),
                            dataTableOutput('annotation_col_table')),
                   tabPanel('Rows annotation table:',
                            icon = icon('align-justify'),
                            br(),
                            p('Example:'),
                            tags$img(src='rows_annotation.png'),
                            dataTableOutput('annotation_row_table')))
        ),
        column(3,
               wellPanel(textInput('main',h4('Plot title:'),value = 'Heatmap plot'),
                         radioButtons('scale',label = tags$strong('scale or not:'),
                                      choices = c('Yes (by row)'='row','Yes (by col)'='column','No'='none'),selected = 'row'),
                         numericInput('fontsize','fontsize:',
                                      min = 1,max = 50,value = 10),
                         radioButtons('angle_col',label = tags$strong('angle col:'),
                                      choices = c('0'=0,'45'=45,'90'=90,'270'=270,'315'=315),
                                      selected = 45,inline = T),
                         
                         fluidRow(column(6,
                                         radioButtons('display_numbers',label = tags$strong('display numbers:'),
                                                      choices = c('TRUE'=TRUE,'FALSE'=FALSE),
                                                      selected = FALSE,inline = T)),
                                  column(6,
                                         textInput('number_color',tags$strong('number color:'),value = 'black'))),
                         sliderInput('fontsize_number','fontsize number:',
                                     min = 0,max = 50,step = 1,value = 13),
                         radioButtons('show_border',label = tags$strong('show border:'),
                                      choices = c('TRUE'='TRUE','FALSE'='FALSE'),
                                      selected = 'TRUE',inline = T),
                         box(colourInput("border_color_select", "Select border colour:", "black"),width = 150),
                         fluidRow(column(6,
                                         numericInput('cellwidth','cellwidth:',
                                                      min = 0,max = 250,step = 1,value = NA)),
                                  column(6,
                                         numericInput('cellheight','cellheight:',
                                                      min = 0,max = 250,step = 1,value = NA))),
                         fileInput('annotation_col',
                                   h4('Upload column annotation data:'),
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         fileInput('annotation_row',
                                   h4('Upload row annotation data:'),
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         
               ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ################################################################################
    # heatmap data table
    heat_data <- reactive({
        infile <- input$heatmap_data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            heat_da <- read.csv(infile,header = T,sep = ',',row.names = 1) else
                heat_da <- read.csv(infile,header = T,sep = '\t',row.names = 1)
    })
    
    output$heatmap_table <- renderDataTable({
        datatable(heat_data())
    })
    
    # column annotation table
    column_anno_data <- reactive({
        infile <- input$annotation_col$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            anno_da <- read.csv(infile,header = T,sep = ',',row.names = 1) else
                anno_da <- read.csv(infile,header = T,sep = '\t',row.names = 1)
    })
    
    output$annotation_col_table <- renderDataTable({
        datatable(column_anno_data())
    })
    
    # row annotation table
    row_anno_data <- reactive({
        infile <- input$annotation_row$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            anno_da <- read.csv(infile,header = T,sep = ',',row.names = 1) else
                anno_da <- read.csv(infile,header = T,sep = '\t',row.names = 1)
    })
    
    output$annotation_row_table <- renderDataTable({
        datatable(row_anno_data())
    })
    ################################################################################
    # heatmap plot
    heatdat <- eventReactive(input$heat_submit,{
        infile <- input$heatmap_data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            heat_da <- read.csv(infile,header = T,sep = ',',row.names = 1) else
                heat_da <- read.csv(infile,header = T,sep = '\t',row.names = 1)
        
        # print(input$scale)
        # print(input$cluster_rows)
        
        # show border color
        if(input$show_border=='FALSE'){
            border_col= NA
        }else{
            border_col=input$border_color_select
        }
        
        # split colors
        heatmap_colo <- unlist(strsplit(input$heatmap_col,split = '_'))
        
        if(input$heat_color_choose == 'palette'){
            heatcolor <-  colorRampPalette(c(heatmap_colo[2], heatmap_colo[3], heatmap_colo[1]))(50)
        }else{
            heatcolor <-  colorRampPalette(c(input$col_3, input$col_2, input$col_1))(50)
        }
        
        # choose gaps_row and gaps_col
        gap_row <- as.numeric(unlist(strsplit(input$gaps_row,split = ',')))
        gap_col <- as.numeric(unlist(strsplit(input$gaps_col,split = ',')))
        
        
        if(input$cluster_rows=='FALSE'){
            gap_r = gap_row
        }else{
            gap_r = NA
        }
        
        if(input$cluster_cols=='FALSE'){
            gap_c = gap_col
        }else{
            gap_c = NA
        }
        
        # ann colors choose
        # print(input$choose_annocol)
        
        if(input$choose_annocol==TRUE){
            eval(parse(text = input$script_anno_col))
        }else{
            anno_color = NA
        }
        # plot now
        # pheatmap(heat_da)
        heatplot<-  pheatmap(heat_da,
                             main = input$main ,
                             scale = input$scale,
                             cluster_rows = as.logical(input$cluster_rows),
                             cluster_cols = as.logical(input$cluster_cols),
                             clustering_method = input$clustering_method,
                             treeheight_row = input$treeheight_row,
                             treeheight_col = input$treeheight_col,
                             clustering_distance_rows = input$clustering_distance_rows,
                             clustering_distance_cols = input$clustering_distance_cols,
                             show_rownames = as.logical(input$show_rownames),
                             show_colnames = as.logical(input$show_colnames),
                             fontsize = input$fontsize,
                             angle_col = as.numeric(input$angle_col),
                             display_numbers = as.logical(input$display_numbers),
                             border_color = border_col,
                             cellwidth = input$cellwidth,
                             cellheight = input$cellheight,
                             color = heatcolor,
                             cutree_rows = as.numeric(input$cutree_rows),
                             cutree_cols = as.numeric(input$cutree_cols),
                             gaps_row = gap_r,
                             gaps_col = gap_c,
                             fontsize_number = as.numeric(input$fontsize_number),
                             annotation_col = column_anno_data(),
                             annotation_row = row_anno_data(),
                             legend = as.logical(input$legend),
                             annotation_legend = as.logical(input$annotation_legend),
                             annotation_colors = anno_color,
                             annotation_names_row = input$annotation_names_row,
                             annotation_names_col = input$annotation_names_col,
                             fontsize_row = input$fontsize_row,
                             fontsize_col = input$fontsize_col,
                             number_color = input$number_color,
        )
    #     
        heatplot <- as.ggplot(heatplot)
        heatplot
    #     # ggsave('heatmap.pdf',
    #     #        height = input$heatmap_height,
    #     #        width = input$heatmap_width,
    #     #        units = 'cm') 
    })

    output$heatmap_plot <- renderPlot({
        heatdat()
    })
    
    
    # save plot
    output$downloadata <- downloadHandler(
        filename = function() {
            paste('heatmap_plot', sep = '.', switch(
                input$format, PDF = 'pdf'))
        },
        
        content = function(file) {
            ggsave(file,
                   height = input$heatmap_height,
                   width = input$heatmap_width,
                   limitsize = FALSE,
                   units = 'cm',
            )})
}

# Run the application 
shinyApp(ui = ui, server = server)
