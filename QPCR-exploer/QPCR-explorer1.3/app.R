library(shiny)
library(DT)
library(Rmisc)
library(ggplot2)
library(ggsci)
library(ggprism)
library(colourpicker)
library(ggsignif)
library(ggpubr)
library(stringr)
library(gg.gap)
library(shinythemes)

ui <- navbarPage(
    theme = shinytheme('united'),
    title = tags$strong('ZhouLab QPCR analysis'),
    # tabPanel(
    #     'Info',icon = icon('align-justify'),
    # ),
    
    tabPanel(
        'QPCR test',icon = icon('chart-line'),
        sidebarLayout(
            sidebarPanel(
                fileInput('data',
                          h4('Upload overlap data'),
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                fluidRow(column(6,
                                textInput('ref_gene','Refence gene',value = 'Actin',placeholder = 'Actin/Gapdh')),
                         column(6,textAreaInput('contrl_group','Control group',value = 'D-2'))
                         ),
                
                fluidRow(column(8,
                                textInput('gene_number','Target gene number without ref gene',value = 'ref1 <- rbind(ref,ref)')),
                         column(4,
                                radioButtons('control_in','Control in Group ?',choices = c(TRUE,FALSE),selected = FALSE,inline = T))),
                fluidRow(column(6,
                                selectInput('x','X axis',choices = c('gene'= 'Target.Name',
                                                                     'group'= 'Sample.Name'),
                                            selected = 'Target.Name',selectize = T)),
                         column(6,
                                selectInput('fill','Fill',choices = c('gene'= 'Target.Name',
                                                                     'group'= 'Sample.Name'),
                                            selected = 'Sample.Name',selectize = T))),
                fluidRow(column(6,
                                radioButtons('colset','Choose color yourself ?',choices = c(TRUE,FALSE),selected = FALSE,inline = T)
                                ),
                                
                         column(6,selectInput('paletee','column color sets',
                            choices = c( 'aaas' ,'lancet' ,'d3', 'brewer', 'discrete' ,'futurama',
                                         'grey' ,'igv' ,'jama' ,'jco' ,'locuszoom' ,'nejm' ,
                                         'npg' ,'ordinal','simpsons' ,'startrek' ,'tron' ,'uchicago' ,'ucscgb'),
                            selected = 'lancet'))),
                
                fluidRow(column(6,textInput('color_my','my color(colors linked with "_")',value = NA)),
                         column(6,colourInput('color_so','Select color name',value = '#F53431'))),
               
                fluidRow(column(6,
                         sliderInput('base_size','theme text size',min = 0,max = 20,value = 16)),
                         column(6,
                         sliderInput('ylab_size','Y title size',min = 0,max = 25,value = 18))),
                
                fluidRow(column(6,
                                numericInput('y_range','Y range',min = 0,max = 10000,value = 10)),
                         column(6,
                                numericInput('minor_breaks','Y breaks',min = 0,max = 1000,value = 2)),
                         ),
                fluidRow(column(6,selectInput('yaxis_style','Y Axis style',
                                              choices = c('prism_minor', 'prism_offset',
                                                          'prism_offset_minor','normal'),selected = 'normal')),
                         column(6,selectInput('xaxis_style','X Axis style',
                                              choices = c('prism_bracket', 'prism_bracket(outside = FALSE)',
                                                          'normal'),
                                              selected = 'normal'))),
                
                textInput('ylab','Y axis label',value = 'mRNA relative expression'),
                hr(style="border-color: #194350"),
                
                fluidRow(column(6,
                                actionButton('submit','submit',icon = icon('frog'))),
                         
                         ),
                hr(style="border-color: #194350"),
                fluidRow(column(4,numericInput('height',
                                      'Height',min = 1,max = 100,step = 1,value = 20)),
                column(4,numericInput('width',
                                      'Width',min = 1,max = 100,step = 1,value = 20))),
                downloadButton('download_plot','Download plot here')
            ),
            
                
        
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        'plot',icon = icon('chart-bar'),
                        h4('Bar Plot shows here'),
                        hr(style="border-color: #8ab6d6"),
                        
                        
                        fluidRow(column(8,
                                        plotOutput('barplot',width = 600,height = 500)),
                                 
                                 column(4,
                                        wellPanel(
                                            # textInput('color_my','my color(colors linked with "_")',value = NA)scales
                                            fluidRow(column(6,selectInput('facet','Facet by',choices = c('gene'= 'Target.Name',
                                                                                 'group'= 'Sample.Name',
                                                                                 'none'='none'),
                                                        selected = 'none',selectize = T)),
                                                     column(6,radioButtons('scales','Scales',choices = c(TRUE,FALSE),selected = TRUE,inline = T))
                                        ),
                                        
                                        fluidRow(column(6,numericInput('nrow','Nrow',min = 0,max = 10,value = 2,step = 1)),
                                                 column(6,numericInput('ncol','Ncol',min = 0,max = 10,value = 2,step = 1))
                                        )))),
                                 
                        hr(style="border-color: #8ab6d6"),
                        wellPanel(
                            fluidRow(
                            column(4,numericInput('angle','X axis text angel',min = 0,max = 360,value = 0,step = 1)),
                            # column(4,numericInput('hjust','X axis text hjust',min = 0,max = 20,value = 0,step = 0.1)),
                            # column(4,numericInput('vjust','X axis text vjust',min = 0,max = 20,value = 0,step = 0.1)),
                            ),
                            
                            fluidRow(
                                column(4,sliderInput('colunm_width','Colunm width',min = 0,max = 2,value = 0.9,step = 0.1)),
                                column(4,sliderInput('errorbar_width','Errorbar width',min = 0,max = 1,value = 0.3,step = 0.1)),
                                column(4,sliderInput('errorbar_size','Errorbar size',min = 0,max = 5,value = 1,step = 0.1)),
                            ),
                            fluidRow(
                                column(4,sliderInput('errorbar_position','Errorbar position',min = 0,max = 2,value = 0.9,step = 0.1)),
                                column(3,numericInput('xpos','Legend xpos',min = 0,max = 1,value = NA,step = 0.1)),
                                column(3,numericInput('ypos','Legend ypos',min = 0,max = 1,value = NA,step = 0.1)),
                                column(2,selectInput('legend_position','Legend position',
                                                     choices = c('none','left','right','top','bottom'),
                                                     selected = 'right')),
                                ),
                            hr(style="border-color: #8ab6d6"),
                            h4('Y axis gap if expression is too high !'),
                            
                            fluidRow(
                                column(3,
                                       radioButtons('ygap','Gap the Y axis ?',choices = c(TRUE,FALSE),selected = FALSE,inline = T)),
                                column(3,
                                       textInput('segments','Segments',value = 'segments=list(c(0,0))')),
                                column(3,
                                       textInput('tick_width','Tick width',value = 'tick_width = c(1,1)')),
                                column(3,
                                       textInput('rel_heights','Rel heights',value = 'rel_heights = c(2,0,4)'))
                            ),
                            
                            
                            
                            # fluidRow(column(4,
                            #                 textInput('rel_heights','Rel heights',value = 'rel_heights = c(2,4)')),
                            #          column(4,numericInput('ylim','Y axis limitation',min = 0,max = 10000,value = NA)),
                            #          column(4,
                            #                 numericInput('upper_ticks','Upper ticks',min = 0,max = 1000,value = 2,step = 1)),
                            # ),
                            
                        )
                    ),
                    tabPanel(
                        'table',icon = icon('clone'),
                        fluidRow(column(6,
                                h4('The data selected from you raw data'),
                                hr(style="border-color: #f96d15"),
                                DT::dataTableOutput('selcted_table')),
                                column(6,
                                h4('This is dT data'),
                                hr(style="border-color: #f96d15"),
                                DT::dataTableOutput('qp_filter_table'),
                                hr(),
                                downloadButton('download_qp_filter_table','Download')
                                )),
                        hr(style="border-color: #325288"),
                        fluidRow(column(6,
                                        h4('Control data'),
                                        hr(style="border-color: #f96d15"),
                                        DT::dataTableOutput('control_table')),
                                 column(6,
                                        h4('Treat data'),
                                        hr(style="border-color: #f96d15"),
                                        DT::dataTableOutput('treat_table')))
                    ),
                    tabPanel(
                        'Results table',icon = icon('clone'),
                        
                        fluidRow(column(6,
                                        h4('Expression data'),
                                        hr(style="border-color: #f96d15"),
                                        DT::dataTableOutput('express_table'),
                                        hr(),
                                        downloadButton('download_express_table','Download')
                                        ),
                                 ),
                        h4('Sumarised data'),
                        hr(style="border-color: #f96d15"),
                        DT::dataTableOutput('sumary_table'),
                        hr(),
                        downloadButton('download_sumary_table','Download')
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    selected_tab <- reactive({
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            pcr <- read.csv(infile,header = T,sep = ',') else
                pcr <- read.csv(infile,header = T,sep = '\t')
        
        qp_dat <- pcr[,c("Sample.Name","Target.Name","CT")]
        qp_dat <- qp_dat[order(qp_dat$Sample.Name),]
        
    })
    
    output$selcted_table <- DT::renderDataTable({
        selected_tab()
    })
    # --------------------------------------------------------------------------
    
    qp_filter_tab <- reactive({
        infile <- input$data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            pcr <- read.csv(infile,header = T,sep = ',') else
                pcr <- read.csv(infile,header = T,sep = '\t')
        
        qp_dat <- pcr[,c("Sample.Name","Target.Name","CT")]
        qp_dat <- qp_dat[order(qp_dat$Sample.Name),]
        
        
        ref <- qp_dat[which(qp_dat$Target.Name== input$ref_gene),]
        
        # gene_number
        # ref1 <- rbind(ref,ref,ref,ref)
        eval(parse(text = input$gene_number))
        ref1 <- ref1[order(ref1$Sample.Name),]
        
        qp_filter <- qp_dat[which(qp_dat$Target.Name!= input$ref_gene),]
        mer <- cbind(qp_filter,ref1)
        
        colnames(mer)[6] <- 'Actin_CT'
        mer$dt <- mer$CT - mer$Actin_CT
        mer
    })
    
    output$qp_filter_table <- DT::renderDataTable({
        qp_filter_tab()
    })
    
    # --------------------------------------------------------------------------control_table treat_table
    output$control_table <- DT::renderDataTable({
        test <- qp_filter_tab()[,c(1,2,7)]
        
        # contrl_group
        con_group <- unlist(strsplit(input$contrl_group,split='\n'))
        control_gro <- c(con_group[1:length(con_group)])
        
        control<- test[which(test$Sample.Name %in% control_gro),]
        control
        
    })
    # --------------------------------------------------------------------------
    output$treat_table <- DT::renderDataTable({
        test <- qp_filter_tab()[,c(1,2,7)]
        
        # contrl_group
        con_group <- unlist(strsplit(input$contrl_group,split='\n'))
        control_gro <- c(con_group[1:length(con_group)])
        
        control<- test[which(test$Sample.Name %in% control_gro),]
        
        if(input$control_in == FALSE){
            treat <- test[-which(test$Sample.Name %in% control_gro),]
            treat <- treat[order(treat$Sample.Name),]
            treat
        }else{
            treat <- test
            treat <- treat[order(treat$Sample.Name),]
            treat
        }
        
        
    })
    
    # --------------------------------------------------------------------------express_table sumary_table
    output$express_table <- DT::renderDataTable({
        test <- qp_filter_tab()[,c(1,2,7)]
        
        # contrl_group
        con_group <- unlist(strsplit(input$contrl_group,split='\n'))
        control_gro <- c(con_group[1:length(con_group)])
        
        control<- test[which(test$Sample.Name %in% control_gro),]
        if(input$control_in == FALSE){
            treat <- test[-which(test$Sample.Name %in% control_gro),]
            treat <- treat[order(treat$Sample.Name),]
            treat
        }else{
            treat <- test
            treat <- treat[order(treat$Sample.Name),]
            treat
        }
        
        treat$ddt <- treat$dt-control$dt
        treat$FC <- 2^-treat$ddt
        treat_clean <- treat[,c("Sample.Name","Target.Name",'FC')]
        treat_clean
    })
    
    output$sumary_table <- DT::renderDataTable({
        test <- qp_filter_tab()[,c(1,2,7)]
        
        # contrl_group
        con_group <- unlist(strsplit(input$contrl_group,split='\n'))
        control_gro <- c(con_group[1:length(con_group)])
        
        control<- test[which(test$Sample.Name %in% control_gro),]
        if(input$control_in == FALSE){
            treat <- test[-which(test$Sample.Name %in% control_gro),]
            treat <- treat[order(treat$Sample.Name),]
            treat
        }else{
            treat <- test
            treat <- treat[order(treat$Sample.Name),]
            treat
        }
        
        treat$ddt <- treat$dt-control$dt
        treat$FC <- 2^-treat$ddt
        treat_clean <- treat[,c("Sample.Name","Target.Name",'FC')]
        
        tgc <- summarySE(treat_clean, measurevar="FC", groupvars=c("Sample.Name","Target.Name"))
        tgc
    })
    
    # --------------------------------------------------------------------------
    # plot
    bar <- eventReactive(input$submit,{
        test <- qp_filter_tab()[,c(1,2,7)]
        
        # contrl_group
        con_group <- unlist(strsplit(input$contrl_group,split='\n'))
        control_gro <- c(con_group[1:length(con_group)])
        
        control<- test[which(test$Sample.Name %in% control_gro),]
        if(input$control_in == FALSE){
            treat <- test[-which(test$Sample.Name %in% control_gro),]
            treat <- treat[order(treat$Sample.Name),]
            treat
        }else{
            treat <- test
            treat <- treat[order(treat$Sample.Name),]
            treat
        }
        
        treat$ddt <- treat$dt-control$dt
        treat$FC <- 2^-treat$ddt
        treat_clean <- treat[,c("Sample.Name","Target.Name",'FC')]
        
        tgc <- summarySE(treat_clean, measurevar="FC", groupvars=c("Sample.Name","Target.Name"))
        
        # base_size ylab_size ylab paletee colunm_width errorbar_width errorbar_size errorbar_position
        # aaas lancet d3 brewer discrete futurama grey igv jama jco  locuszoom nejm npg ordinal
        # simpsons startrek tron uchicago ucscgb
    {   if(input$paletee == 'aaas'){set_color = scale_fill_aaas()
        }else if(input$paletee == 'lancet'){set_color = scale_fill_lancet()
        }else if(input$paletee == 'd3'){set_color = scale_fill_d3()
        }else if(input$paletee == 'brewer'){set_color = scale_fill_brewer()
        }else if(input$paletee == 'discrete'){set_color = scale_fill_discrete()
        }else if(input$paletee == 'futurama'){set_color = scale_fill_futurama()
        }else if(input$paletee == 'grey'){set_color = scale_fill_grey()
        }else if(input$paletee == 'igv'){set_color = scale_fill_igv()
        }else if(input$paletee == 'jama'){set_color = scale_fill_jama()
        }else if(input$paletee == 'jco'){set_color = scale_fill_jco()
        }else if(input$paletee == 'locuszoom'){set_color = scale_fill_locuszoom()
        }else if(input$paletee == 'nejm'){set_color = scale_fill_nejm()
        }else if(input$paletee == 'npg'){set_color = scale_fill_npg()
        }else if(input$paletee == 'ordinal'){set_color = scale_fill_ordinal()
        }else if(input$paletee == 'simpsons'){set_color = scale_fill_simpsons()
        }else if(input$paletee == 'startrek'){set_color = scale_fill_startrek()
        }else if(input$paletee == 'tron'){set_color = scale_fill_tron()
        }else if(input$paletee == 'uchicago'){set_color = scale_fill_uchicago()
        }else if(input$paletee == 'ucscgb'){set_color = scale_fill_ucscgb()}
    }
        # xpos ypos legend_position
        if(is.na(input$xpos)){
            legendposition <- input$legend_position
        }else{
            legendposition <- c(input$xpos,input$ypos)
            }
        
      
        
          
   p <- ggplot(tgc, aes(x=get(input$x), y=FC, fill=get(input$fill))) +
            geom_col( position = position_dodge2(),width = input$colunm_width) +
            geom_errorbar(aes(ymin=FC-se, ymax=FC+se), width=input$errorbar_width,
                          position = position_dodge(input$errorbar_position),
                          size=input$errorbar_size) +
            theme_prism(base_size = input$base_size) +
            ylab(input$ylab) + xlab('') +
            theme(axis.title = element_text(size = input$ylab_size),
                  axis.text.x = element_text(angle = input$angle),
                  legend.background = element_blank(),
                  legend.position = legendposition,
                  )
       
   
   if(input$yaxis_style == 'normal'){
       p1 = p + scale_y_continuous(
                              limits = c(0, input$y_range),
                              expand = c(0.05,0),
                              breaks = seq(0, input$y_range, input$minor_breaks))
   }else{
       p1 = p + scale_y_continuous(guide = input$yaxis_style,
                          limits = c(0, input$y_range),
                          expand = c(0.05,0),
                          breaks = seq(0, input$y_range, input$minor_breaks))
   }
   
   # color choose
   if(input$colset == TRUE){
       # split color
       my_col <- unlist(strsplit(input$color_my,split = '_'))
       # length(my_col)
       col_values = c(my_col[1:length(my_col)])
       
       p2 = p1 + scale_fill_manual(values = col_values)
   }else{
       p2 = p1 + set_color
   }
   
   # x style choose
   if(input$xaxis_style=='prism_bracket(outside = FALSE)'){
       p3 = p2 + scale_x_discrete(guide = guide_prism_bracket(outside = FALSE))
   }else if(input$xaxis_style== 'prism_bracket'){
       p3 = p2 + scale_x_discrete(guide = "prism_bracket")
   }else{
       p3 = p2
   }


   # ----------------------------------------------------------scales nrow ncol
   if(input$facet == 'none'){
       p4 = p3
   }else if(input$facet == 'Sample.Name'){
       if(input$scales == TRUE){
           p4 = p3 + facet_wrap(~Sample.Name,nrow = input$nrow,ncol = input$ncol,scales= "free")
       }else{
           p4 = p3 + facet_wrap(~Sample.Name,nrow = input$nrow,ncol = input$ncol)
       }
       
   }else{
       if(input$scales == TRUE){
           p4 = p3 + facet_wrap(~Target.Name,nrow = input$nrow,ncol = input$ncol,scales= "free")
       }else{
           p4 = p3 + facet_wrap(~Target.Name,nrow = input$nrow,ncol = input$ncol)
       }
       
   }
   # ----------------------------------------------------------
   # gap loop ygap segments tick_width rel_heights ylim gapsubmit
   if(input$ygap == FALSE){
       p4
   }else{
       eval(parse(text = input$segments))
       eval(parse(text = input$tick_width))
       eval(parse(text = input$rel_heights))
       
       gg.gap(plot=p4,
              segments= segments,
              tick_width = tick_width,
              rel_heights= rel_heights,
              ylim=c(0,input$y_range))
   }
   
           
    })
    
    # -------------------------------------------------------------------------output plot
    output$barplot <- renderPlot({
        bar()
    })
    
    # -------------------------------------------------------------------------download plot
    output$download_plot <- downloadHandler(
        filename = function() {
            paste('barplot','pdf', sep = '.')
        },
        
        content = function(file) {
            ggsave(file,
                   height = input$height,
                   width = input$width,
                   limitsize = FALSE,
                   units = 'cm',
            )})
    
    # --------------------------------------------------------------------------download table
    # download_qp_filter_table download_express_table download_sumary_table
    output$download_qp_filter_table <- downloadHandler(
        filename = function() {
            paste('DT_data','csv', sep = '.')
        },
        
        content = function(file) {
            write.csv(qp_filter_tab(),file,row.names = F)
            })
    # --------------------------------------------------
    output$download_express_table <- downloadHandler(
        filename = function() {
            paste('Expression_data','csv', sep = '.')
        },
        
        content = function(file) {
            test <- qp_filter_tab()[,c(1,2,7)]
            
            # contrl_group
            con_group <- unlist(strsplit(input$contrl_group,split='\n'))
            control_gro <- c(con_group[1:length(con_group)])
            
            control<- test[which(test$Sample.Name %in% control_gro),]
            treat <- test[-which(test$Sample.Name %in% control_gro),]
            treat <- treat[order(treat$Sample.Name),]
            
            treat$ddt <- treat$dt-control$dt
            treat$FC <- 2^-treat$ddt
            treat_clean <- treat[,c("Sample.Name","Target.Name",'FC')]
            
            write.csv(treat_clean,file,row.names = F)
        })
    # --------------------------------------------------
    output$download_sumary_table <- downloadHandler(
        filename = function() {
            paste('Summaryied_data','csv', sep = '.')
        },
        
        content = function(file) {
            test <- qp_filter_tab()[,c(1,2,7)]
            
            # contrl_group
            con_group <- unlist(strsplit(input$contrl_group,split='\n'))
            control_gro <- c(con_group[1:length(con_group)])
            
            control<- test[which(test$Sample.Name %in% control_gro),]
            treat <- test[-which(test$Sample.Name %in% control_gro),]
            treat <- treat[order(treat$Sample.Name),]
            
            treat$ddt <- treat$dt-control$dt
            treat$FC <- 2^-treat$ddt
            treat_clean <- treat[,c("Sample.Name","Target.Name",'FC')]
            
            tgc <- summarySE(treat_clean, measurevar="FC", groupvars=c("Sample.Name","Target.Name"))
            write.csv(tgc,file,row.names = F)
            })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
