options(connectionObserver = NULL)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(shinydashboard)
library(shiny)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(clusterProfiler)
library(stringr)
library(enrichplot)
library(ggplot2)
library(ggprism)
library(colourpicker)
# 

header <- dashboardHeader(title = 'Enrichment Analysis tool!')

sidebar <- dashboardSidebar(
    sidebarMenu(
        h4('1.GO Enrichment'),
        menuItem('Upload Genelist',tabName = 'upload_genelist',icon = icon('dog')),
        menuItem('GO Enrichment',tabName = 'go_analysis',icon = icon('braille')),
        menuItem('KEGG Enrichment',tabName = 'kegg_analysis',icon = icon('sort-amount-up')),
        hr(),
        h4('2.GSEA Enrichment'),
        menuItem('Upload FC Genelist',tabName = 'upload_fc_genelist',icon = icon('piggy-bank')),
        menuItem('GSEA GO Enrich',tabName = 'gsea_go',icon = icon('cat')),
        menuItem('GSEA KEGG Enrich',tabName = 'gsea_kegg',icon = icon('earlybirds')),
        hr(),
        h4('3.Personalized Plot'),
        menuItem('Barplot',tabName = 'barplot',icon = icon('chart-bar')),
        menuItem('Dotplot',tabName = 'dotplot',icon = icon('paw'))
    )
)

body <- dashboardBody(
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))
    tabItems(
        {tabItem(tabName = 'upload_genelist',
                fluidRow(
                    box(title = 'Load data',solidHeader = TRUE,collapsible = TRUE,status="primary",
                       fileInput('genelist',
                                 h3('choose your genelist here:'),
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                    
                    pickerInput(
                        inputId = "anno_select",
                        label = "Choose species annotation:",
                        choices = c("Human"="org.Hs.eg.db", "Mouse"="org.Mm.eg.db","Rat"="org.Rn.eg.db",
                                    "Fly"="org.Dm.eg.db", "Yeast"="org.Sc.sgd.db","Arabidopsis"="org.At.tair.db",
                                    "Zebrafish"="org.Dr.eg.db", "Worm"="org.Ce.eg.db","Bovine"="org.Bt.eg.db",
                                    "Rhesus"="org.Mmu.eg.db", "Chicken"="org.Gg.eg.db","Pig"="org.Ss.eg.db",
                                    "Canine"="org.Cf.eg.db", "E coli strain K12"="org.EcK12.eg.db","Xenopus"="org.Xl.eg.db",
                                    "Anopheles"="org.Ag.eg.db", "Chimp"="org.Pt.eg.db","Malaria"="org.Pf.plasmo.db",
                                    "E coli strain Sakai"="org.EcSakai.eg.db", "Myxococcus xanthus DK 1622"="org.Mxanthus.db"),
                        options = list(
                            style = "btn-danger"),
                        selected = "org.Hs.eg.db")),
                    box(title = 'Transformed table',solidHeader = TRUE,collapsible = TRUE,status="primary",
                        DT::dataTableOutput('gene_trans'))),
                fluidRow(box(title = 'data table',solidHeader = TRUE,collapsible = TRUE,status="success",
                    DT::dataTableOutput('genedata')),
                    
                    ))},
        {tabItem(tabName = 'go_analysis',
                
                fluidRow(box(title = 'GO enrichment table',solidHeader = TRUE,collapsible = TRUE,status="success",width = 12,
                    
                    dropdown(
                        tags$h3("List of Inputs"),
                        
                        pickerInput(
                            inputId = "enrich_type",
                            label = "Enrich type",
                            choices = c("ALL CHOOSE"="ALL","Biological Process" = "BP", "Molecular Function"="MF", "Cell Component"="CC"),
                            options = list(style = "btn-primary"),
                            selected = "BP"),
                        numericInput('g_pcut','Pvalue cutoff',
                                     min = 0,max = 1,value = 0.05,step = 0.01),
                        numericInput('g_qcut','Qvalue cutoff',
                                     min = 0,max = 1,value = 0.2,step = 0.01),
                        actionButton('go_submit',
                                     'submit',icon = icon('arrow-circle-right')),
                        downloadButton('go_results_download','GO results download',icon = icon('file-download')),
                        hr(),
                        h5('Simplify output from enrichGO and gseGO by removing redundancy of enriched GO terms.'),
                        downloadButton('go_results_simplyfy_download','GO simplify results download'),
                        hr(),
                        switchInput(
                            inputId = "gplot_button",
                            label = "Plot ?", 
                            labelWidth = "80px",
                            value = FALSE,
                        ),
                        
                        style = "unite", icon = icon("gear"),
                        status = "danger", width = "300px",
                        animate = animateOptions(
                            enter = animations$fading_entrances$fadeInLeftBig,
                            exit = animations$fading_exits$fadeOutLeftBig),
                        
                        tooltip = tooltipOptions(title = "Click to see inputs !")),
                    
                    h5('Wait for a minute:'),
                    hr(),
                    withSpinner(type = 5,size = 0.5,
                                DT::dataTableOutput('godata'))),
                box(title = 'barplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                    plotOutput('go_barplot')),
                
                box(title = 'dotplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                    plotOutput('go_dotplot')),
                
                box(title = 'emapplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                    plotOutput('go_emap_plot')),
                
                box(title = 'cnetplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                    plotOutput('go_cnet_plot')),
        ))},
        {tabItem(tabName = 'kegg_analysis',
                
                fluidRow(box(title = 'KEGG enrichment table',solidHeader = TRUE,collapsible = TRUE,status="success",width = 12,
                             
                             dropdown(
                                 
                                 numericInput('k_pcut','Pvalue cutoff',
                                              min = 0,max = 1,value = 0.05,step = 0.01),
                                 numericInput('k_qcut','Qvalue cutoff',
                                              min = 0,max = 1,value = 0.2,step = 0.01),
                                 selectInput('k_species','KEGG species:',
                                             choices = c('Homo sapiens (human)'='hsa','	Mus musculus (mouse)'='mmu','Rattus norvegicus (rat)'='rno'),
                                             selected = 'hsa'),
                                 actionButton('kegg_submit',
                                              'submit',icon = icon('arrow-circle-right')),
                                 hr(),
                                 downloadButton('kegg_results_download','KEGG results download',icon = icon('file-download')),
                                 hr(),
                                 switchInput(
                                     inputId = "kplot_button",
                                     label = "Plot ?", 
                                     labelWidth = "80px",
                                     value = FALSE,
                                 ),
                                 
                                 style = "unite", icon = icon("gear"),
                                 status = "danger", width = "300px",
                                 animate = animateOptions(
                                     enter = animations$fading_entrances$fadeInLeftBig,
                                     exit = animations$fading_exits$fadeOutLeftBig),
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")),
                             
                             h5('Wait for a minute:'),
                             hr(),
                             withSpinner(type = 5,size = 0.5,
                                         DT::dataTableOutput('keggdata'))),
                         box(title = 'barplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                             plotOutput('kegg_barplot')),
                         
                         box(title = 'dotplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                             plotOutput('kegg_dotplot')),
                ))},
        {tabItem(tabName = 'upload_fc_genelist',
                fluidRow(
                    box(title = 'Load FC data',solidHeader = TRUE,collapsible = TRUE,status="success",
                        fileInput('fc_genelist',
                                  h3('choose your FC genelist here:'),
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        selectInput("fc_anno_select","Choose species annotation:",
                            choices = c("Human"="org.Hs.eg.db", "Mouse"="org.Mm.eg.db","Rat"="org.Rn.eg.db",
                                        "Fly"="org.Dm.eg.db", "Yeast"="org.Sc.sgd.db","Arabidopsis"="org.At.tair.db",
                                        "Zebrafish"="org.Dr.eg.db", "Worm"="org.Ce.eg.db","Bovine"="org.Bt.eg.db",
                                        "Rhesus"="org.Mmu.eg.db", "Chicken"="org.Gg.eg.db","Pig"="org.Ss.eg.db",
                                        "Canine"="org.Cf.eg.db", "E coli strain K12"="org.EcK12.eg.db","Xenopus"="org.Xl.eg.db",
                                        "Anopheles"="org.Ag.eg.db", "Chimp"="org.Pt.eg.db","Malaria"="org.Pf.plasmo.db",
                                        "E coli strain Sakai"="org.EcSakai.eg.db", "Myxococcus xanthus DK 1622"="org.Mxanthus.db"),
                            selected = "org.Hs.eg.db")),
                    box(title = 'FC data table',solidHeader = TRUE,collapsible = TRUE,status="success",
                        DT::dataTableOutput('fc_genedata'))),
                fluidRow(
                    box(title = 'Transformed table',solidHeader = TRUE,collapsible = TRUE,status="primary",
                        DT::dataTableOutput('fc_gene_trans')),
                    box(title = 'merge data table',solidHeader = TRUE,collapsible = TRUE,status="primary",
                        DT::dataTableOutput('gene_merge_data')),
                ))},
        {tabItem(tabName = 'gsea_go',
                fluidRow(box(title = 'GSEA GO enrichment table',solidHeader = TRUE,collapsible = TRUE,status="success",width = 12,
                             
                             dropdown(
                                
                                 pickerInput(
                                     inputId = "gsea_enrich_type",
                                     label = "Enrich type",
                                     choices = c("ALL CHOOSE"="ALL","Biological Process" = "BP", "Molecular Function"="MF", "Cell Component"="CC"),
                                     options = list(style = "btn-primary"),
                                     selected = "BP"),
                                 numericInput('gg_pcut','Pvalue cutoff',
                                              min = 0,max = 1,value = 1,step = 0.01),
                                 numericInput('minGSSize ','minGSSize',
                                              min = 0,max = 10000,value = 10,step = 1),
                                 numericInput('maxGSSize','maxGSSize',
                                              min = 0,max = 10000,value = 500,step = 1),
                                 actionButton('ggo_submit',
                                              'submit',icon = icon('arrow-circle-right')),
                                 hr(),
                                 downloadButton('ggo_results_download','GSEA GO results download',icon = icon('file-download')),

                                 style = "unite", icon = icon("gear"),
                                 status = "danger", width = "300px",
                                 animate = animateOptions(
                                     enter = animations$fading_entrances$fadeInLeftBig,
                                     exit = animations$fading_exits$fadeOutLeftBig),
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")),
                             
                             h5('Wait for a minute:'),
                             hr(),
                             withSpinner(type = 5,size = 0.5,
                                         DT::dataTableOutput('ggodata'))),
                         box(title = 'gseaplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                             
                             fluidRow(column(6,dropdownButton(
                                 
                                 fluidRow(column(6,textInput('geneSetID','geneSetID',value = NA,placeholder = 'GO:0034470/hsa04510'),
                                 selectInput('by','plot type',
                                             choices = c('runningScore','all','preranked'),selected = 'all'),
                                 
                                 actionButton('gsea_submit','submit',icon = icon('arrow-circle-right')),
                                 hr(),
                                 numericInput('theme_base','theme_base',
                                              min = 1,max = 25,value = NA)),
                                 
                                 column(6,colourInput('color','Choose color'),
                                        textInput('color1','ranked line color',value = 'black'),
                                        textInput('color_line','middle line color',value = 'firebrick'),
                                        textInput('color_vline','sumit line color',value = '#FA5860'))),
                                 
                                 fluidRow(column(6,numericInput('height1',
                                                                'Height',min = 1,max = 100,step = 1,value = 15)),
                                          column(6,numericInput('width1',
                                                                'Width',min = 1,max = 100,step = 1,value = 22))),
                                 
                                 circle = TRUE, status = "danger",
                                 icon = icon("gear"), width = "350px",
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")),
                                 hr(),
                                 ),
                             column(6,downloadButton('down_plot1',
                                                     'Download plot here'),align='right')),
                             plotOutput('gseaplot')),

                         box(title = 'gseaplot2',solidHeader = TRUE,collapsible = TRUE,status="warning",
                             
                             fluidRow(column(6,dropdownButton(
                                 
                                 textInput('geneSetID2','geneSetID(multiple id linked with"_")',value = NA,placeholder = 'GO:0034470/hsa04510'),
                                                 
                                                 awesomeCheckboxGroup(inputId = "by2",label = "plot type", 
                                                     choices = c("first"=1, "second"=2, "third"=3),
                                                     selected = c(1,2,3),
                                                     inline = TRUE, 
                                                     status = "success"),
                                                 fluidRow(column(6,textInput('color_line2','line color(multiple color linked with"_")',value = 'firebrick')),
                                                          column(6,h5(tags$strong('show pvalue')),
                                                                 awesomeCheckbox(inputId = "pvalue_table",label = "pvalue table", 
                                                              value = FALSE))),
                                                 numericInput('theme_base2','theme_base',
                                                              min = 1,max = 25,value = 16),
                                                 actionButton('gsea2_submit','submit',icon = icon('arrow-circle-right')),
                                                 
                                 
                                 fluidRow(column(6,numericInput('height2',
                                                                'Height',min = 1,max = 100,step = 1,value = 15)),
                                          column(6,numericInput('width2',
                                                                'Width',min = 1,max = 100,step = 1,value = 22))),
                                 
                                 circle = TRUE, status = "danger",
                                 icon = icon("gear"), width = "350px",
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")),
                                 hr(),
                             ),
                             column(6,downloadButton('down_plot2',
                                                     'Download plot here'),align='right')),
                             plotOutput('gseaplot2')),
                         
       
                ))},
        {tabItem(tabName = 'gsea_kegg',
                fluidRow(box(title = 'GSEA KEGG enrichment table',solidHeader = TRUE,collapsible = TRUE,status="success",width = 12,
                             
                             dropdown(
                                 
                                 selectInput('gsea_k_species','KEGG species:',
                                             choices = c('Homo sapiens (human)'='hsa','	Mus musculus (mouse)'='mmu','Rattus norvegicus (rat)'='rno'),
                                             selected = 'hsa'),
                                 numericInput('gk_pcut','Pvalue cutoff',
                                              min = 0,max = 1,value = 1,step = 0.01),
                                 numericInput('minGSSize2 ','minGSSize',
                                              min = 0,max = 10000,value = 10,step = 1),
                                 numericInput('maxGSSize2','maxGSSize',
                                              min = 0,max = 10000,value = 500,step = 1),
                                 actionButton('gke_submit',
                                              'submit',icon = icon('arrow-circle-right')),
                                 hr(),
                                 downloadButton('gke_results_download','GSEA KEGG results download',icon = icon('file-download')),
                                 
                                 style = "unite", icon = icon("gear"),
                                 status = "danger", width = "300px",
                                 animate = animateOptions(
                                     enter = animations$fading_entrances$fadeInLeftBig,
                                     exit = animations$fading_exits$fadeOutLeftBig),
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")),
                             
                             h5('Wait for a minute:'),
                             hr(),
                             withSpinner(type = 5,size = 0.5,
                                         DT::dataTableOutput('gkedata'))),
                         box(title = 'gseaplot',solidHeader = TRUE,collapsible = TRUE,status="warning",
                             
                             fluidRow(column(6,dropdownButton(
                                 
                                 fluidRow(column(6,textInput('k_geneSetID','geneSetID',value = NA,placeholder = 'GO:0034470/hsa04510'),
                                                 selectInput('k_by','plot type',
                                                             choices = c('runningScore','all','preranked'),selected = 'all'),
                                                 
                                                 actionButton('k_gsea_submit','submit',icon = icon('arrow-circle-right')),
                                                 hr(),
                                                 numericInput('k_theme_base','theme_base',
                                                              min = 1,max = 25,value = NA)),
                                          
                                          column(6,colourInput('k_color','Choose color'),
                                                 textInput('k_color1','ranked line color',value = 'black'),
                                                 textInput('k_color_line','middle line color',value = 'firebrick'),
                                                 textInput('k_color_vline','sumit line color',value = '#FA5860'))),
                                 
                                 fluidRow(column(6,numericInput('k_height1',
                                                                'Height',min = 1,max = 100,step = 1,value = 15)),
                                          column(6,numericInput('k_width1',
                                                                'Width',min = 1,max = 100,step = 1,value = 22))),
                                 
                                 circle = TRUE, status = "danger",
                                 icon = icon("gear"), width = "350px",
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")),
                                 hr(),
                             ),
                             column(6,downloadButton('k_down_plot1',
                                                     'Download plot here'),align='right')),
                             plotOutput('k_gseaplot')),
                         
                         box(title = 'gseaplot2',solidHeader = TRUE,collapsible = TRUE,status="warning",
                             
                             fluidRow(column(6,dropdownButton(
                                 
                                 textInput('k_geneSetID2','geneSetID(multiple id linked with"_")',value = NA,placeholder = 'GO:0034470/hsa04510'),
                                 
                                 awesomeCheckboxGroup(inputId = "k_by2",label = "plot type", 
                                                      choices = c("first"=1, "second"=2, "third"=3),
                                                      selected = c(1,2,3),
                                                      inline = TRUE, 
                                                      status = "success"),
                                 fluidRow(column(6,textInput('k_color_line2','line color(multiple color linked with"_")',value = 'firebrick')),
                                          column(6,h5(tags$strong('show pvalue')),
                                                 awesomeCheckbox(inputId = "k_pvalue_table",label = "pvalue table", 
                                                                 value = FALSE))),
                                 numericInput('k_theme_base2','theme_base',
                                              min = 1,max = 25,value = 16),
                                 actionButton('k_gsea2_submit','submit',icon = icon('arrow-circle-right')),
                                 
                                 
                                 fluidRow(column(6,numericInput('k_height2',
                                                                'Height',min = 1,max = 100,step = 1,value = 15)),
                                          column(6,numericInput('k_width2',
                                                                'Width',min = 1,max = 100,step = 1,value = 22))),
                                 
                                 circle = TRUE, status = "danger",
                                 icon = icon("gear"), width = "350px",
                                 
                                 tooltip = tooltipOptions(title = "Click to see inputs !")),
                                 hr(),
                             ),
                             column(6,downloadButton('k_down_plot2',
                                                     'Download plot here'),align='right')),
                             plotOutput('k_gseaplot2')),

                ))},
        {tabItem(tabName = 'barplot',
                fluidRow(column(4,
                                wellPanel(
                                    fileInput('go_data',
                                              h4('Upload GO data:'),
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    colourInput('bar_color','color name',value = '#FF82C7'),
                                    fluidRow(
                                        column(6,textInput('fill_col','fill color',value = 'grey')),
                                        column(6,textInput('bor_col','border color',value = 'black')),
                                    ),
                                    numericInput('theme_size','theme size',
                                                 min = 1,max = 25,value = 16),
                                    numericInput('line_size','theme line size',
                                                 min = 1,max = 10,value = 1),
                                    checkboxInput('border','show border',value = FALSE),
                                    selectInput('linetp','line type',choices = c('solid','dashed'),selected = 'solid'),
                                    textInput('title','plot title',value = 'GO Enrichment barplot'),
                                    fluidRow(
                                        column(6,numericInput('col_width','colunm width',
                                                              min = 0,max = 10,value = 0.85)),
                                        column(6,numericInput('fill_alpha','fill alpha',
                                                              min = 0,max = 1,value = 1,step = 0.1)),
                                    ),
                                    
                                    actionButton('bar_submit',
                                                 'submit',icon = icon('hand-point-up')),
                                    
                                    
                                    
                                    fluidRow(column(6,numericInput('b_height',
                                                                   'Height',min = 1,max = 100,step = 1,value = 15)),
                                             column(6,numericInput('b_width',
                                                                   'Width',min = 1,max = 100,step = 1,value = 22))),
                                    
                                    downloadButton('down_bar', 'Download plot here',icon = icon('cloud-download-alt'))
                                ),
                                ),
                         column(8,
                                box(title = 'barplot',solidHeader = TRUE,collapsible = TRUE,status="primary",width = 12,height = 650,
                                plotOutput('my_bar',height = 550)
                                )))
                )},
        tabItem(tabName = 'dotplot',
                fluidRow(column(4,
                                wellPanel(
                                    fileInput('kegg_data',
                                              h4('Upload KEGG data:'),
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    colourInput('kegg_color','color name',value = '#28E081'),
                                    fluidRow(
                                        column(6,textInput('low_col','low color',value = 'green')),
                                        column(6,textInput('high_col','high color',value = 'red')),
                                    ),
                                    numericInput('axis_text_sz','axis text size',
                                                 min = 1,max = 25,value = 14),
                                    numericInput('axis_title_sz','axis title size',
                                                 min = 1,max = 30,value = 16),
                                    sliderInput('size_range','point size range',min = 0,max = 25,value = c(2,10),step = 1),
                                    textInput('x_expand','x axis expand',value = '0.01_0.01'),
                                    
                                    textInput('grid_col','grid color',value = 'grey'),
                                    textInput('kegg_title','plot title',value = 'KEGG Enrichment dotplot'),
                                    
                                    
                                    actionButton('dot_submit',
                                                 'submit',icon = icon('hand-point-up')),
                                    
                                    
                                    
                                    fluidRow(column(6,numericInput('d_height',
                                                                   'Height',min = 1,max = 100,step = 1,value = 15)),
                                             column(6,numericInput('d_width',
                                                                   'Width',min = 1,max = 100,step = 1,value = 22))),
                                    
                                    downloadButton('down_dot', 'Download plot here',icon = icon('cloud-download-alt'))
                                ),
                ),
                column(8,
                       box(title = 'dotplot',solidHeader = TRUE,collapsible = TRUE,status="primary",width = 12,height = 650,
                           plotOutput('my_dot',height = 550)
                       )))
                )
    )
)

ui <- dashboardPage(skin = "green",header, sidebar, body)

server <- function(input, output) {

    # --------------------------------------------------------------------------------
    # upload gene list
    
    output$genedata <- DT::renderDataTable({
        infile <- input$genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        
    })
    
    # 
    # upload fc gene list
    
    output$fc_genedata <- DT::renderDataTable({
        infile <- input$fc_genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            fcres <- read.csv(infile,header = T,sep = ',') else
                fcres <- read.csv(infile,header = T,sep = '\t')
        colnames(fcres)[1] <- 'SYMBOL'
        fcres
        
    })
    
    # --------------------------------------------------------------------------------
    # upload gene transform
    output$gene_trans <- DT::renderDataTable({
        infile <- input$genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        res[,1] <- as.character(res[,1])
        trans_gene = bitr(res[,1], fromType="SYMBOL", toType=c( "ENTREZID"), OrgDb=input$anno_select)
        
    })
    
    # 
    # upload fc gene transform
    output$fc_gene_trans <- DT::renderDataTable({
        infile <- input$fc_genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            fcres <- read.csv(infile,header = T,sep = ',') else
                fcres <- read.csv(infile,header = T,sep = '\t')
        
        fcres[,1] <- as.character(fcres[,1])
        fc_trans_gene = bitr(fcres[,1], fromType="SYMBOL", toType=c( "ENTREZID"), OrgDb=input$fc_anno_select)
        
    })
    
    # -----------------------------------------------------------------------------------
    # merge data
    output$gene_merge_data <- DT::renderDataTable({
        infile <- input$fc_genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            fcres <- read.csv(infile,header = T,sep = ',') else
                fcres <- read.csv(infile,header = T,sep = '\t')
        
        colnames(fcres)[1] <- 'SYMBOL'
        
        fcres[,1] <- as.character(fcres[,1])
        
        fc_trans_gene = bitr(fcres[,1], fromType="SYMBOL", toType=c( "ENTREZID"), OrgDb=input$fc_anno_select)
        
        mer_dat <- merge(fcres,fc_trans_gene,by="SYMBOL",all=F)
        
        mer_dat <- mer_dat[order(mer_dat[,2],decreasing = T),]
    })
    # --------------------------------------------------------------------------------
    {# GO enrichment
    go_res <- eventReactive(input$go_submit,{
        
        infile <- input$genelist$datapath

        if (is.null(infile))
            return(NULL)

        d <- infile
        type <- str_sub(d,-3)

        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')

        res[,1] <- as.character(res[,1])
        trans_gene = bitr(res[,1], fromType="SYMBOL", toType=c( "ENTREZID"), OrgDb=input$anno_select)

        gene <- trans_gene$ENTREZID

        ego <- enrichGO(
            gene          = gene,
            keyType       = "ENTREZID",
            OrgDb         = input$anno_select,
            ont           = input$enrich_type,
            pAdjustMethod = "BH",
            pvalueCutoff  = as.numeric(input$g_pcut),
            qvalueCutoff  = as.numeric(input$g_qcut),
            readable      = TRUE)
       
       
    })
    
    output$godata <- DT::renderDataTable({
        go_data <- data.frame(go_res())
        datatable(go_data[,1:7])
    })
    # DONWLOAD GO results
    
    output$go_results_download <- downloadHandler(
        filename = function() {
            paste('GO_Enrichment_res', '.csv',sep = "")
        },
        
        content = function(file) {
            write.csv(data.frame(go_res()),file,row.names = FALSE)
        })
    
    # DONWLOAD GO SIMPLITY results
    go_sim_res <- eventReactive(input$go_submit,{
        
        simp_data<- simplify(go_res(),
                             cutoff = 0.7,
                             by = "p.adjust",
                             select_fun = min,
                             measure = "Wang")
    })
    
    output$go_results_simplyfy_download <- downloadHandler(
        filename = function() {
            paste('GO_Enrichment_simplify_res', '.csv',sep = "")
        },
        
        content = function(file) {
            write.csv(go_sim_res(),file,row.names = FALSE)
        })
    }
    ############################################################################
    {# KEGG ENRICHMENT
    kegg_res <- eventReactive(input$kegg_submit,{
        
        infile <- input$genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            res <- read.csv(infile,header = T,sep = ',') else
                res <- read.csv(infile,header = T,sep = '\t')
        
        res[,1] <- as.character(res[,1])
        trans_gene = bitr(res[,1], fromType="SYMBOL", toType=c( "ENTREZID"), OrgDb=input$anno_select)
        
        gene <- trans_gene$ENTREZID
        
        ekegg <- enrichKEGG(
            gene              = gene,
            keyType           = "kegg",
            organism          = input$k_species,
            pvalueCutoff      = as.numeric(input$k_pcut),
            pAdjustMethod     = "BH",
            qvalueCutoff      = as.numeric(input$k_qcut),
            )
        
        anno_ekegg<-setReadable(ekegg,OrgDb = input$anno_select,keyType = "ENTREZID")
        
        
    })
    
    output$keggdata <- DT::renderDataTable({
        kegg_data <- data.frame(kegg_res())
        datatable(kegg_data[,1:7])
    })
    # DONWLOAD kegg results
    
    output$kegg_results_download <- downloadHandler(
        filename = function() {
            paste('KEGG_Enrichment_res', '.csv',sep = "")
        },
        
        content = function(file) {
            write.csv(data.frame(kegg_res()),file,row.names = FALSE)
        })
    }
    # -------------------------------------------------------------------------------
    {# gsea go analysis
    ggo_res <- eventReactive(input$ggo_submit,{
        infile <- input$fc_genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            fcres <- read.csv(infile,header = T,sep = ',') else
                fcres <- read.csv(infile,header = T,sep = '\t')
        
        colnames(fcres)[1] <- 'SYMBOL'
        
        fcres[,1] <- as.character(fcres[,1])
        
        fc_trans_gene = bitr(fcres[,1], fromType="SYMBOL", toType=c( "ENTREZID"), OrgDb=input$fc_anno_select)
        
        mer_dat <- merge(fcres,fc_trans_gene,by="SYMBOL",all=F)
        
        mer_dat <- mer_dat[order(mer_dat[,2],decreasing = T),]
        
        mer_dat <- mer_dat[!duplicated(mer_dat$ENTREZID),]
        genefc <- mer_dat[,2]
        names(genefc) <- mer_dat$ENTREZID
        
        print(input$fc_anno_select)
        
    gse_go <- gseGO(
        geneList          = genefc,
        ont               = input$gsea_enrich_type,
        OrgDb             = get(input$fc_anno_select),
        keyType           = "ENTREZID",
        pvalueCutoff      = as.numeric(input$gg_pcut),
        pAdjustMethod     = "BH",
        minGSSize         = as.numeric(input$minGSSize),
        maxGSSize         = as.numeric(input$maxGSSize),
        seed              = T,
        )
    
    })
    
    output$ggodata <- DT::renderDataTable({
        gs_go <- data.frame(ggo_res())
        datatable(gs_go[,1:7])
    })
    
    # DONWLOAD gsea go results
    
    output$ggo_results_download <- downloadHandler(
        filename = function() {
            paste('GSEA_GO_Enrichment_res', '.csv',sep = "")
        },
        
        content = function(file) {
            anno_gso <-setReadable(ggo_res(),OrgDb = get(input$fc_anno_select),keyType = "ENTREZID")
            gs_go_dat <- data.frame(anno_gso)
            gs_go_dat<- gs_go_dat[which(gs_go_dat$pvalue<0.05),]
            
            write.csv(gs_go_dat,file,row.names = FALSE)
        })
    # ----------------------------------------------------------------------------
    # gsea kegg analysis
    gke_res <- eventReactive(input$gke_submit,{
        infile <- input$fc_genelist$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            fcres <- read.csv(infile,header = T,sep = ',') else
                fcres <- read.csv(infile,header = T,sep = '\t')
        
        colnames(fcres)[1] <- 'SYMBOL'
        
        fcres[,1] <- as.character(fcres[,1])
        
        fc_trans_gene = bitr(fcres[,1], fromType="SYMBOL", toType=c( "ENTREZID"), OrgDb=input$fc_anno_select)
        
        mer_dat <- merge(fcres,fc_trans_gene,by="SYMBOL",all=F)
        
        mer_dat <- mer_dat[order(mer_dat[,2],decreasing = T),]
        
        mer_dat <- mer_dat[!duplicated(mer_dat$ENTREZID),]
        genefc <- mer_dat[,2]
        names(genefc) <- mer_dat$ENTREZID
        

        gse_kegg <- gseKEGG(
            geneList          = genefc,
            organism          = input$gsea_k_species,
            keyType           = "kegg",
            pvalueCutoff      = as.numeric(input$gk_pcut),
            pAdjustMethod     = "BH",
            minGSSize         = as.numeric(input$minGSSize2),
            maxGSSize         = as.numeric(input$maxGSSize2),
            seed              = T,
        )
        
    })
    
    output$gkedata <- DT::renderDataTable({
        gs_ke <- data.frame(gke_res())
        datatable(gs_ke[,1:7])
    })
    
    # DONWLOAD gsea kegg results
    
    output$gke_results_download <- downloadHandler(
        filename = function() {
            paste('GSEA_KEGG_Enrichment_res', '.csv',sep = "")
        },
        
        content = function(file) {
            anno_gke <-setReadable(gke_res(),OrgDb = get(input$fc_anno_select),keyType = "ENTREZID")
            gs_ke_dat <- data.frame(anno_gke)
            gs_ke_dat<- gs_ke_dat[which(gs_ke_dat$pvalue<0.05),]
            
            write.csv(gs_ke_dat,file,row.names = FALSE)
        })
 }
    ############################################################################
    # plot
    
    {output$go_barplot <- renderPlot({
        if(input$gplot_button == 'FALSE'){
        }else{ barplot(go_res(),showCategory=10)}
    })
    
    output$go_dotplot <- renderPlot({
        if(input$gplot_button == 'FALSE'){
        }else{ dotplot(go_res(),showCategory=10)}
    })
    
    output$go_emap_plot <- renderPlot({
        if(input$gplot_button == 'FALSE'){
        }else{emapplot(pairwise_termsim(go_res()),showCategory=10)}
    })
    
    output$go_cnet_plot <- renderPlot({
        if(input$gplot_button == 'FALSE'){
        }else{cnetplot(go_res(),showCategory=3)}
    })}
    
    # ------------------------------------------kegg plot
    {output$kegg_barplot <- renderPlot({
        if(input$kplot_button == 'FALSE'){
            
        }else{barplot(kegg_res(),showCategory=10)}
        
    })
    
    output$kegg_dotplot <- renderPlot({
        if(input$kplot_button == 'FALSE'){
            
        }else{dotplot(kegg_res(),showCategory=10)}
        
    })}
    
    {# -----------------------------------------------------------gsea go plot
    gsea_p1 <- eventReactive(input$gsea_submit,{
        
        tmp <- data.frame(ggo_res())
        titl = tmp[input$geneSetID,'Description']
       p<- gseaplot(x = ggo_res(),
                 geneSetID = input$geneSetID,
                 by = input$by,
                 title = titl,
                 color = input$color1,
                 color.line = input$color_line,
                 color.vline = input$color_vline,
                 )
        
       if(is.na(input$theme_base)){
           p
       }else{p + theme_prism(base_size = as.numeric(input$theme_base))}
       
    })
    
        output$gseaplot <- renderPlot({
            gsea_p1()
        })
   # download plot down_plot1
        output$down_plot1 <- downloadHandler(
            filename = function() {
                paste('GSEA1_plot', 'pdf',sep = '.')},
            
            content = function(file) {
                ggsave(file,
                       height = input$height1,
                       width = input$width1,
                       limitsize = FALSE,
                       units = 'cm',
                )})

    ############################################################################
    # gsea2 plot
        gsea_p2 <- eventReactive(input$gsea2_submit,{
            
            tmp <- data.frame(ggo_res())
            
            if(length(input$by2)==3){
                subplots = c(1,2,3)
            }else if(length(input$by2)==2){
                subplots <- c(as.numeric(input$by2[1]),as.numeric(input$by2[2]))
                print(subplots)
            }else{
                subplots = as.numeric(input$by2)
            }

            # multiplot
            b=unlist(strsplit(input$geneSetID2,split = '_'))
            set <- c(b[1:length(b)])
            # multicolor set
            col=unlist(strsplit(input$color_line2,split = '_'))
            multicol <- c(col[1:length(col)])
            
            if(length(b)==1){
                titl = tmp[input$geneSetID2,'Description']
            }else{
                titl=''
            }
             gseaplot2(x = ggo_res(),
                         geneSetID = set,
                         title = titl,
                         subplots = subplots,
                         color = multicol,
                         base_size = input$theme_base2,
                         pvalue_table = input$pvalue_table,
                         ES_geom = "line",
            )
            
        })
        
        output$gseaplot2 <- renderPlot({
            gsea_p2()
        })
        # download plot down_plot1
        output$down_plot2 <- downloadHandler(
            filename = function() {
                paste('GSEA2_plot', 'pdf',sep = '.')},
            
            content = function(file) {
                ggsave(file,
                       height = input$height2,
                       width = input$width2,
                       limitsize = FALSE,
                       units = 'cm',
                )})
    }
        
    # -----------------------------------------------------------gsea kegg plot
    {gsea_p1_k <- eventReactive(input$k_gsea_submit,{
        
        tmp <- data.frame(gke_res())
        k_titl = tmp[input$k_geneSetID,'Description']
        p<- gseaplot(x = gke_res(),
                     geneSetID = input$k_geneSetID,
                     by = input$k_by,
                     title = k_titl,
                     color = input$k_color1,
                     color.line = input$k_color_line,
                     color.vline = input$k_color_vline,
        )
        
        if(is.na(input$k_theme_base)){
            p
        }else{p + theme_prism(base_size = as.numeric(input$k_theme_base))}
        
    })
    
    output$k_gseaplot <- renderPlot({
        gsea_p1_k()
    })
    # download plot down_plot1
    output$k_down_plot1 <- downloadHandler(
        filename = function() {
            paste('GSEA1_kegg_plot', 'pdf',sep = '.')},
        
        content = function(file) {
            ggsave(file,
                   height = input$k_height1,
                   width = input$k_width1,
                   limitsize = FALSE,
                   units = 'cm',
            )})
    
    ############################################################################
    # gsea2 plot
    gsea_p2_k <- eventReactive(input$k_gsea2_submit,{
        
        tmp <- data.frame(gke_res())
        
        if(length(input$k_by2)==3){
            subplots = c(1,2,3)
        }else if(length(input$k_by2)==2){
            subplots <- c(as.numeric(input$k_by2[1]),as.numeric(input$k_by2[2]))
            print(subplots)
        }else{
            subplots = as.numeric(input$k_by2)
        }
        
        # multiplot
        b=unlist(strsplit(input$k_geneSetID2,split = '_'))
        set <- c(b[1:length(b)])
        # multicolor set
        col=unlist(strsplit(input$k_color_line2,split = '_'))
        multicol <- c(col[1:length(col)])
        
        if(length(b)==1){
            titl = tmp[input$k_geneSetID2,'Description']
        }else{
            titl=''
        }
        gseaplot2(x = gke_res(),
                  geneSetID = set,
                  title = titl,
                  subplots = subplots,
                  color = multicol,
                  base_size = input$k_theme_base2,
                  pvalue_table = input$k_pvalue_table,
                  ES_geom = "line",
        )
        
    })
    
    output$k_gseaplot2 <- renderPlot({
        gsea_p2_k()
    })
    # download plot down_plot1
    output$k_down_plot2 <- downloadHandler(
        filename = function() {
            paste('GSEA2_kegg_plot', 'pdf',sep = '.')},
        
        content = function(file) {
            ggsave(file,
                   height = input$k_height2,
                   width = input$k_width2,
                   limitsize = FALSE,
                   units = 'cm',
            )})    
  }      
        
    # -----------------------------------------------------------my bar plot
    {my_barplot <- eventReactive(input$bar_submit,{
        infile <- input$go_data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            go <- read.csv(infile,header = T,sep = ',') else
                go <- read.csv(infile,header = T,sep = '\t')
        
        go$pva <- -log10(go[,2])
        
        go <- go[order(go$pva,decreasing = F),]
        
        go$name <- factor(go[,1],levels = go[,1])
        
bar_p <-  ggplot(data = go,aes(x=pva,y=name)) +
            geom_bar(stat = 'identity',
                     fill=input$fill_col,
                     color=input$bor_col,
                     width = as.numeric(input$col_width),
                     alpha = as.numeric(input$fill_alpha),
                     linetype = input$linetp) +
            xlab('-log10 Pvalue') +
            ylab('') +
            theme_prism(base_size = as.numeric(input$theme_size),
                        base_line_size = as.numeric(input$line_size),
                        border = input$border) +
            labs(title = input$title)
        
if(input$border=='FALSE'){
    bar_p + scale_y_discrete(guide = "prism_bracket") + scale_x_continuous( guide = "prism_offset")
}else{
    bar_p
}
        
    })
    
    output$my_bar <- renderPlot({
        my_barplot()
    })
    
    # download bar plot
    output$down_bar <- downloadHandler(
        filename = function() {
            paste('barplot', 'pdf',sep = '.')},
        
        content = function(file) {
            ggsave(file,
                   height = input$b_height,
                   width = input$b_width,
                   limitsize = FALSE,
                   units = 'cm',
            )})
}
    # -----------------------------------------------------------my dot plot
    my_dotplot <- eventReactive(input$dot_submit,{
        infile <- input$kegg_data$datapath
        
        if (is.null(infile))
            return(NULL)
        
        d <- infile
        type <- str_sub(d,-3)
        
        if(type=='csv')
            kegg <- read.csv(infile,header = T,sep = ',') else
                kegg <- read.csv(infile,header = T,sep = '\t')
        
        kegg$pva <- -log10(kegg[,4])
        
        colnames(kegg)[2] <- 'count'
        kegg <- kegg[order(kegg$pva,decreasing = F),]
        colnames(kegg)[6] <- '-log10 pvalue'
        
        kegg$name <- factor(kegg[,1],levels = kegg[,1])
        
        ggplot(data = kegg,aes(x=Enrich_ratio,y=name,color=`-log10 pvalue`,size=count)) +
            xlab('Rich Factor') +
            ylab('') +
            geom_point() +
            labs(title = input$kegg_title) +
            theme_bw() +
            scale_size_continuous(range = c(as.numeric(input$size_range[1]),as.numeric(input$size_range[2]))) +
            scale_x_continuous(expand = c(as.numeric(unlist(strsplit(input$x_expand,split = '_'))[1]),
                                          as.numeric(unlist(strsplit(input$x_expand,split = '_'))[2]))) +
            scale_color_gradient(low = input$low_col,high = input$high_col) +
            theme(
                panel.grid = element_line(colour = input$grid_col),
                panel.background = element_blank(),
                axis.text = element_text(size = as.numeric(input$axis_text_sz),color='black'),
                axis.title = element_text(size = as.numeric(input$axis_title_sz)),
                plot.title = element_text(size = as.numeric(input$axis_title_sz),
                                          face = 'bold',hjust = 0.5),
                axis.ticks.length = unit(0.3,'cm')
            )
        
    })
    
    output$my_dot <- renderPlot({
        my_dotplot()
    })
    
    # download dot plot
    output$down_dot <- downloadHandler(
        filename = function() {
            paste('dotplot', 'pdf',sep = '.')},
        
        content = function(file) {
            ggsave(file,
                   height = input$d_height,
                   width = input$d_width,
                   limitsize = FALSE,
                   units = 'cm',
            )})
}

# RUN
shinyApp(ui, server)


