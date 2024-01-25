#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny) 
library(shinydashboard)
library(data.table) 
library(readxl)
library(reshape2)
library(ggplot2) 
library(ggpmisc)
library(plotly)
library(dplyr) 
library(readr)
library(DT) 
library(shinyWidgets) 
library(shinyjs) 
library(stringr) 
library(shinythemes)
library(dbplyr)
library(tidyverse)
library(igraph) 
library(gtsummary)
library(gt)

df_dsp_purif_stream_results_entity = read.table(file = "Stream_Results v240124 BBF.txt", header = TRUE, sep = ',')

df_dsp_purif_stream_results_entity_product = df_dsp_purif_stream_results_entity %>%
  filter(stream == 'Product')

df_dsp_filter_ts = read.table(file = "TS_Query v2 v240124 BBF.txt", header = TRUE, sep = ',')


df_dsp_membrane_log_results_entity = read.table(file = "Membrane v240124 BBF.txt", header = TRUE, sep = ',')



pink_shades <- c(
  "#FF6961","#FFC0CB","#DB7093","#F6653B","#E36C75","#DB7093", "#C71585","#FFBDC8","#FF3EA3","#FF621E", 
  "#DB7093","#FFBBC5" ,"#FFB8C3" ,"#FFB6C1" ,"#FFA2BD" ,"#FF8FBA", "#FF7CB7", "#FF69B4", "#FF53AB" ,
  "#FF299B" ,"#FF1493", "#F52B93", "#ED4193" ,"#E35993", "#DB7093", "#D6598F", "#D1428C","#CC2B88",
  "#F6653B","#ED6958","#E36C75","#DB7093"
)





ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'DSP Dashboard'),
                    dashboardSidebar(
                      collapsed = FALSE, 
                      div(htmlOutput("welcome"), style = "padding: 20px"),
                      sidebarMenu(
                        menuItem("DSP KPI", tabName = "dsp_kpi_page", icon = icon("bar-chart")),
                        menuItem("In-Process Data - Expt View", tabName = "in_process_expt_data_page", icon = icon("bar-chart")),
                        menuItem("In-Process Data - Stream View", tabName = "in_process_stream_data_page", icon = icon("bar-chart")),
                        menuItem("Membrane Data", tabName = "membrane_page", icon = icon("table")),
                        menuItem("Unit Op Time-Series", tabName = "ts_page", icon = icon("table")),
                        menuItem("Process Flow Diagrams", tabName = "pfd_page", icon = icon("bar-chart"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'dsp_kpi_page',
                                fluidRow(h4("Data Selection"),
                                         box(
                                           width = 12,
                                           
                                           box(
                                             column(
                                               width = 12,
                                               DT::dataTableOutput("dsp_kpi_table"),
                                               actionButton("do_refresh_dsp_kpi_tab", "Load data"),
                                               h5('NOTE: table filtered for "Product" streams only')
                                             )),
                                           box(
                                             column(
                                               width = 12,
                                               tableOutput("dsp_kpi_selectedRows") # Selected rows on the right side
                                               
                                               
                                             )
                                           )
                                           
                                         )
                                         
                                ),
                                fluidRow(h4("Data Plots"),
                                         tabsetPanel(
                                           tabPanel("Step Recovery - Unit Op ID",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('step_unit_op_id_barplot')
                                                      )
                                                    ),
                                                    downloadButton('step_unit_op_id_download', 'Download Plot')
                                                    
                                                    
                                           ),
                                           tabPanel("Mass Balance - Unit Op ID",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('mass_balance_unit_op_id_barplot')
                                                      )
                                                    ),
                                                    downloadButton('mass_balance_unit_op_id_download', 'Download Plot')
                                                    
                                           ),
                                           tabPanel("Step Recovery & Mass Balance - Unit Op ID",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('step_mass_combo_unit_op_type_barplot')
                                                      )
                                                    ),
                                                    downloadButton('step_mass_combo_unit_op_type_download', 'Download Plot')
                                           ),
                                           tabPanel("Step Recovery - Unit Op Type",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('step_unit_op_type_barplot')
                                                      ),
                                                      box(
                                                        tableOutput('step_unit_op_type_summary')
                                                      )
                                                    ),
                                                    downloadButton('step_unit_op_type_download', 'Download Plot')
                                           ),
                                           tabPanel("Mass Balance - Unit Op Type",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('mass_balance_unit_op_type_barplot')
                                                      ),
                                                      box(
                                                        tableOutput('mass_balance_unit_op_type_summary')
                                                      )
                                                    ),
                                                    downloadButton('mass_balance_unit_op_type_download', 'Download Plot')
                                           )
                                           
                                         )
                                         
                                )
                        ),
                        tabItem(tabName = 'in_process_expt_data_page',
                                selectInput(
                                  'in_process_selection',
                                  label = 'Select Unique Condition IDs',
                                  choices = df_dsp_purif_stream_results_entity$condition_id_name,
                                  multiple = FALSE,
                                  width = '400px'
                                ),
                                fluidRow(h4("Data Plots"),
                                         fluidRow(
                                           box(
                                             plotOutput('pH_lineplot')
                                           ),
                                           box(
                                             plotOutput('conductivity_lineplot')
                                           )
                                         ),
                                         fluidRow(
                                           box(
                                             downloadButton('pH_download', 'Download Plot')
                                           ),
                                           box(
                                             downloadButton('conductivity_download', 'Download Plot')
                                           )
                                         ),fluidRow(
                                           box(
                                             plotOutput('od600_lineplot')
                                           ),
                                           box(
                                             plotOutput('total_solids_lineplot')
                                           )
                                         ),
                                         fluidRow(
                                           box(
                                             downloadButton('od600_download', 'Download Plot')
                                           ),
                                           box(
                                             downloadButton('total_solids_download', 'Download Plot')
                                           )
                                         )
                                )
                                ),
                        tabItem(tabName = 'in_process_stream_data_page',
                                fluidRow(h4("Data Selection"),
                                         box(
                                           width = 12,
                                           
                                           box(
                                             column(
                                               width = 12,
                                               DT::dataTableOutput("in_process_stream_table"),
                                               actionButton("do_refresh_in_process_stream_tab", "Load data")
                                             )),
                                           box(
                                             column(
                                               width = 12,
                                               tableOutput("in_process_stream_selectedRows") # Selected rows on the right side
                                               
                                               
                                             )
                                           )
                                           
                                         )
                                         
                                ),
                                fluidRow(h4("Data Plots"),
                                         tabsetPanel(
                                           tabPanel("pH",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('pH_stream_boxplot')
                                                      ),
                                                      box(
                                                        tableOutput('pH_stream_summary')
                                                      )
                                                    ),
                                                    downloadButton('pH_stream_download', 'Download Plot'),
                                                    
                                                    
                                           ),
                                           tabPanel("Conductivity",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('conductivity_stream_boxplot')
                                                      ),
                                                      box(
                                                        tableOutput('conductivity_stream_summary')
                                                      )
                                                    ),
                                                    downloadButton('conductivity_stream_download', 'Download Plot'),
                                                    
                                           ),
                                           tabPanel("OD600",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('od600_stream_boxplot')
                                                      ),
                                                      box(
                                                        tableOutput('od600_stream_summary')
                                                      )
                                                    ),
                                                    downloadButton('od600_stream_download', 'Download Plot'),
                                                    
                                                    
                                                    
                                                    
                                           ),
                                           tabPanel("Total Solids",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('total_solids_stream_boxplot')
                                                      ),
                                                      box(
                                                        tableOutput('total_solids_stream_summary')
                                                      )
                                                    ),
                                                    downloadButton('total_solids_stream_download', 'Download Plot'),
                                                    
                                                    
                                                    
                                           )
                                           
                                         )
                                )
                                
                                
                        ),
                        tabItem(tabName = 'membrane_page',
                                fluidRow(h4("Data Selection"),
                                         box(
                                           width = 12,
                                           
                                           box(
                                             column(
                                               width = 12,
                                               DT::dataTableOutput("table"),
                                               actionButton("do_refresh_membrane_tab", "Load data")
                                             )),
                                           box(
                                             column(
                                               width = 12,
                                               tableOutput("selectedRows") # Selected rows on the right side
                                               
                                               
                                             )
                                           )
                                           
                                         )
                                         
                                ),
                                fluidRow(h4("Data Plots"),
                                         tabsetPanel(
                                           tabPanel("NWP - Part No.",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('nwp_part_boxplot')
                                                      ),
                                                      box(
                                                        tableOutput('nwp_part_summary')
                                                      )
                                                    ),
                                                    downloadButton('nwp_part_download', 'Download Plot'),
                                                    
                                                    
                                           ),
                                           tabPanel("NWP - Membrane ID",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('nwp_mem_id_boxplot')
                                                      ),
                                                      box(
                                                        tableOutput('nwp_mem_id_summary')
                                                      )
                                                    ),
                                                    downloadButton('nwp_mem_id_download', 'Download Plot'),
                                                    
                                           ),
                                           tabPanel("NWP - Membrane ID/Experiment",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('nwp_mem_exp_barplot')
                                                      ),
                                                      box(
                                                        tableOutput('nwp_mem_exp_summary')
                                                      )
                                                    ),
                                                    downloadButton('nwp_mem_exp_download', 'Download Plot'),
                                                    
                                                    
                                                    
                                                    
                                           ),
                                           tabPanel("NWP - Membrane Surface Area",
                                                    fluidRow(
                                                      box(
                                                        plotOutput('nwp_area_boxplot')
                                                      ),
                                                      box(
                                                        tableOutput('nwp_area_summary')
                                                      )
                                                    ),
                                                    downloadButton('nwp_area_download', 'Download Plot'),
                                                    
                                                    
                                                    
                                           ),
                                           tabPanel("Usage Count - Membrane ID",
                                                    plotOutput('uses_mem_id_barplot'),
                                                    downloadButton('uses_mem_id_download', 'Download Plot')
                                           )
                                           
                                         )
                                )
                        ),
                        tabItem(tabName = 'ts_page',
                                selectInput(
                                  'ts_selection',
                                  label = 'Select Run ID',
                                  choices = df_dsp_filter_ts$TRIAL_IDUNITS,
                                  multiple = TRUE,
                                  width = '400px'
                                ),
                                fluidRow(h4("Data Plots"),
                                         fluidRow(
                                           box(
                                             plotOutput('flux_time_scatterplot')
                                           ),
                                           box(
                                             plotOutput('flux_conc_scatterplot')
                                           )
                                         ),
                                         fluidRow(
                                           box(
                                             downloadButton('flux_time_download', 'Download Plot')
                                           ),
                                           box(
                                             downloadButton('flux_conc_download', 'Download Plot')
                                           )
                                         ),fluidRow(
                                           box(
                                             plotOutput('flux_dia_scatterplot')
                                           ),
                                           box(
                                             plotOutput('tmp_time_scatterplot')
                                           )
                                         ),
                                         fluidRow(
                                           box(
                                             downloadButton('flux_dia_download', 'Download Plot')
                                           ),
                                           box(
                                             downloadButton('tmp_time_download', 'Download Plot')
                                           )
                                         )
                                )
                                
                                
                        ),
                        tabItem(tabName = "pfd_page",
                                selectInput(
                                  'pfd_selection',
                                  label = 'Select Experiment IDs',
                                  choices = df_dsp_purif_stream_results_entity$experiment_id_name,
                                  multiple = FALSE,
                                  width = '400px'
                                ),
                                plotOutput('pfd_plot'),
                                downloadButton('pfd_download', 'Download Plot')
                                
                        )
                        
                      )   
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # DSP KPI PAGE
  
  
  output$dsp_kpi_table <- DT::renderDataTable({
    req(input$do_refresh_dsp_kpi_tab)
    DT::datatable(
      df_dsp_purif_stream_results_entity_product %>%
        select(unit_operation_id_name, experiment_id_name),
      selection = 'multiple', 
      options = list(
        paging = FALSE,
        scrollY = "400px",    
        scrollCollapse = TRUE,
        autoWidth = TRUE,     
        columnDefs = list(list(width = '200px', targets = c(0, 1))),  
        rowCallback = JS(
          "function(row, data, index){",
          "$('td:eq(0) input', row).on('click', function(e) {",
          "  Shiny.onInputChange('dsp_kpi_selectedRows', {",
          "    id: data[0],",  
          "    checked: this.checked",
          "  });",
          "});",
          "}"
        )
      )
    )
  })
  
  # Output selected rows
  output$dsp_kpi_selectedRows <- render_gt({
    dsp_kpi_selected_rows <- input$dsp_kpi_table_rows_selected # Get selected rows
    if (!is.null(dsp_kpi_selected_rows) && length(dsp_kpi_selected_rows) > 0) {
      dsp_kpi_selected_data <- df_dsp_purif_stream_results_entity_product %>% select(unit_operation_id_name, experiment_id_name) %>% slice(dsp_kpi_selected_rows)
      dsp_kpi_selected_data
    } else {
      "No rows selected"
    }
  })
  
  
  
  
  # Visualizations:
  
  # Unit Operation ID vs. Step Recovery
  
  step_unit_op_id_plotInput <- reactive({
    
    selected_rows <<- input$dsp_kpi_table_rows_selected 
    
    ggplot(data =  df_dsp_purif_stream_results_entity_product %>% 
             mutate(plot_title = 'Step Recovery vs. Unit Op ID') %>%
             slice(selected_rows),
           aes(x = reorder(unit_operation_id_name,unit_operation_number), y = protein_step_recovery, fill = unit_operation_id_name)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=round(protein_step_recovery,1)), position=position_dodge(width=0.9), vjust=-0.25) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'Protein Step Recovery (%)', x = 'Unit Operation ID') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
  })
  
  output$step_unit_op_id_barplot <- renderPlot({
    
    print(step_unit_op_id_plotInput())
    
  })
  
  output$step_unit_op_id_download <- downloadHandler(
    filename = function() {'Step_Recovery_vs_Unit_Op_ID.pdf'},
    content = function(file) {
      ggsave(file, plot = step_unit_op_id_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  
  # Unit Operation ID vs. Mass Balance
  
  mass_balance_unit_op_id_plotInput <- reactive({
    
    selected_rows <- input$dsp_kpi_table_rows_selected 
    
    
    ggplot(data =  df_dsp_purif_stream_results_entity_product %>% 
             mutate(plot_title = 'Mass Balance vs. Unit Op ID') %>%
             slice(selected_rows),
           aes(x = reorder(unit_operation_id_name,unit_operation_number), y = protein_mass_balance, fill = unit_operation_id_name)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=round(protein_mass_balance,1)), position=position_dodge(width=0.9), vjust=-0.25) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'Protein Mass Balance (%)', x = 'Unit Operation ID') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
  })
  
  output$mass_balance_unit_op_id_barplot <- renderPlot({
    
    print(mass_balance_unit_op_id_plotInput())
    
  })
  
  output$mass_balance_unit_op_id_download <- downloadHandler(
    filename = function() {'Mass_Balance_vs_Unit_Op_ID.pdf'},
    content = function(file) {
      ggsave(file, plot = mass_balance_unit_op_id_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  
  
  # Unit Operation ID vs. Step Recovery/Mass Balance
  
  step_mass_combo_unit_op_type_plotInput <- reactive({
    
    selected_rows <- input$dsp_kpi_table_rows_selected 
    
    
    ggplot(data =  df_dsp_purif_stream_results_entity_product %>% 
             slice(selected_rows) %>%
             select(
               sample_id_name,
               unit_operation_id_name,
               unit_operation_number,
               stream,
               experiment_id_name,
               protein_step_recovery,
               protein_mass_balance
             ) %>%
             pivot_longer(
               cols = c('protein_step_recovery','protein_mass_balance'),
               names_to = "KPI",
               values_to = "value"
             ) %>%
             mutate(
               KPI = gsub('_', ' ', KPI),
               KPI = str_to_title(KPI),
               KPI = gsub('Hplc','HPLC',KPI),
               plot_title = 'Step Recovery/Mass Balance vs. Unit Op ID'),
           
           aes(x = reorder(unit_operation_id_name,unit_operation_number), y = value, fill = KPI)) +
      geom_bar(stat="identity", position = position_dodge2(reverse = TRUE)) +
      scale_fill_manual(values=pink_shades, guide = guide_legend(reverse = TRUE)) +
      labs(y = 'Step Recovery/Mass Balance (%)', x = 'Unit Operation ID') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
  })
  
  output$step_mass_combo_unit_op_type_barplot <- renderPlot({
    
    print(step_mass_combo_unit_op_type_plotInput())
    
  })
  
  output$step_mass_combo_unit_op_type_download <- downloadHandler(
    filename = function() {'Step_Mass_Balance_Combo_vs_Unit_Op_ID.pdf'},
    content = function(file) {
      ggsave(file, plot = step_mass_combo_unit_op_type_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  
  # Unit Operation vs. Step Recovery (%)
  
  
  
  step_unit_op_type_plotInput <- reactive({
    
    selected_rows <- input$dsp_kpi_table_rows_selected 
    
    ggplot(
      data =  df_dsp_purif_stream_results_entity_product %>%
        slice(selected_rows) %>%
        select(
          unit_operation_type,
          unit_operation_number,
          protein_step_recovery,
          protein_mass_balance
        ) %>%
        filter(!is.na(unit_operation_type)) %>%
        group_by(unit_operation_type) %>%
        summarise(mean_unit_operation_number = mean(unit_operation_number, na.rm = TRUE),
                  mean_protein_step_recovery = mean(protein_step_recovery, na.rm = TRUE),
                  mean_protein_mass_balance = mean(protein_mass_balance, na.rm = TRUE),
                  stdev_protein_step_recovery = sd(protein_step_recovery, na.rm = TRUE),
                  stdev_protein_mass_balance = sd(protein_mass_balance, na.rm = TRUE)
        ) %>%
        mutate(plot_title = 'Step Recovery vs. Unit Op Type'),
      aes(x = reorder(unit_operation_type, mean_unit_operation_number), y = mean_protein_step_recovery, fill = unit_operation_type)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(
        ymin = mean_protein_step_recovery - stdev_protein_step_recovery,
        ymax = mean_protein_step_recovery + stdev_protein_step_recovery,
        width = 0.4
      )
      ) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'Protein Step Recovery (%)', x = 'Unit Operation Type') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
  })
  
  output$step_unit_op_type_barplot <- renderPlot({
    
    print(step_unit_op_type_plotInput())
    
  })
  
  output$step_unit_op_type_download <- downloadHandler(
    filename = function() {'Step_Recovery_vs_Unit_Op_Type.pdf'},
    content = function(file) {
      ggsave(file, plot = step_unit_op_type_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  
  output$step_unit_op_type_summary <- render_gt({
    
    selected_rows <- input$dsp_kpi_table_rows_selected 
    
    table <- df_dsp_purif_stream_results_entity_product %>%
      slice(selected_rows) %>%
      select(unit_operation_type, protein_step_recovery) %>%
      group_by(unit_operation_type) %>%
      summarise(Mean = round(mean(protein_step_recovery, na.rm = TRUE),2),
                'Std Dev' = round(sd(protein_step_recovery, na.rm = TRUE),2),
                CV = round((sd(protein_step_recovery, na.rm = TRUE) / mean(protein_step_recovery, na.rm = TRUE)) * 100,2),
                Min = round(min(protein_step_recovery, na.rm = TRUE),2),
                Max = round(max(protein_step_recovery, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(protein_step_recovery, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(protein_step_recovery,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(protein_step_recovery, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(protein_step_recovery,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  
  
  # Unit Operation vs. Mass Balance (%)
  
  
  
  mass_balance_unit_op_type_plotInput <- reactive({
    
    selected_rows <- input$dsp_kpi_table_rows_selected 
    
    ggplot(data =  df_dsp_purif_stream_results_entity_product %>%
             slice(selected_rows) %>%
             select(
               unit_operation_type,
               unit_operation_number,
               protein_step_recovery,
               protein_mass_balance
             ) %>%
             filter(!is.na(unit_operation_type)) %>%
             group_by(unit_operation_type) %>%
             summarise(mean_unit_operation_number = mean(unit_operation_number, na.rm = TRUE),
                       mean_protein_step_recovery = mean(protein_step_recovery, na.rm = TRUE),
                       mean_protein_mass_balance = mean(protein_mass_balance, na.rm = TRUE),
                       stdev_protein_step_recovery = sd(protein_step_recovery, na.rm = TRUE),
                       stdev_protein_mass_balance = sd(protein_mass_balance, na.rm = TRUE)
             ) %>%
             mutate(plot_title = 'Mass Balance vs. Unit Op Type'),
           aes(x = reorder(unit_operation_type, mean_unit_operation_number), y = mean_protein_mass_balance, fill = unit_operation_type)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(
        ymin = mean_protein_mass_balance - stdev_protein_mass_balance,
        ymax = mean_protein_mass_balance + stdev_protein_mass_balance,
        width = 0.4
      )
      ) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'Protein Mass Balance (%)', x = 'Unit Operation Type') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
  })
  
  
  
  output$mass_balance_unit_op_type_barplot <- renderPlot({
    
    print(mass_balance_unit_op_type_plotInput())
    
  })
  
  
  output$mass_balance_unit_op_type_download <- downloadHandler(
    filename = function() {'Mass_Balance_vs_Unit_Op_Type.pdf'},
    content = function(file) {
      ggsave(file, plot = mass_balance_unit_op_type_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  
  
  
  output$mass_balance_unit_op_type_summary <- render_gt({
    
    selected_rows <- input$dsp_kpi_table_rows_selected # Get selected rows
    
    table <- df_dsp_purif_stream_results_entity_product %>%
      slice(selected_rows) %>%
      select(unit_operation_type, protein_mass_balance) %>%
      group_by(unit_operation_type) %>%
      summarise(Mean = round(mean(protein_mass_balance, na.rm = TRUE),2),
                'Std Dev' = round(sd(protein_mass_balance, na.rm = TRUE),2),
                CV = round((sd(protein_mass_balance, na.rm = TRUE) / mean(protein_mass_balance, na.rm = TRUE)) * 100,2),
                Min = round(min(protein_mass_balance, na.rm = TRUE),2),
                Max = round(max(protein_mass_balance, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(protein_mass_balance, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(protein_mass_balance,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(protein_mass_balance, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(protein_mass_balance,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  # In-Process Data - Experimental View
  
  
  
  # Line graph for pH throughput experiment
  
  pH_plotInput <- reactive({
    selected_condition_ids <- input$in_process_selection 
    
    ggplot(
      data = df_dsp_purif_stream_results_entity %>%
        mutate(
          plot_title = 'pH vs. Unit Operation ID',
          unit_operation_stream = paste(unit_operation_type, stream, sep = '-')
        ) %>%
        filter((condition_id_name == selected_condition_ids) & (stream == 'Product' | stream == 'Feed')),
      aes(x=reorder(factor(unit_operation_stream),unit_operation_number), y=ph, group=condition_id_name, color = condition_id_name)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'pH', x = 'Unit Operation Type') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16)) 
    
    
  })
  
  output$pH_lineplot <- renderPlot({
    
    print(pH_plotInput())
    
  })
  
  output$pH_download <- downloadHandler(
    filename = function() {'pH.pdf'},
    content = function(file) {
      ggsave(file, plot = pH_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  # Line graph for conductivity throughout experiment
  
  conductivity_plotInput <- reactive({
    
    selected_condition_ids <- input$in_process_selection # Get selected rows
    
    ggplot(
      data = df_dsp_purif_stream_results_entity %>%
        mutate(
          plot_title = 'Conductivity vs. Unit Operation ID',
          unit_operation_stream = paste(unit_operation_type, stream, sep = '-')
        ) %>%
        filter((condition_id_name == selected_condition_ids) & (stream == 'Product' | stream == 'Feed')),
      aes(x=reorder(factor(unit_operation_stream),unit_operation_number), y=conductivity_mscm, group=condition_id_name, color = condition_id_name)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'Conductivity (mS/cm)', x = 'Unit Operation Type') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
      theme(text = element_text(size = 16)) 
    
  })
  
  output$conductivity_lineplot <- renderPlot({
    
    print(conductivity_plotInput())
    
  })
  
  output$conductivity_download <- downloadHandler(
    filename = function() {'Conductivity.pdf'},
    content = function(file) {
      ggsave(file, plot = conductivity_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  # Line graph for OD600 throughput experiment
  
  od600_plotInput <- reactive({
    
    selected_condition_ids <- input$in_process_selection # Get selected rows
    
    ggplot(
      data = df_dsp_purif_stream_results_entity %>%
        mutate(
          plot_title = 'OD600 vs. Unit Operation ID',
          unit_operation_stream = paste(unit_operation_type, stream, sep = '-')
        ) %>%
        filter((condition_id_name == selected_condition_ids) & (stream == 'Product' | stream == 'Feed')),
      aes(x=reorder(factor(unit_operation_stream),unit_operation_number), y=od600, group=condition_id_name, color = condition_id_name)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'OD600', x = 'Unit Operation Type') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
      theme(text = element_text(size = 16)) 
    
  })
  
  output$od600_lineplot <- renderPlot({
    
    print(od600_plotInput())
    
  })
  
  output$od600_download <- downloadHandler(
    filename = function() {'OD600.pdf'},
    content = function(file) {
      ggsave(file, plot = od600_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  # Line graph for Total Solids throughput experiment
  
  total_solids_plotInput <- reactive({
    
    selected_condition_ids <- input$in_process_selection # Get selected rows
    
    ggplot(
      data = df_dsp_purif_stream_results_entity %>%
        mutate(
          plot_title = 'Total Solids vs. Unit Operation ID',
          unit_operation_stream = paste(unit_operation_type, stream, sep = '-')
        ) %>%
        filter((condition_id_name == selected_condition_ids) & (stream == 'Product' | stream == 'Feed')),
      aes(x=reorder(factor(unit_operation_stream),unit_operation_number), y=total_solids_gkg, group=condition_id_name, color = condition_id_name)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'Total Solids (g/kg)', x = 'Unit Operation Type') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
      theme(text = element_text(size = 16)) 
    
    
  })
  
  output$total_solids_lineplot <- renderPlot({
    
    print(total_solids_plotInput())
    
  })
  
  output$total_solids_download <- downloadHandler(
    filename = function() {'Total_Solids.pdf'},
    content = function(file) {
      ggsave(file, plot = total_solids_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  # In-Process Data - Stream View
  
  # Data selection
  
  
  output$in_process_stream_table <- DT::renderDataTable({
    req(input$do_refresh_in_process_stream_tab)
    DT::datatable(
      df_dsp_purif_stream_results_entity %>%
        select(sample_id_name, experiment_id_name),
      selection = 'multiple', # Enable multiple row selection
      options = list(
        paging = FALSE,
        scrollY = "400px",    # Set the maximum height with a scrollbar
        scrollCollapse = TRUE,
        autoWidth = TRUE,      # Automatically adjust column widths
        columnDefs = list(list(width = '200px', targets = c(0, 1))),  # Set initial column widths
        rowCallback = JS(
          "function(row, data, index){",
          "$('td:eq(0) input', row).on('click', function(e) {",
          "  Shiny.onInputChange('in_process_stream_selectedRows', {",
          "    id: data[0],",  # Adjust this based on your data structure
          "    checked: this.checked",
          "  });",
          "});",
          "}"
        )
      )
    )
  })
  
  # Output selected rows
  output$in_process_stream_selectedRows <- render_gt({
    in_process_stream_selected_rows <- input$in_process_stream_table_rows_selected # Get selected rows
    if (!is.null(in_process_stream_selected_rows) && length(in_process_stream_selected_rows) > 0) {
      in_process_stream_selected_data <- df_dsp_purif_stream_results_entity %>% select(sample_id_name, experiment_id_name) %>% slice(in_process_stream_selected_rows)
      in_process_stream_selected_data
    } else {
      "No rows selected"
    }
  })
  
  
  
  
  # pH vs. Stream Type - Boxplot
  
  
  
  pH_stream_plotInput <- reactive({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    ggplot(data =  df_dsp_purif_stream_results_entity %>%
             slice(selected_rows) %>%
             mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
             select(
               unit_operation_stream_type,
               unit_operation_number,
               ph,
               conductivity_mscm,
               od600,
               total_solids_gkg
             ) %>%
             filter(!is.na(unit_operation_stream_type)) %>%
             group_by(unit_operation_stream_type) %>%
             summarise(mean_unit_operation_number = mean(unit_operation_number, na.rm = TRUE),
                       mean_pH = mean(ph, na.rm = TRUE),
                       mean_conductivity_mscm = mean(conductivity_mscm, na.rm = TRUE),
                       mean_od600 = mean(od600, na.rm = TRUE),
                       mean_total_solids_gkg = mean(total_solids_gkg, na.rm = TRUE),
                       stdev_ph = sd(ph, na.rm = TRUE),
                       stdev_conductivity_mscm = sd(conductivity_mscm, na.rm = TRUE),
                       stdev_od600 = sd(od600, na.rm = TRUE),
                       stdev_total_solids_gkg = sd(total_solids_gkg, na.rm = TRUE)
             ) %>%
             mutate(plot_title = 'pH vs. Stream Type'),
           aes(x = reorder(unit_operation_stream_type, mean_unit_operation_number), y = mean_pH, fill = unit_operation_stream_type)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(
        ymin = mean_pH - stdev_ph,
        ymax = mean_pH + stdev_ph,
        width = 0.4
      )
      ) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'pH', x = 'Stream Type') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
  })
  
  output$pH_stream_boxplot <- renderPlot({
    
    print(pH_stream_plotInput())
    
  })
  
  output$pH_stream_download <- downloadHandler(
    filename = function() {'pH_stream.pdf'},
    content = function(file) {
      ggsave(file, plot = pH_stream_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  output$pH_stream_summary <- render_gt({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    table <- df_dsp_purif_stream_results_entity %>%
      slice(selected_rows) %>%
      mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
      select(unit_operation_stream_type, ph) %>%
      group_by(unit_operation_stream_type) %>%
      summarise(Mean = round(mean(ph, na.rm = TRUE),2),
                'Std Dev' = round(sd(ph, na.rm = TRUE),2),
                CV = round((sd(ph, na.rm = TRUE) / mean(ph, na.rm = TRUE)) * 100,2),
                Min = round(min(ph, na.rm = TRUE),2),
                Max = round(max(ph, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(ph, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(ph,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(ph, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(ph,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  
  
  # Conductivity vs. Stream Type - Boxplot
  
  
  
  conductivity_stream_plotInput <- reactive({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    ggplot(data =  df_dsp_purif_stream_results_entity %>%
             slice(selected_rows) %>%
             mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
             select(
               unit_operation_stream_type,
               unit_operation_number,
               ph,
               conductivity_mscm,
               od600,
               total_solids_gkg
             ) %>%
             filter(!is.na(unit_operation_stream_type)) %>%
             group_by(unit_operation_stream_type) %>%
             summarise(mean_unit_operation_number = mean(unit_operation_number, na.rm = TRUE),
                       mean_pH = mean(ph, na.rm = TRUE),
                       mean_conductivity_mscm = mean(conductivity_mscm, na.rm = TRUE),
                       mean_od600 = mean(od600, na.rm = TRUE),
                       mean_total_solids_gkg = mean(total_solids_gkg, na.rm = TRUE),
                       stdev_ph = sd(ph, na.rm = TRUE),
                       stdev_conductivity_mscm = sd(conductivity_mscm, na.rm = TRUE),
                       stdev_od600 = sd(od600, na.rm = TRUE),
                       stdev_total_solids_gkg = sd(total_solids_gkg, na.rm = TRUE)
             ) %>%
             mutate(plot_title = 'Conductivity vs. Stream Type'),
           aes(x = reorder(unit_operation_stream_type, mean_unit_operation_number), y = mean_conductivity_mscm, fill = unit_operation_stream_type)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(
        ymin = mean_conductivity_mscm - stdev_conductivity_mscm,
        ymax = mean_conductivity_mscm + stdev_conductivity_mscm,
        width = 0.4
      )
      ) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'Conductivity (mS/cm)', x = 'Stream Type') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
  })
  
  output$conductivity_stream_boxplot <- renderPlot({
    
    print(conductivity_stream_plotInput())
    
  })
  
  output$conductivity_stream_download <- downloadHandler(
    filename = function() {'Conductivity_stream.pdf'},
    content = function(file) {
      ggsave(file, plot = conductivity_stream_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  output$conductivity_stream_summary <- render_gt({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    table <- df_dsp_purif_stream_results_entity %>%
      slice(selected_rows) %>%
      mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
      select(unit_operation_stream_type, conductivity_mscm) %>%
      group_by(unit_operation_stream_type) %>%
      summarise(Mean = round(mean(conductivity_mscm, na.rm = TRUE),2),
                'Std Dev' = round(sd(conductivity_mscm, na.rm = TRUE),2),
                CV = round((sd(conductivity_mscm, na.rm = TRUE) / mean(conductivity_mscm, na.rm = TRUE)) * 100,2),
                Min = round(min(conductivity_mscm, na.rm = TRUE),2),
                Max = round(max(conductivity_mscm, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(conductivity_mscm, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(conductivity_mscm,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(conductivity_mscm, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(conductivity_mscm,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  
  # OD600 vs. Stream Type - Boxplot
  
  
  
  od600_stream_plotInput <- reactive({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    ggplot(data =  df_dsp_purif_stream_results_entity %>%
             slice(selected_rows) %>%
             mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
             select(
               unit_operation_stream_type,
               unit_operation_number,
               ph,
               conductivity_mscm,
               od600,
               total_solids_gkg
             ) %>%
             filter(!is.na(unit_operation_stream_type)) %>%
             group_by(unit_operation_stream_type) %>%
             summarise(mean_unit_operation_number = mean(unit_operation_number, na.rm = TRUE),
                       mean_pH = mean(ph, na.rm = TRUE),
                       mean_conductivity_mscm = mean(conductivity_mscm, na.rm = TRUE),
                       mean_od600 = mean(od600, na.rm = TRUE),
                       mean_total_solids_gkg = mean(total_solids_gkg, na.rm = TRUE),
                       stdev_ph = sd(ph, na.rm = TRUE),
                       stdev_conductivity_mscm = sd(conductivity_mscm, na.rm = TRUE),
                       stdev_od600 = sd(od600, na.rm = TRUE),
                       stdev_total_solids_gkg = sd(total_solids_gkg, na.rm = TRUE)
             ) %>%
             mutate(plot_title = 'OD600 vs. Stream Type'),
           aes(x = reorder(unit_operation_stream_type, mean_unit_operation_number), y = mean_od600, fill = unit_operation_stream_type)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(
        ymin = mean_od600 - stdev_od600,
        ymax = mean_od600 + stdev_od600,
        width = 0.4
      )
      ) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'OD600', x = 'Stream Type') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
  })
  
  output$od600_stream_boxplot <- renderPlot({
    
    print(od600_stream_plotInput())
    
  })
  
  output$od600_stream_download <- downloadHandler(
    filename = function() {'OD600_stream.pdf'},
    content = function(file) {
      ggsave(file, plot = od600_stream_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  
  output$od600_stream_summary <- render_gt({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    table <- df_dsp_purif_stream_results_entity %>%
      slice(selected_rows) %>%
      mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
      select(unit_operation_stream_type, od600) %>%
      group_by(unit_operation_stream_type) %>%
      summarise(Mean = round(mean(od600, na.rm = TRUE),2),
                'Std Dev' = round(sd(od600, na.rm = TRUE),2),
                CV = round((sd(od600, na.rm = TRUE) / mean(od600, na.rm = TRUE)) * 100,2),
                Min = round(min(od600, na.rm = TRUE),2),
                Max = round(max(od600, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(od600, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(od600,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(od600, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(od600,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  
  # Total Solids vs. Stream Type - Boxplot
  
  
  
  total_solids_stream_plotInput <- reactive({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    ggplot(data =  df_dsp_purif_stream_results_entity %>%
             slice(selected_rows) %>%
             mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
             select(
               unit_operation_stream_type,
               unit_operation_number,
               ph,
               conductivity_mscm,
               od600,
               total_solids_gkg
             ) %>%
             filter(!is.na(unit_operation_stream_type)) %>%
             group_by(unit_operation_stream_type) %>%
             summarise(mean_unit_operation_number = mean(unit_operation_number, na.rm = TRUE),
                       mean_pH = mean(ph, na.rm = TRUE),
                       mean_conductivity_mscm = mean(conductivity_mscm, na.rm = TRUE),
                       mean_od600 = mean(od600, na.rm = TRUE),
                       mean_total_solids_gkg = mean(total_solids_gkg, na.rm = TRUE),
                       stdev_ph = sd(ph, na.rm = TRUE),
                       stdev_conductivity_mscm = sd(conductivity_mscm, na.rm = TRUE),
                       stdev_od600 = sd(od600, na.rm = TRUE),
                       stdev_total_solids_gkg = sd(total_solids_gkg, na.rm = TRUE)
             ) %>%
             mutate(plot_title = 'Total Solids vs. Stream Type'),
           aes(x = reorder(unit_operation_stream_type, mean_unit_operation_number), y = mean_total_solids_gkg, fill = unit_operation_stream_type)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(
        ymin = mean_total_solids_gkg - stdev_total_solids_gkg,
        ymax = mean_total_solids_gkg + stdev_total_solids_gkg,
        width = 0.4
      )
      ) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'Total Solids (g/kg)', x = 'Stream Type') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
  })
  
  output$total_solids_stream_boxplot <- renderPlot({
    
    print(total_solids_stream_plotInput())
    
  })
  
  output$total_solids_stream_download <- downloadHandler(
    filename = function() {'Total_solids_stream.pdf'},
    content = function(file) {
      ggsave(file, plot = total_solids_stream_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  output$total_solids_stream_summary <- render_gt({
    
    selected_rows <- input$in_process_stream_table_rows_selected
    
    table <- df_dsp_purif_stream_results_entity %>%
      slice(selected_rows) %>%
      mutate(unit_operation_stream_type = paste(unit_operation_type, stream, sep='-')) %>%
      select(unit_operation_stream_type, total_solids_gkg) %>%
      group_by(unit_operation_stream_type) %>%
      summarise(Mean = round(mean(total_solids_gkg, na.rm = TRUE),2),
                'Std Dev' = round(sd(total_solids_gkg, na.rm = TRUE),2),
                CV = round((sd(total_solids_gkg, na.rm = TRUE) / mean(total_solids_gkg, na.rm = TRUE)) * 100,2),
                Min = round(min(total_solids_gkg, na.rm = TRUE),2),
                Max = round(max(total_solids_gkg, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(total_solids_gkg, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(total_solids_gkg,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(total_solids_gkg, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(total_solids_gkg,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  
  
  
  # Unit Op Membrane INFO
  
  output$table <- renderDT({
    req(input$do_refresh_membrane_tab)
    datatable(
      df_dsp_membrane_log_results_entity %>%
        select(unit_operation_id_name, membrane_id_name,membrane_part_number, nwp_lm2hr),
      selection = 'multiple', # Enable multiple row selection
      options = list(
        paging = FALSE,
        scrollY = "400px",    # Set the maximum height with a scrollbar
        scrollCollapse = TRUE,
        autoWidth = TRUE,      # Automatically adjust column widths
        columnDefs = list(list(width = '200px', targets = c(0, 1))),  # Set initial column widths
        rowCallback = JS(
          "function(row, data, index){",
          "$('td:eq(0) input', row).on('click', function(e) {",
          "  Shiny.onInputChange('selectedRows', {",
          "    id: data[0],",  # Adjust this based on your data structure
          "    checked: this.checked",
          "  });",
          "});",
          "}"
        )
      )
    )
  })
  
  # Output selected rows
  output$selectedRows <- render_gt({
    selected_rows <- input$table_rows_selected # Get selected rows
    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      selected_data <- df_dsp_membrane_log_results_entity %>% select(membrane_id_name, unit_operation_id_name) %>% slice(selected_rows)
      selected_data
    } else {
      "No rows selected"
    }
  })
  
  
  # NWP vs. Membrane Part Number
  
  plotInput <- reactive({
    # generate bins based on input$bins from ui.R
    selected_rows <- input$table_rows_selected # Get selected rows

        
    ggplot(data =  df_dsp_membrane_log_results_entity %>% 
                         mutate(plot_title = 'Membrane NWP Results') %>%
                         slice(selected_rows),
                       aes(x = membrane_part_number, y = nwp_lm2hr, fill = membrane_part_number)) +
      geom_boxplot() +
      scale_fill_manual(values=pink_shades) +
      geom_jitter(color="black", size=1, alpha=0.5) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'NWP (L/m2-hr)', x = 'Membrane Part No.', switch = 'x') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
  })
  
  output$nwp_part_boxplot <- renderPlot({
    # generate bins based on input$bins from ui.R
    print(plotInput())
  })
  
  
  
  output$nwp_part_download <- downloadHandler(
    filename = function() {'NWP_vs_Membrane_Part_No.pdf'},
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "pdf", width = 7, height = 4, units = "in")
    }
  )
  
  
  output$nwp_part_summary <- render_gt({
    
    selected_rows <- input$table_rows_selected # Get selected rows

    table <- df_dsp_membrane_log_results_entity %>%
      slice(selected_rows) %>%
      select(membrane_part_number, nwp_lm2hr) %>%
      group_by(membrane_part_number) %>%
      summarise(Mean = round(mean(nwp_lm2hr, na.rm = TRUE),2),
                'Std Dev' = round(sd(nwp_lm2hr, na.rm = TRUE),2),
                CV = round((sd(nwp_lm2hr, na.rm = TRUE) / mean(nwp_lm2hr, na.rm = TRUE)) * 100,2),
                Min = round(min(nwp_lm2hr, na.rm = TRUE),2),
                Max = round(max(nwp_lm2hr, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  # NWP vs. Membrane ID
  
  nwp_mem_id_plotInput <- reactive({
    
    selected_rows <- input$table_rows_selected # Get selected rows
    
    ggplot(data =  df_dsp_membrane_log_results_entity %>% 
             mutate(plot_title = 'Membrane NWP Results') %>%
             slice(selected_rows),
           aes(x = membrane_id_name, y = nwp_lm2hr, fill = membrane_part_number)) +
      geom_boxplot() +
      scale_fill_manual(values=pink_shades) +
      geom_jitter(color="black", size=1, alpha=0.5) +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'NWP (L/m2-hr)', x = 'Membrane ID Name.', switch = 'x') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
  })
  
  output$nwp_mem_id_boxplot <- renderPlot({
    
    print(nwp_mem_id_plotInput())
    
  })
  
  output$nwp_mem_id_download <- downloadHandler(
    filename = function() {'NWP_vs_Membrane_ID.pdf'},
    content = function(file) {
      ggsave(file, plot = nwp_mem_id_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  output$nwp_mem_id_summary <- render_gt({
    
    selected_rows <- input$table_rows_selected # Get selected rows

    table <- df_dsp_membrane_log_results_entity %>%
      slice(selected_rows) %>%
      select(membrane_id_name, nwp_lm2hr) %>%
      group_by(membrane_id_name) %>%
      summarise(Mean = round(mean(nwp_lm2hr, na.rm = TRUE),2),
                'Std Dev' = round(sd(nwp_lm2hr, na.rm = TRUE),2),
                CV = round((sd(nwp_lm2hr, na.rm = TRUE) / mean(nwp_lm2hr, na.rm = TRUE)) * 100,2),
                Min = round(min(nwp_lm2hr, na.rm = TRUE),2),
                Max = round(max(nwp_lm2hr, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2)
      )
    table
  })
  
  
  
  
  # NWP vs. Unit Op ID
  
  nwp_mem_exp_plotInput <- reactive({
    
    selected_rows <- input$table_rows_selected # Get selected rows
    
    ggplot(data =  df_dsp_membrane_log_results_entity %>% 
             mutate(plot_title = 'Membrane NWP Results') %>%
             slice(selected_rows),
           aes(x = unit_operation_id_name, y = nwp_lm2hr, fill = membrane_id_name)) +
      geom_bar(stat="identity") +
      scale_fill_manual(values=pink_shades) +
      labs(y = 'NWP (L/m2-hr)', x = '', switch = 'x') +
      facet_grid(~membrane_id_name, scales = 'free_x', space = 'free') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
    
  })
  
  output$nwp_mem_exp_barplot <- renderPlot({
    
    print(nwp_mem_exp_plotInput())
    
  })
  
  output$nwp_mem_exp_download <- downloadHandler(
    filename = function() {'NWP_vs_Unit_Op_ID.pdf'},
    content = function(file) {
      ggsave(file, plot = nwp_mem_exp_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  output$nwp_mem_exp_summary <- render_gt({
    
    selected_rows <- input$table_rows_selected # Get selected rows

    table <- df_dsp_membrane_log_results_entity %>%
      slice(selected_rows) %>%
      select(membrane_id_name, nwp_lm2hr) %>%
      group_by(membrane_id_name) %>%
      summarise(Mean = round(mean(nwp_lm2hr, na.rm = TRUE),2),
                'Std Dev' = round(sd(nwp_lm2hr, na.rm = TRUE),2),
                CV = round((sd(nwp_lm2hr, na.rm = TRUE) / mean(nwp_lm2hr, na.rm = TRUE)) * 100,2),
                Min = round(min(nwp_lm2hr, na.rm = TRUE),2),
                Max = round(max(nwp_lm2hr, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2)
      )
    table
  })
  
  
  # NWP vs. SA
  
  nwp_area_boxplot_plotInput <- reactive({
    
    selected_rows <- input$table_rows_selected # Get selected rows
    
    ggplot(data =  df_dsp_membrane_log_results_entity %>% 
             mutate(plot_title = 'Membrane NWP Results',
                    membrane_area_m2 = as.character(membrane_area_m2)
             ) %>%
             slice(selected_rows),
           aes(x = membrane_area_m2, y = nwp_lm2hr, fill = membrane_area_m2)) +
      geom_boxplot() +
      scale_fill_manual(values=pink_shades) +
      geom_jitter(color="black", size=1, alpha=0.5) +
      labs(y = 'NWP (L/m2-hr)', x = 'Membrane Area (m2)', switch = 'x') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
  })
  
  output$nwp_area_boxplot <- renderPlot({
    
    print(nwp_area_boxplot_plotInput())
    
  })
  
  output$nwp_area_download <- downloadHandler(
    filename = function() {'NWP_vs_Surface_Area.pdf'},
    content = function(file) {
      ggsave(file, plot = nwp_area_boxplot_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  output$nwp_area_summary <- render_gt({
    
    selected_rows <- input$table_rows_selected # Get selected rows

    table <- df_dsp_membrane_log_results_entity %>%
      slice(selected_rows) %>%
      select(membrane_area_m2, nwp_lm2hr) %>%
      group_by(membrane_area_m2) %>%
      summarise(Mean = round(mean(nwp_lm2hr, na.rm = TRUE),2),
                'Std Dev' = round(sd(nwp_lm2hr, na.rm = TRUE),2),
                CV = round((sd(nwp_lm2hr, na.rm = TRUE) / mean(nwp_lm2hr, na.rm = TRUE)) * 100,2),
                Min = round(min(nwp_lm2hr, na.rm = TRUE),2),
                Max = round(max(nwp_lm2hr, na.rm = TRUE),2),
                'Upper Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.75) + (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2),
                'Lower Outlier Level' = round(quantile(nwp_lm2hr, na.rm = TRUE, probs = 0.25) - (1.5 * IQR(nwp_lm2hr,na.rm = TRUE)),2)
      )
    table
  })
  
  # Number of Uses
  
  
  
  uses_mem_id_plotInput <- reactive({
    
    selected_rows <- input$table_rows_selected # Get selected rows
    
    ggplot(data =  df_dsp_membrane_log_results_entity %>%
             group_by(membrane_id_name) %>%
             count() %>% 
             filter(membrane_id_name %in% df_dsp_membrane_log_results_entity[selected_rows, "membrane_id_name"]) %>%
             mutate(plot_title = 'Membrane NWP Results'),
           aes(x = reorder(membrane_id_name,+n), y = n, fill = n)) +
      geom_bar(stat="identity") +
      scale_fill_continuous() +
      labs(y = 'Number of Uses', x = 'Membrane ID', switch = 'x') +
      facet_wrap(vars(plot_title)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
  })
  
  output$uses_mem_id_barplot <- renderPlot({
    
    print(uses_mem_id_plotInput())
    
  })
  
  output$uses_mem_id_download <- downloadHandler(
    filename = function() {'Membrane_Usage.pdf'},
    content = function(file) {
      ggsave(file, plot = uses_mem_id_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  # Unit Op Time-Series 
  
  
  # Flux vs. Time
  
  flux_time_plotInput <- reactive({
    
    selected_run_ids <- input$ts_selection 
    
    ggplot(
      data = df_dsp_filter_ts %>%
        filter(TRIAL_IDUNITS %in% selected_run_ids) %>%
        mutate(plot_title = 'Flux vs. Duration'),
      aes(x = DURATION_MINUTES, y = FLUXLMH, color = TRIAL_IDUNITS)) +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'Flux (LMH)', x = 'Duration (min)') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
    
  })
  
  output$flux_time_scatterplot <- renderPlot({
    
    print(flux_time_plotInput())
    
  })
  
  output$flux_time_download <- downloadHandler(
    filename = function() {'Flux_Time.pdf'},
    content = function(file) {
      ggsave(file, plot = flux_time_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  
  # Flux vs. Concentration
  
  flux_conc_plotInput <- reactive({
    
    selected_run_ids <- input$ts_selection 
    
    ggplot(
      data = df_dsp_filter_ts %>%
        filter(TRIAL_IDUNITS %in% selected_run_ids) %>%
        mutate(plot_title = 'Flux vs. Concentration Factor'),
      aes(x = CFX, y = FLUXLMH, color = TRIAL_IDUNITS)) +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'Flux (LMH)', x = 'Concentration Factor (X)') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
    
  })
  
  output$flux_conc_scatterplot <- renderPlot({
    
    print(flux_conc_plotInput())
    
  })
  
  output$flux_conc_download <- downloadHandler(
    filename = function() {'Flux_Conc.pdf'},
    content = function(file) {
      ggsave(file, plot = flux_conc_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  # Flux vs. Diavolumes
  
  flux_dia_plotInput <- reactive({
    
    selected_run_ids <- input$ts_selection 
    
    ggplot(
      data = df_dsp_filter_ts %>%
        filter(TRIAL_IDUNITS %in% selected_run_ids) %>%
        mutate(plot_title = 'Flux vs. Diavolumes'),
      aes(x = DV_1X, y = FLUXLMH, color = TRIAL_IDUNITS)) +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'Flux (LMH)', x = 'Diavolumes (X)') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
    
  })
  
  output$flux_dia_scatterplot <- renderPlot({
    
    print(flux_dia_plotInput())
    
  })
  
  output$flux_dia_download <- downloadHandler(
    filename = function() {'Flux_Dia.pdf'},
    content = function(file) {
      ggsave(file, plot = flux_dia_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  
  # TMP vs. Time
  
  tmp_time_plotInput <- reactive({
    
    selected_run_ids <- input$ts_selection 
    
    ggplot(
      data = df_dsp_filter_ts %>%
        filter(TRIAL_IDUNITS %in% selected_run_ids) %>%
        mutate(plot_title = 'TMP vs. Duration'),
      aes(x = DURATION_MINUTES, y = TMPPSI, color = TRIAL_IDUNITS)) +
      geom_point() +
      scale_color_manual(values=pink_shades) +
      facet_wrap(vars(plot_title)) +
      labs(y = 'TMP (psi)', x = 'Duration (min)') +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      theme(text = element_text(size = 16))
    
    
    
  })
  
  output$tmp_time_scatterplot <- renderPlot({
    
    print(tmp_time_plotInput())
    
  })
  
  output$tmp_time_download <- downloadHandler(
    filename = function() {'TMP_Time.pdf'},
    content = function(file) {
      ggsave(file, plot = tmp_time_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  
  # PFD Page
  
  
  pfd_plotInput <- reactive({
    
    selected_experiment_ids <- input$pfd_selection # Get selected rows
    
    pfd_data = df_dsp_purif_stream_results_entity %>% filter((experiment_id_name %in% selected_experiment_ids) & (feed_unit_operation_id_name != unit_operation_id_name)) %>% select(from = feed_unit_operation_id_name,to = unit_operation_id_name) %>% na.omit()
    
    
    g = graph_from_data_frame(pfd_data, directed = TRUE)
    coords = layout_as_tree(g)
    colnames(coords) = c("x", "y")
    
    output_df = as_tibble(coords) %>%
      mutate(step = vertex_attr(g, "name"),
             label = step,
             x = x*-1)
    
    plot_nodes = output_df %>%
      mutate(xmin = x - 0.35,
             xmax = x + 0.35,
             ymin = y - 0.25,
             ymax = y + 0.25)
    
    plot_edges = pfd_data %>%
      mutate(id = row_number()) %>%
      pivot_longer(cols = c("from", "to"),
                   names_to = "s_e",
                   values_to = "step") %>%
      left_join(plot_nodes, by = "step") %>%
      select(-c(label, y, xmin, xmax)) %>%
      mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
      select(-c(ymin, ymax))
    
    p = ggplot() +
      geom_rect(data = plot_nodes,
                mapping = aes(xmin = xmin, ymin = ymin,
                              xmax = xmax, ymax = ymax),
                alpha = 0.5)
    
    p = p +
      geom_text(data = plot_nodes,
                mapping = aes(x = x, y = y, label = label))
    
    p = p +
      geom_path(data = plot_edges,
                mapping = aes(x = x, y = y, group = id),
                colour = "#585c45",
                arrow = arrow(length = unit(0.3, "cm"), type = "closed"))
    
    plot(p)
    
    
    
    
    
  })
  
  output$pfd_plot <- renderPlot({
    
    print(pfd_plotInput())
    
  })
  
  output$pfd_download <- downloadHandler(
    filename = function() {'PFD.pdf'},
    content = function(file) {
      ggsave(file, plot = pfd_plotInput(), device = "pdf", width = 21 , height = 14 , units = "in")
    }
  )
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
