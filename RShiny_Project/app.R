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





# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'DSP Dashboard'),
                    dashboardSidebar(
                      collapsed = FALSE, 
                      div(htmlOutput("welcome"), style = "padding: 20px"),
                      sidebarMenu(
                        menuItem("DSP KPI", tabName = "dsp_kpi_page", icon = icon("bar-chart")),
                        menuItem("In-Process Data - Expt View", tabName = "in_process_expt_data_page", icon = icon("bar-chart"))#,
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
                                )#,
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
      labs(y = 'HPLC LF Step Recovery', x = 'Unit Operation ID') +
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
      labs(y = 'HPLC LF Mass Balance', x = 'Unit Operation ID') +
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
      labs(y = 'HPLC LF Step Recovery/Mass Balance (%)', x = 'Unit Operation ID') +
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
      labs(y = 'HPLC LF Step Recovery', x = 'Unit Operation Type') +
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
      labs(y = 'HPLC LF Mass Balance (%)', x = 'Unit Operation Type') +
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
  
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
