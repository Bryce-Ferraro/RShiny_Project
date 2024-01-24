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
                        menuItem("DSP KPI", tabName = "dsp_kpi_page", icon = icon("bar-chart"))
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
                        )
                      )   
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
