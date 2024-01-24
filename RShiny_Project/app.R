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
ui <- fluidPage(

   
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
