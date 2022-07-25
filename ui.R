library(shiny)
library(leaflet)

today <- format(Sys.Date(), '%Y%m%d')
yesterday <- format(Sys.Date() - 1, '%Y%m%d')
time <- as.integer(format(Sys.time(), '%H'))

if (time >= 11){
  hour <- '12'
} else {
  hour <- '06'
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$style("
        #controls {
          background-color: #ddd;
          align: center
        }
               "),
  tabPanel('SNODAS', 
           tags$style(type = "text/css", "#map {height: calc(100vh - 8px) !important;}"),
           
  leafletOutput('map'), 
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = "20vw", height = "auto",
                tabsetPanel(
                tabPanel('Assimilation', 
                         tags$div(paste0('Current Model: ', hour, 'z' )),
                         actionButton('get_points', 'Select Points in the Polygon', width='250px', icon=icon('cogs'),
                                      style="background-color: #6dd367;"),
                         actionButton('delete_point', 'Delete Selected Point', width='250px', icon=icon('cut'),
                                      style='color: #fff; background-color: #00f'),
                         actionButton('add_points', 'Add A New Zero SWE Point', width='250px', icon=icon('calculator'),
                                      style='color: #fff; background-color: #00f'),
                         numericInput('delta', 'SWE Delta', value = 0, step = 0.01),
                         numericInput("density", 'Snow Density', value = 0, step = 1.0),
                         actionButton('save_edits', 'Save Point Edits', width='250px', icon=icon('save'),
                                      style='color: #fff; background-color: #00f'),
                         actionButton('process', 'Process Assimilation Points', width='250px', icon=icon('gears'),
                                      style='background-color:#ECE43F; color:#000'),
                         selectInput('region', 'Assim Process Region', choices = list('US', 'East', 'West'), 
                                     selected='US', width='250px'),
                         actionButton('export_points', 'Export Points to Shapfile', width='250px', icon=icon('save'),
                                      style='background-color: #f00'),
                         fileInput('load_url', 'Alternate Points Shapefile Location', width='250px'),
                         actionButton('load_manual', 'Load Custom Points Shapefile', width='250px', icon=icon('upload'),
                                      style='color: #fff; background-color: #00f'),
                         actionButton('undo_delete', 'Undo Deletes', width='250px', icon=icon('undo'),
                                      style='background-color:#ECE43F; color:#000'),
                         actionButton('add_labels', 'Add D_SWE_OM Labels', width='250px', icon=icon('cogs'),
                                      style='color: #fff; background-color:#00f'),

                         uiOutput('legend')),
                
                tabPanel('Remote Sensing Layers',
                         dateInput('viirs_date', 'VIIRS 1 Day Composite', Sys.Date() -1 ),
                         actionButton('get_viirs', 'Add VIIRS 1 Day Composite to Map', width='265px', icon=icon('satellite-dish'),
                                      style="color: #fff; background-color: #00f"),
                         dateInput('modis_date', 'MODIS True Color', Sys.Date() - 1),
                         actionButton('get_modis', 'Add MODIS True Color to Map', width='265px', icon=icon('satellite-dish'),
                                      style='color: #fff; background-color: #00f')),
                
                tabPanel('Map Point Analysis',
                         selectInput("measure_type", label='SWE or SD Measurement', 
                                     choices = c('SWE', 'SD', 'Both'), selected='SWE'),
                         plotOutput('errors')),
                
                tabPanel('SNODAS Graphs', 
                             uiOutput('sno_depth', style='overflow-y: scroll')),
                
                tabPanel('Points Table',
                         div(DT::DTOutput('table1'), style='font-size: 80%'),
                         actionButton('delete_row', 'Delete Selected Row', width='250px', icon=icon('cut'),
                                      style='color: #fff, background-color: #00f')
                )
                )
                
  )
  )
))
