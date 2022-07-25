
library(dplyr)
library(DT)
library(fs)
library(ggplot2)
library(gridExtra)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(leafpm)
library(lwgeom)
library(rgdal)
library(sf)
library(shiny)

# Setup
# Set up the Global Variables (Map, etc)
#setwd('../SNODAS_Assimilation/')
today <- format(Sys.Date(), '%Y%m%d')
yesterday <- format(Sys.Date() - 1, '%Y%m%d')
time <- as.integer(format(Sys.time(), '%H'))

if (time >= 11){
  hour <- '12'
} else {
  hour <- '06'
}
assim_folder <- file.path(path_home(), 'Documents/Assim/data')
points_filename <- paste0('ssm1054_md_based_', yesterday, hour, '_', today, hour, '.zip')

if (!dir.exists(file.path(assim_folder, today))){
  dir.create(file.path(assim_folder, today)) 
}

assim_points_zip <- paste0('ssm1054_md_based_', yesterday, hour, '_', today, hour, '.zip')
if (!file.exists(file.path(assim_folder, today, assim_points_zip))){
  assim_points <- paste0('https://www.nohrsc.noaa.gov/pro/assim_points/',
                         substring(today, 1, 6), '/', points_filename)
  
  points_save <- file.path(assim_folder, today, points_filename)
  
  download.file(assim_points, points_save)
  
  unzip(points_save, overwrite=TRUE)
}

if (!file.exists(file.path(assim_folder, today, gsub('.{3}$', 'shp', assim_points_zip)))){
  unzip(file.path(assim_folder, today, assim_points_zip), exdir=file.path(assim_folder, today))
} 


shp_to_pnts <- function(shp_filename = file.path(assim_folder, today, gsub('.{3}$', 'shp', assim_points_zip))){
  assim_point_shp <- st_read(shp_filename)
  assim_point_shp <- assim_point_shp %>%
    mutate(delta = case_when(
      D_SWE_OM <= -1 ~ 'icon1',
      D_SWE_OM > -1     & D_SWE_OM <= -0.5   ~ 'icon2',
      D_SWE_OM > -0.5   & D_SWE_OM <= -0.25  ~ 'icon3',
      D_SWE_OM > -0.25  & D_SWE_OM <= -0.10  ~ 'icon4',
      D_SWE_OM > -0.10  & D_SWE_OM <= -0.075 ~ 'icon5',
      D_SWE_OM > -0.075 & D_SWE_OM <= -0.05  ~ 'icon6',
      D_SWE_OM > -0.05  & D_SWE_OM <= -0.025 ~ 'icon7',
      D_SWE_OM > -0.025 & D_SWE_OM <= -0.01  ~ 'icon8',
      D_SWE_OM > -0.01  & D_SWE_OM <= -0.001 ~ 'icon9',
      D_SWE_OM > -0.001 & D_SWE_OM <= 0.001  ~ 'icon10',
      D_SWE_OM > 0.001  & D_SWE_OM <= 0.01   ~ 'icon11',
      D_SWE_OM > 0.01   & D_SWE_OM <= 0.025  ~ 'icon12',
      D_SWE_OM > 0.025  & D_SWE_OM <= 0.05   ~ 'icon13',
      D_SWE_OM > 0.05   & D_SWE_OM <= 0.075  ~ 'icon14',
      D_SWE_OM > 0.075  & D_SWE_OM <= 0.10   ~ 'icon15',
      D_SWE_OM > 0.10   & D_SWE_OM <= 0.25   ~ 'icon16',
      D_SWE_OM > 0.25   & D_SWE_OM <= 0.50   ~ 'icon17',
      D_SWE_OM > 0.05   & D_SWE_OM <= 1.0    ~ 'icon18',
      D_SWE_OM > 1.0 ~ 'icon19'
    )) 
  
  assim_point_shp <- assim_point_shp %>%
    mutate(station_type_agg = substring(STATION_TY, 1, 5))
  
  st_crs(assim_point_shp) <- 4326
  
  return(assim_point_shp)
}

pointIcons <- iconList(
  icon1 = makeIcon('../DarkBlue_Square_Icon.png', iconWidth=16, iconHeight=16),
  icon2 = makeIcon('../Blue_Square_Icon.png', iconWidth=16, iconHeight=16),
  icon3 = makeIcon('../Cyan_Square_Icon.png', iconWidth=16, iconHeight=16),
  icon4 = makeIcon('../DarkBlue_Pentagon_Icon.png', iconWidth=12, iconHeight=12),
  icon5 = makeIcon('../Blue_Pentagon_Icon.png', iconWidth=12, iconHeight=12),
  icon6 = makeIcon('../Cyan_Pentagon_Icon.png', iconWidth=12, iconHeight=12),
  icon7 = makeIcon('../DarkBlue_Diamond_Icon.png', iconWidth=8, iconHeight=8),
  icon8 = makeIcon('../Blue_Diamond_Icon.png', iconWidth=8, iconHeight=8),
  icon9 = makeIcon('../Cyan_Diamond_Icon.png', iconWidth=8, iconHeight=8),
  icon10 = makeIcon('../Yellow_Diamond_Icon.png', iconWidth=6, iconHeight=6),
  icon11 = makeIcon('../LightRed_Diamond_Icon.png', iconWidth=8, iconHeight=8),
  icon12 = makeIcon('../Red_Diamond_Icon.png', iconWidth=8, iconHeight=8),
  icon13 = makeIcon('../BrickRed_Diamond_Icon.png', iconWidth=8, iconHeight=8),
  icon14 = makeIcon('../LightRed_Pentagon_Icon.png', iconWidth=12, iconHeight=12),
  icon15 = makeIcon('../Red_Pentagon_Icon.png', iconWidth=12, iconHeight=12),
  icon16 = makeIcon('../BrickRed_Pentagon_Icon.png', iconWidth=12, iconHeight=12),
  icon17 = makeIcon('../LightRed_Square_Icon.png', iconWidth=16, iconHeight=16),
  icon18 = makeIcon('../Red_Square_Icon.png', iconWidth=16, iconHeight=16),
  icon19 = makeIcon('../BrickRed_Square_Icon.png', iconWidth=16, iconHeight=16)
)

assim_point_shp <- shp_to_pnts()
flightlines <- readOGR('../flines/flines.shp')




shinyServer(function(input, output, session) {

  output$map <- renderLeaflet({
    
    
    leaflet() %>%
      setView(-93, 42.5, zoom=4 ) %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group='Dark') %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group='Nat Geo') %>%
      addWMSTiles(paste0('https://idpgis.ncep.noaa.gov/arcgis/services/', 
                         'NWS_Observations/NOHRSC_Snow_Analysis/MapServer/WMSServer?'),
                  layers='1', options=list(format='image/png32', transparent='true', 
                                           opacity=0.75), group = 'SNODAS') %>%
      
      addMarkers(data=assim_point_shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', assim_point_shp$STATION_ID, 
                                '</br>',  'Delta: ', 
                                round(assim_point_shp$D_SWE_OM*39.3701, 2), '"',
                                '</br>Observed SWE: ', round(assim_point_shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', assim_point_shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                assim_point_shp$STATION_ID, 
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Observations') %>%
      addCircleMarkers(data=assim_point_shp %>% filter(TRUE_FLAG == 1),
                       group='Observed SWE',
                       color='darkgreen',
                       radius=7,
                       weight=3,
                       fillOpacity=0.0,
                       stroke=TRUE,
                       ) %>%
      addCircleMarkers(data=assim_point_shp %>% filter(OB_SWE == 0),
                       group='True Zeros',
                       radius=5,
                       weight=3,
                       color='yellow',
                       fillOpacity=0.0,
                       stroke=TRUE) %>%
      addPolylines(data=flightlines,
                   color='darkblue',
                   group='FLINES',
                   label=~NAME) %>%
      
      hideGroup('FLINES') %>%
      addLayersControl(
        baseGroups = c('Dark', 'Nat Geo'), 
        overlayGroups = c('SNODAS', 'Observations', 'FLINES', 'Observed SWE', 'True Zeros'),
        options = layersControlOptions(collapsed=FALSE)) %>%
      
      addPmToolbar(
        toolbarOptions = pmToolbarOptions(drawCircle = FALSE,
                                          drawMarker = FALSE,
                                          drawPolyline = FALSE, 
                                          drawRectangle = FALSE,position = 'topleft'),
        drawOptions = pmDrawOptions(allowSelfIntersection = FALSE, snappable = FALSE),
      ) %>%
      hideGroup('Observed SWE') %>%
      hideGroup('True Zeros')
  })
  
  output$legend <- renderUI({
    tags$img(src='https://www.nohrsc.noaa.gov/pub/staff/scarter/SNODAS_Legend.png', width="350")
  })
  selected_points <- reactiveValues()
  region_polygons <- reactiveValues()
  
  observeEvent(input$get_points, {
    req(input$map_draw_stop)
    # Convert the leaflet draw polygon to a spatial object
    feat <- input$map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol=2, byrow= TRUE)
    if (input$get_points == 0){
      region_polygons$shp <- st_sf(st_sfc(st_polygon(list(coords))), crs=st_crs(4326))
    } else {
      region_polygons$shp <- bind_rows(region_polygons$shp, st_sf(st_sfc(st_polygon(list(coords))), crs=st_crs(4326)))
    }
    
    # Create a new SF object of the assim points within the polygon
    selected_points$shp <- st_filter(assim_point_shp, region_polygons$shp)
    
    # Draw the points in the polygon on the map and modify the layer control widget
    leafletProxy('map') %>%
      addMarkers(data=selected_points$shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', selected_points$shp$STATION_ID, 
                                '</br>',  'Delta: ', 
                                round(selected_points$shp$D_SWE_OM*39.3701, 2), '"',
                                '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                selected_points$shp$STATION_ID, 
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Assim Points')%>%
      
      addLayersControl(
        baseGroups = c('Dark', 'Nat Geo'), 
        overlayGroups = c('SNODAS', 'Observations', 'FLINES', 'Observed SWE', 'True Zeros'),
        options = layersControlOptions(collapsed=FALSE))
    
  })  
  
  observeEvent(input$get_points, {
    req(input$map_draw_edited_features)
    region_polygons$shp <- NULL
    feat <- input$map_draw_edited_features
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol=2, byrow= TRUE)
    region_polygons$shp <- st_sf(st_sfc(st_polygon(list(coords))), crs=st_crs(4326))
    selected_points$shp <- st_filter(shp_to_pnts(), region_polygons$shp)
    
    leafletProxy('map') %>%
      addMarkers(data=selected_points$shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', selected_points$shp$STATION_ID, 
                                '</br>',  'Delta: ', 
                                round(selected_points$shp$D_SWE_OM*39.3701, 2), '"',
                                '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                selected_points$shp$STATION_ID, 
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Assim Points')
  })
  
  observeEvent(input$map_marker_click, {
    if (input$get_points > 0){
      station <- input$map_marker_click$id
      station <- selected_points$shp %>% filter(STATION_ID == station)
      
      updateNumericInput(session, 'delta', value=station$D_SWE_OM)
      updateNumericInput(session, 'density', value=station$MD_DENSITY)
    }
  })
  
  observeEvent(input$save_edits, {
    station <- input$map_marker_click$id
    if (!is.na(station)){
      if (input$delta != selected_points$shp[which(selected_points$shp$STATION_ID == station), ]$D_SWE_OM ){
        point_edit <- selected_points$shp %>%
          filter(STATION_ID == station) %>%
          mutate(D_SWE_OM = input$delta) %>%
          mutate(delta = case_when(
            D_SWE_OM <= -1 ~ 'icon1',
            D_SWE_OM > -1     & D_SWE_OM <= -0.5   ~ 'icon2',
            D_SWE_OM > -0.5   & D_SWE_OM <= -0.25  ~ 'icon3',
            D_SWE_OM > -0.25  & D_SWE_OM <= -0.10  ~ 'icon4',
            D_SWE_OM > -0.10  & D_SWE_OM <= -0.075 ~ 'icon5',
            D_SWE_OM > -0.075 & D_SWE_OM <= -0.05  ~ 'icon6',
            D_SWE_OM > -0.05  & D_SWE_OM <= -0.025 ~ 'icon7',
            D_SWE_OM > -0.025 & D_SWE_OM <= -0.01  ~ 'icon8',
            D_SWE_OM > -0.01  & D_SWE_OM <= -0.001 ~ 'icon9',
            D_SWE_OM > -0.001 & D_SWE_OM <= 0.001  ~ 'icon10',
            D_SWE_OM > 0.001  & D_SWE_OM <= 0.01   ~ 'icon11',
            D_SWE_OM > 0.01   & D_SWE_OM <= 0.025  ~ 'icon12',
            D_SWE_OM > 0.025  & D_SWE_OM <= 0.05   ~ 'icon13',
            D_SWE_OM > 0.05   & D_SWE_OM <= 0.075  ~ 'icon14',
            D_SWE_OM > 0.075  & D_SWE_OM <= 0.10   ~ 'icon15',
            D_SWE_OM > 0.10   & D_SWE_OM <= 0.25   ~ 'icon16',
            D_SWE_OM > 0.25   & D_SWE_OM <= 0.50   ~ 'icon17',
            D_SWE_OM > 0.05   & D_SWE_OM <= 1.0    ~ 'icon18',
            D_SWE_OM > 1.0 ~ 'icon19'
          )) 
        selected_points$shp[which(selected_points$shp$STATION_ID == station), ] <- point_edit
      } else if (input$density != selected_points$shp[which(selected_points$shp$STATION_ID == station), ]$MD_DENSITY){
        point_edit <- selected_points$shp %>%
          filter(STATION_ID == station) %>%
          mutate(MD_DENSITY = input$density) %>%
          mutate(MD_SWE = MD_DENSITY * MD_DEPTH) %>%
          mutate(D_SWE_OM = OB_SWE - MD_SWE) %>%
          mutate(delta = case_when(
            D_SWE_OM <= -1 ~ 'icon1',
            D_SWE_OM > -1     & D_SWE_OM <= -0.5   ~ 'icon2',
            D_SWE_OM > -0.5   & D_SWE_OM <= -0.25  ~ 'icon3',
            D_SWE_OM > -0.25  & D_SWE_OM <= -0.10  ~ 'icon4',
            D_SWE_OM > -0.10  & D_SWE_OM <= -0.075 ~ 'icon5',
            D_SWE_OM > -0.075 & D_SWE_OM <= -0.05  ~ 'icon6',
            D_SWE_OM > -0.05  & D_SWE_OM <= -0.025 ~ 'icon7',
            D_SWE_OM > -0.025 & D_SWE_OM <= -0.01  ~ 'icon8',
            D_SWE_OM > -0.01  & D_SWE_OM <= -0.001 ~ 'icon9',
            D_SWE_OM > -0.001 & D_SWE_OM <= 0.001  ~ 'icon10',
            D_SWE_OM > 0.001  & D_SWE_OM <= 0.01   ~ 'icon11',
            D_SWE_OM > 0.01   & D_SWE_OM <= 0.025  ~ 'icon12',
            D_SWE_OM > 0.025  & D_SWE_OM <= 0.05   ~ 'icon13',
            D_SWE_OM > 0.05   & D_SWE_OM <= 0.075  ~ 'icon14',
            D_SWE_OM > 0.075  & D_SWE_OM <= 0.10   ~ 'icon15',
            D_SWE_OM > 0.10   & D_SWE_OM <= 0.25   ~ 'icon16',
            D_SWE_OM > 0.25   & D_SWE_OM <= 0.50   ~ 'icon17',
            D_SWE_OM > 0.05   & D_SWE_OM <= 1.0    ~ 'icon18',
            D_SWE_OM > 1.0 ~ 'icon19'
          ))
        selected_points$shp[which(selected_points$shp$STATION_ID == station), ] <- point_edit
      }
      leafletProxy('map') %>%
        clearGroup('Assim Points') %>%
        addMarkers(data=selected_points$shp,
                   icon = ~pointIcons[delta],
                   popup = paste0('Station: ', selected_points$shp$STATION_ID, 
                                  '</br>',  'Delta: ', 
                                  round(selected_points$shp$D_SWE_OM*39.3701, 2), '"',
                                  '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                  '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                  '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                  selected_points$shp$STATION_ID, 
                                  '">Station Page on NOHRSC'),
                   layerId = ~STATION_ID,
                   group = 'Assim Points')
    }
  })
  observeEvent(input$export_points, {
    if (!dir.exists(file.path(assim_folder, today, 'nudging_layers'))){
      dir.create(file.path(assim_folder, today, 'nudging_layers'))
    }
    
    if (length(input$map_draw_edited_features) > 0){
      feat <- input$map_draw_edited_feature
    } else {
      feat <- input$map_draw_new_feature
    }
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol=2, byrow= TRUE)
    
    assim_process_region <- st_sf(st_sfc(st_polygon(list(coords))), crs=st_crs(4326))
    points_filename <- paste0('ssm1054_md_based_', format(Sys.Date() -1, '%Y%m%d'),
                              '12_', format(Sys.Date(), '%Y%m%d'), '12_', tolower(input$region),
                              '.shp')
    poly_filnename <- paste0('process_region_', format(Sys.Date() - 1, '%Y%m%d'),
                             '12_', format(Sys.Date(), '%Y%m%d'), '12_swe_', tolower(input$region),
                             '.shp')
    st_write(selected_points$shp, file.path(assim_folder, today, 'nudging_layers', 
                                            points_filename), delete_dsn = TRUE)
    st_write(region_polygons$shp, file.path(assim_folder, today, 'nudging_layers',
                                            poly_filnename), delete_dsn =TRUE)
  })
  
  deleted_points <- reactiveValues()
  
  observeEvent(input$delete_point, {
    deleted_points$list <- append(deleted_points$list, input$map_marker_click$id)
    selected_points$shp <- selected_points$shp %>%
      filter(STATION_ID != input$map_marker_click$id)
    leafletProxy('map') %>%
      clearGroup('Assim Points') %>%
      addMarkers(data=selected_points$shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', selected_points$shp$STATION_ID, 
                                '</br>',  'Delta: ', 
                                round(selected_points$shp$D_SWE_OM*39.3701, 2), '"',
                                '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                selected_points$shp$STATION_ID, 
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Assim Points')
  }) 
  observeEvent(input$undo_delete, {
    if (length(deleted_points$list) > 0){
      idx <- length(deleted_points$list)
      point_id <- deleted_points$list[idx]
      print(deleted_points$list)
      deleted_points$list <- deleted_points$list[-idx]
      print('test')
      print(deleted_points$list)
      selected_points$shp <- selected_points$shp %>%
        bind_rows(assim_point_shp %>% filter(STATION_ID == point_id))
      leafletProxy('map') %>%
        clearGroup('Assim Points') %>%
        addMarkers(data=selected_points$shp,
                   icon = ~pointIcons[delta],
                   popup = paste0('Station: ', selected_points$shp$STATION_ID, 
                                  '</br>',  'Delta: ', 
                                  round(selected_points$shp$D_SWE_OM*39.3701, 2), '"',
                                  '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                  '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                  '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                  selected_points$shp$STATION_ID, 
                                  '">Station Page on NOHRSC'),
                   layerId = ~STATION_ID,
                   group = 'Assim Points')
    }
  })
  
  observeEvent(input$add_points, {
    selected_points$shp <- selected_points$shp %>% add_row(
      STATION_ID = paste('New Zero Point', sample(1:100, 1)),
      D_SWE_OM = 0.0,
      delta = 'icon10',
      LATITUDE = input$map_click$lat,
      LONGITUDE = input$map_click$lng,
      geometry = st_sfc(st_point(c(input$map_click$lng, input$map_click$lat)))
    )
    
    leafletProxy('map') %>%
      clearGroup('Assim Points') %>%
      addMarkers(data=selected_points$shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', selected_points$shp$STATION_ID, 
                                '</br>',  'Delta: ', 
                                round(selected_points$shp$D_SWE_OM*39.3701, 2), '"',
                                '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                selected_points$shp$STATION_ID, 
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Assim Points')
  })
  
  observeEvent(input$load_manual, {
    assim_point_shp <- shp_to_pnts(input$load_url)
    leafletProxy('map') %>%
      clearGroup('Observations') %>%
      addMarkers(data=assim_point_shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', assim_point_shp$STATION_ID, 
                                '</br>',  'Delta: ', 
                                round(assim_point_shp$D_SWE_OM*39.3701, 2), '"',
                                '</br>Observed SWE: ', round(assim_point_shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', assim_point_shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                assim_point_shp$STATION_ID, 
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Observations')
  })
  
  observeEvent(input$process, {
    if (!dir.exists(file.path(assim_folder, today, 'nudging_layers'))){
      dir.create(file.path(assim_folder, today, 'nudging_layers'))
    }
    polygons <- region_polygons$shp %>% st_transform(3857)
    
    for (i in 1:nrow(region_polygons$shp)){
      if (nrow(region_polygons$shp) == 1) {
        symmetrical_difference <- polygons[1,] %>% 
          st_buffer(1000) %>%
          st_difference(polygons[1,])
        
      }
      else {
        if (i == 1){
          symmetrical_difference <- polygons[i,] %>% 
            st_buffer(1000) %>% 
            st_difference(polygons[i,])
          
          
        } else {
          x <- polygons[i,] %>%
            st_buffer(1000) %>%
            st_difference(polygons[i,])
          
          symmetrical_difference <- bind_rows(symmetrical_difference, x)
        }
      }
    }
    
    for (i in 1:nrow(symmetrical_difference)){
      if (i == 1){
        perimeter <- st_perimeter(symmetrical_difference)
      } else {
        perimeter <- perimeter + st_perimeter(symmetrical_difference[i,])
      }
    }
    
    number_of_points <- perimeter / 20000
    
    new_zeros <- st_sample(symmetrical_difference, 
                           as.integer(number_of_points[[1]]))
    
    new_zeros <- st_sf(data.frame(STATION_ID = 
                                    paste0('buffer_', 
                                           sample(1:length(new_zeros), 
                                                  size = length(new_zeros), 
                                                  replace = F)), 
                                  geom=new_zeros))
    
    new_zeros <- new_zeros %>%
      mutate(delta = 'icon10', 
             D_SWE_OM = 0.0) %>% 
      st_transform(4326)
    
    selected_points$shp <- selected_points$shp %>%
      bind_rows(new_zeros) %>%
      mutate(POINT_X = st_coordinates(.)[,1],
             POINT_Y = st_coordinates(.)[,2],
             X = st_coordinates(.)[,1],
             Y = st_coordinates(.)[,2])
    
    selected_points$shp <- selected_points$shp %>%
      filter(!is.null(POINT_X))
    
    
    leafletProxy('map') %>%
      clearGroup('Assim Points') %>%
      addMarkers(data=selected_points$shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', selected_points$shp$STATION_ID,
                                '</br>',  'Delta: ',
                                round(selected_points$shp$D_SWE_OM*39.3701, 2), '"',
                                '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=',
                                selected_points$shp$STATION_ID,
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Assim Points') 
    
  })
  
    points_in_view <- reactive({
    if (is.null(input$map_bounds))
      return(points_in_view[FALSE, ])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lonRng <- range(bounds$east, bounds$west)
    if (input$get_points == 0 ){
      subset(assim_point_shp,
             LATITUDE >= latRng[1] & LATITUDE <= latRng[2] &
               LONGITUDE  >= lonRng[1] & LONGITUDE <= lonRng[2])
    } else {
      selected_points$shp
    }
  })
    
  observeEvent(input$add_labels, {
    point_labels <- points_in_view()
    print(nrow(point_labels))
    leafletProxy('map') %>%
      addLabelOnlyMarkers(data=point_labels,
                          group='Labels',
                          label = ~as.character(round(D_SWE_OM, 4)),
                          labelOptions=labelOptions(noHide = T, 
                                                    direction='top', 
                                                    textOnly=TRUE,
                                                    textsize = '14px')) %>%
      addLayersControl(
        baseGroups=c('Dark', 'Nat Geo'),
        overlayGroups = c('SNODAS', 'Observations', 'FLINES', 'Observed SWE', 'True Zeros', 'Labels'),
        options=layersControlOptions(collapsed=FALSE)
      )
  })
  
  viirs_1d <- 'https://floods.ssec.wisc.edu/tiles/RIVER-FLDglobal-composite/'
  viirs_1d_end <- '/000000/{z}/{x}/{y}.png'
  observeEvent(input$get_viirs, {
    viirs_1d_url <- paste0(viirs_1d, format(as.Date(input$viirs_date), '%Y%m%d'), viirs_1d_end)
    leafletProxy('map') %>%
      addTiles(viirs_1d_url, group='VIIRS 1D') %>%
      addLayersControl(
        baseGroups = c('Dark', 'Nat Geo'), 
        overlayGroups = c('VIIRS 1D', 'MODIS', 'SNODAS', 'Assim Points','Observations', 'FLINES'),
        options = layersControlOptions(collapsed=FALSE))
    
  })
  
  observeEvent(input$get_modis, {
    leafletProxy('map') %>%
      addProviderTiles('NASAGIBS.ModisTerraTrueColorCR', options=providerTileOptions(time = format(as.Date(input$modis_date), '%Y-%m-%d')),group='MODIS') %>%
      addLayersControl(
        baseGroups = c('Dark', 'Nat Geo'),  
        overlayGroups = c('VIIRS 1D', 'MODIS', 'SNODAS', 'Assim Points','Observations', 'FLINES'),
        options = layersControlOptions(collapsed=FALSE))
    
  })

  
  output$errors <- renderPlot({
    points <- points_in_view() %>%
      filter(abs(D_SWE_OM) < 0.25)  %>%
      mutate(obs_type = case_when(
        TRUE_FLAG ==1 ~ 'SWE Obs',
        TRUE_FLAG == -1 ~ 'SD Obs'))
    
    if (input$measure_type == 'SWE'){
      filter_points <- points %>%
        filter(TRUE_FLAG == 1) %>%
        filter(MD_SWE > 0 & OB_SWE > 0) 
    } else if (input$measure_type == 'SD'){ 
      filter_points <- points %>%
        filter(TRUE_FLAG == -1) %>%
        filter(MD_DEPTH > 0 & OB_DEPTH > 0)
    } else {filter_points <- points}
    
    p1 <- ggplot(filter_points, aes(x=D_SWE_OM * 39.3701, y=obs_type)) +
      geom_violin(trim=FALSE) +
      title('Density of Errors by Station Type') +
      xlab('Error (Inches)') + 
      geom_vline(xintercept=0, color='green') +
      geom_vline(xintercept=1, color='red') +
      geom_vline(xintercept=-1, color='red') +
      ylab('Station Type') + coord_flip()
    
    regression <- lm(points_in_view()$MD_SWE ~ points_in_view()$OB_SWE)
    if (nrow(points_in_view()) == 0)
      return(NULL)
    
    points <- points_in_view() %>%
      filter(abs(D_SWE_OM) < 0.25)
    
    if (input$measure_type == 'SWE'){
      filter_points1 <- points %>%
        filter(TRUE_FLAG == 1) %>%
        mutate(ob_inches = OB_SWE * 39.3701) %>%
        mutate(md_inches = MD_SWE * 39.3701)
      
    } else if (input$measure_type == 'SD'){
      filter_points1 <- points %>%
        filter(TRUE_FLAG == -1) %>%
        mutate(ob_inches = OB_DEPTH * 39.3701) %>%
        mutate(md_inches = MD_DEPTH * 39.3701)
    } else {
      filter_points1 <- points %>%
        mutate(ob_inches = OB_SWE * 39.3701) %>%
        mutate(md_inches = MD_SWE * 39.3701)
    }
    
    p2 <- ggplot(filter_points1, aes(x=ob_inches, y=md_inches, color=station_type_agg)) +
      geom_point() +
      geom_abline(intercept=0, slope=1, color='red') + 
      labs(title = 'SNODAS Modeled SWE as a Function of Observed SWE', 
           x ='Observed SWE (Inches)',
           y= 'Modeled SWE (Inches)',
           subtitle=paste0('Coefficient of Determination: ',round(regression[[1]][[2]], 2))
      )
    
    grid.arrange(p1, p2, nrow=2)
    
  })
  graph_date_1 <- Sys.Date() - 5
  graph_date_2 <- Sys.Date() + 3
  
  url_generator <- function(graph_number, station_id){
    url <- paste0('https://www.nohrsc.noaa.gov/interactive/html/graph_only.php?w=600&h=400&by=', 
                  format(Sys.Date(), '%Y'), 
                  '&bm=', format(graph_date_1, '%m'),
                  '&bd=', format(graph_date_1, '%d'),
                  '&bh=06&ey=', format(graph_date_2, '%Y'),
                  '&em=', format(graph_date_2, '%m'),
                  '&ed=', format(graph_date_2, '%d'),
                  '&eh=06&data=', graph_number, '&region=us&units=1&station=', station_id)
    return(url)
  }
  
  graphs <- c(-1, -2, -3, -4, -5, -6, -7, -8)
  
  output$sno_depth <- renderUI({
    urls <- sapply(graphs, url_generator, station_id = input$map_marker_click$id)
    tags$div(
      tags$a(href=urls[[1]], tags$img(src = urls[[1]], width='100%')),
      tags$a(href=urls[[2]], tags$img(src = urls[[2]], width='100%')),
      tags$a(href=urls[[3]], tags$img(src = urls[[3]], width='100%')),
      tags$a(href=urls[[4]], tags$img(src = urls[[4]], width='100%')),
      tags$a(href=urls[[5]], tags$img(src = urls[[5]], width='100%')),
      tags$a(href=urls[[6]], tags$img(src = urls[[6]], width='100%')),
      tags$a(href=urls[[7]], tags$img(src = urls[[7]], width='100%')),
      tags$a(href=urls[[8]], tags$img(src = urls[[8]], width='100%'))
    )
  })
  
  
  point_table <- reactive({
    if (input$get_points == 0){
      bounds <- input$map_bounds
      assim_point_shp %>% filter(
        between(LONGITUDE, bounds$west, bounds$east),
        between(LATITUDE, bounds$south, bounds$north)) %>%
        select(STATION_ID, STATION_TY, TRUE_FLAG, OB_SWE, 
               MD_SWE, D_SWE_OM, OB_DEPTH, MD_DEPTH, OB_DENSITY, 
               MD_DENSITY)
    } else {
      bounds <- input$map_bounds
      selected_points$shp %>% filter(
        between(LONGITUDE, bounds$west, bounds$east),
        between(LATITUDE, bounds$south, bounds$north)) %>%
        select(STATION_ID, STATION_TY, TRUE_FLAG, OB_SWE, 
               MD_SWE, D_SWE_OM, OB_DEPTH, MD_DEPTH, OB_DENSITY, 
               MD_DENSITY)
    }
  })

  output$table1 <- DT::renderDataTable({
    
    DT::datatable(
      point_table(),
      options = list(
        autoWidth = FALSE, scrollX = TRUE)
    )
  })
  
  observeEvent(input$map_marker_click, {
    clickId <- input$map_marker_click$id
    dataTableProxy('table1') %>%
      selectRows(which(point_table()$STATION_ID == clickId)) %>%
      selectPage(which(input$table1_rows_all == which(point_table()$STATION_ID == clickId)) %/% input$table1_state$length + 1)
    
  })
  observeEvent(input$delete_row, {
    req(input$map_draw_new_feature)
    selected_points$shp <- selected_points$shp %>%
      filter(STATION_ID != input$map_marker_click$id)
    leafletProxy('map') %>%
      clearGroup('Assim Points') %>%
      addMarkers(data=selected_points$shp,
                 icon = ~pointIcons[delta],
                 popup = paste0('Station: ', selected_points$shp$STATION_ID, 
                                '</br>',  'Delta: ', 
                                round(selected_points$shp$D_SWE_OM * 39.3701, 2), '"',
                                '</br>Observed SWE: ', round(selected_points$shp$OB_SWE * 39.3701, 2), '"',
                                '</br>Observation Time: ', selected_points$shp$OB_SWE_T,
                                '</br><a href="https://www.nohrsc.noaa.gov/interactive/html/graph.html?units=0&region=us&station=', 
                                selected_points$shp$STATION_ID, 
                                '">Station Page on NOHRSC'),
                 layerId = ~STATION_ID,
                 group = 'Assim Points')
  })
})

