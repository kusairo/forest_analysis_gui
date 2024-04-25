#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lidR)
library(terra)
library(colorspace)
library(sf)
library(leaflet)


source("function.R", local = TRUE)
source('general.R', local = TRUE)


options(shiny.maxRequestSize = 5000*1024^2)
options(lidR.raster.default = "terra")
set_lidr_threads(0)

shinyServer(function(input, output, session) {
# ----**refresh application**----
  observeEvent(input$in_refresh, {
    if (input$in_refresh==1){
      session$reload()
    }
  })
  
# ----**UI OUTPUT**----
  # ---- main settings ----
  output$ui_NormarizeMethods <- renderUI({
    if (input$in_Normalized){
      radioButtons('in_NormarizeMethods', label = NULL, choices = list("正規化しない" = "NONORM"), selected = "NONORM", inline=TRUE)
    }
    else{
      radioButtons('in_NormarizeMethods', label = NULL, choices = list("追加のDTMを参照" = 'DTM', "点群から地面を分類して参照（ALS点群のみ利用可）" = "CLASSIFY", "正規化しない" = "NONORM"), selected = "NONORM", inline=TRUE)
    }
  })
  
  output$ui_ProcessSummary <- renderText({
    switch(input$in_SourceType,
      'PCD' = {
        if (input$in_Normalized){
          '点群データ（las/laz）のみ必要です。「樹高」も利用可能です。'
        }
        else{
          switch (input$in_NormarizeMethods,
            'DTM' = '点群データ（las/laz）のほかに，地面高を示すラスターデータ（asc/tif/tiff）が必要です。それぞれの位置座標は統一されている必要があります。「樹高」も利用可能です。',
            'CLASSIFY' = '点群データ（las/laz）のみ必要です。「樹高」も利用可能です。',
            'NONORM' = '点群データ（las/laz）のみ必要です。「樹高」は計算されません。「立木位置」等のみ利用可能です。'
          )
        }
      },
      'RASTER' = {
        if (input$in_Normalized){
          'ラスターデータ（asc/tif/tiff）のみ利用可能です。「樹高」も利用可能です。'
        }
        else{
          switch (input$in_NormarizeMethods,
            'DTM' = '地表高を示すラスターデータ（asc/tif/tiff）のほかに，地面高を示すラスターデータ（asc/tif/tiff）が必要です。それぞれの位置座標は統一されている必要があります。「樹高」も利用可能です。',
            'CLASSIFY' = '処理を実行できません。地上点分類はALS点群に対してのみ有効です。',
            'NONORM' = 'ラスターデータ（asc/tif/tiff）のみ利用可能です。「樹高」は計算されません。「立木位置」等のみ利用可能です。'
          )
        }
      }
    )
  })
  
  output$ui_MainData <- renderUI({
    switch (input$in_SourceType,
      'PCD' = {
        box(
          title = span(icon('file'),'点群ファイル（las/laz）'),
          status = 'primary',
          solidHeader = TRUE,
          fileInput('in_LasFile', label = NULL, accept = c('.las', '.laz')),
          numericInput('in_LasCrs', label = '座標系の指定（EPSG）', min = 0, step = 1, value = 6675)
        )
      },
      'RASTER' = {
        box(
          title = span(icon('file'),'ラスターデータ（asc/tif/tiff）'),
          status = 'primary',
          solidHeader = TRUE,
          fileInput('in_DsmFile', label = NULL, accept = c('.asc', '.tif', '.tiff')),
          numericInput('in_DsmCrs', label = '座標系の指定（EPSG）', min = 0, step = 1, value = 6675)
        )
      }
    )
  })
  
  output$ui_SubData <- renderUI({
    if (!input$in_Normalized && input$in_NormarizeMethods == 'DTM'){
      box(
        title = span(icon('file'),'地面高のラスターデータ（asc/tif/tiff）'),
        status = 'primary',
        solidHeader = TRUE,
        fileInput('in_DtmFile', label = NULL, accept = c('.asc', '.tif', '.tiff')),
        numericInput('in_DtmCrs', label = '座標系の指定（EPSG）', min = 0, step = 1, value = 6675)
      )
    }
  })

  # ---- result summary ----
  
  
# ----**MAIN PROCESS**----
  # mainInput <- eventReactive(input$execute, {
  #   
  #   input$las_file
  # })
  # dtmInput <- eventReactive(input$execute, {
  #   input$dtm_file
  # })
  # las <- eventReactive(input$execute, {readLAS(lasInput()$datapath)})
  # 
  # dtm <- 
  # if (class(dtm) == "try-error") {
  #   
  # }
  # dtm <- eventReactive(input$execute, {
  #   t <- try(rast(dtmInput()$datapath))
  #   
  #   if (class(t) == "try-error") {
  #     t <- generate_dtm(las = las(), 
  #                  res = as.double(input$tin_res), 
  #                  sloop_smooth = input$csf_ss,
  #                  class_threshold = as.double(input$csf_ct),
  #                  cloth_resolution = as.double(input$csf_cr),
  #                  time_step = as.double(input$csf_ts),
  #                  crs = input$las_crs)}
  #   
  #   return(t)
  #   })
  # chm <- eventReactive(input$execute, {generate_chm(las = las(), 
  #                                                   dtm = dtm(), 
  #                                                   res = as.double(input$tin_res), 
  #                                                   crs = input$las_crs)})
  # itd <- eventReactive(input$execute, {individual_tree_detection(chm = chm(), 
  #                                                                ws = as.double(input$lmf_fixed_ws),
  #                                                                crs = input$las_crs)})

  
  # ---- process ----
  results <- eventReactive(input$in_execute, {
    
    name <- NA
    las <- NA
    chm <- NA
    dsm <- NA
    dtm <- NA
    ttops <- NA
    crowns <- NA
    
    output$ui_process <- renderText({paste0('PROCESS START')})
    
    print('start')
    switch(input$in_SourceType,
           'PCD' = {
             print(input$in_LasFile$datapath)
             las <- readLAS(input$in_LasFile$datapath)
             name <- input$in_LasFile$name
             
             if (input$in_Normalized){
               dsm <- generate_dsm(las = las, res = 0.33, crs = 6675) #!!!!
               chm <- dsm
               ttops <- detect_trees(chm_dsm = chm, ws = 3, crs = 6675)
               crowns <- segment_crowns(chm_dsm = chm, ttops = ttops, crs = 6675)
               print(paste0('PCD', 'normed'))
             }
             else{
               dsm <- generate_dsm(las = las, res = 0.33, crs = 6675) #!!!!
               switch (input$in_NormarizeMethods,
                       'DTM' = {
                         print(input$in_DtmFile$datapath)
                         dtm <- rast(input$in_DtmFile$datapath)
                         
                         chm <- calc_differ(dsm, dtm)
                         ttops <- detect_trees(chm_dsm = chm, ws = 3, crs = 6675)
                         crowns <- segment_crowns(chm_dsm = chm, ttops = ttops, crs = 6675)
                         print(paste0('PCD', 'dtm'))
                       },
                       'CLASSIFY' = {
                         dtm <- generate_dtm(las = las, res = 0.33, crs = 6675) #!!!!
                         chm <- calc_differ(dsm, dtm)
                         ttops <- detect_trees(chm_dsm = chm, ws = 3, crs = 6675)
                         crowns <- segment_crowns(chm_dsm = chm, ttops = ttops, crs = 6675)
                         print(paste0('PCD', 'classify'))
                       },
                       'NONORM' = {
                         ttops <- detect_trees(chm_dsm = dsm, ws = 3, crs = 6675)
                         crowns <- segment_crowns(chm_dsm = dsm, ttops = ttops, crs = 6675)
                         print(paste0('PCD', 'no_norm'))
                         
                       }
               )
             }
           },
           
           'RASTER' = {
             if (input$in_Normalized){
               dsm <- rast(input$in_DsmFile$datapath)
               chm <- dsm
               name <- input$in_DsmFile$name
               ttops <- detect_trees(chm_dsm = chm, ws = 3, crs = 6675)
               crowns <- segment_crowns(chm_dsm = chm, ttops = ttops, crs = 6675)
               print(paste0('RASTER', 'normed'))
             }
             else{
               dsm <- rast(input$in_DsmFile$datapath)
               name <- input$in_DsmFile$name
               switch (input$in_NormarizeMethods,
                       'DTM' = {
                         dtm <- rast(input$in_DtmFile$datapath)
                         chm <- calc_differ(dsm, dtm)
                         ttops <- detect_trees(chm_dsm = chm, ws = 3, crs = 6675)
                         crowns <- segment_crowns(chm_dsm = chm, ttops = ttops, crs = 6675)
                         print(paste0('RASTER', 'dtm'))
                         
                       },
                       'CLASSIFY' = {
                         output$ui_process_start <- renderText('This process is valid!')
                         print(paste0('RASTER', 'classify'))
                         
                       },
                       'NONORM' = {
                         ttops <- detect_trees(chm_dsm = dsm, ws = 3, crs = 6675)
                         crowns <- segment_crowns(chm_dsm = dsm, ttops = ttops, crs = 6675)
                         print(paste0('RASTER', 'nonorm'))
                         
                       }
               )
             }
           }
    )
    
    # post-process for results
    # ttops$geometry <- st_multipoint(st_coordinates(ttops$geometry), dim = 'XYZ')
    
    print('finish')
    return (
      list(name = name, 
           las = las, 
           chm = chm, 
           dsm = dsm, 
           dtm = dtm, 
           ttops = ttops, 
           crowns = crowns,
           xmin = xmin(dsm),
           ymin = ymin(dsm),
           xmax = xmax(dsm),
           ymax = ymax(dsm))
      )
    })
  output$ui_process <- renderText({paste0('FINISHED: ', results()$name)})
  

  # ----**render UI**----

  # output$pcdname <- renderText({paste0(lasInput()$name, las()@header$`File Signature`)})
  
  # output$crossplot <- renderPlot({
  #   plot_crossection(las(), colour_by = factor(Classification))
  # })

  
  output$dtmplot <- renderPlot({plot(results()$dtm)})
  output$dtm_metrics <- renderUI(tags$ul(
    tags$li(paste0('CSR=EPSG: ', input$in_LasCrs)),
    tags$li(paste0('resolution[m]: ', input$in_DtmResolution)),
    tags$li(paste0('ground classification: ', input$in_ClassifyGroundMethod)),
    tags$li(paste0('complement: ', input$in_DtmComplement))
  ))
  output$dtm_info <- renderUI(tags$ul(
    tags$li(paste0(''))
  ))
  
  # ---- dtm results ----
  output$ui_dtmmap <- renderLeaflet({
    contour_dtm <- project(as.contour(results()$dtm, nlevels = 10), 'epsg:4326')
    leaflet(options = leafletOptions(minZoom = 14, maxZoom = 22, wheelPxPerZoomLevel = 250)) %>%
      addTiles(urlTemplate = 'https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png',
               attribution = "<a href='https://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>",
               options = tileOptions(minZoom = 14, maxZoom = 22, maxNativeZoom=18, minNativeZoom=0)
      ) %>%
      addPolylines(data = contour_dtm, color = 'red', weight = 2)
  })
  
  
  # ---- itd results ----
  output$ui_itdmap <- renderLeaflet({
    poly_crown <- project(as.polygons(results()$crowns), 'epsg:4326')
    leaflet(options = leafletOptions(minZoom = 14, maxZoom = 22, wheelPxPerZoomLevel = 250)) %>%
      addTiles(urlTemplate = 'https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png',
               attribution = "<a href='https://maps.gsi.go.jp/development/ichiran.html' target='_blank'>地理院タイル</a>",
               options = tileOptions(minZoom = 14, maxZoom = 22, maxNativeZoom=18, minNativeZoom=0)
      ) %>%
      addPolygons(data = poly_crown, color = 'red', fill = TRUE, fillOpacity = 0, weight = 2)
    # addRasterImage(as.contour(results()$dtm))
      # addMarkers(data = itd_points())
  })

  
  # output$itdplot <- renderPlot({
  #   if (~is.na(results()$chm)) {
  #     plot(results()$chm)
  #   }
  #   else {
  #     plot(results()$dsm)
  #   }
  #   plot(sf::st_geometry(results()$ttops), add = TRUE, pch = 3)
  # })
  # output$crownplot <- renderPlot({
  #   plot(results()$crowns, col = pastel.colors(200))
  #   plot(sf::st_geometry(results()$ttops), add = TRUE, pch = 3)
  # })


  # ----**DOWNLOAD OUTPUTS**----
  output$export_dtm <- downloadHandler(
    filename = function() {
      paste0(gsub("\\..+$", "", results()$name), '-dtm.tif')
    },
    content = function(file) {
      writeRaster(results()$dtm, file, overwrite=TRUE)
    }
  )
  output$export_chm <- downloadHandler(
    filename = function() {
      paste0(gsub("\\..+$", "", results()$name), '-chm.tif')
    },
    content = function(file) {
      writeRaster(results()$chm, file, overwrite=TRUE)
    }
  )
  output$export_ttops <- downloadHandler(
    filename = function() {
      paste0(gsub("\\..+$", "", results()$name), '-ttops.csv')
    },
    content = function(file) {
      st_write(st_zm(results()$ttops), file, layer_options = "GEOMETRY=AS_XY", append=FALSE)
    }
  )
  output$export_crown <- downloadHandler(
    filename = function() {
      paste0(gsub("\\..+$", "", results()$name), '-crown.tif')
    },
    content = function(file) {
      writeRaster(results()$crowns, file, overwrite=TRUE)    
    }
  )
  
# ----last of shiny server----
})