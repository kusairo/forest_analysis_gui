library(shiny)
library(shinydashboard)
source('general.R', local = TRUE)


# ---- header ui ----
header <- dashboardHeader(
  title = HTML('ALS・UAVのための森林解析 GUI'),
  disable = FALSE,
  titleWidth = 400,
  dropdownMenu(
    type = 'message',
    icon = icon('circle-info'),
    badgeStatus = 'primary',
    headerText = 'ライセンス',
    messageItem(
      from = 'lidR',
      message = '内部処理全般',
      icon = icon('github'),
      href = 'https://github.com/r-lidar/lidR'
    ),
    messageItem(
      from = 'terra',
      message = 'ラスターデータ処理全般',
      icon = icon('github'),
      href = 'https://github.com/rspatial/terra'
    ),
    messageItem(
      from = 'Shiny',
      message = 'UI全般',
      icon = icon('pager'),
      href = 'https://shiny.posit.co/'
    ),
    messageItem(
      from = 'shinydashboard',
      message = 'UIカスタマイズ',
      icon = icon('pager'),
      href = 'https://rstudio.github.io/shinydashboard/index.html'
    )
  )
)

# ---- sidebar ui ----
sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = 'sidebar',
    style = "position:fixed; overflow:visible",
    
    # -- sidebar menu
    menuItem('入力・処理設定',
      tabName = 'tab_settings',
      icon = icon('gear')
    ),
    menuItem('結果概要',
             tabName = 'tab_summary',
             icon = icon('newspaper')
    ),
    menuItem('地形モデル',
      tabName = 'tab_dtm',
      icon = icon('mound')
    ),
    menuItem('単木抽出結果',
      tabName = 'tab_itd',
      icon = icon('tree')
    ),
    menuItem('結果の出力', 
      tabName = 'tab_export',
      icon = icon('file-export')
    ),
    menuItem('使い方',
      tabName = 'tab_usage',
      icon = icon('question-circle')
    ),
    
    menuItem('ライセンス',
             tabName = 'tab_license',
             icon = icon('circle-info')
    )
  )
)

# ---- body ui ----
body <- dashboardBody(
  tags$head(
    tags$style(HTML('.gear { font-size: 20px; }')),
    tags$style(HTML('.mound { font-size: 20px; }')),
    tags$style(HTML('.tree { font-size: 20px; }')),
    tags$style(HTML('.newspaper { font-size: 20px; }')),
    tags$style(HTML('.question-circle { font-size: 20px; }')),
    tags$style(HTML(".shiny-output-error { visibility: hidden; }")),
    tags$style(HTML(".shiny-output-error:before { visibility: hidden; }")),
    
    tags$style(HTML(''))
  ),
  tags$script(HTML("$('body').addClass('fixed');")),
  
  tabItems(
    # ---- main settings ui ----
    tabItem(tabName = 'tab_settings',
      h1("入力・処理設定"),
      h2('0. ガイド'),
      
      # ---- first guide ----
      h2("1. 処理とデータの選択"),
      p("利用可能なデータの種類を選択したのち，処理の方法を指定してください。"),
      fluidRow(
        column(width = 8,
          box(
            title = HTML('処理に利用するデータの種類'),
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            radioButtons('in_SourceType', label = NULL, choices = list("点群" = 'PCD', "ラスター（数値形状モデル）" = 'RASTER'), selected = 'PCD', inline = TRUE),
            checkboxInput('in_Normalized', label = "データはすでに正規化されています。（Z値から地面高は除去されています。CHM等。）", value = FALSE)
          ),
          box(
            title = HTML('データの正規化方法（地面高の参照方法）'),
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            uiOutput('ui_NormarizeMethods'),
          )
        ),
        column(width = 4,
          box(
            title = HTML('必要データ・注意事項'),
            width = NULL,
            status = 'warning',
            solidHeader = TRUE,
            uiOutput('ui_ProcessSummary')
          )
        )
,
      ),

      # ---- file input ----
      h2("2. 入力データの選択"),
      p('必要な入力データを選択します。本システムはデータを一時ファイルとしてコピーします。データの選択後，読み込みが終了するまでお待ちください。'),
      fluidRow(
        uiOutput('ui_MainData'),
        uiOutput('ui_SubData')
      ),

      # ---- option ----
      h2("3. 処理パラメータの調整（オプション）"),
      p('各処理に係るパラメータを調整できます。本システムのアルゴリズムは全て点群解析ライブラリ「lidR」に依存しています。詳しくは，「lidR」のドキュメントをご覧ください。'),
      fluidRow(
        column(width = 4,
          # ---- itd settings ----
          box(
            title = span(icon('tree'),'単木抽出'),
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            radioButtons(inputId = 'in_ItdMethod',
              label = '樹頂点検出手法',
              choices = list('固定サイズLocal Maximum Filter法' = 'lmf_fixed'
                             # 'Local Maximum Filter with variable windows' = 'lmf_variable'
                             ),
              selected = 'lmf_fixed'
              ),
            tags$hr(),
            sliderInput(inputId = "in_LmsFixedWs", label = 'ウィンドウサイズ[m]', min = 0, max = 5, value = 3, step = 0.1)
          ),
          
          box(
            title = span(icon('burst'), '樹冠抽出'),
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            radioButtons(inputId = 'in_SegmentMethod',
              label = '樹冠抽出アルゴリズム',
              choices = list('Dalponte et al.(2016)' = 'dalponte2016'
                             # 'Li et al.(2012)' = 'li2012'
              ),
              selected = 'dalponte2016'
            )
          ),
        ),
        
        column(width = 4,
          # ---- ground settings ----
          box(
            title = span(icon('magnifying-glass'), '地上点分類'),
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            radioButtons(inputId = 'in_ClassifyGroundMethod',
                         label = '地上点分類法',
                         choices = list('Cloth Simulation Function' = 'CSF'
                                        # 'Progressive Morphological Filter' = 'PMF',
                                        # 'Multiscale Curvature Classification' = 'MCC'
                                        ),
                         selected = 'CSF'
                         ),
            tags$hr(),
            checkboxInput(inputId = 'in_CsfSmooth', label = 'sloop smooth', value = FALSE),
            sliderInput(inputId = 'in_CsfThreshold', label = 'class threshold', min = 0, max = 2, value = 0.5, step = 0.1),
            sliderInput(inputId = 'in_CsfResolution', label = 'cloth resolution', min = 0, max = 2, value = 0.15, step = 0.01),
            sliderInput(inputId = 'in_CsfTimeStep', label = 'time step', min = 0, max = 2, value = 0.65, step = 0.01)
          )
        ),
        column(width = 4,
          # ---- dtm settings ----
          box(
            title = span(icon('mound'), 'DSM・DTM生成'),
            width = NULL,
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            radioButtons(inputId = 'in_DtmComplement',
                         label = '補完法',
                         choices = list('Triangular Irregular Network' = 'TIN'
                                        # 'Invert Distance Weighting' = 'IDW',
                                        # 'Kriging' = 'Kriging'
                         ),
                         selected = 'TIN'
            ),
            tags$hr(),
            sliderInput(inputId = 'in_DtmResolution', label = '解像度[cm/pix]', min = 0, max = 2, value = 0.33, step = 0.01),
          )
        )
      ),

      # ---- control button ----
      actionButton('in_execute', label = '実行', icon = icon('play')),
      actionButton('in_refresh', label = 'リセット', icon = icon('rotate')),
      fluidRow(
        box(
          title = 'コマンド',
          width = NULL,
          solidHeader = TRUE,
          textOutput('ui_process_start')
        )
      )
    ),
    
    # ---- summary ui ----
    tabItem(tabName = 'tab_summary',
            h1('結果概要'),
    ),

    # ---- dtm ui ----
    tabItem(tabName = 'tab_dtm',
      h1('Digital Terrain Models'),
      fluidRow(
        column(width = 6,
          box(width = NULL,
            title = 'DTM plot',
            textOutput(outputId = 'test_text'),
            
            plotOutput(outputId = 'dtmplot')
          )
        ),
        column(width = 6,
          box(width = NULL,
            title = 'generate metrics',
            htmlOutput(outputId = 'dtm_metrics')
            # textOutput(outputId = 'dtm_metrics')
          ),
          box(width = NULL,
            title = 'DTM Information',
            htmlOutput(outputId = 'dtm_info')
          )
        )
      )
    ),

    # ---- itd ui ----
    tabItem(tabName = 'tab_itd',
            h1('Individual Tree Detection'),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = 'ITD plot',
                         plotOutput(outputId = 'itdplot')),
                     box(width = NULL,
                         title = 'Crown plot',
                         plotOutput(outputId = 'crownplot'))
              ),
              column(width = 6,
                     box(width = NULL,
                         title = 'tree information',
                         tableOutput(outputId = 'treeinfo'))
              )
            )
    ),

    # ---- export ui ----
    tabItem(tabName = 'tab_export',
            h1('出力'),
            h2('出力の注意'),
            p(),
            downloadButton("export_dtm", 
                           label = "Download DTM"),
            downloadButton("export_chm", 
                           label = "Download CHM"),
            downloadButton("export_ttops", 
                           label = "Download TreeTops"),
            downloadButton("export_crown", 
                           label = "Download Crown")
    ),

    # ---- usage ui ----
    tabItem(tabName = 'tab_usage',
            h1('使い方'),
    ),

    # ---- license ui ----
    tabItem(tabName = 'tab_license',
            h1('ライセンス'),
    )
  )
)




# ---- ui construction ----
ui <- dashboardPage(
  header = header, 
  sidebar = sidebar, 
  body = body
)

# ---- last of shiny ui ----