# Libraries ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyquant)
library(tidyverse)
library(modeltime)
library(timetk)
library(modeltime.ensemble)

library(bslib)
library(thematic)


# for reactable table

library(reactable)
library(reactablefmtr)
library(MetBrewer)
library(crosstalk)
library(htmltools)


# Sourcing info_card.R ----------------------------------------------------

source("00_scripts/info_card.r")

# Theme w/ {bslib} & {thematic} -------------------------------------------

shinyOptions(bootstrapLib = TRUE)
thematic::thematic_shiny(font = "auto")

light <- bslib::bs_theme(
  version        = 4,
  bootswatch     = "flatly",
  fg             = "#000000",
  bg             = "white",
  base_font      = font_google("Acme"),
  heading_font   = font_google("Acme"),
  #Bangers
  font_scale     = 0.8,
  primary        = "#466060",
  secondary      = "#81A684",
  success        = "#0E0F19",
  danger         = "#A2CD48",
  warning        = "#0E0F19",
  info           = "#57886C"
)

# Sourcing  ---------------------------------------------------------------

source("00_scripts/rename_models.R")

# Data From Forecast ------------------------------------------------------

inverted_forecast_submodels_2_tbl <-
  read_rds("01_data_for_shiny/inverted_forecast_submodels_2_tbl.rds") %>%
  rename_models()

submodels_2_summary_metrics <-
  read_rds("01_data_for_shiny/submodels_2_inversion_test_summary_metrics_tbl.rds") %>%
  rename_models()

three_models_future_forecast_data <-
  read_rds("01_data_for_shiny/rf_svm_ensemble_fit_tbl.rds") %>%
  rename_models()

test_residuals <-
  read_rds("01_data_for_shiny/test_residuals.rds") %>%
  rename_models()

train_residuals <-
  read_rds("01_data_for_shiny/train_residuals.rds") %>%
  rename_models()

resamples_tscv <-
  read_rds("01_data_for_shiny/resamples_tscv_plan.rds")



# Table Formatting --------------------------------------------------------

resampling_summary_metrics <-
  readRDS("01_data_for_shiny/model_tbl_tuned_resample_accuracy.rds") %>%
  rename_models() %>%
  select(.model_desc, mae, mape, mase, smape, rmse, rsq) %>%
  arrange(rmse) %>%
  mutate(across(.cols = mae:rsq, .fns = ~ round(., 3))) %>% 
  mutate(rsq = case_when(rsq == NA ~ "NA",
                        TRUE ~ as.character(rsq)))

## *Table  ------------------------------------------------------------------

crosstalk_data <- SharedData$new(resampling_summary_metrics)

rmse_slider <- filter_slider(
  id = "rmse",
  label = "RMSE",
  column = ~ rmse,
  step = 0.05,
  ticks = FALSE,
  sharedData = crosstalk_data)
model_filter <- filter_select(
  id = "models",
  label = "Models",
  group = ~ .model_desc,
  sharedData = crosstalk_data)
rsq_slider <- filter_slider(
  id = "mae",
  label = "MAE",
  column = ~ mae,
  sharedData = crosstalk_data)


## *Theme for Reactable -----------------------------------------------------

pal <- met.brewer(name = "VanGogh3")


## * Table Itself ----------------------------------------------------------

tbl <- htmltools::browsable(
  tagList(
    tags$br(),
    br(),
    reactable(crosstalk_data, 
              compact = TRUE,
              showSortIcon = FALSE,
              pagination = FALSE,
              height = 300,
              columns = list(
                .model_desc = colDef(
                  name = "Models",
                  minWidth = 100
                ),
                mae = colDef(
                  name = "MAE",
                  defaultSortOrder = "asc",
                  minWidth = 60
                ),
                mape = colDef(
                  name = "MAPE",
                  defaultSortOrder = "asc",
                  minWidth = 60
                ),
                mase = colDef(
                  name = "MASE",
                  defaultSortOrder = "asc",
                  minWidth = 60
                ),
                smape = colDef(
                  name = "SMAPE",
                  defaultSortOrder = "desc"
                ),
                rmse = colDef(
                  name = "RMSE",
                  defaultSortOrder = "asc",
                  minWidth = 60,
                  align = "center",
                  cell = color_tiles(resampling_summary_metrics, pal)
                ),
                rsq = colDef(
                  name = "RSQ",
                  defaultSortOrder = "desc",
                  minWidth = 60
                )
              ))))



# UI ----------------------------------------------------------------------


ui <- navbarPage(
  title = h3("Electricity Demand & Generation Forecast"),
  inverse = FALSE,
  collapsible = TRUE,
  theme = light,
  
  
  # * CSS -------------------------------------------------------------------
  
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tags$body(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  shinyjs::useShinyjs(),
  
  # * Tab Panel 1 -----------------------------------------------------------
  
  
  tabPanel(
    title = h4("Fit on test set"),
    
    div(class = "container-fluid",
        div(
          class = "header text-left",
          id    = "acc_metrics",
          h3(
            HTML(
              "<b style = \"padding-left: 20px; color: #466060;\">Accuracy Metrics</b>"
            )
          )
        )),
    
    div(
      class = "container-fluid",
      fluidRow(
        # class = "container-fluid",
        id    = "favorite_cards",
        

# ** Metric Boxes ---------------------------------------------------------

        
        column(
          width = 3,
          info_card(
            title     = "RMSE",
            bg_color = "secondary",
            text_color = "white",
            value     = verbatimTextOutput(outputId = "rmse_metric"),
            main_icon = "wolf-pack-battalion"
          )
        ),
        
        
        column(
          width = 3,
          info_card(
            title     = "MAE",
            bg_color = "info",
            text_color = "white",
            value     = verbatimTextOutput(outputId = "mae_metric"),
            main_icon = "spider"
          )
        ),
        
        
        column(
          width = 3,
          info_card(
            title     = "RSQ",
            bg_color = "primary",
            text_color = "white",
            value     = verbatimTextOutput(outputId = "rsq_metric")
          )
        ),
        
        column(
          width = 3,
          info_card(
            title     = "MAPE",
            bg_color = "warning",
            text_color = "white",
            value     = verbatimTextOutput(outputId = "mape_metric"),
            main_icon = "percent"
          )
        )
      ),
      br(),

# ** Input Boxes ----------------------------------------------------------
      div(class = "container-fluid",
          id    = "application_ui",
          fluidRow(
            column(width = 3,
                   wellPanel(
                     div(
                       id = "main_input",
                       pickerInput(
                         inputId = "models_tab_1",
                         label   = h3("Models"),
                         choices = c(
                           "RANGER - Tuned",
                           "RANGER",
                           "XGBOOST",
                           "NNET",
                           "EARTH",
                           "PROPHET - Tuned",
                           "KERNLAB - Tuned",
                           "PROPHET",
                           "PROPHET W/ XGBOOST",
                           "KERNLAB"
                         ),
                         multiple = FALSE,
                         selected = "RANGER - Tuned",
                         option   = pickerOptions(
                           actionsBox = FALSE,
                           liveSearch = TRUE,
                           size       = 5
                         )
                       )
                     ),
                     pickerInput(
                       inputId = "demand_or_generation",
                       label   = h3("Demand / Generation"),
                       choices = c("Demand",
                                   "Generation"),
                       multiple = FALSE,
                       selected = "Demand",
                       option   = pickerOptions(
                         actionsBox = FALSE,
                         liveSearch = TRUE,
                         size       = 2
                       )
                     ),
                     div(
                       id = "input_buttons",
                       actionButton(
                         inputId = "apply",
                         label = "Apply",
                         icon = icon("play")
                       ),
                       div(
                         class = "pull-right",
                         actionButton(
                           inputId = "reset",
                           label = "Reset",
                           icon = icon("sync")
                         )
                       )
                     )
                   ),
                   
                   br(),
                   br(), ),
            column(width = 9,
                   div(
                     class = "panel",
                     div(
                       class = "panel-body",
                       style = "padding: 20px;",
                       plotlyOutput(outputId = "plot_forecast"),
                     )
                   ))
          )),
      br(),
      br(),
      hr()
    )
  ),
  
  
  # * Tab Panel 2 -----------------------------------------------------------
  
  
  tabPanel(
    title = h4("Resampling Insights"),
    div(class = "container-fluid",
        div(class = "header text-left",
            
            ## ** Header for Tab Panel 2 ----
            h3(
              HTML(
                "<b style = \"padding-left: 15px; color: #466060;\">Resampling Plan</b>"
              )
            ),
            p(class = "lead",
              HTML("<b style = \"padding-left: 20px; color: #466060;\">Models' Durability Test</b>")
            )
          )
        ),
    div(
      class = "panel-body",
        ## ** Plot for Tab Panel 2 ----
        plotlyOutput(outputId = "resamples_tscv_plan_plot"),
      
      
      ## ** Table for Tab Panel 2 ----
      div(
        id = "table",
        div(class = "container-fluid",
            br(),
            div(class = "table-title", h3(HTML("<b style = \"color: #466060;\">Resampled Model Accuracy</b>"))),
            br(),
            div(
              bscols(
                widths = c(3, 3, 3),
                model_filter,
                rmse_slider,
                rsq_slider),
            div(class = "lead",
                "Resampling strategy allows us to see how our models perform over time. We can see", code("RANGER"), "tuned and untuned, and", code("KERNLAB"), "tuned and untuned perform great on the resampling test sets compared to other models. 
                We can also observe some models like", code("PROPHET"),  "that perform great on", code("test_split"), "on", code("Fit on test set"),
                "tab drop their performance when applied resampling strategy.")),
            br(),
            tbl,
            br())
      )
    )
  ),
  
  # * Tab Panel 3 -----------------------------------------------------------
  
  
  tabPanel(
    title = h4("Forecast"),
    div(
      class = "container-fluid",
      div(class = "header text-left",
          id    = "text_tab_3",
          
          ## ** Header for Tab Panel 3 ----
          h3(
            HTML(
              "<b style = \"padding-left: 15px; color: #466060;\">Top 3 Models Forecast</b>"
            )
          ),
          p(
            class = "lead",
            HTML("<b style = \"padding-left: 15px; color: #466060;\">Ensemble Average is not always the best approach.
                 The base RANGER model seem to be a better forecasting approach. </b>")
          ),
          p(class = "lead",
            HTML("<b style = \"padding-left: 15px; color: #466060;\">Ensemble Average was performed on base RANGER and KERNLAB models.</b>")
          ),
          p(class = "lead",
            HTML("<b style = \"padding-left: 15px; color: #466060;\">Post-Forecast Diagnostics on the base RANGER model
                 clearly implies lag 14 and lag 30 were not properly picked up.</b>")
            )
          ),
      br(),
      fluidRow(
        column(
          width = 3,
          wellPanel(
            
            ## ** Inputs for Tab Panel 3 ----
            pickerInput(
              inputId = "models_tab_3",
              label   = h3("Models"),
              choices = c("RANGER",
                          "KERNLAB",
                          "ENSEMBLE"),
              multiple = FALSE,
              selected = "RANGER",
              option   = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                size       = 3
              )
            )
            ,
            pickerInput(
              inputId = "demand_or_generation_tab_3",
              label   = h3("Demand / Generation"),
              choices = c("Demand",
                          "Generation"),
              multiple = FALSE,
              selected = "Demand",
              option   = pickerOptions(
                actionsBox = FALSE,
                liveSearch = TRUE,
                size       = 2
              )
            ),
            div(
              id = "input_buttons",
              actionButton(
                inputId = "apply_tab_3",
                label = "Apply",
                icon = icon("play")
              ),
              div(
                class = "pull-right",
                actionButton(
                  inputId = "reset_tab_3",
                  label = "Reset",
                  icon = icon("sync")
                )
              )
            )
          ),
          
          br(),
          br(),
        ),
        column(width = 9,
               div(
                 class = "panel",
                 div(
                   class = "panel-body",
                   style = "padding: 20px;",
                   plotlyOutput(outputId = "plot_ensemble_forecast"),
                 )
               ))
      ),
    ),
  ),
  
  
  
  
  
  
  
  tabPanel(
    # * TabPanel 4 ------------------------------------------------------------
    
    
    
    title = h4("Post-Forecast Diagnostics"),
    
    div(class = "container-fluid",
        id    = "application_ui",
        fluidRow(
          column(
            width = 3,
            wellPanel(div(
              id = "main_input",
              pickerInput(
                inputId = "models_tab_4",
                label   = h3("Models"),
                choices = c(
                  "PROPHET - Tuned",
                  "RANGER - Tuned",
                  "KERNLAB - Tuned",
                  "PROPHET",
                  "XGBOOST",
                  "PROPHET W/ XGBOOST",
                  "KERNLAB",
                  "RANGER",
                  "NNET",
                  "EARTH",
                  "ENSEMBLE"
                ),
                multiple = FALSE,
                selected = "PROPHET - Tuned",
                option   = pickerOptions(
                  actionsBox = FALSE,
                  liveSearch = TRUE,
                  size       = 5
                )
              ),
              pickerInput(
                inputId  = "demand_or_generation_tab_4",
                label    = h3("Demand / Generation"),
                choices  = c("Demand", "Generation"),
                multiple = FALSE,
                selected = "Demand",
                option   = pickerOptions(
                  actionsBox = FALSE,
                  liveSearch = TRUE,
                  size       = 2
                )
              ),
              div(
                id = "input_buttons",
                actionButton(
                  inputId = "apply_tab_4",
                  label = "Apply",
                  icon = icon("play")
                ),
                div(
                  class = "pull-right",
                  actionButton(
                    inputId = "reset_tab_4",
                    label = "Reset",
                    icon = icon("sync")
                  )
                ),
                
              )
            )),
            
            # Line breaks ----
            br(),
            br(),
            
            # div(
            #   class = "well",
            #   h3("About the Data"),
            #   id = "lorem_ipsum",
            #   p(
            #     tags$small(
            #       "The data comes from",
            #       code("USgas"),
            #       "R package. It provides an overview of demand
            #     for natural gas in the US in a time-series format.",
            #       a(
            #         class = "btn btn-secondary btn-sm",
            #         href = "https://github.com/RamiKrispin/USgas",
            #         target = "_blank",
            #         "Learn More"
            #       )
            #     )
            #   ),
            # )
          ),
          column(width = 9,
                 div(
                   class = "panel",
                   div(
                     class = "panel-body",
                     style = "padding: 20px;",
                     tabsetPanel(
                       type = "pills",
                       
                       tabPanel(title = "In-Sample Residuals Plot",
                                plotlyOutput(outputId = "plotly_residuals_in_sample")),
                       
                       tabPanel(
                         title = "Out-of-Sample Residuals Plot",
                         plotlyOutput(outputId = "plotly_residuals_out_of_sample")
                       ),
                       
                       tabPanel(
                         title = "In-Sample Residuals ACF Plot",
                         plotlyOutput(outputId = "plotly_residuals_acf_in_sample")
                       ),
                       
                       tabPanel(
                         title = "Out-of-Sample Residuals ACF Plot",
                         plotlyOutput(outputId = "plotly_residuals_acf_out_of_sample")
                       )
                     )
                   )
                   
                 ))
        ))
  ),
  
  div(style = "height:50px;")
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # bs_themer()
  
  

# Server - Tab Panel 1 ---------------------------------------------------

  
  observeEvent(eventExpr = input$reset, handlerExpr = {
    updatePickerInput(session = session,
                      inputId = "models_tab_1",
                      selected = "RANGER - Tuned")
    
    updatePickerInput(session = session,
                      inputId = "demand_or_geneation",
                      selected = "DEMAND")
    
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply")
    })
    
  })
  
  
  ## ** Data Reactivity for Tab Panel 1 ---------------------------------------
  
  models_selection_reactive <-
    eventReactive(eventExpr = input$apply, {
      inverted_forecast_submodels_2_tbl %>%
        filter(.model_desc %in% c(input$models_tab_1, "ACTUAL")) %>%
        filter(type == input$demand_or_generation %>% tolower())
      
    }, ignoreNULL = FALSE)
  
  
  
  ## ** Metrics Reactivity | Tab Panel 1 --------------------------------------
  
  
  rmse_metric <-
    eventReactive(eventExpr = input$apply, {
      submodels_2_summary_metrics %>%
        filter(.model_desc == input$models_tab_1) %>%
        filter(type == input$demand_or_generation %>% tolower()) %>%
        select(rmse) %>%
        mutate(rmse = rmse %>% scales::comma())
      
    }, ignoreNULL = FALSE)
  
  mae_metric <-
    eventReactive(eventExpr = input$apply, {
      submodels_2_summary_metrics %>%
        filter(.model_desc == input$models_tab_1) %>%
        filter(type == input$demand_or_generation %>% tolower()) %>%
        select(mae) %>%
        mutate(mae = mae %>% scales::comma())
      
    }, ignoreNULL = FALSE)
  
  rsq_metric <-
    eventReactive(eventExpr = input$apply, {
      submodels_2_summary_metrics %>%
        filter(.model_desc == input$models_tab_1) %>%
        filter(type == input$demand_or_generation %>% tolower()) %>%
        select(rsq) %>%
        round(3)
      
    }, ignoreNULL = FALSE)
  
  mape_metric <-
    eventReactive(eventExpr = input$apply, {
      submodels_2_summary_metrics %>%
        filter(.model_desc == input$models_tab_1) %>%
        filter(type == input$demand_or_generation %>% tolower()) %>%
        select(mape) %>%
        round(3)
      
    }, ignoreNULL = FALSE)
  
  
  
  ## ** Metrics Output Id Tab Panel 1 ------------------------------------------
  
  output$rmse_metric <- renderText({
    rmse_metric()$rmse
  })
  
  output$mae_metric <- renderText({
    mae_metric()$mae
  })
  
  output$rsq_metric <- renderText({
    rsq_metric()$rsq
  })
  
  output$mape_metric <- renderText({
    mape_metric()$mape
  })
  
  
  ## ** Plot for Tab Panel 1 --------------------------------------------------
  
  output$plot_forecast <- renderPlotly({
    models_selection_reactive() %>%
      plot_modeltime_forecast(.title = "Fit on Testing Set")
    
  })
  

# Server - Tab Panel 2 --------------------------------------------------

  
  ## ** Plot for Tab Panel 2 ----
  output$resamples_tscv_plan_plot <- renderPlotly({
    
   resamples_tscv %>% 
      plot_time_series_cv_plan(date_time, series, .interactive = T)
  })
  
  

# Server - Tab Panel 3 ----------------------------------------------------

  
  # ** Data Reactivity for Tab Panel 3 -----------------------------------------
  
  three_models_future_forecast_data_reactive <-
    eventReactive(eventExpr = input$apply_tab_3, {
      three_models_future_forecast_data %>%
        filter(.model_desc %in% c(input$models_tab_3, "ACTUAL")) %>%
        filter(type == input$demand_or_generation %>% tolower())
      
    }, ignoreNULL = FALSE)
  
  
  
  # ** Plot for Tab Panel 3 --------------------------------------------------
  
  output$plot_ensemble_forecast <- renderPlotly({
    g <- three_models_future_forecast_data_reactive() %>%
      plot_modeltime_forecast(.interactive = F, .title = "Forecast on Future Data") +
      labs(subtitle = "Ensemble does not seem to be better than the base models") +
      scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))
    
    ggplotly(g)
  })
  
  
  #  Server - Tab Panel 4 --------------------------------------------------
  
  observeEvent(eventExpr = input$reset_tab_4, handlerExpr = {
    updatePickerInput(session = session,
                      inputId = "models_tab_4",
                      selected = "PROPHET - Tuned")
    
    updatePickerInput(session = session,
                      inputId = "demand_or_generation_tab_4",
                      selected = "Demand")
    
    shinyjs::delay(ms = 100, expr = {
      shinyjs::click(id = "apply_tab_4")
    })
    
  })
  
  
  # ** Data Reactivity for Tab Panel 4 ---------------------------------------
  
  out_of_sample_residuals <-
    eventReactive(
      eventExpr = input$apply_tab_4,
      valueExpr = {
        test_residuals %>%
          filter(.model_desc == input$models_tab_4) %>%
          filter(type        == input$demand_or_generation_tab_4 %>% tolower())
      },
      ignoreNULL = FALSE
    )
  
  in_sample_residuals <-
    eventReactive(
      eventExpr = input$apply_tab_4,
      valueExpr = {
        train_residuals %>%
          filter(.model_desc == input$models_tab_4)
      },
      ignoreNULL = FALSE
    )
  
  
  
  
  # ** Plots for Tab Panel 4 -------------------------------------------------
  
  output$plotly_residuals_out_of_sample <- renderPlotly({
    out_of_sample_residuals() %>%
      plot_modeltime_residuals(.title = "")
  })
  
  output$plotly_residuals_in_sample <- renderPlotly({
    in_sample_residuals() %>%
      plot_modeltime_residuals(.title = "")
  })
  
  
  output$plotly_residuals_acf_in_sample <- renderPlotly({
    in_sample_residuals() %>%
      plot_modeltime_residuals(.type = "acf",
                               .title = "")
  })
  
  output$plotly_residuals_acf_out_of_sample <- renderPlotly({
    out_of_sample_residuals() %>%
      plot_modeltime_residuals(.type = "acf",
                               .title = "")
  })
  
  
}


shinyApp(ui = ui, server = server)
