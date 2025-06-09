#' Generate a Yearly Bar Chart
#'
#' This Shiny module creates a bar chart displaying the count of occurrences by year for a selected column in a given table. The chart includes a legend with a color-coded representation of the column values. This module is designed to be used within a Shiny application.
#'
#' @param id The module identifier.
#' @param title The title of the tab.
#' @param theme The color (status) of the tab.
#' @param spinner_colour The color of the spinner shown during plot generation.
#' @param table The input data table.
#'
#' @export
yearBarUI <- function(id, title = "", theme = "", spinner_colour = "#96c296", table) {
  
  ns <- NS(id)
  
  tabPanel(
    
    title = title,
    height = "800px",
    status = theme,
    
    materialSwitch(inputId = ns("switch_to_percentage"),
                   label = "Show percentages", 
                   status = "info"),
    
    plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour),
    
    fluidRow(
      column(width = 1),
      column(width = 9,
             
             sliderInput(inputId =  ns("year_bar_slider"), 
                         label = "Select Year Range:",
                         min = as.numeric(min(table$year, na.rm = TRUE)), 
                         max = as.numeric(max(table$year, na.rm = TRUE)),
                         value = c(1980,
                                   as.numeric(max(table$year, na.rm = TRUE))),
                         step = 1,
                         sep = "")
             
      )
    )
  )
  
  
}

#' Server Function for Percentage Yearly Bar Chart
#'
#' This server function generates a percentage bar chart by year based on the input table, column name, and display option. The resulting chart displays the percentage of occurrences for each year and includes a legend with a color-coded representation of the selected column values.
#'
#' @param id The module identifier.
#' @param table The data table used for chart generation.
#' @param column The name of the column to use for chart generation.
#' @param order The order of legend items. Default is c("reported", "not reported").
#' @param display The value of the column to display. Default is "reported".
#' @param text Additional annotation text for the chart.
#' @param colours Colors for the chart elements.
#'
#' @export
yearBarServer <- function(id, table, column, order = c("reported", "not reported"), 
                          display="reported", text="", colours = c("#73D055FF", "grey")){
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly({
        
        if(input$switch_to_percentage){
          
          if(length(display)>1){
            
            x <- ggplot2::enquo(column)
            cols <- setNames(colours, order)
            
            table <- table %>%
              mutate(x = factor(!!rlang::sym(column), levels = order))
            
            table %>%
              filter(!year == "unknown") %>%
              filter(!year == "") %>%
              filter(year >= min(input$year_bar_slider), 
                     year <= max(input$year_bar_slider)) %>%
              select(uid, year, x) %>%
              distinct() %>%
              select(-uid) %>%
              group_by_all() %>%
              count() %>%
              group_by(year) %>%
              mutate(percent = n/sum(n) * 100) %>%
              filter(x %in% display) %>%
              ungroup() %>%
              plot_ly(x = ~year,
                      type = 'bar',
                      y = ~percent,
                      colors = cols,
                      color = ~x) %>%
              layout(showlegend = TRUE,
                     yaxis = list(title = paste0("% of publications (", display, ")"), range = c(0, 100)),
                     xaxis = list(title = ""), barmode = "stack",
                     annotations =
                       list(x = 1, y = -0.2, text = text,
                            showarrow = F, xref='paper', yref='paper',
                            xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                            font=list(size=12, color="black")))
            
          } else {
            
            x <- ggplot2::enquo(column)
            
            table %>%
              filter(!year == "unknown") %>%
              filter(!year == "") %>%
              select(uid, year, !!x) %>%
              filter(year >= min(input$year_bar_slider), 
                     year <= max(input$year_bar_slider)) %>%
              distinct() %>%
              select(-uid) %>%
              group_by_all() %>%
              count() %>%
              group_by(year) %>%
              mutate(percent = n/sum(n) * 100) %>%
              filter(!!rlang::sym(column) %in% display) %>%
              plot_ly(x = ~year, 
                      y = ~percent,
                      mode = "markers", 
                      marker = list(color = first(colours)),
                      hoverinfo = 'text',
                      textposition = "none",
                      text = ~paste("<b>Year:</b> ", year,
                                    "<br><b>Percentage:</b>", round(percent, 2), "<b>%<b>")
              ) %>%
              layout(showlegend = FALSE,
                     yaxis = list(title = paste0("% of publications (", display, ")"), range = c(0, 100)),
                     xaxis = list(title = ""), barmode = "stack",
                     annotations = list(x = 1, y = -0.2, text = text,
                                        showarrow = F, xref = "paper", yref = "paper",
                                        xanchor = "right", yanchor = "bottom", xshift = 0, yshift = 0,
                                        font = list(size = 12, color = "black")))
            
          }
          
        } else {
          
          x <- ggplot2::enquo(column)
          cols <- setNames(colours, order)
          
          table <- table %>%
            mutate(x = factor(!!rlang::sym(column), levels = order))
          
          table %>%
            filter(!year == "unknown") %>%
            filter(!year == "") %>%
            filter(year >= min(input$year_bar_slider), 
                   year <= max(input$year_bar_slider)) %>%
            select(uid, year, x) %>%
            distinct() %>%
            group_by(year, x) %>%
            count() %>%
            plot_ly(x = ~year,
                    type = 'bar',
                    y = ~n,
                    colors = cols,
                    color = ~x,
                    hoverinfo = 'text',
                    textposition = "none",
                    text = ~paste("<b>Info:</b> ", x,
                                  "<br><b>Number of Publications:</b>", n,
                                  "<br><b>Year:</b>", year)) %>%
            layout(showlegend = TRUE,
                   yaxis = list(title = 'Number of publications'),
                   xaxis = list(title = ""), barmode='stack',
                   annotations =
                     list(x = 1, y = -0.2, text = text,
                          showarrow = F, xref='paper', yref='paper',
                          xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                          font=list(size=12, color="black")))
          
          
        }
        
      })
    }
  )}


#' Generate a Bar Chart by Year
#'
#' This Shiny module creates a count bar chart by year based on a specified table and selected column. The resulting chart displays the count of occurrences of the values of the selected column for each year, along with a legend featuring a color-coded representation of those values. Intended for use within a Shiny application.
#'
#' @param id The module identifier.
#' @param title The title of the tab.
#' @param theme The color (status) of the tab.
#' @param spinner_colour The color of the spinner shown while the plot is generating.
#' @param table The input data table.
#'
#' @export
yearBarUI_included_only <- function(id, title = "", theme = "", spinner_colour = "#96c296", table) {
  
  ns <- NS(id)
  
  tabPanel(
    
    title = title,
    height = "800px",
    status = theme,
    
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour)
      )),
    
    fluidRow(
      column(width = 1),
      column(width = 9,
             sliderInput(inputId =  ns("included_year_slider"), 
                         label = "Select Year Range:",
                         min = as.numeric(min(table$year, na.rm = TRUE)), 
                         max = as.numeric(max(table$year, na.rm = TRUE)),
                         value = c(1980,
                                   as.numeric(max(table$year, na.rm = TRUE))),
                         step = 1,
                         sep = "")
             
      )
    )
  )
  
  
}

#' Server Function for Percentage Yearly Bar Chart
#'
#' This server function generates a percentage bar chart by year based on the input table, column name, and display option. The resulting chart displays the percentage of occurrences for each year and includes a legend with a color-coded representation of the selected column values.
#'
#' @param id The module identifier.
#' @param table The data table used for chart generation.
#' @param column The name of the column to use for chart generation.
#' @param text Additional annotation text for the chart.
#' @param colour Color for the chart elements.
#'
#' @export
yearBarServer_included_only <- function(id, table, column, 
                                        text="", 
                                        colour = "#73D055FF"
){
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly({
        
        x <- ggplot2::enquo(column)
        
        table <- table %>%
          mutate(x = factor(!!rlang::sym(column)))
        
        table %>%
          filter(!year == "unknown") %>%
          filter(!year == "") %>%
          filter(x == "included") %>%
          filter(year >= min(input$included_year_slider), 
                 year <= max(input$included_year_slider)) %>%
          group_by(year, x) %>%
          count() %>%
          plot_ly(x = ~year,
                  type = 'bar',
                  y = ~n,
                  colors = colour,
                  color = ~x,
                  hoverinfo = 'text',
                  textposition = "none",
                  text = ~paste("<b>Info:</b> ", x,
                                "<br><b>Number of Publications:</b>", n,
                                "<br><b>Year:</b>", year)) %>%
          layout(showlegend = FALSE,
                 yaxis = list(title = 'Number of publications'),
                 xaxis = list(title = "", tickangle = -45, ticklen = 4), barmode='stack',
                 annotations =
                   list(x = 1, y = -0.2, text = text,
                        showarrow = F, xref='paper', yref='paper',
                        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                        font=list(size=12, color="black")))  
      })
    }
  )}

#' Generate a Pie Chart Indicating Completion
#'
#' This Shiny module creates a pie chart indicating the completion percentage based on a specified table and column. The chart displays the count of occurrences of the values of the selected column and includes a legend with a color-coded representation of those values. Intended for use within a Shiny application.
#'
#' @param id The module identifier.
#'
#' @export
completionPieUI <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns("plot"), height="150px")
}


#' Generate a Pie Chart for Tagging Completion
#'
#' This server function generates a pie chart indicating the completion percentage of tagging based on the provided inputs. The chart represents the percentage of occurrences of the values in the specified column for each category (complete or not complete). It also offers an option to remove failed status rows and displays the completion percentage with a color-coded legend.
#'
#' @param id The module identifier.
#' @param table The data table to use for generating the chart.
#' @param identifier The name of the column to use for generating the chart.
#' @param included_studies The table used for bringing in included studies.
#' @param remove_failed Logical, indicating whether to remove rows with status "failed". Default is FALSE.
#'
#' @export
completionPieServer <- function(id, table, identifier, included_studies, remove_failed = FALSE){
  moduleServer(
    id,
    function(input, output, session) {
      
      output$plot <- renderPlotly({
        
        colors <- c("#450e44", '#808080')
        
        if(remove_failed == TRUE){
          
          table <- table %>%
            filter(!status == "failed")
          table$status <- "tagged"
          
        } else {
          
          table$status <- "tagged"
        }
        
        df_count <- included_studies %>%
          left_join(table, by=identifier, multiple="all") %>%
          mutate(cat = ifelse(is.na(status), "Not Complete", "Complete")) %>%
          select(uid, cat) %>%
          distinct() %>%
          group_by(cat) %>%
          count()
        
        plot_ly(type='pie', labels=df_count$cat, values=df_count$n,
                textinfo='label+percent',
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 2)),
                insidetextorientation='radial') %>% 
          layout(showlegend = FALSE,
                 margin = list(b = 30, l = 30, r = 30, t = 30, pad = 0, autoexpand = TRUE))
        
      })
    })
}


#' Box Containing Pie Chart Indicating Completion
#'
#' This shiny module generates a box containing a pie chart showing the tagged completion percentage of a given dataframe.
#'
#' @param id The module identifier.
#' @param title The title of the box/pie chart.
#' @param theme The status or colour theme of the box.
#' @param spinner_colour The colour of the loading spinner.
#' @param info_text The text to be shown in the box above the pie chart.
#'
#' @export
pie_completion_UI <- function(id, title, theme, spinner_colour, info_text) {
  ns <- NS(id)
  
  box(
    
    title = title,
    status = theme,
    width = 4,
    height = "300px",
    info_text,
    plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour)
  )
  
}

#' Pie Chart Showing Tagged Completion Percentage
#'
#' This shiny module generates a box containing a pie chart showing the tagged completion percentage of a given dataframe. This can also
#' include the number of studies missing a doi.
#'
#' @param id The module identifier.
#' @param table The data table to use for generating the chart.
#' @param identifier The name of the column to use for generating the chart.
#' @param included_studies The table used for bringing in included studies.
#' @param colour_not_complete The colour for the section of pie showing "not complete".
#' @param colour_complete The colour for the section of pie showing "complete".
#' @param remove_failed Removes the rows where the status == "failed".
#' @param incl_missing_doi Includes the number of studies missing a doi to the chart == "failed".
#'
#' @export
pie_completion_Server <- function(id, table, identifier, included_studies, colour_not_complete, colour_complete, incl_missing_doi = FALSE, remove_failed = FALSE){
  moduleServer(
    id,
    function(input, output, session) {
      
      output$plot <- renderPlotly({
        
        # If the user is wanting to include missing doi number in pie chart
        if(incl_missing_doi == TRUE){
          
          # If the user wants to remove the failed status - used for pdf retrieval %
          if (remove_failed == TRUE) {
            
            table <- table %>%
              filter(status == "found")
            
          }
          
          table$status <- "tagged"
          
          df_count <- included_studies %>%
            left_join(table, by=identifier, multiple="all") %>%
            mutate(cat = ifelse(is.na(status), "Not Complete", "Complete")) %>%
            mutate(cat = ifelse(doi == "", "Missing DOI", cat)) %>% 
            select(identifier, cat) %>%
            group_by(cat) %>%
            count() %>% 
            ungroup() %>% 
            mutate(colour = case_when(
              cat == "Complete" ~ colour_complete,
              cat == "Not Complete" ~ colour_not_complete,
              TRUE ~ "#454545" 
            ))
          
        } else {
          
          table$status <- "tagged"
          
          colors <- c(colour_complete, colour_not_complete)
          
          df_count <- included_studies %>%
            left_join(table, by=identifier, multiple="all") %>%
            mutate(cat = ifelse(is.na(status), "Not Complete", "Complete")) %>%
            select(identifier, cat) %>%
            group_by(cat) %>%
            count() %>% 
            ungroup() %>% 
            mutate(colour = case_when(
              cat == "Complete" ~ colour_complete,
              cat == "Not Complete" ~ colour_not_complete,
              TRUE ~ "#454545" 
            ))
        }
        
        plot_ly(type='pie', labels=df_count$cat, values=df_count$n,
                textinfo='label+percent',
                marker = list(colors = df_count$colour,
                              line = list(color = '#FFFFFF', width = 2)),
                height = 150,
                insidetextorientation='radial') %>% 
          layout(showlegend = FALSE,
                 margin = list(b = 30, l = 20, r = 20, t = 45, pad = 0, autoexpand = FALSE))
        
      })
    })
}



#' Generate a Sunburst Plot UI
#'
#' This Shiny module creates a sunburst plot.
#'
#' @param id The module identifier.
#' @param title The title of the tab.
#' @param theme The color (status) of the tab.
#' @param spinner_colour The color of the loading spinner.
#'
#' @export
sunburstUI <- function(id, title = "", theme = "", spinner_colour) {
  ns <- NS(id)
  
  tabPanel(
    
    title = title,
    status = theme,
    width = 12,
    height = "800px",
    plotlyOutput(ns("plot")) %>% withSpinner(color = spinner_colour)
    
  )
  
}

#' Generate a Sunburst Plot Module
#'
#' This Shiny module generates a sunburst plot based on the provided dataset.
#'
#' @param id The module identifier.
#' @param data The dataset containing the information for visualization.
#'
#' @export
sunburstSever <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$plot <- renderPlotly({
        
        sb_data <- data %>%
          filter(!grepl("^Unknown", name))
        
        df <- format_as_sunburst(sb_data, main_category, sub_category1, name)
        
        plot_ly(df,
                ids = ~ids,
                labels = ~labels,
                parents = ~parents,
                type = 'sunburst',
                values =  ~value,
                branchvalues = "total",
                insidetextorientation = 'auto',
                insidetextfont = list(size = 12),
                height = 800,
                width = 800,
                marker = list(colors = viridis::viridis(length(unique(data$main_category))),
                              line = list(color = "white", width = 1)),
                source = "sunburstPlot") %>%
          layout(autosize = F,
                 paper_bgcolor = "transparent",
                 plot_bgcolor = 'transparent',
                 margin = list(b = 40, l = 30, r = 30, t = 0, pad = 0, autoexpand = FALSE)
          )
      })
    })
}


#' PICO Multi-Select UI
#'
#' This Shiny module creates a bar plot showing the number of publications based on the selected columns.
#'
#' @param id The module identifier.
#' @param multi_select Specifies whether to use 1 or 2 dropdown menus.
#' @param table The data table for generating the chart.
#' @param column The name of the first column for generating the chart.
#' @param column2 The name of the second column (if multi-select is TRUE and 2 dropdown menus are required).
#' @param label1 The label above the first input.
#' @param label2 The label above the second input.
#' @param title The title of the tab.
#' @param theme The color status of the tab.
#' @param spinner_colour The color of the spinner used for loading the plot.
#'
#' @export
pico_multi_select_UI <- function(id,
                                 multi_select = TRUE,
                                 table,
                                 column,
                                 column2,
                                 label1,
                                 label2,
                                 title = "", 
                                 theme,
                                 spinner_colour) {
  
  ns <- NS(id)
  shinyFeedback::useShinyFeedback()
  
  if (multi_select){
    
    tabPanel(
      title = title,
      status = theme,
      
      tags$style(HTML('.btn-light {
                    background-color: #efefef !important;
                    color: black !important;
                    }')),
      
      # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
      
      pickerInput(
        inputId = ns("select_cat_picker"),
        label = label1,
        choices = sort(unique(column)),
        selected = sort(unique(column[!column %in% c("Unknown")])),
        multiple = TRUE,
        options = pickerOptions(noneSelectedText = "Please Select",
                                virtualScroll = 100,
                                actionsBox = TRUE,
                                size = 10,
        )
      ),
      
      suppressWarnings({
        selectizeInput(
          inputId = ns("select_type_picker"),
          label = label2,
          choices = sort(unique(column2)),
          selected = sort(unique(column2)),
          multiple = TRUE,
          options =  list(maxItems = 10,
                          virtualScroll = 100,
                          actionsBox = TRUE,
                          size = 10,
                          liveSearch = TRUE,
                          placeholder = "Select up to 10",
                          server = TRUE
          )
        )
      }),
      
      plotlyOutput(ns("multi_select_plot")) %>% withSpinner(color = spinner_colour),
      
      fluidRow(
        column(width = 1),
        column(width = 9,
               sliderInput(inputId =  ns("pico_year_slider"), 
                           label = "Select Year Range:",
                           min = as.numeric(min(table$year, na.rm = TRUE)), 
                           max = as.numeric(max(table$year, na.rm = TRUE)) + 2,
                           value = c(1980,
                                     as.numeric(max(table$year, na.rm = TRUE))),
                           step = 1,
                           sep = "")
        )
      )
    )
  } else {
    
    tabPanel(
      title = title,
      status = theme,
      
      tags$style(HTML('.btn-light {
                    background-color: #efefef !important;
                    color: black !important;
                    }')),
      
      pickerInput(
        inputId = ns("select_cat_picker"),
        label = label1,
        choices = sort(unique(column)),
        selected = sort(unique(column[!column %in% c("Unknown")])),
        multiple = TRUE,
        options = pickerOptions(noneSelectedText = "Please Select",
                                virtualScroll = 100,
                                actionsBox = TRUE,
                                size = 10,
        )
      ),
      
      plotlyOutput(ns("single_select_plot")) %>% withSpinner(color = spinner_colour),
      
      fluidRow(
        column(width = 1),
        column(width = 9,
               sliderInput(inputId =  ns("pico_year_slider"), 
                           label = "Select Year Range:",
                           min = as.numeric(min(table$year, na.rm = TRUE)), 
                           max = as.numeric(max(table$year, na.rm = TRUE)) + 2,
                           value = c(1980,
                                     as.numeric(max(table$year, na.rm = TRUE))),
                           step = 1,
                           sep = "")
        )
      )
      
    )
  }
}



#' Multi-Select Bar Chart for PICO Tabs
#'
#' This is the server function for the PICO multi-select module. It generates a bar chart by year based on the inputs.
#'
#' @param id The module identifier.
#' @param multi_select Specifies whether to use 1 or 2 dropdown menus.
#' @param table The data table for generating the chart.
#' @param column The name of the first column for generating the chart.
#' @param column2 The name of the second column (if multi-select is TRUE and 2 dropdown menus are required).
#' @param text The text to display below the chart stating the origin of the data.
#'
#' @export
pico_multi_select_Server  <- function(id,
                                      multi_select = TRUE,
                                      table,
                                      column,
                                      column2,
                                      text){
  moduleServer(
    id,
    function(input, output, session) {
      
      if (multi_select){
        
        interventions_in_cat <- reactive({
          
          data <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>% 
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            filter(column %in% input$select_cat_picker) %>% 
            filter(column2 %in% input$select_type_picker)
          return(data)
          
        })
        
        output$multi_select_plot <- renderPlotly({
          
          colours_col2 <- interventions_in_cat() %>%
            select(column2) %>%
            distinct() %>%
            top_n(10)
          
          color_palette <- c("#696969", "#800000", "#006400", "#000080", "#9acd32",
                             "#ff0000", "#ff8c00", "#ffd700", "#40e0d0", "#00ff00",
                             "#ba55d3", "#00fa9a", "#0000ff", "#ff00ff", "#1e90ff",
                             "#fa8072", "#dda0dd", "#ff1493", "#87cefa", "#ffe4b5")
          #color_palette <- brewer.pal(n = 10, name = 'Paired')
          color_mapping <- setNames(color_palette, colours_col2$column2)
          
          interventions_in_cat() %>%
            filter(year >= min(input$pico_year_slider), 
                   year <= max(input$pico_year_slider)) %>%
            distinct() %>%
            #separate_rows(column, sep = ", ") %>%
            group_by(year, column, column2) %>%
            count() %>% 
            plot_ly(x = ~year,
                    type = 'bar',
                    y = ~n,
                    color = ~column2,
                    colors = color_mapping,
                    hoverinfo = 'text',
                    textposition = "none",
                    text = ~paste("<b>Target:</b> ", column,
                                  "<br><b>Type:</b> ", column2,
                                  "<br><b>Number of Publications:</b>", n,
                                  "<br><b>Year:</b>", year)
                    
            ) %>%
            layout(showlegend = TRUE,
                   yaxis = list(title = 'Number of Publications'),
                   xaxis = list(title = "", 
                                #tickangle = -45, 
                                ticklen = 5), 
                   barmode = 'dodge',
                   hoverlabel = list(bgcolor = "white", 
                                     font = list(size = 14)),
                   annotations =
                     list(x = 1, y = -0.2, text = text,
                          showarrow = F, xref='paper', yref='paper',
                          xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                          font=list(size=12, color="black")))
          
        })
        
        dynamic_updated_target_selection <- reactive({
          
          table_list <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>% 
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>%
            filter(column %in% input$select_cat_picker)
          
          table_list <- unique(table_list$column2)
          table_list <- as.vector(table_list)
          table_list <- sort(table_list)
          
          return(table_list)
          
        })
        
        observe({
          updateSelectizeInput(session, "select_type_picker",
                               server = TRUE,
                               choices = dynamic_updated_target_selection(),
                               selected = dynamic_updated_target_selection()[1:5])
          
        })
        
        observeEvent(input$select_type_picker, {
          
          selected_years <- table %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>% 
            filter(column2 %in% input$select_type_picker) %>%
            pull(year)
          
          updateSliderInput(session, "pico_year_slider",
                            min = min(selected_years),
                            max = max(selected_years),
                            value = c(1980,
                                      max(selected_years)),
                            step = 1)
        })
        
      } else {
        
        
        interventions_in_cat <- reactive({
          
          data <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>% 
            filter(column %in% input$select_cat_picker) 
          
          return(data)
          
        })
        
        output$single_select_plot <- renderPlotly({
          
          colours_col <- interventions_in_cat() %>%
            select(column) %>%
            distinct()
          
          #color_palette <- brewer.pal(n = 20, name = 'Paired')
          color_palette <- c("#696969", "#800000", "#006400", "#000080", "#9acd32",
                             "#ff0000", "#ff8c00", "#ffd700", "#40e0d0", "#00ff00",
                             "#ba55d3", "#00fa9a", "#0000ff", "#ff00ff", "#1e90ff",
                             "#fa8072", "#dda0dd", "#ff1493", "#87cefa", "#ffe4b5")
          color_mapping <- setNames(color_palette, colours_col$column)
          
          interventions_in_cat() %>%
            filter(year >= min(input$pico_year_slider), 
                   year <= max(input$pico_year_slider)) %>%
            distinct() %>%
            #separate_rows(column, sep = ", ") %>%
            group_by(year, column) %>%
            count() %>% 
            plot_ly(x = ~year,
                    type = 'bar',
                    y = ~n,
                    color = ~column,
                    colors = color_mapping,
                    hoverinfo = 'text',
                    textposition = "none",
                    text = ~paste("<b>Main Category:</b> ", column,
                                  #"<br><b>Type:</b> ", column2,
                                  "<br><b>Number of Publications:</b>", n,
                                  "<br><b>Year:</b>", year)
                    
            ) %>%
            layout(showlegend = TRUE,
                   yaxis = list(title = 'Number of Publications'),
                   xaxis = list(title = "", 
                                #tickangle = -45, 
                                ticklen = 5), 
                   barmode = 'dodge',
                   hoverlabel = list(bgcolor = "white", 
                                     font = list(size = 14)),
                   annotations =
                     list(x = 1, y = -0.2, text = text,
                          showarrow = F, xref='paper', yref='paper',
                          xanchor='right', yanchor='bottom', xshift=0, yshift=0,
                          font=list(size=12, color="black")))
          
        })
        
        observeEvent(input$select_cat_picker, {
          
          selected_years <- table %>%
            mutate(column = as.factor(!!rlang::sym(column))) %>% 
            filter(column %in% input$select_cat_picker) %>%
            pull(year)
          
          updateSliderInput(session, "pico_year_slider",
                            min = min(selected_years),
                            max = max(selected_years),
                            value = c(1980,
                                      max(selected_years)),
                            step = 1)
        })
        
      }
    }
  )
}


#' Info Box Module UI
#'
#' This Shiny module creates an info box with a title and information text.
#'
#' @param id The module identifier.
#' @param title The title of the info box.
#' @param info_text The text to be shown in the info box.
#' @param theme The color theme of the info box.
#'
#' @export
plot_interpret_UI <- function(id, 
                              title = "How to interpret this plot", 
                              info_text = "",
                              theme) {
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = title,
      background = theme,
      info_text)
    
  )
}


#' Info Box Module Server
#'
#' This is the server function for the info box module. It currently doesn't have any specific functionality.
#'
#' @param id The module identifier.
#'
#' @export
plot_interpret_Server  <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}


#' Search Database Module UI
#'
#' This Shiny module creates the search page for the app.
#'
#' @param id The module identifier.
#' @param table A dataframe containing the data for searching.
#'
#' @export
search_UI <- function(id, table) {
  ns <- NS(id) 
  
  
  tagList(
    
    tabBox(width= 12,
           status = "primary",
           id = ns("search_tabs"),
           side = "left",
           
           tabPanel(
             value="basic_search_tab",
             title = "Basic search",
             
             textAreaInput(
               inputId = ns("topic1"),
               label = "add keywords separated by commas:",
               value = ""),
             
             radioGroupButtons(
               inputId = ns("search1_type"),
               label = "Combine keywords with",
               choices = c("AND", "OR"),
               status = "primary",
               individual = TRUE,
               checkIcon = list(
                 yes = tags$i(class = "fa fa-circle",
                              style = "color: green"),
                 no = tags$i(class = "fa fa-circle-o",
                             style = "color: green"))
             ),
             
             
             actionBttn(
               inputId = ns("search_button"),
               label = "Search database",
               style = "unite",
               color = "primary"
             ),
             
             actionBttn(
               inputId = ns("reset_search"),
               label = "Reset search query",
               style = "unite",
               color = "success"
             )),
           
           tabPanel(
             title = "Advanced search",
             value="adv_search_tab",
             
             
             textAreaInput(
               inputId = ns("topic1_adv"),
               label = "Search #1: add keywords separated by commas:",
               value = ""),
             
             radioGroupButtons(
               inputId = ns("search1_type_adv"),
               label = "Combine keywords with",
               choices = c("AND", "OR"),
               status = "primary",
               individual = TRUE,
               checkIcon = list(
                 yes = tags$i(class = "fa fa-circle",
                              style = "color: green"),
                 no = tags$i(class = "fa fa-circle-o",
                             style = "color: green"))
             ),
             
             textAreaInput(
               inputId = ns("topic2_adv"),
               label = "Search #2: add keywords separated by commas:",
               value = ""
             ),
             
             radioGroupButtons(
               inputId = ns("search2_type_adv"),
               label = "Combine keywords with",
               choices = c("AND", "OR"),
               status = "primary",
               individual = TRUE,
               checkIcon = list(
                 yes = tags$i(class = "fa fa-circle",
                              style = "color: green"),
                 no = tags$i(class = "fa fa-circle-o",
                             style = "color: green"))
             ),
             
             radioGroupButtons(
               inputId = ns("comb_1_2"),
               label = "Combine searches with...",
               choices = c("AND", "OR"),
               status = "primary"),
             
             actionBttn(
               inputId = ns("adv_search_button"),
               label = "Search database",
               style = "unite",
               color = "danger"
             ),
             
             actionBttn(
               inputId = ns("adv_reset_search"),
               label = "Reset search query",
               style = "unite",
               color = "success"
             )
           )
    ),
    
    
    box(width= 12,
        maximizable = TRUE,
        status = "primary",
        title = "Citations in database",
        
        textOutput(ns("search_results_text")),
        tags$head(tags$style("#search_results_text{color: green;
                                       font-size: 20px;
                                       font-style: italic;
                                       }"
        )),
        
        div(style="display: inline-block;vertical-align:top; width: 50px;",
            
            
            dropdown(inputId = ns("dropdown_menu"),
                     
                     tags$h3("Filter studies"),
                     
                     shinyjs::useShinyjs(),
                     
                     #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
                     sliderInput(ns("year_slider"), 
                                 "Year Published", 
                                 as.numeric(min(table$year, na.rm=TRUE)),
                                 as.numeric(max(table$year, na.rm=TRUE)),
                                 value = c(min(table$year, na.rm=TRUE), max(table$year, na.rm=TRUE)), sep=""),
                     
                     
                     
                     uiOutput(ns("dynamic_dropdowns")),
                     
                     style = "unite", 
                     icon = icon("filter"),
                     inline =TRUE,
                     status = "danger", width = "600px",
                     animate = animateOptions(
                       enter = animations$fading_entrances$fadeInLeftBig,
                       exit = animations$fading_exits$fadeOutRightBig),
                     tooltip = tooltipOptions(title = "Click to filter studies"),
                     
                     actionBttn(inputId = ns("submit_filters"),
                                label = "Apply filters"),
                     
                     prettySwitch(inputId = ns("highly_sensitive"),
                                  label = "High sensitivity")
            )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 50px;",
            
            dropdown(
              downloadBttn(
                ns("download_csv"),
                label = "Download citations in CSV format",
                style = "unite",
                color = "primary",
                size = "sm",
                block = FALSE,
                no_outline = TRUE
              ),
              downloadBttn(
                ns("download_endnote"),
                label = "Download citations in Endnote tab delimited format",
                style = "unite",
                color = "primary",
                size = "sm",
                block = FALSE,
                no_outline = TRUE
              ),
              downloadBttn(
                ns("download_syrf"),
                label = "Download citations in SyRF upload format",
                style = "unite",
                color = "primary",
                size = "sm",
                block = FALSE,
                no_outline = TRUE
              ),
              
              br(),
              p("Note for Rayyan export option below: download file and open in MS Excel first on your computer. Save as .csv in excel, then import saved file into Rayyan"),
              downloadBttn(
                ns("download_rayyan"),
                label = "Download citations in Rayyan upload format",
                style = "unite",
                color = "primary",
                size = "sm",
                block = FALSE,
                no_outline = TRUE
              ),
              
              style = "unite", icon = icon("download"),
              inline = TRUE,
              status = "success", width = "600px",
              animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig),
              tooltip = tooltipOptions(title = "Click to download relevant studies")
              
            ),
            
        ),
        
        
        DT::dataTableOutput(ns("search_results_studies")) %>% withSpinner(color="#96c296")
        
    )
    
    
  )
  
  
}


#' Search Page Module
#'
#' This Shiny module generates the search page for the app, allowing users to search and filter studies.
#'
#' @param id The module identifier.
#' @param table A dataframe of included studies with metadata.
#' @param combined_pico_table A combined dataframe of PICO tags.
#' @param pico_data A list of dynamic search updates based on the PICO dropdown filters.
#' @param citations_for_download A dataframe containing citations for download.
#' @param project_name The name of the project.
#'
#' @export
search_Server <- function(id, 
                          table, 
                          combined_pico_table,
                          pico_data = list(),
                          citations_for_download,
                          project_name = "") {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- NS(id)
      
      
      # Creates list for dropdown menus
      dynamic_dropdowns <- list()
      output$dynamic_dropdowns <- renderUI({
        dynamic_dropdowns <- lapply(pico_data, function(item) {
          pico_dropdown_UI(
            id = ns(item$id),
            label1 = item$label1,
            label2 = item$label2,
            label3 = item$label3,
            label4 = item$ilabel4,
            column1 = item$table[[item$column1]],
            column2 = item$table[[item$column2]],
            column3 = item$table[[item$column3]],
            column4 = item$table[[item$column4]],
            filter_no = item$filter_no
          )
        })
        do.call(tagList, dynamic_dropdowns)
      })
      
      # Creates list for dropdown menus reactivity
      pico_element_list <- list()
      pico_element_list <- lapply(pico_data, function(pico_item) {
        pico_dropdown_Server(
          id = pico_item$id,
          table = pico_item$table,
          column1 = pico_item$column1,
          column2 = pico_item$column2,
          column3 = pico_item$column3,
          column4 = pico_item$column4,
          filter_no = pico_item$filter_no
          
        )
      })
      
      # Creates table list for filtering data
      pico_table_list <- list()
      pico_table_list <- lapply(pico_data, function(element) element$table)
      
      
      # Create reactive values as triggers
      values <- reactiveValues()
      values$search_query <- ""
      values$reset_button <- ""
      values$submit_filters <- ""
      
      
      observeEvent(input$reset_search, {
        
        
        updateTextAreaInput(session, "topic1",
                            value = "")
        
        values$search_query <- ""
        values$reset_button <- "reset"
        values$submit_filters <- ""
        
        dynamic_dropdowns <- list()
        output$dynamic_dropdowns <- renderUI({
          dynamic_dropdowns <- lapply(pico_data, function(item) {
            pico_dropdown_UI(
              id = ns(item$id),
              label1 = item$label1,
              label2 = item$label2,
              label3 = item$label3,
              label4 = item$ilabel4,
              column1 = item$table[[item$column1]],
              column2 = item$table[[item$column2]],
              column3 = item$table[[item$column3]],
              column4 = item$table[[item$column4]],
              filter_no = item$filter_no
            )
          })
          do.call(tagList, dynamic_dropdowns)
          
        })
        
        updateSliderInput(session = session,
                          inputId = "year_slider",
                          label = "Year Published",
                          min = as.numeric(min(table$year, na.rm=TRUE)),
                          max = as.numeric(max(table$year, na.rm=TRUE)),
                          value = c(min(table$year, na.rm=TRUE), max(table$year, na.rm=TRUE)))
        
        shinyjs::click("dropdown_menu")
        
        observe({
          # Introduce a delay of 1 second
          shinyjs::delay(250, {
            # Run the click function after the delay
            shinyjs::click("dropdown_menu")
          })
        })
      })
      
      observeEvent(input$adv_reset_search, {
        
        
        updateTextAreaInput(session, "topic1_adv",
                            value = "")
        
        updateTextAreaInput(session, "topic2_adv",
                            value = "")
        
        values$search_query <- ""
        values$reset_button <- "reset"
        values$submit_filters <- ""
        
        dynamic_dropdowns <- list()
        output$dynamic_dropdowns <- renderUI({
          dynamic_dropdowns <- lapply(pico_data, function(item) {
            pico_dropdown_UI(
              id = ns(item$id),
              label1 = item$label1,
              label2 = item$label2,
              label3 = item$label3,
              label4 = item$ilabel4,
              column1 = item$table[[item$column1]],
              column2 = item$table[[item$column2]],
              column3 = item$table[[item$column3]],
              column4 = item$table[[item$column4]],
              filter_no = item$filter_no
            )
          })
          do.call(tagList, dynamic_dropdowns)
          
        })
        
        updateSliderInput(session = session,
                          inputId = "year_slider",
                          label = "Year Published",
                          min = as.numeric(min(table$year, na.rm=TRUE)),
                          max = as.numeric(max(table$year, na.rm=TRUE)),
                          value = c(min(table$year, na.rm=TRUE), max(table$year, na.rm=TRUE)))
        
        shinyjs::click("dropdown_menu")
        
        observe({
          # Introduce a delay of 1 second
          shinyjs::delay(250, {
            # Run the click function after the delay
            shinyjs::click("dropdown_menu")
          })
        })
      })
      
      
      observeEvent(c(input$search_button, input$adv_search_button),  {
        
        values$search_query <- "NOT BLANK"
        values$reset_button <- ""
        
      },  ignoreInit = TRUE)
      
      observeEvent(c(input$submit_filters),  {
        
        values$reset_button <- ""
        values$submit_filters <- "clicked"
      },  ignoreInit = TRUE)
      
      
      # getting your search results - reactive object search_results runs query on data and returns datatable
      search_query <- eventReactive(c(input$search_button, input$adv_search_button), {
        
        if(input$search_tabs == "basic_search_tab"){
          
          query1 <- input$topic1 %>%
            str_trim() %>%
            str_replace_all(pattern = ", ", repl = ",") %>%
            str_replace_all(pattern = " ,", repl = ",") %>%
            str_split("\\,") %>%
            unlist() %>%
            as.list() %>%
            lapply(trimws) %>% 
            lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
                                    x, ignore.case = TRUE))
          
          query1 <- ifelse(input$search1_type == "OR",
                           paste(query1,collapse="|"),
                           paste0("^(?=.*", paste0(query1, collapse=")(?=.*"),
                                  ").*$"))
          
          query1 <- query1 %>%
            str_trim()
          
          return(query1)
          
        }
        
        else{
          
          query1_adv <- input$topic1_adv %>%
            str_trim() %>%
            str_split("\\,") %>%
            unlist() %>%
            as.list() %>%
            lapply(trimws) %>% 
            lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
                                    x, ignore.case = TRUE))
          
          query1_adv <- ifelse(input$search1_type_adv == "OR",
                               paste(query1_adv,collapse="|"),
                               paste0("^(?=.*", paste0(query1_adv, collapse=")(?=.*"),
                                      ").*$"))
          
          
          query1_adv <- query1_adv %>%
            str_trim()
          
          
          query2_adv <- input$topic2_adv %>%
            str_trim() %>%
            str_split("\\,") %>%
            unlist() %>%
            as.list() %>%
            lapply(trimws) %>% 
            lapply(function(x) gsub(".*", paste0("[[:<:]]", x, "[[:>:]]"),
                                    x, ignore.case = TRUE))
          
          query2_adv <- ifelse(input$search2_type_adv == "OR",
                               paste(query2_adv,collapse="|"),
                               paste0("^(?=.*", paste0(query2_adv, collapse=")(?=.*"),
                                      ").*$"))
          
          
          query2_adv <- query2_adv %>%
            str_trim()
          
          try(query_final <- c(query1_adv, query2_adv))
          return(query_final)
        }
      })
      
      
      search_results <- reactive({
        
        # If there is no search query and reset button pressed, return entire table
        if(values$search_query == "" & values$reset_button == "reset"){
          
          selected_studies <- table
          
        }
        
        # If there is no search query and no reset then proceed with entire table
        else if(values$search_query == ""){
          
          selected_studies <- table
          
        }
        
        
        else if(search_query()[1] == ""){
          
          selected_studies <- table
          
        }
        
        # if there is a search query
        else{
          
          selected_studies <- table
          
          if(input$search_tabs == "basic_search_tab"){
            
            # If search has been performed in Adv search then user goes back to basic search there is a reset.
            if (length(search_query()) > 1){
              
              shinyjs::click("adv_reset_search")
              
            } else{
              
              selected_studies <- selected_studies[with(selected_studies,
                                                        grepl(search_query(),
                                                              paste(title, abstract, keywords),
                                                              ignore.case=TRUE,
                                                              perl=TRUE)),]
              
              withProgress(message = 'Performing search',
                           detail = 'This may take a little while...', value = 0, {
                             for (i in 1:25) {
                               incProgress(1/15)
                               Sys.sleep(0.2)
                             }
                           })
            }
          } else if (is.na(search_query()[1]) | is.na(search_query()[2])){
            
            
            shinyjs::click("adv_reset_search")
            
            selected_studies <- table
            
          } else{
            
            selected_studies1 <- table[with(table,
                                            grepl(search_query()[1],
                                                  paste(title, abstract, keywords),
                                                  ignore.case=TRUE,
                                                  perl=TRUE)),]
            selected_studies2 <- table[with(table,
                                            grepl(search_query()[2],
                                                  paste(title, abstract, keywords),
                                                  ignore.case=TRUE,
                                                  perl=TRUE)),]
            
            
            if(input$comb_1_2 == "AND"){
              
              selected_studies <- table %>%
                filter(uid %in% selected_studies1$uid & uid %in% selected_studies2$uid) %>%
                distinct() 
            }
            
            else{
              selected_studies <- table %>%
                filter(uid %in% c(selected_studies1$uid, selected_studies2$uid)) %>%
                distinct()
            }
            
            withProgress(message = 'Performing search',
                         detail = 'This may take a little while...', value = 0, {
                           for (i in 1:25) {
                             incProgress(1/15)
                             Sys.sleep(0.2)
                           }
                         })
            
          }}
        return(selected_studies)
        
      })
      
      
      filter_results <- reactive({
        
        selected_studies <- search_results()
        
        # If reset button clicked then tidy entire table and return
        if(values$reset_button == "reset"){
          
          selected_studies <- as.data.frame(selected_studies)
          
          selected_studies <- selected_studies %>%
            mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
            arrange(desc(year))
          
          combined_pico_table <- unique(combined_pico_table)
          selected_studies <- selected_studies %>%
            mutate(title = ifelse(!is.na(doi) & doi != "", 
                                  paste0("<a href='", link, "' target='_blank'>", title, "</a>"), 
                                  title)) %>%
            select(uid, year, author, journal, title) %>%
            left_join(combined_pico_table, by="uid") %>%
            distinct() 
          
          selected_studies <- as.data.frame(selected_studies) %>%
            ungroup()
          
          return(selected_studies)
          
          
        }
        
        # If apply filter button pressed, proceed to filter section
        if(values$submit_filters == "clicked") {
          
          #if(input$submit_filters > 0){
          
          input$submit_filters
          
          # If number of pico dataframes for dropdowns is > 0 then...
          if (length(pico_table_list) > 0) {
            
            for (i in (1:length(pico_table_list))){
              
              # Loop through each dataframe and filter
              new_table <- pico_table_list[[i]] %>%
                filter(name %in% isolate(pico_element_list[[i]]())) %>%
                select(uid) 
              
              # Only keep the rows that have a matching "uid"
              selected_studies <- selected_studies %>%
                semi_join(new_table, by = "uid")
            }
          }
          
          # Use year slider to filter 
          selected_studies <- selected_studies %>%
            mutate(year = as.numeric(as.character(year))) %>%
            filter(year >= isolate(input$year_slider[[1]])) %>%
            filter(year <= isolate(input$year_slider[[2]]))
          
        }
        
        # Warning if no results found
        if(nrow(selected_studies) < 1){
          shinyalert("Warning",
                     "Search returned 0 results. Please make a new selection.", type = "info")
          return(selected_studies)
        }
        
        # Tidy section for use in the datatable
        selected_studies <- as.data.frame(selected_studies)
        
        selected_studies <- selected_studies %>%
          mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
          arrange(desc(year))
        
        combined_pico_table <- unique(combined_pico_table)
        
        selected_studies <- selected_studies %>%
          mutate(title = ifelse(!is.na(doi) & doi != "", 
                                paste0("<a href='", link, "' target='_blank'>", title, "</a>"), 
                                title)) %>%
          select(uid, year, author, journal, title) %>%
          left_join(combined_pico_table, by="uid") %>%
          distinct() 
        
        selected_studies <- as.data.frame(selected_studies) %>%
          ungroup()
        
        return(selected_studies)
      })
      
      
      output$search_results_text <- renderText({
        
        # If there is no query and no filters
        if(values$search_query == "" & values$submit_filters == ""){
          
          paste0("All ", length(filter_results()$uid), " citations loaded. Use the search box above or apply filters to identify relevant studies!")
          
        }
        
        # If there is no query and only filters
        else if(values$submit_filters == "clicked" & values$search_query == ""){
          paste0("Your filters identified a total of ", length(filter_results()$uid), " citations")
        }
        
        # If there is an advanced search query
        else if(search_query()[1] != "" & !is.na(search_query()[2])){
          
          if(input$comb_1_2 == "AND"){
            
            translated_query <- paste0(search_query()[1],  " AND ", search_query()[2])
          } else {
            
            translated_query <- paste0(search_query()[1],  " OR ", search_query()[2])
          }
          
          translated_query <- gsub("\\|", " OR ", translated_query)
          translated_query <- gsub("\\)\\(\\?\\=\\.\\*", " AND ", translated_query)
          translated_query <- gsub("\\?\\=\\.\\*", "", translated_query)
          translated_query <- gsub("\\.\\*\\$", "", translated_query)
          translated_query <- gsub("\\^", "", translated_query)
          translated_query <- gsub("AND", " AND ", translated_query)
          translated_query <- gsub("OR", " OR ", translated_query)
          translated_query <- gsub("\\[.{5}\\]", "", translated_query)
          
          if(values$submit_filters == "clicked" & values$search_query == "NOT BLANK"){
            
            paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ", translated_query,
                   " in the title, abstract, and keywords fields. This search is NOT sensitive to case.",
                   " Your search and additional filters identified a total of ", length(filter_results()$uid), " studies")
          }
          
          else{
            
            paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ",
                   translated_query,
                   " in the title OR abstract OR keywords fields. This search is NOT sensitive to case.",
                   " Your search identified a total of ", length(filter_results()$uid), " studies with no additional filters.")
          }}
        
        # If there is a basic search query
        else if(search_query()[1] != "" & is.na(search_query()[2])){
          
          translated_query <- search_query()[1]
          
          translated_query <- gsub("\\|", " OR ", translated_query)
          translated_query <- gsub("\\)\\(\\?\\=\\.\\*", " AND ", translated_query)
          translated_query <- gsub("\\?\\=\\.\\*", "", translated_query)
          translated_query <- gsub("\\.\\*\\$", "", translated_query)
          translated_query <- gsub("\\^", "", translated_query)
          translated_query <- gsub("AND", " AND ", translated_query)
          translated_query <- gsub("OR", " OR ", translated_query)
          translated_query <- gsub("\\[.{5}\\]", "", translated_query)
          
          
          # If there is a search query with filters added
          if(values$submit_filters == "clicked" & values$search_query == "NOT BLANK"){
            
            paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ", translated_query,
                   " in the title, abstract, and keywords fields. This search is NOT sensitive to case.",
                   " Your search and additional filters identified a total of ", length(filter_results()$uid), " studies")
          }
          
          # If there is a search query with no filters added
          else if (values$submit_filters == "" & values$search_query == "NOT BLANK") {
            
            paste0("Your translated search query tells the application to find ", project_name, " papers with a regex match to ",
                   translated_query,
                   " in the title OR abstract OR keywords fields. This search is NOT sensitive to case.",
                   " Your search identified a total of ", length(filter_results()$uid), " studies with no additional filters.")
          }}
        
        
        
        
        
        else{
          
          paste0("All ", length(filter_results()$uid), " citations loaded. Use the search box above or apply filters to identify relevant studies!")
          
        }
        
      })
      
      
      # Reactive datatable showing studies and search results
      output$search_results_studies <- DT::renderDataTable({
        
        DT::datatable(
          filter_results()[,2:ncol(filter_results())],
          rownames = FALSE,
          escape = FALSE,
          options = list(
            language = list(
              zeroRecords = "No records found",
              emptyTable = "No records found"),
            deferRender = FALSE,
            scrollY = 600,
            scrollX = 100,
            scroller = TRUE,
            columnDefs = list(
              list(
                targets = c(0), #target for JS code
                width = "15px"),
              list(
                targets = c(1), #target for JS code
                render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 30 ?",
                  "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                  "}")
              ),      
              list( targets = c(4:(ncol(filter_results())-2)), # columns 4, 5, and 6
                    render = JS(
                      "function(data, type, row, meta) {",
                      "  if (type === 'display' && data) {",
                      "  var words = data.split(';');",
                      " var formattedText = words.map(function(word) {",
                      "  var color =  '#' + ('000000' + Math.floor(Math.random()*16777215).toString(16)).slice(-6);",
                      "      var textColor = (parseInt(color.substring(1), 16) > 0xffffff / 2) ? 'black' : 'white';",
                      "      return '<span style=\"background-color:' + color + '; color:' + textColor + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
                      # "      return '<span style=\"background-color:' + color + '; padding: 3px; border-radius: 5px; margin-right: 5px;\">' + word + '</span>';",
                      "    }).join('; ');",
                      "    return formattedText;",
                      "  }",
                      "  return data;",
                      "}")
              )
            )
          )
          
        )
      })
      
      
      
      
      
      # Download citations sever side --------
      # download refs button server side -csv
      output$download_csv <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".csv", sep="")
        },
        content = function(file) {
          write.csv(search_results_download(), file, row.names = FALSE)
        }
      )
      
      search_results_download <- reactive({
        
        results <- citations_for_download %>%
          filter(uid %in% !!filter_results()$uid) %>%
          mutate(abstract = "")
        
      })
      
      search_results_download_syrf <- reactive({
        
        # tbl(con, "unique_citations"), filter, collect
        results <- citations_for_download %>%
          filter(uid %in% !!filter_results()$uid)
        
        rresults <- results %>%
          rename(Authors = author,
                 Title = title,
                 Url = url,
                 Year = year,
                 DOI= doi,
                 PublicationName = journal) %>%
          mutate(AlternateName = "",
                 AuthorAddress = "",
                 ReferenceType = "",
                 Keywords = keywords,
                 CustomId = uid,
                 PdfRelativePath = paste0(uid, ".pdf"),
                 Abstract = "") %>%
          select(Title,
                 Authors,
                 PublicationName,
                 AlternateName,
                 Abstract,
                 Url,
                 AuthorAddress,
                 Year,
                 DOI,
                 ReferenceType,
                 Keywords,
                 CustomId,
                 PdfRelativePath)
        
        
      })
      
      # download refs button server side - endnote
      output$download_syrf <- downloadHandler(
        filename = function() {
          paste0("citations-syrf-", Sys.Date(),
                 ".csv", sep="")
        },
        content = function(file) {
          write.csv(search_results_download_syrf(), file,
                    col.names=TRUE, row.names = F, na="")
        })
      
      search_results_download_endnote <- reactive({
        
        # tbl(con, "unique_citations"), filter, collect
        results <- citations_for_download %>%
          filter(uid %in% !!filter_results()$uid)
        
        results <- results %>%
          filter(uid %in% search_results()$uid) %>%
          mutate("Reference Type" = "Journal Article") %>%
          mutate(isbn = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
          rename("Custom 1" = uid,
                 "Secondary Title" = journal,
                 "ISBN/ISSN" = isbn) %>%
          select("Reference Type", "author", "year",
                 "Secondary Title", "doi", "title",
                 "pages", "volume", "number",
                 "Custom 1", "ISBN/ISSN") %>%
          mutate(abstract = "")
        
        names(results) <- toTitleCase(names(results))
        
        results <- results %>%
          rename("DOI"= Doi)
        
        return(results)
        
      })
      
      # download refs button server side -endnote
      output$download_endnote <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".txt", sep="")
        },
        content = function(file) {
          write.table(search_results_download_endnote(), file, sep="\t",
                      col.names=TRUE, row.names = F, quote=FALSE, na="")
        })
      
      
    }
  )
}




#' PICO Dropdown Module UI
#'
#' This Shiny module creates reactive dropdown menus for filtering based on PICO elements.
#'
#' @param id The module identifier.
#' @param column1 The name of the first column to use for generating the dropdown.
#' @param column2 The name of the second column to use for generating the second dropdown.
#' @param column3 The name of the third column to use for generating the third dropdown.
#' @param column4 The name of the fourth column to use for generating the fourth dropdown.
#' @param label1 The label shown above the first dropdown.
#' @param label2 The label shown above the second dropdown.
#' @param label3 The label shown above the third dropdown.
#' @param label4 The label shown above the fourth dropdown.
#' @param filter_no The number of dropdown menus required.
#' 
#' @export
pico_dropdown_UI <- function(id,
                             column1, column2, column3, column4,
                             label1, label2, label3, label4,
                             filter_no) {
  ns <- NS(id)
  
  if (filter_no == 1){
    pickerInput(
      inputId = ns("dropdown_filter1"),
      label = label1,
      choices = c(sort(unique(column1))),
      multiple = TRUE,
      selected = c(sort(unique(column1))),
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        style = "btn-primary"))
    
  }
  
  else if (filter_no == 2){
    tagList(
      
      pickerInput(
        inputId = ns("dropdown_filter2"),
        label = label2,
        choices = c(sort(unique(column2))),
        selected = c(sort(unique(column2))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),
      
      pickerInput(
        inputId = ns("dropdown_filter1"),
        label = label1,
        choices = c(sort(unique(column1))),
        multiple = TRUE,
        selected = c(sort(unique(column1))),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary"))
    )
  }
  
  else if (filter_no == 3){
    
    
    tagList(
      pickerInput(
        inputId = ns("dropdown_filter3"),
        label = label3,
        choices = c(sort(unique(column3))),
        selected = c(sort(unique(column3))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),
      pickerInput(
        inputId = ns("dropdown_filter2"),
        label = label2,
        choices = c(sort(unique(column2))),
        selected = c(sort(unique(column2))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),
      
      pickerInput(
        inputId = ns("dropdown_filter1"),
        label = label1,
        choices = c(sort(unique(column1))),
        multiple = TRUE,
        selected = c(sort(unique(column1))),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary"))
    )
  } else if (filter_no == 4){
    
    tagList(
      
      pickerInput(
        inputId = ns("dropdown_filter4"),
        label = label4,
        choices = c(sort(unique(column4))),
        selected = c(sort(unique(column4))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),
      
      pickerInput(
        inputId = ns("dropdown_filter3"),
        label = label3,
        choices = c(sort(unique(column3))),
        selected = c(sort(unique(column3))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),
      
      pickerInput(
        inputId = ns("dropdown_filter2"),
        label = label2,
        choices = c(sort(unique(column2))),
        selected = c(sort(unique(column2))),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary")
      ),
      
      pickerInput(
        inputId = ns("dropdown_filter1"),
        label = label1,
        choices = c(sort(unique(column1))),
        multiple = TRUE,
        selected = c(sort(unique(column1))),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          style = "btn-primary"))
    )
    
  }
  
  
}


#' PICO Dropdown Module Server
#'
#' This Shiny module server function handles the dynamic behavior of reactive dropdown menus based on PICO elements.
#'
#' @param id The module identifier.
#' @param table The data table containing the columns used for filtering.
#' @param column1 The name of the first column to be used for generating the first dropdown.
#' @param column2 The name of the second column to be used for generating the second dropdown.
#' @param column3 The name of the third column to be used for generating the third dropdown.
#' @param column4 The name of the fourth column to be used for generating the fourth dropdown.
#' @param filter_no The number of dropdown menus required.
#'
#' @export
pico_dropdown_Server <- function(id, table,
                                 column1, column2, column3, column4,
                                 filter_no) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      if (filter_no == 1){
        dynamic_dropdown_search <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            filter(column1 %in% input$dropdown_filter1) %>%
            distinct(name) %>%
            pull() %>%
            sort()
        })
        
        
      }
      
      else if (filter_no == 2){
        
        dynamic_dropdown_search2 <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>% 
            filter(column2 %in% input$dropdown_filter2) %>%
            distinct(column1) %>% 
            mutate(column1 = as.character(column1)) %>% 
            pull() %>% 
            sort()
          
        })
        
        observe({
          updatePickerInput(session = session, ("dropdown_filter1"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })
        
        dynamic_dropdown_search <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            filter(column1 %in% input$dropdown_filter1) %>%
            distinct(name) %>% 
            pull() %>% 
            sort()
        })
        
        
      }
      
      else if (filter_no == 3){
        
        dynamic_dropdown_search3 <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>% 
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>% 
            filter(column3 %in% input$dropdown_filter3) %>% 
            distinct(column2) %>% 
            mutate(column2 = as.character(column2)) %>% 
            pull() %>% 
            sort()
          
        })
        observe({
          updatePickerInput(session = session, ("dropdown_filter2"),
                            choices = dynamic_dropdown_search3(),
                            selected = dynamic_dropdown_search3())
        })
        
        
        dynamic_dropdown_search2 <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>% 
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>% 
            filter(column3 %in% input$dropdown_filter3) %>% 
            filter(column2 %in% input$dropdown_filter2) %>%
            distinct(column1) %>%
            mutate(column1 = as.character(column1)) %>%
            pull() %>% 
            sort()
          
        })
        observe({
          updatePickerInput(session = session, ("dropdown_filter1"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })
        
        dynamic_dropdown_search <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            filter(column1 %in% input$dropdown_filter1) %>%
            distinct(name) %>% 
            pull() %>% 
            sort()
        })
        
      }      
      
      else if (filter_no == 4){
        
        dynamic_dropdown_search3 <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>% 
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>% 
            mutate(column4 = as.factor(!!rlang::sym(column4))) %>%
            filter(column4 %in% input$dropdown_filter4) %>% 
            distinct(column3) %>% 
            mutate(column3 = as.character(column3)) %>% 
            pull() %>% 
            sort()
          
        })
        
        observe({
          updatePickerInput(session = session, ("dropdown_filter3"),
                            choices = dynamic_dropdown_search3(),
                            selected = dynamic_dropdown_search3())
        })
        
        dynamic_dropdown_search2 <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>% 
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>% 
            mutate(column4 = as.factor(!!rlang::sym(column4))) %>%
            filter(column4 %in% input$dropdown_filter4) %>% 
            filter(column3 %in% input$dropdown_filter3) %>% 
            distinct(column2) %>% 
            mutate(column2 = as.character(column2)) %>% 
            pull() %>% 
            sort()
          
        })
        
        observe({
          updatePickerInput(session = session, ("dropdown_filter2"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })
        
        dynamic_dropdown_search <- reactive({
          
          pico_list <- table %>%
            mutate(column1 = as.factor(!!rlang::sym(column1))) %>%
            mutate(column2 = as.factor(!!rlang::sym(column2))) %>% 
            mutate(column3 = as.factor(!!rlang::sym(column3))) %>% 
            mutate(column4 = as.factor(!!rlang::sym(column4))) %>%
            filter(column4 %in% input$dropdown_filter4) %>% 
            filter(column3 %in% input$dropdown_filter3) %>%
            filter(column2 %in% input$dropdown_filter2) %>% 
            distinct(column1) %>% 
            mutate(column1 = as.character(column1)) %>% 
            pull() %>% 
            sort()
          
        })
        
        observe({
          updatePickerInput(session = session, ("dropdown_filter1"),
                            choices = dynamic_dropdown_search2(),
                            selected = dynamic_dropdown_search2())
        })
        
        
      }
      
      return(dynamic_dropdown_search)
      
    }
  )
}

#' Download table UI
#'
#' This Shiny module UI function creates the UI
#'
#' @param id The module identifier.
#'
#' @export
download_table_UI <- function(id) {
  ns <- NS(id)
  
  
  div(style="display: inline-block;vertical-align:top; width: 50px;",
      
      dropdown(
        downloadBttn(
          ns("download_csv"),
          label = "Download citations in CSV format",
          style = "unite",
          color = "primary",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        ),
        downloadBttn(
          ns("download_endnote"),
          label = "Download citations in Endnote tab delimited format",
          style = "unite",
          color = "primary",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        ),
        downloadBttn(
          ns("download_syrf"),
          label = "Download citations in SyRF upload format",
          style = "unite",
          color = "primary",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        ),
        
        br(),
        p("Note for Rayyan export option below: download file and open in MS Excel first on your computer. Save as .csv in excel, then import saved file into Rayyan"),
        downloadBttn(
          ns("download_rayyan"),
          label = "Download citations in Rayyan upload format",
          style = "unite",
          color = "primary",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        ),
        
        style = "unite", icon = icon("download"),
        inline = TRUE,
        status = "success", width = "600px",
        animate = animateOptions(
          enter = animations$fading_entrances$fadeInLeftBig,
          exit = animations$fading_exits$fadeOutRightBig),
        tooltip = tooltipOptions(title = "Click to download relevant studies")
        
      )
      
  )
}

#' Download table server
#'
#' This Shiny module server handles the server-side logic for downloading citation metadata
#'
#' @param id The module identifier.
#' @param table 
#'
#' @export
download_table_Server <- function(id, table) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)  
      
      # Download citations sever side --------
      # download refs button server side -csv
      output$download_csv <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".csv", sep="")
        },
        
        content = function(file) {
          write.csv(search_results_download(), file, row.names = FALSE)
        }
      )
      
      search_results_download <- reactive({
        
        results <- citations_for_dl %>%
          filter(uid %in% !!table$uid)
        
      })
      
      search_results_download_syrf <- reactive({
        
        results <- citations_for_dl %>%
          filter(uid %in% !!table$uid)
        
        rresults <- results %>%
          rename(Authors = author,
                 Title = title,
                 Abstract = abstract,
                 Url = url,
                 Year = year,
                 DOI= doi,
                 PublicationName = journal) %>%
          mutate(AlternateName = "",
                 AuthorAddress = "",
                 ReferenceType = "",
                 Keywords = keywords,
                 CustomId = uid,
                 PdfRelativePath = paste0(uid, ".pdf")) %>%
          select(Title,
                 Authors,
                 PublicationName,
                 AlternateName,
                 Abstract,
                 Url,
                 AuthorAddress,
                 Year,
                 DOI,
                 ReferenceType,
                 Keywords,
                 CustomId,
                 PdfRelativePath)
        
        
      })
      
      # download refs button server side - endnote
      output$download_syrf <- downloadHandler(
        filename = function() {
          paste0("citations-srf-", Sys.Date(),
                 ".csv", sep="")
        },
        content = function(file) {
          write.csv(search_results_download_syrf(), file,
                    #col.names=TRUE, 
                    row.names = F, na="")
        })
      
      search_results_download_endnote <- reactive({
        
        results <- citations_for_dl %>%
          filter(uid %in% !!table$uid)
        
        results <- results %>%
          filter(uid %in% table$uid) %>%
          mutate("Reference Type" = "Journal Article") %>%
          mutate(isbn = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
          rename("Custom 1" = uid,
                 "Secondary Title" = journal,
                 "ISBN/ISSN" = isbn) %>%
          select("Reference Type", "author", "year",
                 "Secondary Title", "doi", "title",
                 "pages", "volume", "number", "abstract",
                 "Custom 1", "ISBN/ISSN") %>%
          mutate(abstract = gsub("\\r\\n|\\r|\\n", "", abstract))
        
        names(results) <- toTitleCase(names(results))
        
        results <- results %>%
          rename("DOI"= Doi)
        
        return(results)
        
      })
      
      # download refs button server side -endnote
      output$download_endnote <- downloadHandler(
        filename = function() {
          paste0("citations-", Sys.Date(),
                 ".txt", sep="")
        },
        content = function(file) {
          write.table(search_results_download_endnote(), file, sep="\t",
                      col.names=TRUE, row.names = F, quote=FALSE, na="")
        })
    }
  )
}

#' Evidence Map UI Module
#'
#' This Shiny module UI function creates the user interface for the evidence map. 
#' It includes dropdown filters, a material switch for comparison mode, action buttons 
#' for rendering and resetting the plot, a plotly evidence map, and a data table of selected studies.
#'
#' @param id The module identifier. Used to namespace the module's UI components.
#' @param switch_label A string for the label of the comparison mode toggle switch.
#' @param guidance_text A string containing guidance or instructional text to display in the sidebar 
#' of the evidence map plot.
#'
#' @return A `tagList` containing the UI components for the evidence map module.
#'
#' @details 
#' The evidence map UI module provides dropdown filters, a comparison toggle, and action buttons for rendering and resetting the plot. 
#' It includes a plotly evidence map with error handling and a data table of selected studies, 
#' all organized into three boxes: Filters and Controls, Evidence Map, and Selected Studies.
#' @export
evidence_map_UI <- function(id, 
                            switch_label = "", 
                            guidance_text = "") {
  ns <- NS(id)
  
  tagList(
    box(
      title="Evidence Map",
      width= 12,
      collapsable = FALSE,
      closable=FALSE,
      solidHeader = TRUE,
      status = "primary",
      id = "pico_bubble_search_tab",
      
      
      uiOutput(ns("picker_inputs_ui")),
      
      materialSwitch(inputId = ns("comparison_switch"),
                     label = switch_label,
                     status = "info", value = FALSE),
      
      useShinyjs(),
      
      actionButton(
        inputId = ns("render_evidence_map"),
        label = "Render Plot",
        icon = icon("diagram-project"),
        class = "btn-lg"
        
      ),
      actionButton(
        inputId = ns("reset_filters"), 
        label = "Reset Filters", 
        icon = icon("redo"),
        class = "btn-lg")
      
      
    ),
    box(
      solidHeader = FALSE,
      width = 12,
      id = ns("bubble_evidence_map"),
      status = "primary",
      height = 900,
      sidebar = boxSidebar(
        id = ns("evidence_map_sidebar"),
        icon = icon("info-circle"),
        guidance_text
      ),
      
      htmlOutput(ns("error_message")),
      plotlyOutput(ns("evidence_map_plot"))
      
    ),
    box(
      title = "Selected studies",
      solidHeader = TRUE,
      width = 12,
      id = "bubble_evidence_map",
      status = "secondary",
      
      download_table_UI(ns("dl_evidence_map")),
      
      DT::dataTableOutput(ns("evidence_map_datatable")) %>% withSpinner(color="#96c296")
      
      
    )
  )
  
}


#' Evidence Map Server Module
#'
#' This Shiny module server function handles the rendering and interactions for an evidence map,
#' including dropdown filters and the plot itself. It allows users to dynamically filter data 
#' and generate a plotly evidence map.
#'
#' @param id The module identifier. Used to namespace the module's UI and server components.
#' @param citations_metadata A data table containing metadata for citations.65
#' @param x_axis_table A data frame containing data related to the x-axis of the evidence map.
#' @param y_axis_table A data frame containing data related to the y-axis of the evidence map.
#' @param legend_table A data frame containing data for the legend categories.
#' @param x_axis_specific_column A string specifying the column name in `x_axis_table` 
#' for filtering specific x-axis values.
#' @param y_axis_specific_column A string specifying the column name in `y_axis_table` 
#' for filtering specific y-axis values.
#' @param legend_specific_column A string specifying the column name in `legend_table` 
#' for filtering specific legend values.
#' @param x_axis_group_column A string specifying the column name in `x_axis_table` 
#' for filtering grouped x-axis values.
#' @param y_axis_group_column A string specifying the column name in `y_axis_table` 
#' for filtering grouped y-axis values.
#' @param legend_group_column A string specifying the column name in `legend_table` 
#' for filtering grouped legend values.
#' @param x_dropdown_group_label A string for the label of the grouped x-axis dropdown filter.
#' @param x_dropdown_specific_label A string for the label of the specific x-axis dropdown filter.
#' @param y_dropdown_group_label A string for the label of the grouped y-axis dropdown filter.
#' @param y_dropdown_specific_label A string for the label of the specific y-axis dropdown filter.
#' @param legend_dropdown_group A string for the label of the grouped legend dropdown filter.
#' @param legend_dropdown_specific A string for the label of the specific legend dropdown filter.
#'
#' @return None. This is a server-side Shiny module and does not return a value. It manages 
#' reactive inputs, updates to filters, and rendering of the evidence map plot within a Shiny app.
#'
#' @details 
#' This module supports an evidence map UI by dynamically creating and responding to user inputs
#' for filtering data by x-axis, y-axis, and legend categories. The module handles rendering
#' the corresponding plotly evidence map based on the filtered data. It also includes functionality
#' for resetting filters and managing state across sessions.
#'
#' @export

evidence_map_Server <- function(id,
                                citations_metadata = citations_for_dl,
                                x_axis_table,
                                y_axis_table, 
                                legend_table, 
                                x_axis_specific_column = "",
                                y_axis_specific_column = "",
                                legend_specific_column = "",
                                x_axis_group_column = "",
                                y_axis_group_column = "",
                                legend_group_column = "",
                                x_dropdown_group_label = "",
                                x_dropdown_specific_label = "",
                                y_dropdown_group_label = "",
                                y_dropdown_specific_label = "",
                                legend_dropdown_group = "",
                                legend_dropdown_specific = "") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id) 
      
      observeEvent(input$comparison_switch, {
        
        # Preserve current input values
        current_y_axis_maincat_select <- isolate(input$y_axis_maincat_select)
        current_x_axis_maincat_select <- isolate(input$x_axis_maincat_select)
        current_legend_maincat_select <- isolate(input$legend_maincat_select)
        current_y_axis_specific_select <- isolate(input$y_axis_specific_select)
        current_x_axis_specific_select <- isolate(input$x_axis_specific_select)
        current_legend_specific_select <- isolate(input$legend_specific_select)
        
        # When switch is used render plot flag to FALSE and disable render button
        render_plot(FALSE)
        shinyjs::disable("render_evidence_map")
        
        # Update UI dynamically
        output$picker_inputs_ui <- renderUI({
          
          # If comparison switch is TRUE then all 6 dropdowns
          if (input$comparison_switch){
            
            tagList(
              fluidRow(column(width = 4,
                              pickerInput(
                                inputId = ns("y_axis_maincat_select"),
                                label = y_dropdown_group_label,
                                choices = sort(unique(y_axis_table[[y_axis_group_column]])),
                                selected = sort(unique(y_axis_table[[y_axis_group_column]])),
                                multiple = TRUE,
                                options = pickerOptions(noneSelectedText = "Please Select",
                                                        virtualScroll = 100,
                                                        actionsBox = TRUE,
                                                        size = 10,
                                                        liveSearch = TRUE
                                )
                              )
              ),
              
              column(width = 4,
                     pickerInput(
                       inputId = ns("x_axis_maincat_select"),
                       label = x_dropdown_group_label,
                       choices = sort(unique(x_axis_table[[x_axis_group_column]])),
                       selected = sort(unique(x_axis_table[[x_axis_group_column]])),
                       multiple = TRUE,
                       options = pickerOptions(noneSelectedText = "Please Select",
                                               virtualScroll = 100,
                                               actionsBox = TRUE,
                                               size = 10,
                                               liveSearch = TRUE
                       )
                     )
              ),
              column(width = 4,
                     pickerInput(
                       inputId = ns("legend_maincat_select"),
                       label = legend_dropdown_group,
                       choices = sort(unique(legend_table[[legend_group_column]])),
                       selected = sort(unique(legend_table[[legend_group_column]])),
                       multiple = TRUE,
                       options = pickerOptions(noneSelectedText = "Please Select",
                                               virtualScroll = 100,
                                               actionsBox = TRUE,
                                               size = 10,
                                               liveSearch = TRUE
                       )
                     )
              )
              
              ),
              fluidRow(column(width = 4,
                              pickerInput(
                                inputId = ns("y_axis_specific_select"),
                                label = y_dropdown_specific_label,
                                choices = sort(unique(y_axis_table[[y_axis_specific_column]])),
                                multiple = TRUE,
                                options = pickerOptions(noneSelectedText = "Please Select",
                                                        virtualScroll = 100,
                                                        actionsBox = TRUE,
                                                        size = 10,
                                                        liveSearch = TRUE,
                                                        maxOptions = 20
                                )
                              )
              ),
              
              column(width = 4,
                     pickerInput(
                       inputId = ns("x_axis_specific_select"),
                       label = x_dropdown_specific_label,
                       choices = sort(unique(x_axis_table[[x_axis_specific_column]])),
                       multiple = TRUE,
                       options = pickerOptions(noneSelectedText = "Please Select",
                                               virtualScroll = 100,
                                               actionsBox = TRUE,
                                               size = 10,
                                               liveSearch = TRUE,
                                               maxOptions = 10
                       )
                     )
              ),
              column(width = 4,
                     pickerInput(
                       inputId = ns("legend_specific_select"),
                       label = legend_dropdown_specific,
                       choices = sort(unique(legend_table[[legend_specific_column]])),
                       multiple = TRUE,
                       options = pickerOptions(noneSelectedText = "Please Select",
                                               virtualScroll = 100,
                                               actionsBox = TRUE,
                                               size = 10,
                                               liveSearch = TRUE,
                                               maxOptions = 10
                       )
                     )
              )
              
              )
            )
          } 
          
          # else just the 4 dropdowns
          else {
            
            tagList(
              fluidRow(column(width = 6,
                              pickerInput(
                                inputId = ns("y_axis_maincat_select"),
                                label = y_dropdown_group_label,
                                choices = sort(unique(y_axis_table[[y_axis_group_column]])),
                                selected = sort(unique(y_axis_table[[y_axis_group_column]])),
                                multiple = TRUE,
                                options = pickerOptions(noneSelectedText = "Please Select",
                                                        virtualScroll = 100,
                                                        actionsBox = TRUE,
                                                        size = 10,
                                                        liveSearch = TRUE
                                )
                              )
              ),
              
              column(width = 6,
                     pickerInput(
                       inputId = ns("x_axis_maincat_select"),
                       label = x_dropdown_group_label,
                       choices = sort(unique(x_axis_table[[x_axis_group_column]])),
                       selected = sort(unique(x_axis_table[[x_axis_group_column]])),
                       multiple = TRUE,
                       options = pickerOptions(noneSelectedText = "Please Select",
                                               virtualScroll = 100,
                                               actionsBox = TRUE,
                                               size = 10,
                                               liveSearch = TRUE
                       )
                     )
              )
              
              ),
              fluidRow(column(width = 6,
                              pickerInput(
                                inputId = ns("y_axis_specific_select"),
                                label = y_dropdown_specific_label,
                                choices = sort(unique(y_axis_table[[y_axis_specific_column]])),
                                multiple = TRUE,
                                options = pickerOptions(noneSelectedText = "Please Select",
                                                        virtualScroll = 100,
                                                        actionsBox = TRUE,
                                                        size = 10,
                                                        liveSearch = TRUE,
                                                        maxOptions = 20
                                )
                              )
              ),
              
              column(width = 6,
                     pickerInput(
                       inputId = ns("x_axis_specific_select"),
                       label = x_dropdown_specific_label,
                       choices = sort(unique(x_axis_table[[x_axis_specific_column]])),
                       multiple = TRUE,
                       options = pickerOptions(noneSelectedText = "Please Select",
                                               virtualScroll = 100,
                                               actionsBox = TRUE,
                                               size = 10,
                                               liveSearch = TRUE,
                                               maxOptions = 10
                       )
                     )
              )
              
              )
            )
          }
          
        }
        )
        
        # Update input values after UI re-renders
        observe({
          updatePickerInput(session, "y_axis_maincat_select", selected = current_y_axis_maincat_select)
          updatePickerInput(session, "x_axis_maincat_select", selected = current_x_axis_maincat_select)
          updatePickerInput(session, "y_axis_specific_select", selected = current_y_axis_specific_select)
          updatePickerInput(session, "x_axis_specific_select", selected = current_x_axis_specific_select)
        })
      })
      
      
      
      # Reactive value to track the previous main category selection
      y_axis_group <- reactiveVal(sort(unique(y_axis_table[[y_axis_group_column]])))
      
      observeEvent(input$y_axis_maincat_select, {
        
        # Compare current selection with the previous one
        if (!identical(input$y_axis_maincat_select, y_axis_group())) {
          
          # Enable render plot button and reset click bubble info
          shinyjs::enable("render_evidence_map")
          click_bubble(NULL)
          
          
          # Update the tracked value of the previous main category
          y_axis_group(input$y_axis_maincat_select)
          
          # Filter based on the selected grouping column
          y_axis_group <- y_axis_table %>%
            filter(.data[[y_axis_group_column]] %in% input$y_axis_maincat_select)
          
          # Find the distinct values which fit in the grouping
          y_axis_distinct <- y_axis_group %>%
            distinct(.data[[y_axis_specific_column]]) %>%
            arrange(.data[[y_axis_specific_column]]) %>%
            pull(.data[[y_axis_specific_column]])
          
          # Only update picker input's `choices` and `selected` if the main category changes
          updatePickerInput(session, "y_axis_specific_select",
                            choices = sort(y_axis_distinct),
                            options = pickerOptions(noneSelectedText = "Please Select",
                                                    virtualScroll = 100,
                                                    maxOptions = 20,
                                                    actionsBox = TRUE,
                                                    size = 10
                            ))
        }
      })
      
      
      # Reactive value to track the previous main category selection
      x_axis_group <- reactiveVal(sort(unique(x_axis_table[[x_axis_group_column]])))
      
      observeEvent(input$x_axis_maincat_select, {
        
        # Compare current selection with the previous one
        if (!identical(input$x_axis_maincat_select, x_axis_group())) {
          
          # Enable render plot button and reset click bubble info
          shinyjs::enable("render_evidence_map")
          click_bubble(NULL)
          
          
          # Update the tracked value of the previous main category
          x_axis_group(input$x_axis_maincat_select)
          
          # Filter based on the selected grouping
          x_group <- x_axis_table %>%
            filter(.data[[x_axis_group_column]] %in% input$x_axis_maincat_select)
          
          # Find the distinct values which fit in the grouping
          x_axis_distinct <- x_group %>%
            distinct(.data[[x_axis_specific_column]]) %>%
            arrange(.data[[x_axis_specific_column]]) %>%
            pull(.data[[x_axis_specific_column]])
          
          # Code for selecting possible x axis specific based on y axis specific input
          #     x_axis_selected <- x_group %>%
          #       select(uid, !!x_type() := x_axis_specific_column) %>%
          #       left_join(y_axis_table[, c("uid", y_axis_specific_column)], by = "uid", relationship = "many-to-many") %>%
          #       rename(!!y_type() := y_axis_specific_column) %>%
          #       filter(.data[[y_type()]] %in% input$y_axis_specific_select) %>%
          #       distinct(.data[[x_type()]]) %>%
          #       pull(.data[[x_type()]])
          
          
          # Only update picker input's `choices` and `selected` if the main category changes
          updatePickerInput(session, "x_axis_specific_select",
                            choices = sort(x_axis_distinct),
                            options = pickerOptions(noneSelectedText = "Please Select",
                                                    virtualScroll = 100,
                                                    maxOptions = 20,
                                                    actionsBox = TRUE,
                                                    size = 10
                            ))
        }
      })
      
      
      
      # Reactive value to track the previous main category selection
      legend_group <- reactiveVal(sort(unique(legend_table[[legend_group_column]])))
      
      observeEvent(input$legend_maincat_select, {
        
        # Compare current selection with the previous one
        if (!identical(input$legend_maincat_select, legend_group())) {
          
          # Enable render plot button and reset click bubble info
          shinyjs::enable("render_evidence_map")
          click_bubble(NULL)
          
          
          # Update the tracked value of the previous main category
          legend_group(input$legend_maincat_select)
          
          # Filter interventions based on the selected main categories
          legend_group <- legend_table %>%
            filter(.data[[legend_group_column]] %in% input$legend_maincat_select)
          
          # Find the distinct values which fit in the grouping
          legend_distinct <- legend_group %>%
            distinct(.data[[legend_specific_column]]) %>%
            arrange(.data[[legend_specific_column]]) %>%
            pull(.data[[legend_specific_column]])
          
          # Code for selecting possible legend specific based on x & y axis specifc input
          # legend_selected <- legend_group %>%
          #   select(uid, !!legend_type() := legend_specific_column) %>%
          #   left_join(y_axis_table[, c("uid", y_axis_specific_column)], by = "uid", relationship = "many-to-many") %>%
          #   rename(!!y_type() := y_axis_specific_column) %>%
          #   filter(.data[[y_type()]] %in% input$y_axis_specific_select) %>%
          #   left_join(x_axis_table[, c("uid", x_axis_specific_column)], by = "uid", relationship = "many-to-many") %>%
          #   rename(!!x_type() := x_axis_specific_column) %>%
          #   filter(.data[[x_type()]] %in% input$x_axis_specific_select) %>%
          #   distinct(.data[[legend_type()]]) %>%
          #   arrange(.data[[legend_type()]]) %>% 
          #   pull(.data[[legend_type()]])
          
          
          # Only update picker input's `choices` and `selected` if the main category changes
          updatePickerInput(session, "legend_specific_select",
                            choices = sort(legend_distinct),
                            options = pickerOptions(noneSelectedText = "Please Select",
                                                    virtualScroll = 100,
                                                    maxOptions = 5,
                                                    actionsBox = TRUE,
                                                    size = 10
                            ))
        }
      })
      
      # Every time the comparison switch is turned on the legend_group is reset
      observeEvent(input$comparison_switch, {
        
        if (input$comparison_switch) {
          
          legend_group(sort(unique(legend_table[[legend_group_column]])))
        } 
      })
      
      
      # Reset button logic
      observeEvent(input$reset_filters, {
        
        # Reset filters to defaults
        updatePickerInput(session, "y_axis_maincat_select", selected = sort(unique(y_axis_table[[y_axis_group_column]])))
        updatePickerInput(session, "x_axis_maincat_select", selected = sort(unique(x_axis_table[[x_axis_group_column]])))
        updatePickerInput(session, "legend_maincat_select", selected = sort(unique(legend_table[[legend_group_column]])))
        updatePickerInput(session, "y_axis_specific_select", selected = NULL)
        updatePickerInput(session, "x_axis_specific_select", selected = NULL)
        updatePickerInput(session, "legend_specific_select", selected = NULL)
        
        # enable render button, set render plot flag to FALSE & click info to NULL
        shinyjs::enable("render_evidence_map")
        render_plot(FALSE)                       
        
        #output$evidence_map_plot <- renderPlotly(NULL)
        click_bubble(NULL)
        
      })
      
      # Reactive table being filtered as it is built, to avoid creating 1 XL table
      data_table <- reactive({
        
        if (input$comparison_switch){
          
          y_axis_table %>%
            select(uid, !!y_type() := y_axis_specific_column) %>%
            filter(.data[[y_type()]] %in% input$y_axis_specific_select) %>%
            left_join(x_axis_table[, c("uid", x_axis_specific_column)], by = "uid", relationship = "many-to-many") %>%
            rename(!!x_type() := x_axis_specific_column) %>%
            filter(.data[[x_type()]] %in% input$x_axis_specific_select) %>%
            left_join(legend_table[, c("uid", legend_specific_column)], by = "uid", relationship = "many-to-many") %>%
            rename(!!legend_type() := legend_specific_column) %>%
            filter(.data[[legend_type()]] %in% input$legend_specific_select) %>%
            distinct()
          
        } else {
          
          y_axis_table %>%
            select(uid, !!y_type() := y_axis_specific_column) %>%
            filter(.data[[y_type()]] %in% input$y_axis_specific_select) %>%
            left_join(x_axis_table[, c("uid", x_axis_specific_column)], by = "uid", relationship = "many-to-many") %>%
            rename(!!x_type() := x_axis_specific_column) %>%
            filter(.data[[x_type()]] %in% input$x_axis_specific_select) %>%
            distinct()
          
        }
        
      })
      
      # Extracting the "type" for each of the variables (i.e. "model", "intervention" etc)
      x_type <- reactive({
        
        x_axis_table %>% 
          distinct(type) %>% 
          pull(type)
      })
      
      y_type <- reactive({
        
        y_axis_table %>% 
          distinct(type) %>% 
          pull(type)
      })
      
      legend_type <- reactive({
        legend_table %>% 
          distinct(type) %>% 
          pull(type)
      })
      
      
      # Create a reactive value for click info so we can reset
      click_bubble <- reactiveVal(NULL) 
      
      # Reactive consumer for click data
      click_data <- reactive({
        
        click <- event_data("plotly_click", priority = "event", source = "B")
        
        if (!is.null(click)) {
          
          # Store the latest click in the reactiveVal
          click_bubble(click)  
        }
        
        return(click)
      })
      
      
      
      # Collect data for the plot, including click data
      bubble_react <- reactive({
        
        if (input$comparison_switch){
          
          req(input$y_axis_specific_select, input$x_axis_specific_select, input$legend_specific_select)
          
          citations_years <- citations_metadata %>%
            select(uid, year)
          
          data <- data_table() %>%
            left_join(citations_years, by = "uid") %>%
            group_by(uid, .data[[y_type()]], .data[[x_type()]], .data[[legend_type()]]) %>%
            count() %>%
            ungroup() %>%
            count(.data[[y_type()]], .data[[x_type()]], .data[[legend_type()]]) %>%
            arrange(.data[[legend_type()]], .data[[x_type()]], .data[[y_type()]])
          
        } else {
          
          req(input$y_axis_specific_select, input$x_axis_specific_select)
          
          citations_years <- citations_metadata %>%
            select(uid, year)
          
          data <- data_table() %>%
            left_join(citations_years, by = "uid") %>%
            group_by(uid, .data[[y_type()]], .data[[x_type()]]) %>%
            count() %>%
            ungroup() %>%
            count(.data[[y_type()]], .data[[x_type()]]) %>%
            arrange(.data[[x_type()]], .data[[y_type()]])
          
          
        }
        
        data$key <- row.names(data)
        data$col <- "#266080"
        
        click_data <- click_data()
        
        # If click_bubble contains click info then add it to the data
        if (!is.null(click_bubble())) {
          
          bubble_react_new <- data %>%
            mutate(selected_colour = key %in% click_data$customdata)
          
          bubble_react_new$selected_colour <- data$key %in% click_data$customdata
          
          if (exists("col_vector")){
            
            bubble_react_new$col <- col_vector
          }
          
          selected_row <- which(rownames(bubble_react_new) %in% click_data$customdata)
          
          if (!bubble_react_new$col[selected_row] == "#47B1A3"){
            
            bubble_react_new$col[selected_row] <- "#47B1A3"
            
            assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
            
          } else{
            
            bubble_react_new$col[selected_row] <- "#266080"
            
            assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
            
          }
          
          
        }
        
        # If there is no click data then continue
        else {
          
          data$col <- "#266080"
          
          bubble_react_new <- data %>%
            mutate(selected_colour = FALSE)
          
          assign("col_vector", bubble_react_new$col, envir = .GlobalEnv)
          
        }
        
        return(bubble_react_new)
        
      })
      
      # Evidence map - table react ----
      # Connect the click data from the bubbles to the citation metadata for showing in the datatable
      table_react <- reactive({
        
        if (input$comparison_switch){
          
          bubble_data <- bubble_react()
          
          if ("selected_colour" %in% names(bubble_data)) {
            table_filter <- bubble_data %>%
              filter(selected_colour == TRUE)
            
          } else {
            
            table_filter <- bubble_data
            
          }
          
          # Use the click data to filter the data
          final_table <- data_table() %>%
            filter(.data[[y_type()]] %in% table_filter[[y_type()]],
                   .data[[x_type()]] %in% table_filter[[x_type()]],
                   .data[[legend_type()]] %in% table_filter[[legend_type()]])
          
          # Add metadata to this
          final_table <- pico %>%
            filter(uid %in% final_table$uid) %>%
            left_join(citations_metadata, by = "uid") %>%
            select(uid, year, author, title, .data[[y_type()]], .data[[x_type()]], .data[[legend_type()]], doi, url) %>%
            mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
            arrange(desc(year))
          
          if (nrow(final_table) > 0 ){
            
            final_table <- final_table %>% 
              mutate(title = ifelse(!is.na(doi) & doi != "", 
                                    paste0("<a href='", link, "' target='_blank'>", title, "</a>"), 
                                    title))      
          }
          
          # Rename columns for final output to fit with chosen input data (e. "Models", "Interventions" etc)
          final_table <- final_table %>%
            select(-doi, -url, -link) %>%
            select(uid, Year = year, Author = author, Title = title,
                   !!paste0(toTitleCase(y_type()), "s") := .data[[y_type()]],
                   !!paste0(toTitleCase(x_type()), "s") := .data[[x_type()]],
                   !!paste0(toTitleCase(legend_type()), "s") := .data[[legend_type()]],
            )
        } 
        
        # For comparison switch == FALSE, use click data to filter and join with metadata
        else{
          
          req(input$y_axis_specific_select, input$x_axis_specific_select)
          
          bubble_data <- bubble_react()
          
          if ("selected_colour" %in% names(bubble_data)) {
            table_filter <- bubble_data %>%
              filter(selected_colour == TRUE)
            
          } else {
            
            table_filter <- bubble_data
            
          }
          
          # Use the click data to filter the data
          final_table <- data_table() %>%
            filter(.data[[y_type()]] %in% table_filter[[y_type()]],
                   .data[[x_type()]] %in% table_filter[[x_type()]])
          
          # Add metadata to this
          final_table <- pico %>%
            filter(uid %in% final_table$uid) %>%
            left_join(citations_metadata, by = "uid") %>%
            select(uid, year, author, title, .data[[y_type()]], .data[[x_type()]], doi, url) %>%
            mutate(link = ifelse(!is.na(doi), paste0("https://doi.org/", doi), url)) %>%
            arrange(desc(year))
          
          if (nrow(final_table) > 0 ){
            
            final_table <- final_table %>% 
              mutate(title = ifelse(!is.na(doi) & doi != "", 
                                    paste0("<a href='", link, "' target='_blank'>", title, "</a>"), 
                                    title))
            
          }
          # Rename columns for final output to fit with chosen input data (e. "Models", "Interventions" etc)
          final_table <- final_table %>%
            select(-doi, -url, -link) %>%
            select(uid, Year = year, Author = author, Title = title,
                   !!paste0(toTitleCase(y_type()), "s") := .data[[y_type()]],
                   !!paste0(toTitleCase(x_type()), "s") := .data[[x_type()]]
            )
          
          
        }
        
        return(final_table)
      })
      
      # Error handling for no plot shown
      plot_data <- reactive({
        
        tryCatch({
          
          bubble_react()
          
        }, error = function(e) {
          
          NULL
        })
      })
      
      # Message to show when no plot is shown
      output$error_message <- renderText({
        
        if (is.null(plot_data()) | render_plot() == "FALSE") {
          
          "Please make selections and click 'Render Plot' to view!<br><br>Click on a bubble to see the corresponding studies."
          
          
        } else {
          
          ""
        }
      })
      
      # Reactive value for when there should be a plot shown
      render_plot <- reactiveVal(FALSE)
      
      # Evidence map - plot -----
      observeEvent(input$render_evidence_map, {
        
        shinyjs::disable("render_evidence_map")
        render_plot(TRUE)
        
        output$evidence_map_plot <- renderPlotly({
          
          data <- plot_data()
          
          req(data)
          req(render_plot())
          
          
          if (input$comparison_switch){
            
            tryCatch({
              
              # Count number selected for legend for jitter in plot
              subcat_count <- data %>%
                ungroup() %>%
                distinct(.data[[legend_type()]]) %>%
                nrow()
              
              # Jitter the plot to account for legend and change shap for selected bubbles
              plot <- data %>%
                ungroup() %>%
                mutate(numeric_x = as.numeric(factor(.data[[x_type()]]))) %>%
                mutate(index = as.numeric(factor(.data[[legend_type()]]))) %>%
                group_by(.data[[y_type()]], .data[[x_type()]]) %>%
                mutate(
                  jitter_base = ifelse(subcat_count > 1, 0.4 / (subcat_count - 1), 0),
                  jittered_x = numeric_x + (index - (subcat_count + 1) / 2) * jitter_base) %>%
                ungroup() %>%
                mutate(shape = ifelse(selected_colour == TRUE, "circle-cross-open", "circle")) 
              
              # Calculate midpoints for line positions
              unique_x_points <- sort(unique(plot$numeric_x))
              line_positions <- head(unique_x_points, -1) + diff(unique_x_points) / 2
              
              # Size referencing
              max_n <- max(plot$n, na.rm = TRUE)
              sizeref_value <- 1 * (max_n/100)
              
              
              # Assign colours to legend and map
              legend_colours <- c(
                dark_blue = "#1A465F",
                dot_text_green = "#64C296",
                coral = "#FF7F50",
                gold = "#FFD700",
                rose_quartz = "#A799B7",
                indian_red = "#D05353",
                slate_grey = "#708090",
                melon = "#DAA49A",
                coyote = "#735F3D",
                lavender = "#E6E6FA"
              )
              
              
              unique_color_var <- unique(input$legend_specific_select)
              color_map <- setNames(legend_colours[1:length(unique_color_var)], unique_color_var)
              
              # Create plot for comparison switch == TRUE
              p <- plot_ly(plot,
                           x = ~jittered_x, y = as.formula(paste0("~", y_type())), size = ~n,
                           color = as.formula(paste0("~", legend_type())),
                           colors = color_map,
                           customdata = ~key,
                           type = 'scatter',
                           mode = 'markers',
                           source = "B",
                           height = 750,
                           fill = ~'',
                           marker = list(symbol = ~shape, sizemode = 'area',
                                         opacity = 0.8, sizeref = sizeref_value,
                                         line = list(color = '#FFFFFF', width = 1),
                                         legendgroup = as.formula(paste0("~", legend_type()))),
                           hoverinfo = 'text',
                           textposition = "none",
                           text = ~paste0(
                             "<br><b>", toTitleCase(y_type()), ":</b> ", .data[[y_type()]],
                             "<br><b>", toTitleCase(x_type()), ":</b> ", .data[[x_type()]],
                             "<br><b>", toTitleCase(legend_type()), ":</b>", .data[[legend_type()]],
                             "<br><b>Number of Publications:</b> ", n
                           )) %>%
                layout(yaxis = list(title = list(text = toTitleCase(y_type()), standoff = 25),
                                    showgrid = TRUE
                ),
                xaxis = list(
                  title = list(text = "", standoff = 25),
                  ticklen = 4,
                  tickvals = unique(plot$numeric_x),
                  ticktext = unique(plot[[x_type()]]),
                  showgrid = FALSE,
                  tickfont = list(
                    size = 14,  # Increase the font size as desired
                    color = "black",
                    family = "Arial, bold"  # Specify bold here
                  )
                ),
                hoverlabel = list(bgcolor = "white",
                                  font = list(size = 14)),
                showlegend = TRUE,
                legend = list(title = list(text = toTitleCase(legend_type())),
                              bordercolor = 'black',
                              borderwidth = 2,
                              y = 0.95,
                              yanchor = 'top'),
                clickmode = "event + select",
                shapes =
                  lapply(line_positions, function(pos) {
                    list(
                      type = "line",
                      x0 = pos, y0 = 0,
                      x1 = pos, y1 = 1,
                      xref = 'x', yref = 'paper',  # Vertical lines along x
                      line = list(color = 'grey', width = 1)
                    )
                  })
                
                )
              return(p)
            }, error = function(e) {
              
              # Return NULL to avoid further processing or showing an error
              return(NULL)  
            })
            
          } else {
            
            tryCatch({
              
              
              plot <- data %>%
                ungroup() %>%
                mutate(numeric_x = as.numeric(factor(.data[[x_type()]]))) %>%
                mutate(shape = ifelse(selected_colour == TRUE, "circle-cross-open", "circle")) 
              
              # Calculate midpoints for line positions
              unique_x_points <- sort(unique(plot$numeric_x))
              line_positions <- head(unique_x_points, -1) + diff(unique_x_points) / 2
              
              max_n <- max(plot$n, na.rm = TRUE)
              sizeref_value <- 1 * (max_n/100)
              
              p <- plot_ly(plot,
                           x = ~numeric_x, y = as.formula(paste0("~", y_type())), size = ~n,
                           customdata = ~key,
                           type = 'scatter',
                           mode = 'markers',
                           source = "B",
                           height = 750,
                           fill = ~'',
                           marker = list(symbol = ~shape, sizemode = 'area',
                                         opacity = 0.8, sizeref = sizeref_value,
                                         line = list(color = '#FFFFFF', width = 1)
                           ),
                           hoverinfo = 'text',
                           textposition = "none",
                           text = ~paste0(
                             "<br><b>", toTitleCase(y_type()), ":</b> ", .data[[y_type()]],
                             "<br><b>", toTitleCase(x_type()), ":</b> ", .data[[x_type()]],
                             "<br><b>Number of Publications:</b> ", n
                           )) %>%
                layout(yaxis = list(title = list(text = toTitleCase(y_type()), standoff = 25),
                                    showgrid = TRUE
                ),
                xaxis = list(
                  title = list(text = "", standoff = 25),
                  ticklen = 4,
                  tickvals = unique(plot$numeric_x),
                  ticktext = unique(plot[[x_type()]]),
                  showgrid = FALSE,
                  tickfont = list(
                    size = 14,  # Increase the font size as desired
                    color = "black",
                    family = "Arial, bold"  # Specify bold here
                  )
                ),
                hoverlabel = list(bgcolor = "white",
                                  font = list(size = 14)),
                clickmode = "event + select",
                shapes =
                  lapply(line_positions, function(pos) {
                    list(
                      type = "line",
                      x0 = pos, y0 = 0,
                      x1 = pos, y1 = 1,
                      xref = 'x', yref = 'paper',  # Vertical lines along x
                      line = list(color = 'grey', width = 1)
                    )
                  })
                
                )
              return(p)
            }, error = function(e) {
              
              # Return NULL to avoid further processing or showing an error
              return(NULL)  
            })
          }
        })
        
        
      })
      
      # Evidence map - datatable -----
      output$evidence_map_datatable <- DT::renderDataTable({
        
        tryCatch({
          
          # Render the datatable
          DT::datatable(
            table_react()[, 2:ncol(table_react())],
            rownames = FALSE,
            escape = FALSE,
            options = list(
              language = list(
                zeroRecords = "Click on a point to show data",
                emptyTable = "Click on a point to show data"
              ),
              deferRender = FALSE,
              scrollY = 600,
              scrollX = 100,
              scroller = TRUE,
              columnDefs = list(
                list(
                  targets = c(2), #target for JS code
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data.length > 100 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                    "}"
                  )
                ),
                list(
                  targets = c(1, 2), #target for JS code
                  render = JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data.length > 15 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                    "}"
                  )
                ),
                list(width = '10%', targets = "_all")
              )
            )
          )
        }, error = function(e) {
          # showNotification(
          #   "Please double-click the plot to reset.",
          #   type = "error",
          #   duration = 5
          # )
          
          # Return an empty datatable to avoid further errors
          DT::datatable(data.frame(), rownames = FALSE)
        })
      })
      
    }
  )
}