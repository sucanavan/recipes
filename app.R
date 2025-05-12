# app.R

library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(stringr)
library(DT)

#— Load your recipes
data_file  <- "test.xlsx"
recipes_df <- read_excel(data_file)

#— Badge colors (hex)
badge_colors <- c(
  vegetarian           = "#28a745",
  vegan                = "#20c997",
  main                 = "#007bff",
  breakfast            = "#17a2b8",
  dessert              = "#ffc107",
  quick                = "#6c757d",
  `gluten-free option` = "#ffc107",
  salad                = "#17a2b8"
)

#— All badges for filtering
all_badges <- recipes_df$Badges %>%
  str_split(",") %>%
  unlist() %>%
  str_trim() %>%
  unique() %>%
  sort()

#— Unit maps for conversions
temperature_units <- c("Celsius" = "C", "Fahrenheit" = "F")
weight_units      <- c("Grams"   = "g", "Ounces"     = "oz")
volume_units      <- c(
  "Tablespoons" = "tbsp",
  "Teaspoons"   = "tsp",
  "Milliliters" = "ml",
  "Cups"        = "cup"
)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Canavan Recipe Collection"),
  
  tabsetPanel(
    #---- Tab 1: Browse Recipes ----
    tabPanel("Browse Recipes",
             sidebarLayout(
               sidebarPanel(
                 textInput("search", "Search recipes", placeholder = "e.g. chicken"),
                 checkboxGroupInput("badge_filter", "Filter by badge", choices = all_badges),
                 hr(),
                 DTOutput("title_list")
               ),
               mainPanel(
                 uiOutput("recipe_details")
               )
             )
    ),
    
    #---- Tab 2: Conversions ----
    tabPanel("Conversions",
             br(),
             wellPanel(
               fluidRow(
                 column(4,
                        numericInput("conv_input", "Value", value = 1, step = 0.1)
                 ),
                 column(8,
                        radioButtons(
                          "conv_category",
                          "Category",
                          choices = c("Temperature", "Weight", "Volume"),
                          inline  = TRUE
                        )
                 )
               ),
               fluidRow(
                 column(6, uiOutput("from_ui")),
                 column(6, uiOutput("to_ui"))
               )
             ),
             fluidRow(
               column(12,
                      tags$h4("Result"),
                      verbatimTextOutput("conv_result")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  #—— Recipe filtering logic
  filtered <- reactive({
    df <- recipes_df
    
    if (length(input$badge_filter)) {
      df <- df %>%
        rowwise() %>%
        filter(all(input$badge_filter %in% str_split(Badges, ",")[[1]] %>% str_trim())) %>%
        ungroup()
    }
    
    if (!is.null(input$search) && nzchar(input$search)) {
      term <- str_to_lower(input$search)
      df <- df %>%
        filter(
          str_detect(str_to_lower(Title), term) |
            str_detect(str_to_lower(Ingredients), term) |
            str_detect(str_to_lower(Steps), term)
        )
    }
    
    df
  })
  
  #—— Render recipe titles as clickable table
  output$title_list <- renderDT({
    datatable(
      filtered() %>% select(Title),
      rownames  = FALSE,
      selection = "single",
      options   = list(dom = 't', pageLength = 8, autoWidth = TRUE)
    )
  })
  
  #—— Show the chosen recipe in detail
  output$recipe_details <- renderUI({
    sel <- input$title_list_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(tags$p("No recipe selected", style = "font-style:italic;"))
    }
    
    rec <- filtered()[sel, ]
    ing <- str_split(rec$Ingredients, ";")[[1]] %>% str_trim()
    raw_steps <- rec$Steps
    stps <- str_split(raw_steps, "(?<=\\.) (?=\\d+\\.)")[[1]] %>% str_trim()
    stps <- str_replace_all(stps, "^\\d+\\.\\s*|\\.$", "")
    
    badge_list <- str_split(rec$Badges, ",")[[1]] %>% str_trim()
    badge_tags <- lapply(badge_list, function(b) {
      col <- badge_colors[b]
      if (is.na(col)) col <- "#6c757d"
      tags$span(b,
                style = paste0(
                  "background-color:", col, ";",
                  "color: #fff;",
                  "padding: 0.3em 0.7em;",
                  "margin-right: 0.5em;",
                  "border-radius: 0.25rem;",
                  "font-size: 0.9em;"
                )
      )
    })
    
    tags$div(
      tags$h2(rec$Title, style = "margin-bottom:0;"),
      tags$p(paste("Source:", rec$Source),
             class = "text-muted",
             style = "font-size:0.85em; margin-top:4px; margin-bottom:16px;"),
      tags$div(badge_tags, style = "margin-bottom:20px;"),
      
      tags$h4("Ingredients"),
      tags$ul(lapply(ing, tags$li)),
      
      tags$h4("Steps"),
      tags$ol(lapply(stps, tags$li))
    )
  })
  
  #—— Dynamic UI for “From” / “To” unit selectors
  output$from_ui <- renderUI({
    req(input$conv_category)
    choices <- switch(input$conv_category,
                      "Temperature" = temperature_units,
                      "Weight"      = weight_units,
                      "Volume"      = volume_units
    )
    selectInput("conv_from", "From unit", choices = choices)
  })
  output$to_ui <- renderUI({
    req(input$conv_category)
    choices <- switch(input$conv_category,
                      "Temperature" = temperature_units,
                      "Weight"      = weight_units,
                      "Volume"      = volume_units
    )
    selectInput("conv_to", "To unit", choices = choices)
  })
  
  #—— Conversion calculation
  conv <- reactive({
    req(input$conv_input, input$conv_category, input$conv_from, input$conv_to)
    x <- input$conv_input
    
    # Label mapping for display
    unit_map <- switch(input$conv_category,
                       "Temperature" = temperature_units,
                       "Weight"      = weight_units,
                       "Volume"      = volume_units
    )
    from_lbl <- names(unit_map)[unit_map == input$conv_from]
    to_lbl   <- names(unit_map)[unit_map == input$conv_to]
    
    # Compute
    if (input$conv_category == "Temperature") {
      if (input$conv_from == "C" && input$conv_to == "F")      val <- x * 9/5 + 32
      else if (input$conv_from == "F" && input$conv_to == "C") val <- (x - 32) * 5/9
      else                                                      val <- x
    } else if (input$conv_category == "Weight") {
      grams <- switch(input$conv_from, g = x, oz = x * 28.3495)
      val   <- switch(input$conv_to, g = grams, oz = grams / 28.3495)
    } else {  # Volume
      ml_val <- switch(input$conv_from,
                       tbsp = x * 14.7868,
                       tsp  = x * 4.92892,
                       ml   = x,
                       cup  = x * 236.588
      )
      val <- switch(input$conv_to,
                    ml   = ml_val,
                    tbsp = ml_val / 14.7868,
                    tsp  = ml_val / 4.92892,
                    cup  = ml_val / 236.588
      )
    }
    
    paste0(x, " ", from_lbl, " = ", round(val, 2), " ", to_lbl)
  })
  
  output$conv_result <- renderText(conv())
}

shinyApp(ui, server)