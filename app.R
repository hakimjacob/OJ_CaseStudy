library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(shinyjs)

options(scipen=999)
options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB max upload

## Read csvs
data_dir <- "data"
store <- read.csv(file.path(data_dir, "Store_table1.csv"))
date  <- read.csv(file.path(data_dir, "Date_table1.csv"))
prod  <- read.csv(file.path(data_dir, "Product_table1.csv")) %>%
    rename("upc" = "id_upc")

## UI
ui <- dashboardPage(
    
    dashboardHeader(title = "Juice Insights",
                    tags$li(
                        a(
                            href = "https://www.heb.com",  # optional: make it clickable
                            img(src = "H-E-B_logo.png", height = "30px"),  # adjust height as needed
                            style = "padding-top:5px; padding-right:10px;"
                        ),
                        class = "dropdown"
                    )
                ),
    
    dashboardSidebar(
        useShinyjs(),
        
        actionButton("reset", "Reset Filters", icon = icon("undo")),
        
        h4("Region"),
        div(style = "display:flex; flex-wrap: wrap;",
            checkboxGroupButtons(
                inputId = "region",
                label = NULL,
                choices = c("All", sort(unique(store$region))),
                selected = "All",
                justified = FALSE,
                status = "danger",
                size = "sm",
                checkIcon = list(yes = icon("circle-check"))
            )
        ),
        
        h4("Store Format"),
        div(style = "display:flex; flex-wrap: wrap;",
            checkboxGroupButtons(
                inputId = "store_segment",
                label = NULL,
                choices = c("All", sort(unique(store$store_segment))),
                selected = "All",
                justified = FALSE,
                status = "warning",
                size = "sm",
                checkIcon = list(yes = icon("circle-check"))
            )
        ),
        
        h4("Product Type"),
        div(style = "display:flex; flex-wrap: wrap;",
            checkboxGroupButtons(
                inputId = "category",
                label = NULL,
                choices = c("All", sort(unique(prod$category))),
                selected = "All",
                justified = FALSE,
                status = "info",
                size = "sm",
                checkIcon = list(yes = icon("circle-check"))
            )
        ),
        
        h4("Fiscal Period"),
        sliderTextInput(
            inputId = "fscl_period",
            label = "",
            choices = sort(unique(date$fscl_period)),
            selected = range(sort(unique(date$fscl_period))),
            grid = TRUE
        ),
        
        h4("Fiscal Week"),
        sliderTextInput(
            inputId = "fscl_week",
            label = "",
            choices = sort(unique(date$fscl_week)),
            selected = range(sort(unique(date$fscl_week))),
            grid = TRUE
        ),
        
        fileInput("newfile", "Upload New Transactions (.csv)", accept = ".csv")
    ),
    
    
    dashboardBody(
        tags$head(
            tags$style(HTML("
                .skin-blue .main-header .navbar { background-color: #c21807; }
                .skin-blue .main-header .logo { background-color: #c21807; }
                .skin-blue .main-sidebar { background-color: #8b0000; }
                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { background-color: #c21807; }
                .box.box-solid.box-primary>.box-header { background: #c21807; color: white; }
                .box.box-solid.box-primary{ border-bottom-color: #8b0000; }
                .btn-group .btn { margin-bottom: 5px; }
            "))
        ),
        
        fluidRow(
            valueBoxOutput("total_baskets"),
            valueBoxOutput("total_sales"),
            valueBoxOutput("avg_items"),
            valueBoxOutput("avg_sales")
        ),
        
        fluidRow(
            box(width = 6, title = "Sales over time", status = "primary", solidHeader = TRUE,
                plotOutput("sales_trend")),
            box(width = 6, title = "Basket sales across stores", status = "primary", solidHeader = TRUE,
                plotOutput("basket_efficiency"))
        ),
        
        fluidRow(
            box(width = 7, title = "KPIs by category", status = "primary", solidHeader = TRUE,
                tableOutput("summary_table")),
            box(width = 5, title = "Sales by fiscal week", status = "primary", solidHeader = TRUE,
                tableOutput("ownbrand_weekly")
            )
        )
    )
)

## Server
server <- function(input, output, session) {
    
    master_data <- reactiveVal({
        trans <- readRDS(file.path(data_dir, "trans_data.rds"))
        trans$dt_id <- as.Date(trans$dt_id, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
        date$dt_id <- as.Date(date$dt_id, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
        
        merged <- merge(trans, store, by = "store_id") %>%
            merge(date, by = "dt_id") %>%
            merge(prod, by = "upc") %>%
            group_by(trip_id) %>%
            mutate(
                items_per_basket = sum(units, na.rm = TRUE),
                sales_per_basket = sum(sales, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            distinct()
        merged
    })
    
#Updates data when new files are added
    observeEvent(input$newfile, {
        new_trans <- read.csv(input$newfile$datapath)
        new_trans$dt_id <- as.Date(new_trans$dt_id, tryFormats = c("%m/%d/%Y", "%Y-%m-%d"))
        new_merged <- merge(new_trans, store, by="store_id") %>%
            merge(date, by="dt_id") %>%
            merge(prod, by="upc") %>%
            group_by(trip_id) %>%
            mutate(
                items_per_basket = sum(units, na.rm = TRUE),
                sales_per_basket = sum(sales, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            distinct()
        
        combined <- bind_rows(master_data(), new_merged) %>% distinct()
        master_data(combined)
    })
    
# Button code to deselect "All" when others are selected
    toggleAll <- function(inputId, inputVals) {
        if ("All" %in% inputVals && length(inputVals) > 1) {
            updateCheckboxGroupButtons(session, inputId, selected = setdiff(inputVals, "All"))
        } else if (!"All" %in% inputVals && length(inputVals) == 0) {
            updateCheckboxGroupButtons(session, inputId, selected = "All")
        }
    }
    
# allows half second of inputs before loading new filter combo    
    observe({
        toggleAll("region", input$region)
        toggleAll("store_segment", input$store_segment)
        toggleAll("category", input$category)
    }) %>% debounce(500) 
    
## REACTIVE FILTERED DATASET
    filtered_data <- reactive({
        df <- master_data()
        req(df, nrow(df) > 0)
        
        if (!("All" %in% input$region)) df <- df %>% filter(region %in% input$region)
        if (!("All" %in% input$store_segment)) df <- df %>% filter(store_segment %in% input$store_segment)
        if (!("All" %in% input$category)) df <- df %>% filter(category %in% input$category)
        
        df <- df %>%
            filter(as.numeric(fscl_period) >= as.numeric(input$fscl_period[1]),
                   as.numeric(fscl_period) <= as.numeric(input$fscl_period[2])) %>%
            filter(as.numeric(fscl_week) >= as.numeric(input$fscl_week[1]),
                   as.numeric(fscl_week) <= as.numeric(input$fscl_week[2]))
        df
    }) %>% debounce(500)
    
# REACTIVE SUMMARIES
    summary_metrics <- reactive({
        df <- filtered_data()
        req(nrow(df) > 0)
        tibble(
            total_baskets = n_distinct(df$trip_id),
            total_sales   = sum(df$sales, na.rm = TRUE),
            avg_items     = mean(df$items_per_basket, na.rm = TRUE),
            avg_sales     = mean(df$sales_per_basket, na.rm = TRUE)
        )
    })
    
# Calculations for KPI boxes
    output$total_baskets <- renderValueBox({
        valueBox(summary_metrics()$total_baskets, "Total Baskets", color = "orange")
    })
    output$total_sales <- renderValueBox({
        valueBox(scales::dollar(summary_metrics()$total_sales), "Total Sales", color = "green")
    })
    output$avg_items <- renderValueBox({
        valueBox(round(summary_metrics()$avg_items, 2), "Avg Items / Basket", color = "blue")
    })
    output$avg_sales <- renderValueBox({
        valueBox(round(summary_metrics()$avg_sales, 2), "Avg Sales / Basket", color = "purple")
    })
    
# Filter reset button
    observeEvent(input$reset, {
        updateCheckboxGroupButtons(session, "region", selected = "All")
        updateCheckboxGroupButtons(session, "store_segment", selected = "All")
        updateCheckboxGroupButtons(session, "category", selected = "All")
        updateSliderTextInput(session, "fscl_period", selected = range(sort(unique(date$fscl_period))))
        updateSliderTextInput(session, "fscl_week", selected = range(sort(unique(date$fscl_week))))
    })
    
### PLOTS ###

##Plot 1
    output$sales_trend <- renderPlot({
        df <- filtered_data()
        req(nrow(df) > 0)
        
        own_brand_codes <- c("HEB", "HEBO", "CM", "CMO", "HCF")
        

        df_summary <- df %>%
            group_by(fscl_week, category) %>%
            summarise(
                Total_Sales = sum(sales, na.rm = TRUE),
                OwnBrand_Sales = sum(
                    sales[grepl(paste(own_brand_codes, collapse = "|"), dsc_upc) |
                              grepl("in-store", sub_commodity, ignore.case = TRUE)],
                    na.rm = TRUE
                ),
                .groups = "drop"
            )
        
# Plot
        ggplot(df_summary, aes(x = factor(fscl_week), y = Total_Sales, fill = category)) +
            geom_col(position = "stack") +
            geom_segment(
                data = df_summary %>% group_by(fscl_week) %>% summarise(
                    Total_Sales_Week = sum(Total_Sales),
                    OwnBrand_Sales_Week = sum(OwnBrand_Sales)
                ),
                aes(
                    x = as.numeric(factor(fscl_week)) - 0.4,
                    xend = as.numeric(factor(fscl_week)) + 0.4,
                    y = OwnBrand_Sales_Week,
                    yend = OwnBrand_Sales_Week,
                    color = "Own Brand"
                ),
                size = 1, linetype = "dashed",
                inherit.aes = FALSE
            ) +
            scale_color_manual(values = c("Own Brand" = "red")) +
            guides(color = guide_legend(override.aes = list(linetype = "dashed", size = 1))) +
            scale_y_continuous(labels = scales::dollar) +
            labs(x = "Fiscal Week", y = "Sales ($)", color = "") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
## Plot 2
    output$basket_efficiency <- renderPlot({
        df <- filtered_data()
        req(nrow(df) > 0)
        
        df_summary <- df %>%
            group_by(store_segment, category) %>%
            summarise(
                Avg_Items = mean(items_per_basket, na.rm = TRUE),
                Avg_Sales = mean(sales_per_basket, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            mutate(store_segment = factor(store_segment, levels = c("Value", "Core", "Up")))
        
        ggplot(df_summary, aes(x = store_segment, y = Avg_Sales, fill = category)) +
            geom_col(position = position_dodge(width = 0.9)) +
            geom_text(aes(label = round(Avg_Sales, 1)),
                      position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
            scale_y_continuous(labels = scales::dollar) +
            labs(title = "Sales per Basket Across Stores",
                 x = "Store Segment", y = "Sales per basket ($)") +
            theme_minimal(base_size = 14)
    })
    
## Table 1
    output$summary_table <- renderTable({
        df <- filtered_data()
        req(nrow(df) > 0)
        
        df %>%
            group_by(category) %>%
            summarise(
                Total_Sales = sum(sales, na.rm = TRUE),
                Total_Baskets = n_distinct(trip_id),
                Avg_Basket_Sales = mean(sales_per_basket, na.rm = TRUE),
                Avg_Items = mean(items_per_basket, na.rm = TRUE),
                Own_Brand_Sales = sum(sales[grepl("HEB|HEBO|CM|CMO|HCF", dsc_upc) |
                                                grepl("in-store", sub_commodity, ignore.case=TRUE)], 
                                      na.rm = TRUE),
                .groups = "drop"
            ) %>%
            mutate(`Own Brand (%)` = round(Own_Brand_Sales / Total_Sales * 100, 1)) %>%
            # Rename columns for display
            rename(
                "Category" = category,
                "Sales ($)" = Total_Sales,
                "Baskets" = Total_Baskets,
                "Sales per basket ($)" = Avg_Basket_Sales,
                "Items per basket" = Avg_Items,
                "Own Brand ($)" = Own_Brand_Sales
            )
    })
    
    
# Second table with fiscal week and own brand info
    output$ownbrand_weekly <- renderTable({
        df <- filtered_data()
        req(nrow(df) > 0)
        
        own_brand_codes <- c("HEB", "HEBO", "CM", "CMO", "HCF")
        
        df_summary <- df %>%
            group_by(fscl_week) %>%
            summarise(
                Total_Sales_Week = sum(sales, na.rm = TRUE),
                OwnBrand_Sales_Week = sum(
                    sales[grepl(paste(own_brand_codes, collapse = "|"), dsc_upc) |
                              grepl("in-store", sub_commodity, ignore.case = TRUE)],
                    na.rm = TRUE
                ),
                .groups = "drop"
            ) %>%
            mutate(`% Own Brand Sales` = round(OwnBrand_Sales_Week / Total_Sales_Week * 100, 1)) %>%
            rename(
                "Fiscal Week" = fscl_week,
                "Sales ($)" = Total_Sales_Week,
                "Own Brand ($)" = OwnBrand_Sales_Week,
                "Own Brand (%)" = `% Own Brand Sales`
            )
    })
    
    
}

shinyApp(ui, server)
