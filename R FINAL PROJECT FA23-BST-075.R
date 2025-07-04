library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(RMariaDB)
library(bslib)
library(stringr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(echarts4r)

con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "superstoredb",
  host = "127.0.0.1",
  port = 3306,
  user = "data_superstore",
  password = "Wasil123$"
)

superstore <- dbGetQuery(con, "SELECT * FROM orders")

superstore$Sales <- as.numeric(superstore$Sales)
superstore$Profit <- as.numeric(superstore$Profit)
superstore$Quantity <- as.numeric(superstore$Quantity)
superstore$ROW_ID <- as.character(superstore$ROW_ID)
ui <- fluidPage(
  theme = bs_theme(),
  titlePanel("Sales Dashboard for a Superstore"),
  sidebarLayout(
    sidebarPanel(
      selectInput("theme_select", "Choose Theme:", choices = c("Default", "Dark")),
      selectInput("filter_category", "Select Category:", choices = NULL),
      selectInput("filter_region", "Select Region:", choices = NULL),
      selectInput("filter_shipmode", "Select Ship Mode:", choices = NULL),
      
      actionButton("reset_click", "Reset Filters", icon = icon("refresh"))
    ),
    mainPanel(
      tabsetPanel(
        id = "dashboard_tabs",
        tabPanel("Overview",
                 fluidRow(
                   column(4, plotlyOutput("info_sales")),
                   column(4, plotlyOutput("info_profit")),
                   column(4, plotlyOutput("info_orders"))
                 )),
        tabPanel("Quantity by Category", echarts4rOutput("animated_category", height = "500px")),
        tabPanel("Sales by Region", echarts4rOutput("live_region", height = "500px")),
        
        tabPanel("Sales by State (Map)", plotlyOutput("state_sales_map")),
        tabPanel("Quantity Analysis (Boxplot)", 
                 selectInput("box_var", "Group by:", choices = c("Segment", 'Sub_Category', "Region", "Ship_Mode")),
                 plotlyOutput("boxplot")),
        tabPanel("Sales Analysis (Violin Plot)",
                 selectInput("violin_group", "Group by:", choices = c("Category", "Segment", "Region")),
                 plotlyOutput("violin_plot")),
        tabPanel("Sales vs Profit (Bubble)", plotlyOutput("plot3")),
        tabPanel("Order Funnel by Ship Mode", plotlyOutput("plot4")),
        tabPanel("Sales by Segment (Area Chart)", plotlyOutput("plot6")),
        tabPanel("Quantity Distribution by Segment (Pie)", echarts4rOutput("animated_pie")),
        tabPanel("Sales Heatmap", plotlyOutput("plot7")),
        tabPanel("Profit by Segment (Line Chart)",  echarts4rOutput("animated_line", height = "600px") ),
        tabPanel("Mean of Sales & Profit", echarts4rOutput("mean_plot", height = "400px")),
        tabPanel("SD of Sales & Profit", echarts4rOutput("sd_plot", height = "400px")),
        tabPanel("Category Hierarchy Tree", plotOutput("category_tree", height = "600px")),
        tabPanel("Quantity Histogram", plotlyOutput("sales_histogram_plot")),
        fluidRow(
          column(12, align = "center",
                 tags$hr(),
                 tags$p("Created by Syeda Rafia Gilani", style = "color:gray; font-size:14px;")
          )
        )
      )
    )
  )
)

state_abbrev <- setNames(state.abb, state.name)

server <- function(input, output, session) {
  filters <- reactiveValues(
    Category = "All",
    Region = "All",
    Ship_Mode = "All"
  )
  
  observeEvent(input$filter_category, {
    filters$Category <- input$filter_category
  }, ignoreInit = TRUE) 
  
  observeEvent(input$filter_region, {
    filters$Region <- input$filter_region
  }, ignoreInit = TRUE)
  
  observeEvent(input$filter_shipmode, {
    filters$Ship_Mode <- input$filter_shipmode
  }, ignoreInit = TRUE)
  
  observe({
    updateSelectInput(session, "filter_category",
                      choices = c("All", unique(superstore$Category)),
                      selected = filters$Category) 
    
    updateSelectInput(session, "filter_region",
                      choices = c("All", unique(superstore$Region)),
                      selected = filters$Region) 
    
    updateSelectInput(session, "filter_shipmode",
                      choices = c("All", unique(superstore$Ship_Mode)),
                      selected = filters$Ship_Mode)
  })
  
  observe({
    theme <- switch(input$theme_select,
                    "Dark" = bs_theme(bg = "#000000", fg = "#FFFFFF", primary = "#00BFFF",
                                      base_font = font_google("Roboto")),
                    bs_theme())
    session$setCurrentTheme(theme)
  })
  
  theme_layout <- reactive({
    if (input$theme_select == "Dark") {
      list(paper_bgcolor = "black", plot_bgcolor = "black", font = list(color = "white"))
    } else {
      list()
    }
  })
  
  observeEvent(event_data("plotly_click", source = "bubble_plot"), {
    ed <- event_data("plotly_click", source = "bubble_plot")
    if (!is.null(ed$customdata) && length(ed$customdata) > 0) {
      clicked_category <- ed$customdata[1]
      filters$Category <- clicked_category
      updateTabsetPanel(session, "dashboard_tabs", selected = "Sales vs Profit (Bubble)")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "region_pie_plot"), {
    ed <- event_data("plotly_click", source = "region_pie_plot")
    if (!is.null(ed$points[[1]]$label)) {
      clicked_region <- ed$points[[1]]$label
      filters$Region <- clicked_region
      updateTabsetPanel(session, "dashboard_tabs", selected = "Sales by Region (Live)")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "funnel_plot"), {
    ed <- event_data("plotly_click", source = "funnel_plot")
    if (!is.null(ed$y) && length(ed$y) > 0) {
      clicked_shipmode <- ed$y[[1]]
      filters$Ship_Mode <- clicked_shipmode
      updateTabsetPanel(session, "dashboard_tabs", selected = "Order Funnel by Ship Mode")
    }
  })
  

  observeEvent(event_data("plotly_click", source = "boxplot_plot"), {
    ed <- event_data("plotly_click", source = "boxplot_plot")
    if (!is.null(ed$x) && length(ed$x) > 0) {
      group <- input$box_var
      clicked_value <- ed$x[[1]]
      
      if (group == "Segment") {
        filters$Category <- clicked_value
      } else if (group == "Region") {
        filters$Region <- clicked_value
      } else if (group == "Ship_Mode") {
        filters$Ship_Mode <- clicked_value
      }
      updateTabsetPanel(session, "dashboard_tabs", selected = "Quantity Analysis (Boxplot)")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "area_plot"), {
    ed <- event_data("plotly_click", source = "area_plot")
    if (!is.null(ed$points[[1]]$text)) {
      seg_match <- stringr::str_match(ed$points[[1]]$text, "Segment: (.*?)<")[,2]
      if (!is.na(seg_match) && length(seg_match) > 0) {
        filters$Category <- seg_match
        updateTabsetPanel(session, "dashboard_tabs", selected = "Sales by Segment (Area Chart)")
      }
    }
  })
  observeEvent(event_data("plotly_click", source = "heatmap_plot"), {
    ed <- event_data("plotly_click", source = "heatmap_plot")
    if (!is.null(ed$x) && !is.null(ed$y) && length(ed$x) > 0 && length(ed$y) > 0) {
      clicked_segment <- ed$x[[1]]
      clicked_region <- ed$y[[1]]
      
      filters$Category <- clicked_segment
      filters$Region <- clicked_region
      updateTabsetPanel(session, "dashboard_tabs", selected = "Sales Heatmap")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "map_plot"), {
    ed <- event_data("plotly_click", source = "map_plot")
    if (!is.null(ed$text) && length(ed$text) > 0) {
      state_clicked <- stringr::str_match(ed$text[[1]], "^(.*)<br>")[,2]
      if (!is.na(state_clicked) && length(state_clicked) > 0) {
        showNotification(paste("You clicked:", state_clicked), type = "message")
      }
      updateTabsetPanel(session, "dashboard_tabs", selected = "Sales by State (Map)")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "histogram_plot"), {
    ed <- event_data("plotly_click", source = "histogram_plot")
    if(!is.null(ed$x) && length(ed$x) > 0) {
      showNotification(paste("Clicked on quantity bin:", round(ed$x, 2)), type = "message")
    }
  })
  
  observeEvent(event_data("plotly_click", source = "violin_plot"), {
    ed <- event_data("plotly_click", source = "violin_plot")
    if (!is.null(ed$x) && length(ed$x) > 0) {
      group <- input$violin_group
      clicked_value <- ed$x[[1]]
      
      if (group == "Category") {
        filters$Category <- clicked_value
      } else if (group == "Segment") {
        filters$Category <- clicked_value
      } else if (group == "Region") {
        filters$Region <- clicked_value
      }
      updateTabsetPanel(session, "dashboard_tabs", selected = "Sales Analysis (Violin Plot)")
    }
  })
  
  observeEvent(input$reset_click, {
    filters$Category <- "All"
    filters$Region <- "All"
    filters$Ship_Mode <- "All"
    
  })
  filtered_data <- reactive({
    data <- superstore
    
    if (filters$Category != "All") {
      data <- data %>% filter(Category == filters$Category)
    }
    if (filters$Region != "All") {
      data <- data %>% filter(Region == filters$Region)
    }
    if (filters$Ship_Mode != "All") {
      data <- data %>% filter(Ship_Mode == filters$Ship_Mode)
    }
    
    data
  })
  
  output$active_filters <- renderUI({
    HTML(paste(
      "<b>Active Filters:</b>",
      if (filters$Category != "All") paste("Category:", filters$Category) else "",
      if (filters$Region != "All") paste(" | Region:", filters$Region) else "",
      if (filters$Ship_Mode != "All") paste(" | Ship Mode:", filters$Ship_Mode) else "",
      if (filters$Category == "All" && filters$Region == "All" && filters$Ship_Mode == "All") "None"
    ))
  })
  output$info_sales <- renderPlotly({
    value <- sum(filtered_data()$Sales, na.rm = TRUE)
    plot_ly(type = 'indicator', mode = 'number', value = value,
            number = list(prefix = "$", font = list(size = 28)),
            title = list(text = "Total Sales")) %>% layout(theme_layout())
  })
  
  output$info_profit <- renderPlotly({
    value <- sum(filtered_data()$Profit, na.rm = TRUE)
    plot_ly(type = 'indicator', mode = 'number', value = value,
            number = list(prefix = "$", font = list(size = 28)),
            title = list(text = "Total Profit")) %>% layout(theme_layout())
  })
  
  output$info_orders <- renderPlotly({
    value <- n_distinct(filtered_data()$ROW_ID)
    plot_ly(type = 'indicator', mode = 'number', value = value,
            number = list(font = list(size = 28)),
            title = list(text = "Total Orders")) %>% layout(theme_layout())
  })
  output$animated_category <- renderEcharts4r({
    invalidateLater(2000, session)
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      group_by(Category) %>%
      summarise(Quantity = sum(Quantity), .groups = "drop") %>%
      e_charts(Category) %>%
      e_pie(Quantity, roseType = "radius", label = list(
        show = TRUE,
        formatter = "{b}: {c}",
        fontSize = 14
      )) %>%
      e_title("Category-wise Quantity") %>%
      e_animation(duration = 1000) %>%
      e_theme("macarons")
  })
  
  output$live_region <- renderEcharts4r({
    invalidateLater(2000, session)
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      group_by(Region) %>%
      summarise(Sales = sum(Sales), .groups = 'drop') %>%
      e_charts(Region) %>%
      e_bar(Sales, name = "Sales") %>%
      e_title("Live Sales by Region") %>%
      e_animation(duration = 800) %>%
      e_theme("walden") %>%
      e_y_axis(
        axisLabel = list(
          fontSize = 14,
          margin = 15,
          formatter = "{value}"
        )
      ) %>%
      e_grid(left = "15%", right = "8%", bottom = "15%", top = "15%")
  })
  
  output$state_sales_map <- renderPlotly({
    state_sales <- filtered_data() %>%
      group_by(State) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop') %>%
      mutate(State_Abbr = state_abbrev[State]) %>%
      filter(!is.na(State_Abbr))
    
    req(nrow(state_sales) > 0)
    
    plot_ly(
      data = state_sales,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~State_Abbr,
      z = ~Sales,
      text = ~paste(State, "<br>Sales: $", round(Sales, 2)),
      colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
      colorbar = list(title = "Total Sales"),
      source = "map_plot"
    ) %>%
      layout(
        title = "Total Sales by U.S. State ",
        geo = list(scope = "usa")
      ) %>% layout(theme_layout())
  })
  
  output$boxplot <- renderPlotly({
    req(input$box_var)
    req(nrow(filtered_data()) > 0)
    
    p <- ggplot(filtered_data(), aes_string(x = input$box_var, y = 'Quantity', fill = input$box_var)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      theme_minimal() +
      labs(title = paste("Quantity by", input$box_var), x = input$box_var, y = 'Quantity') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p, source = "boxplot_plot") %>% layout(theme_layout())
  })
  
  output$violin_plot <- renderPlotly({
    req(input$violin_group)
    req(nrow(filtered_data()) > 0)
    
    p <- ggplot(filtered_data(), aes_string(x = input$violin_group, y = "Sales", fill = input$violin_group)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      scale_y_log10() +    
      theme_minimal() +
      labs(title = paste("Violin plot of Sales grouped by", input$violin_group),
           x = input$violin_group, y = "Sales ")
    
    ggplotly(p, source = "violin_plot") %>% layout(theme_layout())
  })
  
  output$plot3 <- renderPlotly({
    data_for_plot <- filtered_data() %>% filter(Profit >= 0)
    req(nrow(data_for_plot) > 0)
    
    plot_ly(
      data_for_plot,
      x = ~Sales,
      y = ~Profit,
      text = ~paste("Category:", Category),
      type = 'scatter',
      mode = 'markers',
      color = ~Category,
      size = ~Quantity,
      marker = list(opacity = 0.6),
      customdata = ~Category,
      source = "bubble_plot"
    ) %>%
      layout(title = 'Sales vs Profit by Category',
             xaxis = list(title = 'Sales'),
             yaxis = list(title = 'Profit')) %>%
      layout(theme_layout())
  })
  
  output$plot4 <- renderPlotly({
    funnel_data <- filtered_data() %>%
      distinct(ROW_ID, Ship_Mode) %>%
      count(Ship_Mode) %>%
      arrange(desc(n))
    
    req(nrow(funnel_data) > 0)
    
    plot_ly(
      type = "funnel",
      y = funnel_data$Ship_Mode,
      x = funnel_data$n,
      textposition = "inside",
      textinfo = "value+percent initial",
      marker = list(color = RColorBrewer::brewer.pal(length(funnel_data$Ship_Mode), "Pastel1")),
      source = "funnel_plot"
    ) %>%
      layout(title = "Order Funnel by Ship Mode") %>%
      layout(theme_layout())
  })
  
  output$plot6 <- renderPlotly({
    area_data <- filtered_data() %>%
      group_by(Segment) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Segment) %>%
      mutate(Index = seq_along(Segment))
    
    req(nrow(area_data) > 0)
    
    plot_ly(
      data = area_data,
      x = ~Index,
      y = ~Sales,
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      text = ~paste("Segment:", Segment, "<br>Sales: $", round(Sales, 2)),
      hoverinfo = 'text',
      source = "area_plot"
    ) %>%
      layout(
        title = "Area Chart of Sales by Segment",
        xaxis = list(title = "Segment", tickvals = area_data$Index, ticktext = area_data$Segment,
                     tickangle = -45),
        yaxis = list(title = "Total Sales")
      ) %>% layout(theme_layout())
  })
  
  output$animated_pie <- renderEcharts4r({
    invalidateLater(1500, session)
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      group_by(Segment) %>%
      summarise(Quantity = sum(Quantity), .groups = "drop") %>%
      e_charts(Segment) %>%
      e_pie(Quantity, radius = "60%") %>%
      e_tooltip(trigger = "item", formatter = "{b}: {c} ({d}%)") %>%
      e_title("Quantity Distribution by Segment") %>%
      e_animation(duration = 1500)
  })
  
  output$plot7 <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Segment, Region) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop')
    
    all_segments <- unique(superstore$Segment)
    all_regions <- unique(superstore$Region)
    df_complete <- expand.grid(Segment = all_segments, Region = all_regions) %>%
      left_join(df, by = c("Segment", "Region")) %>%
      mutate(Sales = replace_na(Sales, 0))
    
    req(nrow(df_complete) > 0)
    
    p <- ggplot(df_complete, aes(x = Segment, y = Region, fill = Sales,
                                 text = paste("Segment:", Segment, "<br>Region:", Region, "<br>Sales: $", round(Sales, 2)))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Total Sales") +
      labs(title = "Sales Heatmap: Region vs Segment ", x = "Segment", y = "Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text", source = "heatmap_plot") %>% layout(theme_layout())
  })
  
  output$animated_line <- renderEcharts4r({
    invalidateLater(2500, session)
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      filter(Profit >= 0) %>%
      group_by(Segment) %>%
      summarise(Profit = sum(Profit), .groups = 'drop') %>%
      arrange(desc(Profit)) %>%
      e_charts(Segment) %>%
      e_line(Profit, name = "Profit", symbol = "circle") %>%
      e_animation(duration = 2500, easing = "cubicOut") %>%
      e_title("Profit by Segment") %>%
      e_tooltip(trigger = "axis")
  })
  
  output$mean_plot <- renderEcharts4r({
    invalidateLater(1500, session)
    req(nrow(filtered_data()) > 0)
    
    df <- filtered_data()
    
    stats <- data.frame(
      Metric = c("Sales", "Profit"),
      Mean = c(mean(df$Sales, na.rm = TRUE), mean(df$Profit, na.rm = TRUE)*17)
    )
    
    stats %>%
      e_charts(Metric) %>%
      e_bar(Mean, name = "Mean") %>%
      e_labels(show = TRUE, position = "top") %>%
      e_tooltip(trigger = "item") %>%
      e_animation(duration = 1500, easing = "bounceOut") %>%
      e_color(c("cornflowerblue", "mediumseagreen")) %>%
      e_y_axis(name = "Mean Value") %>%
      e_x_axis(name = "Metric") %>%
      e_title("Mean of Sales & Profit")
  })
  
  output$sd_plot <- renderEcharts4r({
    invalidateLater(1500, session)
    req(nrow(filtered_data()) > 1)
    
    df <- filtered_data()
    
    stats <- data.frame(
      Metric = c("Sales", "Profit"),
      SD = c(sd(df$Sales, na.rm = TRUE), sd(df$Profit, na.rm = TRUE))
    )
    
    stats %>%
      e_charts(Metric) %>%
      e_bar(SD, name = "Standard Deviation") %>%
      e_title("Standard Deviation of Sales and Profit") %>%
      e_tooltip(trigger = "item", formatter = "{b}: {c}") %>%
      e_animation(duration = 2000, easing = "bounceOut") %>%
      e_color(c("tomato", "seagreen")) %>%
      e_y_axis(name = "Standard Deviation") %>%
      e_x_axis(name = "Metric")
  })
  
  
  category_graph <- reactive({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    sales_superstore <- sum(data$Sales, na.rm = TRUE)
    sales_category <- data %>%
      group_by(Category) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop')
    
    sales_subcat <- data %>%
      group_by(Category, Sub_Category) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop')
    
    edges_cat <- sales_category %>%
      mutate(from = "Superstore", to = Category) %>%
      select(from, to)
    
    edges_subcat <- sales_subcat %>%
      select(from = Category, to = Sub_Category)
    
    edges <- bind_rows(edges_cat, edges_subcat)
    
    g <- graph_from_data_frame(edges, directed = TRUE)
    
    V(g)$sales <- NA_real_
    
    V(g)$sales[V(g)$name == "Superstore"] <- sales_superstore
    
    for(cat in sales_category$Category) {
      V(g)$sales[V(g)$name == cat] <- sales_category$Sales[sales_category$Category == cat]
    }
    
    for(subcat in sales_subcat$Sub_Category) {
      V(g)$sales[V(g)$name == subcat] <- sales_subcat$Sales[sales_subcat$Sub_Category == subcat]
    }
    
    V(g)$sales[is.na(V(g)$sales)] <- 0
    
    g
  })
  
  output$category_tree <- renderPlot({
    g <- category_graph()
    req(!is.null(g) && vcount(g) > 0)
    
    sales_for_scaling <- V(g)$sales
    sales_for_scaling[sales_for_scaling == 0] <- min(sales_for_scaling[sales_for_scaling > 0], na.rm = TRUE) / 10
    
    labels <- paste0(V(g)$name, "\n$", format(round(V(g)$sales, 0), big.mark = ","))
    sales_scaled <- scales::rescale(log1p(sales_for_scaling), to = c(2, 8))
    
    p <- ggraph(g, layout = 'tree', circular = FALSE) +
      geom_edge_diagonal(alpha = 0.6) +
      geom_node_point(aes(size = sales_scaled), color = "steelblue", alpha = 0.8) +
      geom_node_text(aes(label = labels), hjust = -0.2, size = 3.5, color = if(input$theme_select == "Dark") "white" else "black") +
      scale_size_identity() +
      theme_void() +
      labs(title = "Category Hierarchy Tree with Sales") +
      coord_cartesian(clip = 'off')
    
    if (input$theme_select == "Dark") {
      p <- p + theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white", hjust = 0.5)
      )
    } else {
      p <- p + theme(plot.title = element_text(hjust = 0.5))
    }
    
    print(p)
  })
  
  output$sales_histogram_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    plot_ly(
      data = filtered_data(),
      x = ~Quantity,
      color = I('lightblue'),
      type = 'histogram',
      opacity = 0.6,
      source = "histogram_plot"
    ) %>%
      layout(
        title = "Quantity Histogram by Orders",
        barmode = "overlay",
        xaxis = list(title = "Quantity"),
        yaxis = list(title = "Count of Orders"),
        legend = list(title = list(text = "<b>Category</b>"))
      ) %>% layout(theme_layout())
  })
  
}

shinyApp(ui, server)