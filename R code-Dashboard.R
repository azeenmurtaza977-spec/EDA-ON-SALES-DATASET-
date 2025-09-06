library(RColorBrewer)
library(lubridate)
library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(shinydashboard)
library(ggplot2)

# Load data
data <- read_csv("C:/Users/SHAH COMPUTERS/Downloads/Sales Dataset.csv")
names(data) <- gsub("[ -]", "_", names(data))  # Clean column names

# Fix date and create Year_Month
data$Order_Date <- parse_date_time(data$Order_Date, orders = c("ymd", "mdy", "dmy"))
data$Year_Month <- format(data$Order_Date, "%Y-%m")

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Project"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Graphs", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Animations", tabName = "animated_graphs", icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    fluidRow(
      column(12, align = "center", h1("FA23-BST-021 (Sales Dashboard)"))
    ),
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales_value"),
                valueBoxOutput("total_profit_value"),
                valueBoxOutput("total_quantity_value")
              ),
              fluidRow(
                column(4, div(style = "background-color: #B784F5; border-radius: 10px; padding: 10px; color: white;", verbatimTextOutput("total_sales"))),
                column(4, div(style = "background-color: #B784F5; border-radius: 10px; padding: 10px; color: white;", verbatimTextOutput("total_profit"))),
                column(4, div(style = "background-color: #B784F5; border-radius: 10px; padding: 10px; color: white;", verbatimTextOutput("total_quantity")))
              ),
              fluidRow(
                infoBoxOutput("top_product_info"),
                infoBoxOutput("top_state_info"),
                infoBoxOutput("top_city_info")
              ),
              p("The overview section highlights the key performance indicators (KPIs) of the sales dashboard and the top-performing product, state, and city in terms of sales.")
      ),
      
      # Visualizations tab
      tabItem(tabName = "visualizations",
              h2("Top 10 Most Sold Subcategories"),
              plotlyOutput("top_subcategories"),
              h2(" 6 Least Sold Subcategories"),
              plotlyOutput("bottom_subcategories"),
              h2("Top 10 Customers by Sales"),
              plotlyOutput("top_customers_treemap"),
              h2("Donut Chart of Sales by Category"),
              plotlyOutput("donut_chart_category"),
              h2("Pie Chart of Sales by Subcategory"),
              plotlyOutput("pie_chart_subcategory"),
              h2("Line Profit Chart"),
              plotlyOutput("line_profit"),
              h2("3D Scatter Plot"),
              plotlyOutput("scatter_3d"),
              h2("3D Surface Plot"),
              plotlyOutput("surface_3d"),
              h2("Violin Plot"),
              plotlyOutput("violin_plot"),
              h2("Line Chart of Sales Trend Over Time"),
              plotlyOutput("line_chart_trend"),
              h2("Density Plot of Sales"),
              plotlyOutput("density_plot"),
              h2("Pie Chart"),
              plotlyOutput("pie_chart"),
              h2("Radial Chart of Sales by Category"),
              plotlyOutput("radial_chart"),
              h2("Sankey Map for Category Sub-Category"),
              plotlyOutput("sankey_map"),
              h2("Top 10 Months with Highest Sales"),
              plotlyOutput("top_months_line_chart"),
              plotlyOutput("bar_chart"),
          
              plotlyOutput("correlation_heatmap")
      ),
      
      # Animated graphs tab
      tabItem(tabName = "animated_graphs",
              plotlyOutput("animated_pie_chart"),
              textOutput("animated_pie_chart_desc"),
              plotlyOutput("animated_donut_chart"),
              textOutput("animated_donut_chart_desc"),
              plotlyOutput("animated_scatter_sales_profit"),
              textOutput("animated_scatter_sales_profit_desc"),
              plotlyOutput("animated_boxplot_sales"),
              textOutput("animated_boxplot_sales_desc"),
              plotlyOutput("animated_bar_chart")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$total_sales_value <- renderValueBox({
    valueBox(
      paste("Total Sales: $", round(sum(data$Sales, na.rm = TRUE), 2)),
      "Total Sales",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$total_profit_value <- renderValueBox({
    valueBox(
      paste("Total Profit: $", round(sum(data$Profit, na.rm = TRUE), 2)),
      "Total Profit",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$total_quantity_value <- renderValueBox({
    valueBox(
      paste("Total Quantity:", sum(data$Quantity, na.rm = TRUE)),
      "Total Quantity",
      icon = icon("box"),
      color = "yellow"
    )
  })
  
  output$top_product_info <- renderInfoBox({
    prod <- data %>% group_by(Sub_Category) %>% summarise(Total = sum(Sales, na.rm = TRUE)) %>% arrange(desc(Total)) %>% slice(1)
    infoBox(
      "Top Product",
      paste("Most Sold Product:", prod$Sub_Category),
      icon = icon("product-hunt"),
      color = "purple"
    )
  })
  
  output$top_state_info <- renderInfoBox({
    state <- data %>% group_by(State) %>% summarise(Total = sum(Sales, na.rm = TRUE)) %>% arrange(desc(Total)) %>% slice(1)
    infoBox(
      "Top State",
      paste("Highest Sales State:", state$State),
      icon = icon("map-marker-alt"),
      color = "orange"
    )
  })
  
  output$top_city_info <- renderInfoBox({
    city <- data %>% group_by(City) %>% summarise(Total = sum(Sales, na.rm = TRUE)) %>% arrange(desc(Total)) %>% slice(1)
    infoBox(
      "Top City",
      paste("Highest Sales City:", city$City),
      icon = icon("city"),
      color = "red"
    )
  })
  
  output$total_sales <- renderText({
    paste("Total Sales:", round(sum(data$Sales, na.rm = TRUE), 2))
  })
  
  output$total_profit <- renderText({
    paste("Total Profit:", round(sum(data$Profit, na.rm = TRUE), 2))
  })
  
  output$total_quantity <- renderText({
    paste("Total Quantity:", sum(data$Quantity, na.rm = TRUE))
  })
  
  output$top_subcategories <- renderPlotly({
    df <- data %>% group_by(Sub_Category) %>% summarise(Sales = sum(Sales)) %>% arrange(desc(Sales)) %>% head(10)
    plot_ly(df, x = ~Sub_Category, y = ~Sales, type = 'bar', color = ~Sub_Category, colors = rainbow(nrow(df))) %>%
      layout(title = "Top 10 Most Sales Subcategories", xaxis = list(title = "Sub Category"), yaxis = list(title = "Sales")) %>%
      add_annotations(text = ~Sales, x = ~Sub_Category, y = ~Sales, showarrow = FALSE, yshift = 10)
  })
  
  output$bottom_subcategories <- renderPlotly({
    df <- data %>% group_by(Sub_Category) %>% summarise(Sales = sum(Sales)) %>% arrange(Sales) %>% head(6)
    plot_ly(df, x = ~Sub_Category, y = ~Sales, type = 'bar', color = ~Sub_Category, colors = rainbow(nrow(df))) %>%
      layout(title = "6 Least Sales Subcategories", xaxis = list(title = "Sub Category"), yaxis = list(title = "Sales")) %>%
      add_annotations(text = ~Sales, x = ~Sub_Category, y = ~Sales, showarrow = FALSE, yshift = 5)
  })
  
  output$donut_chart_category <- renderPlotly({
    df <- data %>% group_by(Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, labels = ~Category, values = ~Sales, type = 'pie', hole = 0.5, text = ~paste(Category, ": ", Sales), textinfo = 'text', marker = list(colors = rainbow(nrow(df)))) %>%
      layout(title = "Donut Chart of Sales by Category", showlegend = FALSE)
  })
  
  output$pie_chart_subcategory <- renderPlotly({
    df <- data %>% group_by(Sub_Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, labels = ~Sub_Category, values = ~Sales, type = 'pie') %>%
      layout(title = "Pie Chart of Sales by Subcategory")
  })
  
  output$line_profit <- renderPlotly({
    df <- data %>% group_by(Year_Month) %>% summarise(Profit = sum(Profit))
    plot_ly(df, x = ~Year_Month, y = ~Profit, type = 'scatter', mode = 'lines+markers')
  })
  
  output$scatter_3d <- renderPlotly({
    df <- data %>% group_by(Category, Year_Month) %>% summarise(Sales = sum(Sales))
    plot_ly(df, x = ~Year_Month, y = ~Category, z = ~Sales, type = 'scatter3d', mode = 'markers') %>%
      layout(
        scene = list(
          xaxis = list(title = "Year-Month"),
          yaxis = list(title = "Category"),
          zaxis = list(title = "Sales")
        )
      )
  })
  
  output$surface_3d <- renderPlotly({
    df <- data %>% group_by(Year_Month, Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, x = ~Year_Month, y = ~Category, z = ~Sales, type = 'mesh3d') %>%
      layout(
        scene = list(
          xaxis = list(title = "Year-Month"),
          yaxis = list(title = "Category"),
          zaxis = list(title = "Sales")
        )
      )
  })
  
  output$violin_plot <- renderPlotly({
    plot_ly(data, x = ~Category, y = ~Sales, type = 'violin')
  })
  
  output$pie_chart <- renderPlotly({
    df <- data %>% group_by(Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, labels = ~Category, values = ~Sales, type = 'pie')
  })
  
  output$sankey_map <- renderPlotly({
    df <- data %>% group_by(Category, Sub_Category) %>% summarise(Sales = sum(Sales))
    
    nodes <- unique(c(as.character(df$Category), as.character(df$Sub_Category)))
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = nodes,
        color = "blue",
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5)
      ),
      link = list(
        source = match(df$Category, nodes) - 1,
        target = match(df$Sub_Category, nodes) - 1,
        value = df$Sales
      )
    ) %>%
      layout(
        title = "Sankey Diagram",
        font = list(size = 10)
      )
  })
  
  output$line_chart_trend <- renderPlotly({
    df <- data %>% group_by(Year_Month) %>% summarise(Sales = sum(Sales))
    plot_ly(df, x = ~Year_Month, y = ~Sales, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Sales Trend Over Time", xaxis = list(title = "Year-Month"), yaxis = list(title = "Sales"))
  })
  
  output$density_plot <- renderPlotly({
    plot_ly(data, x = ~Sales, type = 'histogram', histnorm = 'probability density') %>%
      layout(title = "Density Plot of Sales", xaxis = list(title = "Sales"), yaxis = list(title = "Density"))
  })
  
  output$top_months_line_chart <- renderPlotly({
    df <- data %>% 
      group_by(Year_Month) %>% 
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>% 
      arrange(desc(Sales)) %>% 
      head(10)
    
    plot_ly(df, x = ~Year_Month, y = ~Sales, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Top 10 Months with Highest Sales", 
             xaxis = list(title = "Year-Month"), 
             yaxis = list(title = "Sales"))
  })
  
  output$bar_chart <- renderPlotly({
    p <- ggplot(data, aes(x = reorder(Category, Sales), y = Sales, fill = Category)) +
      geom_col() +
      labs(title = "Sales by Category", x = "Category", y = "Sales") +
      theme_classic() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$animated_pie_chart <- renderPlotly({
    df <- data %>% group_by(Year_Month, Category) %>% summarise(Sales = sum(Sales, na.rm = TRUE))
    p <- plot_ly(df, labels = ~Category, values = ~Sales, frame = ~Year_Month, type = 'pie') %>%
      animation_slider(
        currentvalue = list(prefix = "Year-Month: ")
      ) %>%
      animation_opts(
        frame = 500,
        transition = 0,
        redraw = TRUE
      )
    p$x$layout$updatemenus = list(
      list(
        type = "buttons",
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(
              NULL,
              list(
                frame = list(duration = 500, redraw = TRUE),
                transition = list(duration = 0),
                fromcurrent = TRUE,
                mode = "immediate"
              )
            )
          ),
          list(
            label = "Pause",
            method = "animate",
            args = list(
              list(
                frame = list(duration = 1000000, redraw = FALSE), # Set a high duration to pause
                transition = list(duration = 0),
                fromcurrent = TRUE,
                mode = "immediate"
              )
            )
          )
        )
      )
    )
    p
  })
  
  output$animated_pie_chart_desc <- renderText({
    "Animated Pie Chart of Sales by Category Over Time"
  })
  
  output$animated_donut_chart <- renderPlotly({
    df <- data %>% group_by(Year_Month, Category) %>% summarise(Sales = sum(Sales, na.rm = TRUE))
    plot_ly(df, labels = ~Category, values = ~Sales, frame = ~Year_Month, type = 'pie', hole = 0.5) %>%
      animation_slider(
        currentvalue = list(prefix = "Year-Month: ")
      ) %>%
      animation_opts(
        frame = 500,
        transition = 0,
        redraw = TRUE
      )
  })
  
  output$animated_donut_chart_desc <- renderText({
    "Animated Donut Chart of Sales by Category Over Time"
  })
  
  output$animated_scatter_sales_profit <- renderPlotly({
    plot_ly(data, x = ~Sales, y = ~Profit, type = 'scatter', mode = 'markers', frame = ~Year_Month) %>%
      animation_slider(
        currentvalue = list(prefix = "Year-Month: ")
      ) %>%
      animation_opts(
        frame = 500,
        transition = 0,
        redraw = TRUE
      )
  })
  
  output$animated_scatter_sales_profit_desc <- renderText({
    "Animated Scatter Plot of Sales vs Profit"
  })
  
  output$animated_boxplot_sales <- renderPlotly({
    df <- data %>% group_by(Category, Year_Month) %>% summarise(Sales = sum(Sales))
    plot_ly(df, x = ~Category, y = ~Sales, type = 'box', frame = ~Year_Month) %>%
      animation_slider(
        currentvalue = list(prefix = "Year-Month: ")
      ) %>%
      animation_opts(
        frame = 500,
        transition = 0,
        redraw = TRUE
      )
  })
  
  output$animated_boxplot_sales_desc <- renderText({
    "Animated Boxplot of Sales by Category Over Time"
  })
  
  output$animated_bar_chart <- renderPlotly({
    df <- data %>%
      group_by(Category, Year_Month) %>%
      summarise(Sales = sum(Sales))
    
    plot_ly(df, x = ~Category, y = ~Sales, frame = ~Year_Month, type = 'bar') %>%
      layout(title = "Sales by Category Over Time",
             xaxis = list(title = "Category"),
             yaxis = list(title = "Sales"),
             barmode = 'group')
  })
  
  output$radial_chart <- renderPlotly({
    df <- data %>% group_by(Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, r = ~Sales, theta = ~Category, type = 'barpolar') %>%
      layout(title = "Sales by Category",
             polar = list(
               radialaxis = list(visible = TRUE),
               angularaxis = list(visible = TRUE)
             ))
  })
  
  output$top_customers_treemap <- renderPlotly({
    req(data) # Ensure data is loaded
    
    df_grouped <- data %>%
      group_by(CustomerName) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(Sales)) %>%
      head(10)
    
    df_grouped$label <- paste(df_grouped$CustomerName, ": $", round(df_grouped$Sales, 2))
    
    plot_ly(df_grouped, labels = ~label, parents = NA, values = ~Sales,
            type = 'treemap', marker = list(colors = ~Sales,
                                            colorscale = 'Blues',
                                            colorbar = list(title = "Sales Amount",
                                                            tickprefix = "$"))) %>%
      layout(title = "Top 10 Customers by Sales")
  })
  output$top_months_line_chart <- renderPlotly({
    df <- data %>% 
      group_by(Year_Month) %>% 
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>% 
      arrange(desc(Sales)) %>% 
      head(10)
    
    plot_ly(df, x = ~Sales, y = ~Year_Month, type = 'scatter', mode = 'markers+text', 
            text = ~paste("Sales: ", Sales), textposition = 'top right') %>%
      layout(title = "Top 10 Months with Highest Sales", 
             xaxis = list(title = "Sales"), 
             yaxis = list(title = "Year-Month"))
  })
  output$bar_chart <- renderPlotly({
    p <- ggplot(data, aes(x = Category, y = Sales, fill = Sub_Category)) +
      geom_col(position = "stack") +
      labs(title = "Sales by Category and Subcategory", x = "Category", y = "Sales") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  output$bar_chart <- renderPlotly({
    df <- data %>% 
      group_by(Category, Sub_Category) %>% 
      summarise(Sales = sum(Sales))
    
    p <- ggplot(df, aes(x = Category, y = Sales, fill = Sub_Category)) +
      geom_col(position = "stack") +
      geom_text(aes(label = Sales), position = position_stack(vjust = 0.5), size = 3) +
      labs(title = "Sales by Category and Subcategory", x = "Category", y = "Sales") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  output$violin_plot <- renderPlotly({
    plot_ly(data, x = ~Category, y = ~Sales, type = 'violin', 
            box = list(visible = TRUE), 
            meanline = list(visible = TRUE)) %>%
      layout(title = "Distribution of Sales of Products by Category", 
             xaxis = list(title = "Category"), 
             yaxis = list(title = "Sales")) %>%
      config(displayModeBar = FALSE)
  })
  output$radial_chart <- renderPlotly({
    df <- data %>% group_by(Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, r = ~Sales, theta = ~Category, type = 'barpolar', 
            color = ~Category, colors = rainbow(nrow(df)),
            text = ~paste(Category, ": ", Sales), hoverinfo = "text") %>%
      layout(title = "Sales by Category (Radial Chart)", 
             polar = list(
               radialaxis = list(visible = TRUE, title = "Sales"),
               angularaxis = list(visible = TRUE, title = "Category")
             ))
  })
  
  output$correlation_heatmap <- renderPlotly({
    df <- data %>% 
      select(Sales, Profit, Quantity)
    
    correlation_matrix <- cor(df, use = "pairwise.complete.obs")
    
    plot_ly(z = correlation_matrix, type = "heatmap", colors = colorRamp(c("lightblue", "blue")), 
            text = round(correlation_matrix, 2), hoverinfo = "text") %>%
      layout(title = "Correlation Heatmap", 
             xaxis = list(title = "", ticktext = colnames(correlation_matrix), tickvals = 0:(ncol(correlation_matrix) - 1)),
             yaxis = list(title = "", ticktext = colnames(correlation_matrix), tickvals = 0:(nrow(correlation_matrix) - 1)))
  })
  output$donut_chart_category <- renderPlotly({
    df <- data %>% group_by(Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, labels = ~Category, values = ~Sales, type = 'pie', hole = 0.5, 
            marker = list(colors = c("purple", "darkblue", "maroon"))) %>%
      layout(title = "Donut Chart of Sales by Category", 
             legend = list(orientation = 'v'))
  })
  output$bottom_subcategories <- renderPlotly({
    df <- data %>% group_by(Sub_Category) %>% summarise(Sales = sum(Sales)) %>% arrange(Sales) %>% head(6)
    plot_ly(df, x = ~Sales, y = ~Sub_Category, type = 'bar', color = ~Sub_Category, colors = rainbow(nrow(df))) %>%
      layout(title = "6 Least Sales Subcategories", xaxis = list(title = "Sales"), yaxis = list(title = "Sub Category")) %>%
      add_annotations(text = ~Sales, x = ~Sales, y = ~Sub_Category, showarrow = FALSE, xshift = 5)
  })
  output$sunburst_chart <- renderPlotly({
    df <- data %>% group_by(Category, Sub_Category) %>% summarise(Sales = sum(Sales))
    plot_ly(df, ids = ~paste(Category, Sub_Category, sep = "/"), labels = ~Sub_Category, parents = ~Category, values = ~Sales, type = 'sunburst') %>%
      layout(title = "Sunburst Chart of Sales by Category and Sub-Category")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)