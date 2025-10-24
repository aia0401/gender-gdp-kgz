library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(plotly)
library(forecast)
library(corrplot)
library(shinythemes)

my_data = read.csv("gender_kgz.csv")

x = my_data %>% 
  select(Year, Indicator.Name, Value) %>%
  pivot_wider(names_from = Indicator.Name, values_from = Value)
gdp_data = read.csv("gdp.csv")
x$Year <- as.numeric(x$Year)
x = left_join(x, gdp_data, by = "Year")
x[ , names(x) != "Year"] = lapply(x[ , names(x) != "Year"], function(col) as.numeric(col))
x = x %>% filter(Year >= 2000)
names(x) = make.names(names(x))
my_data = my_data %>% distinct(Year, Indicator.Name, .keep_all = TRUE)
data = x
ui = fluidPage(
  theme = shinytheme("cerulean"),
  tags$h1("What does the relationship between gender equality and GDP in 
             Kyrgyzstan reveal about the economic value of human development?",
          style = "color:purple; font-size: 24px; font-weight: bold;"),
  sidebarLayout(
    sidebarPanel(
      selectInput("analysisType", "Select Analysis:",
                  choices = c("Single Regression", "Correlation Plot", 
                              "GDP Forecast")),
      conditionalPanel(
        condition = "input.analysisType == 'Single Regression'",
        selectInput("var", "Select a variable:", 
                    choices = names(data)[!names(data) %in% c("Year", "GDP")])
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.analysisType == 'Correlation Plot'",
        plotOutput("corrPlot")
      ),
      conditionalPanel(
        condition = "input.analysisType != 'Correlation Plot'",
        plotlyOutput("mainPlot")
      ),
      verbatimTextOutput("modelSummary")
    )
  )
)

server = function(input, output) {
  output$corrPlot <- renderPlot({
    numeric_data = data %>% select(-Year)
    c_data = cor(numeric_data, use = "pairwise.complete.obs")
    if(ncol(c_data) > 1){
      colnames(c_data) <- abbreviate(colnames(c_data), minlength = 12)
      corrplot(c_data, method="color", tl.cex=0.7, tl.srt=45,
               col=colorRampPalette(c("blue","white","orange"))(200))
    } else {
      plot.new()
      text(0.5, 0.5, "Not enough overlapping data for correlation plot")
    }
  })
  
  output$mainPlot = renderPlotly({
    if (input$analysisType == "Single Regression"){
      p = ggplot(data, aes_string(x=input$var, y="GDP")) +
        geom_point(color="#E6E6FA", size=3.5) +
        geom_smooth(method="lm", se=FALSE, color="#BDB5D5") +
        labs(title=paste("GDP vs.", input$var))
      ggplotly(p)
    } 
    else if (input$analysisType == "GDP Forecast"){
      ts_data = ts(data$GDP, start=min(data$Year), frequency=1)
      fit = auto.arima(ts_data)
      fcast = forecast(fit, h=5)
      autoplot(fcast)
    }
  })
  
  output$modelSummary = renderPrint({
    if (input$analysisType == "Single Regression"){
      model = lm(as.formula(paste("GDP ~", input$var)), data=data)
      summary(model)
    }
  })
}


shinyApp(ui = ui, server = server)
