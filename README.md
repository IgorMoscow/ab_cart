# ab_cart
Sys.getlocale(category = "LC_ALL")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(ggplot2) 
library(plotly) 
library(crosstalk) 
library(dplyr) 
library(googleAuthR) 
library(googleAnalyticsR)
library(lubridate)
library(RGA)
rsconnect::deployApp(upload = FALSE)
ga_auth(email = "ugris07@mail.ru") 
ga_account_list()

viewId = 176273797 
d <- google_analytics(
    viewId = viewId, date_range = c("2020-01-30", today()), 
    metrics = c("transactionRevenue"), 
    dimensions = c( "eventAction", "date"), 
    anti_sample = TRUE, 
    filtersExpression = "ga:eventCategory==AB") 
p <- d %>% filter(eventAction %in% c("cart_new", "cart_old")) %>% 
    ggplot(aes(x = date, y = transactionRevenue, color = eventAction))+ 
    geom_point()+ 
    geom_line(lwd =2)+ 
    theme_bw()
tab <- d %>% filter(eventAction %in% c("cart_new", "cart_old")) %>% group_by(eventAction) %>% summarize(avg = mean(transactionRevenue))
names(tab) <- c("вариант корзины", "доход")

trans <- google_analytics(
    viewId = viewId, date_range = c("2020-01-30", today()), 
    metrics = c("transactions", "sessions"), 
    dimensions = c( "eventAction", "date"), 
    anti_sample = TRUE, filtersExpression = "ga:eventCategory==AB") 

conv <- trans %>% filter(eventAction %in% c("cart_new", "cart_old")) %>% 
    group_by(date, eventAction) %>% summarize(sessions = sum(sessions), transactions =
                                                  sum(transactions)) %>% mutate(
                                                      conversions = transactions/sessions)
conv1 <- trans %>% filter(eventAction %in% c("cart_new", "cart_old")) %>% 
    group_by(eventAction) %>% summarize(sessions = sum(sessions), transactions =
                                            sum(transactions)) %>% mutate(
                                                conversions = transactions/sessions)
names(conv1) <- c("вариант корзины", "сессиии", "транзакции", "вариант корзины")
p2 <- ggplot(data = conv, aes(x = date, y = conversions, color = eventAction))+ 
    geom_point()+ 
    geom_line(lwd =2)+ 
    theme_bw()



ui <- dashboardPage(
    dashboardHeader(title = "Тест Корзина"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(title = "Доход" ,plotOutput(outputId = "graph1"), 
                solidHeader = TRUE, background = "red", width = 9),
            infoBoxOutput(outputId = "box1", width = 3),
            box(title = "Доход", tableOutput(outputId = "table1"), width = 3,
                solidHeader = TRUE, background = "purple")),
        fluidRow(box(title = "конверсии" ,plotOutput(outputId = "graph2"), 
                     solidHeader = TRUE, background = "navy", width = 8),
                 infoBoxOutput(outputId = "box2", width = 4),
                 box(title = "конверсии", tableOutput(outputId = "table2"), width = 4,
                     solidHeader = TRUE, background = "green"))))


server <- function(input, output) {
    
    output$graph1 <- renderPlot({p})
    output$table1 <- renderTable({tab})
    output$graph2 <- renderPlot({p2})
    output$table2 <- renderTable({conv1})
}

shinyApp(ui = ui, server = server)
