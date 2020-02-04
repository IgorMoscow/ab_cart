# ab_cart
library(ggplot2)
library(plotly)
library(crosstalk)
library(shiny)
library(shinydashboard)
library(dplyr)
library(googleAuthR)
library(googleAnalyticsR)

ga_auth(email = "ugris07@mail.ru")
ga_account_list()

viewId = 176273797
library(lubridate)

d <- google_analytics(viewId = viewId,
                      date_range = c("2020-01-30", today()),
                      metrics = c("transactionRevenue"),
                      dimensions = c( "eventAction", "date"), 
                      anti_sample = TRUE, filtersExpression = "ga:eventCategory==AB")
p <- d %>% filter(eventAction %in% c("cart_new", "cart_old")) %>% 
  ggplot(aes(x = date, y = transactionRevenue, color = eventAction))+
  geom_point()+
  geom_line()+
  theme_bw()

plot <- ggplotly(p)
tab <- d %>% filter(eventAction %in% c("cart_new", "cart_old")) %>% group_by(eventAction) %>% summarize(avg = mean(transactionRevenue))
names(tab) <- c("вариант корзины", "доход")
