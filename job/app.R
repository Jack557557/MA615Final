#Package install
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(usmap)
library(stats)
library(base)
library(tidyr)
library(tm)
library(tidytext)
library(wordcloud)
library(igraph)
library(wordcloud2)
library(shinydashboard)
library(ggraph)


n <- 1

listings<-read.csv("listings.csv")
listings<-listings%>% separate(location, c("city", "state"), ", ")
listings$description<-as.character(listings$description)
listings[is.na(listings)] <- "Whole Country"
#ui
ui <- dashboardPage(
  
  dashboardHeader(title = "Job Text Mining"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Words", tabName = "Words", icon = icon("chart-bar")),
    menuItem("Words_cloud", tabName = "Words_cloud", icon = icon("cloud")),
    menuItem("Phrases", tabName = "Phrases", icon = icon("chart-bar")),
    menuItem("Phrases_cloud", tabName = "Phrases_cloud", icon = icon("cloud")),
    menuItem("Phrases_Network", tabName = "Phrases_Network", icon = icon("project-diagram")),
    menuItem("Keywords", tabName = "Keywords", icon = icon("chart-bar"))
  )),
  
  dashboardBody(
    #first
    tabItems(tabItem(tabName = "Words",
                     fluidRow(
                       plotOutput("plot2")
                     )
    ),
    
    tabItem(tabName = "Words_cloud",
     fluidRow(
       wordcloud2Output('wordcloud')
     )
    ),
    tabItem(tabName = "Keywords",
            fluidRow(
              sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                label = "Search..."),
              h3("For example, years or data"),
              plotOutput("plot5"),
              plotOutput("plot6")
              
            )
    ),
    tabItem(tabName = "Phrases",
              fluidRow(
                plotOutput("plot4")
              )
    ),
    tabItem(tabName = "Phrases_cloud",
            fluidRow(
              wordcloud2Output('wordcloud2')
            )
    ),
    tabItem(tabName = "Phrases_Network",
            fluidRow(
              plotOutput("plot7")
            )
    ),
    tabItem(tabName = "dashboard",
            
            fluidRow(
              column(4,
                     selectInput("company",
                                 "Company:",
                                 c("All",
                                   unique(as.character(listings$company))))),
              
              column(4,
                     selectInput("state",
                                 "State:",
                                 c("All",
                                   unique(as.character(listings$state)))))
            ),
            dataTableOutput("table1")
            
            
         )
  
    
  ))
)
  

  

# plot
server <- function(input, output) {
  
  output$wordcloud <- renderWordcloud2({
    data <- listings
    tidy_list <- listings %>% unnest_tokens(word,description)
    #remove stop words
    data(stop_words)
    tidy_list <- tidy_list %>%
      anti_join(stop_words)
    data<-tidy_list %>% count(word, sort = TRUE)%>%head(50)
    wordcloud2(data, color = "random-light", backgroundColor = "grey")
    
  }) 
  
  output$plot2 <- renderPlot({
    data <- listings
    tidy_list <- listings %>% unnest_tokens(word,description)
    #remove stop words
    data(stop_words)
    tidy_list <- tidy_list %>%
      anti_join(stop_words)
    #most frequency words
    ggplot(data=tidy_list %>% count(word, sort = TRUE)%>% head(20)%>%mutate(word = reorder(word, n)),aes(n, word))+geom_col() +
      labs(y = NULL)
    
    
  })
  
  output$wordcloud2 <- renderWordcloud2({
    data <- listings
    tidy_list2 <- listings %>% unnest_tokens(bigram, description, token = "ngrams", n = 2)
    
    #most frequency words
    tidy_list2_separated <- tidy_list2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    tidy_list2_filtered <- tidy_list2_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    tidy_list2_counts <- tidy_list2_filtered %>% 
      count(word1, word2, sort = TRUE)
    
    
    tidy_list2_counts$word<-paste(tidy_list2_counts$word1,tidy_list2_counts$word2)
    tidy_list2_counts$word1<-NULL
    tidy_list2_counts$word2<-NULL
    data<- tidy_list2_counts%>%head(50)
    data<-select(data,"word","n")
    wordcloud2(data, color = "random-light", backgroundColor = "grey")
    
  })
  
  output$plot4 <- renderPlot({
    data <- listings
    tidy_list2 <- listings %>% unnest_tokens(bigram, description, token = "ngrams", n = 2)
    
    #most frequency words
    tidy_list2_separated <- tidy_list2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    tidy_list2_filtered <- tidy_list2_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    tidy_list2_counts <- tidy_list2_filtered %>% 
      count(word1, word2, sort = TRUE)
    
    
    tidy_list2_counts$word<-paste(tidy_list2_counts$word1,tidy_list2_counts$word2)
    ggplot(data= tidy_list2_counts %>% head(20)%>%mutate(word = reorder(word, n)),aes(n, word))+geom_col() +
      labs(y = NULL)
    
  })
   
  #table1
  output$table1 <- renderDataTable({
    data <- listings
    if (input$company != "All") {
      data <- data[data$company == input$company,]
    }
    if (input$state != "All") {
      data <- data[data$state == input$state,]
    }
    data<-select(data,city,summary)
    data
  })
  
  output$plot5 <- renderPlot({
    data <- listings
    tidy_list2 <- listings %>% unnest_tokens(bigram, description, token = "ngrams", n = 2)
    stop_words[1142,1]="stupid"
    #most frequency words
    tidy_list2_separated <- tidy_list2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    tidy_list2_filtered <- tidy_list2_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    tidy_list2_counts<-tidy_list2_filtered %>%
      filter(word2 == input$searchText) %>%
      count(word1, word2, sort = TRUE)
    tidy_list2_counts$word<-paste(tidy_list2_counts$word1,tidy_list2_counts$word2)
    ggplot(data= tidy_list2_counts %>% head(20)%>%mutate(word = reorder(word, n)),aes(n, word))+geom_col() +
      labs(y = NULL)
    
  })
  
  output$plot7 <- renderPlot({
    
    tidy_list2 <- listings %>% unnest_tokens(bigram, description, token = "ngrams", n = 2)
    
    #most frequency words
    tidy_list2_separated <- tidy_list2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    tidy_list2_filtered <- tidy_list2_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    tidy_list2_counts <- tidy_list2_filtered %>% 
      count(word1, word2, sort = TRUE)
    
    tidy_list2_graph <- tidy_list2_counts %>%
      filter(n > 20) %>%
      graph_from_data_frame()
    
    set.seed(2020)
    
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    ggraph(tidy_list2_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
    
  })
  
  output$plot6 <- renderPlot({
    data <- listings
    tidy_list2 <- listings %>% unnest_tokens(bigram, description, token = "ngrams", n = 2)
    stop_words[1142,1]="NULL"
    #most frequency words
    tidy_list2_separated <- tidy_list2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    tidy_list2_filtered <- tidy_list2_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    tidy_list2_counts<-tidy_list2_filtered %>%
      filter(word1 == input$searchText) %>%
      count(word1, word2, sort = TRUE)
    tidy_list2_counts$word<-paste(tidy_list2_counts$word1,tidy_list2_counts$word2)
    ggplot(data= tidy_list2_counts %>% head(20)%>%mutate(word = reorder(word, n)),aes(n, word))+geom_col() +
      labs(y = NULL)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

