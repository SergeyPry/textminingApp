

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
   
   # Application title

   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tags$div(class="navbar-header", tags$h1("Text Mining Plots App", style = "color: green")),
        
        textAreaInput("userText", "Please enter the text you would like to analyze",value = '', height = '400px'),
        
        
        
        selectInput("textDataset", "Choose one of text datasets",
                                     
           c("Alice in Wonderland" = "19033",  
             "Jane Austen - Pride & Prejudice" = "janeaustenr",
              "Treatise on Light by Christiaan Huygens" = "14725",
              "Around the World in 80 Days by Jules Verne" = "103",
             "US constitution" = "US_constitution",
             "I have a dream by Martin Luther King" = "dream")),
        
        sliderInput("numWords", "Choose the number of words for frequency plot", min = 10, max = 50, value = 10),
        
        sliderInput("wcSize", "Choose the number of words for the wordcloud", min = 100, max = 400, value = 200)

        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        
        tags$h2("Text mining plot results for your input dataset"),
        
        fluidRow(
          
          column(6, 
                 
                 tags$div(class="panel panel-primary", tags$div(class="panel-heading", tags$h1("Word size", class="panel-title")),
                          tags$div(class="panel-body", 
                                   
                                   plotOutput("wordLength", height = "300px")
                          ))
                 
                 
          ),
          
          column(6, 
                 
                 tags$div(class="panel panel-primary", tags$div(class="panel-heading", tags$h1("Letter frequency", class="panel-title")),
                          tags$div(class="panel-body", 
                                   
                                   plotOutput("letterFreq", height = "300px")
                          ))
                 
                 
          )
          
        ),
        
        
        
        fluidRow(
          
          column(6, 
                 
                 tags$div(class="panel panel-primary", tags$div(class="panel-heading", tags$h1("Frequency Plot", class="panel-title")),
                          tags$div(class="panel-body", 
                                   
                                   plotOutput("freqPlot", width = "400px", height = "600px")
                          ))
                 
                 
          ),
          
          column(6, 
                 
                 tags$div(class="panel panel-primary", tags$div(class="panel-heading", tags$h1("Word cloud", class="panel-title")),
                          tags$div(class="panel-body", 
                                   
                                   plotOutput("wordcloud", width = "400px", height = "600px")
                          ))
                 
                 
                 )

        )
  
      )
   )
)

      
      
      
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # load libraries
  library(janeaustenr)
  library(dplyr)
  library(tidytext)
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(RColorBrewer)
  library(gutenbergr)
  library(ggplot2)
  library(stringr)
  library(qdap)
  
  # reactive function to select a relevant text dataset
  selectTextDataset <- reactive({
    
    
    # obtain a dataset with which to work
    if(input$userText != ''){
      textData <- input$userText
      
    } else {
      
      # obtain the information from the dataset input form for the retrieval of the actual texts
      textDataId <- input$textDataset
      
      # Jane Austen - Pride & Prejudice
      if(textDataId == "janeaustenr"){
        
        textData <- prideprejudice[1:2000]
        
      }
      
      # Alice in Wonderland
      if(textDataId == "19033"){
        
        td_frame <- gutenberg_download(gutenberg_id = as.integer(textDataId), 
                                       meta_fields = "title")
        textData <- td_frame$text[1:2000]
        
      }
      
      
      
      # Treatise on Light by Christiaan Huygens
      if(textDataId == "14725"){
        
        td_frame <- gutenberg_download(gutenberg_id = as.integer(textDataId), meta_fields = "title")
        
        textData <- td_frame$text[1:2000]
        
      }
      
      
      # Around the World in 80 Days by Jules Verne
      
      if(textDataId == "103"){
        
        td_frame <- gutenberg_download(gutenberg_id = as.integer(textDataId), meta_fields = "title")
        textData <- td_frame$text[1:2000]
        
      }
      
      # US constitution
      
      if(textDataId == "dream"){
        
        filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
        textData <- readLines(filePath)
       
      }    
      
      
      # I have a dream by Martin Luther King
      
      if(textDataId == "US_constitution"){
        wd <- getwd()
        filePath <- "const.txt"
        textData <- readLines(filePath)      
      }
      
    }
    
    textData
    
  })
  
  # generate a document term matrix
  createDTM <- reactive({
    # obtain text dataset
    textData <- selectTextDataset()
    
    # Load the data as a corpus
    docs <- Corpus(VectorSource(textData))
    
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    docs <- tm_map(docs, removeWords, c("shall", "will", "can", "must", "said", "one", "two", "may"))
    
    
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    
    dtm <- TermDocumentMatrix(docs)
    dtm
    
  })
  
  
  # plot of the word length distribution
  output$wordLength <- renderPlot({
    dtm <- createDTM()
    
    words <- dtm %>%
      as.matrix %>%
      rownames %>%
      (function(x) x[nchar(x) < 20])
    
    wLPlot <-  data.frame(nletters=nchar(words)) %>%
      ggplot(aes(x=nletters)) +
      geom_histogram(binwidth=0.5, fill = "#a8b6bf") +
      geom_vline(xintercept=mean(nchar(words)),
                 colour="#e62739", size=1, alpha=.5) +
      labs(x="Number of Letters", y="Number of Words")
    
    print(wLPlot)  
    
  })

  
  
  # generate a plot for letter frequencies 
  output$letterFreq <- renderPlot({
    
    dtm <- createDTM()
    
    words <- dtm %>%
      as.matrix %>%
      rownames %>%
      (function(x) x[nchar(x) < 20])
    
    lfp <- words %>%
      str_split("") %>%
      sapply(function(x) x[-1]) %>%
      unlist %>%
      dist_tab %>%
      mutate(Letter=factor(toupper(interval),
                           levels=toupper(interval[order(freq, decreasing = TRUE)]))) %>%
      ggplot(aes(Letter, weight=percent)) +
      geom_bar(fill = "#a8b6bf") +
      labs(y="Proportion") +
      scale_y_continuous(breaks=seq(0,16, 2),
                         label=function(x) paste0(x, "%"), expand=c(0,0), limits=c(0,16))
    
    print(lfp)
  })
  
  
  
# generate a word cloud plot
  output$wordcloud <- renderPlot({
    
    dtm <- createDTM()
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    
    d$word <- d$word[order(d$freq, decreasing = TRUE)]
    
    cloudSize <- input$wcSize
    
    wcPlot<- wordcloud(words = d$word, freq = d$freq, scale =c(6, 0.5),min.freq = 1,
              max.words=cloudSize, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    print(wcPlot)
    
  })  
    
  # generate a plot of frequencies of top words in a text
  output$freqPlot <- renderPlot({
    
    dtm <- createDTM()    
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    text_df <- data.frame(word = names(v),freq=v)
    
    text_df$word <- factor(text_df$word, levels = text_df$word[order(text_df$freq)])
    
    
    numberWords <- input$numWords
    
    freqP <- ggplot(data= text_df[1:numberWords,], aes(word, freq, fill = "green")) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "Frequency") +
        coord_flip()
    
    
    print(freqP)
    
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

