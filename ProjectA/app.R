# Link to active app: https://butts029.shinyapps.io/ProjectA/

# Libraries
library(shiny)         # For creating app
library(tidyverse)     # For general manipulation
library(rtweet)        # For pulling tweets
library(RWeka)         # For tokenizing
library(tm)            # For creating corpus
library(qdap)          # For processing text
library(textstem)      # For lemmatization
library(wordcloud)     # For creating word cloud

# Define UI for application
ui <- fluidPage(

    titlePanel("Twitter and COVID 19"),

    # Sidebar with a select input for hashtag selection
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "hashtag",
                        label = "Select Hashtag",
                        choices = c("#COVID",
                                    "#COVID19",
                                    "#COVID-19",
                                    "#COVID_19"),
                        selected = "#COVID",
                        multiple = FALSE)
        ),


        mainPanel(
           plotOutput("wordcloud", height = 750, width = 750),
           tableOutput("sum_table"),
           plotOutput("sum_bar")
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Authentication process
    create_token(
        app = "finalproject",
        consumer_key = "yVnBvFRZsC0W3EEsDOIlMeIU2",
        consumer_secret = "bKS67NF1pNcrjpkuB4u62Wgaivs3A6zCdTeVa51IMktcdUanVu",
        access_token = "859134854-EvgfutSmqGAZmPgk73K312WSaHbWUqdTAWHKXuaI",
        access_secret = "pjL3WMkRqm1go24kLHnRzVq1qmQNHKm8cUmhkSbx0H4RZ"
    )
    
    process_tweets <- function(term){
        # Pull in the tweets
        tweets <- search_tweets(term,
                                n = 500,
                                include_rts = FALSE)
        
        # Function to create unigrams from data
        # only look at unigrams because believe those will be most informative
        tokenizer <- function(x) {
            NGramTokenizer(x, Weka_control(min = 1, max = 1))
        }
        
        tweets$text <- tweets$text %>%
            iconv("UTF-8", "ASCII", sub = "")
        
        # Create corpus and preprocess the tweets
        tweets_cp <- tweets %>%
            add_column(doc_id = 1:nrow(tweets), .before = 1) %>%
            DataframeSource() %>%
            VCorpus() %>%
            # remove hashtags
            tm_map(content_transformer(function(x) str_remove_all(x, pattern = "#\\w*"))) %>%
            # remove urls
            tm_map(content_transformer(function(x) str_remove_all(x, "https://[\\w|.|/]*"))) %>%
            tm_map(content_transformer(replace_abbreviation)) %>%
            tm_map(content_transformer(replace_contraction)) %>%
            tm_map(removePunctuation) %>%
            tm_map(removeNumbers) %>%
            tm_map(content_transformer(str_to_lower)) %>%
            # Remove english and spanish stop words as both were present in initial work
            # Also remove covid as it is expected to be in many of the tweets with a covid hashtag
            tm_map(removeWords,c(stopwords("en"), stopwords("spanish"), "covid")) %>%
            tm_map(stripWhitespace) %>%
            tm_map(content_transformer(lemmatize_strings))
        
        # Tokenize and remove sparse terms
        tweets_dtm <- DocumentTermMatrix(tweets_cp,
                                        control = list(tokenize = tokenizer)) %>%
                        removeSparseTerms(.99)
        
        # convert DTM to a tibble for analysis and remove the search term "dog"
        tweets_tbl <- as_tibble(as.matrix(tweets_dtm)) 
        
        # Remove rows with no remaining terms
        tweets_tbl <- tweets_tbl[rowSums(tweets_tbl) > 0,]
        
        return(tweets_tbl)
    }
    
    covid_tbl <- process_tweets("#COVID")
    covid19_tbl <- process_tweets("#COVID19")
    dash_tbl <- process_tweets("#COVID-19")
    underscore_tbl <- process_tweets("#COVID_19")

    # Create a word cloud based on the hashtag that is selected
    output$wordcloud <- renderPlot({
        tbl <- switch(input$hashtag, 
                        "#COVID" = covid_tbl,
                        "#COVID19" = covid19_tbl,
                        "#COVID-19" = dash_tbl,
                        "#COVID_19" = underscore_tbl)
        
        wordcloud(colnames(tbl), 
                  colSums(tbl), 
                  max.words = 50, 
                  colors = "purple")
    })
    
    # Will only be run once because not reactive
    output$sum_table <- renderTable({
        # Limit tables to those with word counts > 5
        covid_5tbl <- colnames(covid_tbl[, colSums(covid_tbl) > 5])
        covid19_5tbl <- colnames(covid19_tbl[, colSums(covid19_tbl) > 5])
        dash_5tbl <- colnames(dash_tbl[, colSums(dash_tbl) > 5])
        underscore_5tbl <- colnames(underscore_tbl[, colSums(underscore_tbl) > 5])
        
        table <- data.frame(c(rep("#COVID", 3), rep("#COVID19", 2), "#COVID-19"),
                            c("#COVID19", "#COVID-19", "#COVID_19", "#COVID-19", "#COVID_19", "#COVID_19"),
                            c(length(intersect(covid_5tbl, covid19_5tbl)),
                              length(intersect(covid_5tbl, dash_5tbl)),
                              length(intersect(covid_5tbl, underscore_5tbl)),
                              length(intersect(covid19_5tbl, dash_5tbl)),
                              length(intersect(covid19_5tbl, underscore_5tbl)),
                              length(intersect(dash_5tbl, underscore_5tbl)))
                   )
        colnames(table) <- c("Hashtag 1", "Hashtag 2", "Number of Overlapping Words")
        table
    })
    
    # Will only be run once because not reactive
    output$sum_bar <- renderPlot({
        options(stringsAsFactors = FALSE)
        covid <- data.frame(word = colnames(covid_tbl), 
                            count = colSums(covid_tbl))
        covid19 <- data.frame(word = colnames(covid19_tbl), 
                              count = colSums(covid19_tbl))
        dash <- data.frame(word = colnames(dash_tbl), 
                      count = colSums(dash_tbl))
        underscore <- data.frame(word = colnames(underscore_tbl), 
                            count = colSums(underscore_tbl))
        
        combined <- full_join(covid, covid19, by = "word") %>%
                        full_join(dash, by = "word") %>%
                        full_join(underscore, by = "word") %>%
                        rowwise() %>%
                        mutate(total_freq = sum(count.x, count.y, 
                                                count.x.x, count.y.y, 
                                                na.rm = TRUE)) %>%
                        ungroup()
        
        top_n(combined, n = 20, wt = total_freq) %>%
            ggplot( aes(x = reorder(word, total_freq), y = total_freq)) +
            geom_col(fill = "darkblue") +
            coord_flip() +
            labs(x = "Word",
                 y = "Frequency Across All Hashtags")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
