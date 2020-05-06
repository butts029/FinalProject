# R Studio API
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(rvest)
library(lubridate)
library(RColorBrewer)


# Data Import and Cleaning

# Get the starting page for the search
page <- read_html("https://scholar.google.com/scholar?start=0&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,39")

# Find the number of results, so that we know how many pages we need to scrape
nodes_pages <- html_nodes(page, "#gs_ab_md .gs_ab_mdw")
num_pages <- as.numeric(str_match(html_text(nodes_pages), "^About ([0-9]*) results")[2])

results <- matrix(NA, nrow = num_pages, ncol = 5)
colnames(results) <- c("Author", "Journal", "Year", "Title", "Link")

# For each page, scrape info
for(i in 0:(num_pages%/%10)){
  # To track progress
  print(i)
  
  # Pull one page at a time
  page <- read_html(paste0("https://scholar.google.com/scholar?start=",10*i,"&q=%22covid-19%22+source:psychology&hl=en&as_sdt=0,39"))
  # Find the elements corresponding to article title, author, journal, year, and link
  nodes_titles <- html_nodes(page, ".gs_rt, h3 span id")
  nodes_cite <- html_nodes(page, ".gs_a")
  nodes_link <- html_nodes(page, "h3 a")
  
  
  # Convert html to text and parse for desired information
  all_info <- html_text(nodes_cite)
  author <- str_extract(all_info, "([a-zA-Z ,.\\-\\u2026]+)") %>%
              str_remove_all(" - $")
  journal <- str_match(all_info, "- ([\\u2026]?[:blank:]?[a-zA-Z ,:&]*[:blank:]?[\\u2026]?)")[,2]%>%
              str_remove_all(", $| $")
  year <- str_match(all_info, "-[a-zA-Z ,&\\u2026[:blank:]]* ([0-9]{4})")[,2]
  # determine whether citation that doesn't have a link
  # This will throw a warning message, but it gives the desired results
  has_link <- which(str_detect(html_nodes(page, "div h3"), 'class=\\"gs_ctu\\"'))
  title <- html_text(nodes_titles) %>%
              str_remove_all("^(\\[PDF\\]\\[PDF\\] |\\[CITATION\\]\\[C\\] |\\[HTML\\]\\[HTML\\] )") 
  link <- html_attr(nodes_link,"href")
  
  # Cover situation where link is in title and not in href tag (eg 1st element on first page as of 5/5/2020)
  # Also need to cover [Citation] entries that don't have a link
  for(j in has_link){
    if(str_detect(title[j], "http")){
      ind_link <- str_extract(title[j], "http[a-zA-Z0-9?_\\-/.: ]*")
      link <- append(link, ind_link, after=(j-1))
      title[j] <- str_remove(title[j], "http[a-zA-Z0-9?_\\-/.: ]*") %>%
        str_remove_all(" $")
    } else {
      link <- append(link, "", after=(j-1))
    }
  
  }
  
  # Save results
  results[(i*10+1):(i*10+length(author)),] <- cbind(author, journal, year, title, link)
  
  # To make sure not overdoing request rate
  Sys.sleep(5)
}

# Visualization - Plot of number of publications in top 10 journals by Year

# Create tibble from results and make Journal titles uniform by capitalizing
results <- as_tibble(results) %>%
              mutate(Journal = str_to_upper(Journal))

# Remove results that don't have a year or a Journal 
#    because graphing on those elements
# Count number of results per journal
arranged_journals <- results %>%
                       filter(!is.na(Year) & Journal != "") %>%
                       group_by(Journal)%>%
                       count() %>%
                       arrange(desc(n))

# Take the top 10 journals with the most results
top_10_journals <- arranged_journals[1:10,"Journal"]

# Create a count of results per year and a total count per journal
#    and make year a date so that can see years that don't have any results
count_by_journal <- results %>%
                      filter(Journal %in% pull(top_10_journals)) %>%
                      select(Journal, Year) %>%
                      mutate(Year = year(paste0(Year,"-12-12"))) %>%
                      group_by(Journal, Year) %>%
                      count() %>% 
                      group_by(Journal) %>%
                      mutate(total = sum(n))

# Create the plot
#   Year along the x-axis and count on the y-axis
#   Color is done with darkest purple = least popular journal,
#                   and darkest green = most popular journal
ggplot(count_by_journal, aes(x = Year, y = n, fill = reorder(Journal, total))) +
  geom_col(position = "dodge", width = 1) +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_brewer(palette = "PRGn") +
  labs(y = "Count",
       fill = "Journal")

