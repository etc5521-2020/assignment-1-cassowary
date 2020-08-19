# Note this script was adapted from
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md


library(tidyverse)
library(rvest)

# Note the following code was originally adapted from
# https://blog.rmhogervorst.nl/blog/2020/04/08/scraping-gdpr-fines/

link <- "https://www.privacyaffairs.com/gdpr-fines/"
page <- read_html(link)


temp <- page %>% html_nodes("script") %>%
  .[9] %>%
  rvest::html_text()

ends <- str_locate_all(temp, "\\]")
starts <- str_locate_all(temp, "\\[")

table1 <- temp %>%
  stringr::str_sub(start = starts[[1]][1,2], end = ends[[1]][1,1]) %>%
  str_remove_all("\\\n") %>%
  str_remove_all("\\\r") %>%
  jsonlite::fromJSON() %>%
  as_tibble() %>%
  mutate(summary = str_remove_all(summary,"<p>|</p>|\n"))


table2 <- temp %>%
  stringr::str_sub(start = starts[[1]][2,2], end = ends[[1]][2,1]) %>%
  str_remove_all("\\\n") %>%
  str_remove_all("\\\r") %>%
  jsonlite::fromJSON() %>%
  as_tibble() %>%
  mutate(summary = str_remove_all(summary,"<p>|</p>|\n"))


all_df <- bind_rows(table1, table2) %>%
  janitor::clean_names() %>%
  mutate(
    authority = str_remove(authority, "\t"),
    article_violated = str_remove(article_violated, '<a href="https://www.privacy-regulation.eu/en/32.htm">') %>%
      str_remove('</a>'),
    article_violated = str_replace_all(article_violated, ", Art", "|Art"),
    type = str_remove(type, '<a href="https://www.privacy-regulation.eu/en/32.htm">') %>%
      str_remove('</a>')
  )

# most frequent articles violated
all_df %>%
  separate_rows(article_violated, sep = "\\|") %>%
  count(article_violated, sort = T)



all_df %>%
  write_tsv("data/gdpr_violations.tsv")


# Getting the actual article text -----------------------------------------

raw_article <- "https://gdpr-info.eu/" %>%
  read_html()

# Get all the urls for specific articles/chapters
gdpr_href <- raw_article %>%
  html_node(xpath = '//*[@id="tablepress-12"]') %>%
  html_nodes("a") %>%
  html_attr("href")

# pull the titles as well
gdpr_titles <- raw_article %>%
  html_node(xpath = '//*[@id="tablepress-12"]') %>%
  html_nodes("a") %>%
  html_attr("data-title")

# pull the numbers of article/chapters
gdpr_numbers <- raw_article %>%
  html_node(xpath = '//*[@id="tablepress-12"]') %>%
  html_nodes("a") %>%
  html_text()

# put it all into a df
gdpr_df <- tibble(
  article = gdpr_numbers,
  title = str_trim(gdpr_titles),
  href = gdpr_href
)

# Tidy up the data, create chapters vs articles
clean_gdpr <- gdpr_df %>%
  mutate(chapter = if_else(str_length(article) > 3, article, NA_character_),
         chapter_title = if_else(str_length(article) > 3, title, NA_character_)) %>%
  fill(chapter, chapter_title) %>%
  filter(!str_detect(article, "Chapter")) %>%
  mutate(article = as.double(article)) %>%
  filter(!is.na(article)) %>%
  select(starts_with("chapter"), article, article_title = title, href)

clean_gdpr

# LONG running outcome
# Get all the raw html from each of the urls for each article
all_articles <- clean_gdpr %>%
  mutate(raw_html = map(href, read_html))

# function to take raw html and turn it into text for that specific article
get_gdpr_text <- function(html_in){

  test_var <- html_in %>%
    html_node(".entry-content") %>%
    html_nodes("ol") %>%
    html_text()

  if (length(test_var) == 0){
    text <- html_in %>%
      html_node(".entry-content > p") %>%
      html_text() %>%
      str_remove("^[:digit:]")
  } else {
    text <- html_in %>%
      html_node(".entry-content") %>%
      html_nodes("ol") %>%
      html_text() %>%
      .[[1]] %>%
      str_replace_all(";\n", "\t") %>%
      str_replace_all(":\n", "\t") %>%
      str_split("\n") %>%
      .[[1]] %>%
      .[. != ""] %>%
      str_replace_all("\t", "\n") %>%
      str_remove("^[:digit:]")
  }


  text

}

# Test
get_gdpr_text(read_html("http://gdpr-info.eu/art-2-gdpr/"))

# unnest the list column of text
clean_articles <- all_articles %>%
  mutate(gdpr_text = map(raw_html, get_gdpr_text)) %>%
  unnest_longer(gdpr_text)

# final dataframe
final_articles <- clean_articles %>%
  group_by(article) %>%
  mutate(sub_article = row_number()) %>%
  relocate(sub_article, .after = "article_title") %>%
  relocate(gdpr_text, .after = "sub_article") %>%
  ungroup() %>%
  mutate(chapter = str_extract(chapter, "[:digit:]+")) %>%
  mutate_at(vars(chapter, article, sub_article), as.double) %>%
  select(-raw_html)

write_tsv(final_articles, "data/gdpr_text.tsv")
