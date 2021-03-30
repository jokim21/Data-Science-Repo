DababyQuotesHtml <- read_html("https://www.brainyquote.com/authors/dababy-quotes")

Dababyquotes <- DababyQuotesHtml %>%
  html_nodes(".oncl_q") %>%
  html_text() 

DaBaby <- DababyQuotesHtml %>%
  html_nodes(".oncl_a") %>%
  html_text()

DaBabyquotes_dat <- data.frame(quote = Dababyquotes, stringsAsFactors = FALSE) %>%
  filter(quote != "\n") %>%
  mutate(DaBaby = DaBaby
         , together = paste('"', as.character(quote), '" --'
                            , as.character(DaBaby), sep=""))

write.csv(DaBabyquotes_dat, "DaBabyQuotes.csv")