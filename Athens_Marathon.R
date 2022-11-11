
# Load libraries ----------------------------------------------------------

library(dplyr)
library(rvest)
library(ggplot2)
library("xml2")
library(ggflags)
library(countrycode) # to convert country names to country codes
library(tidytext) # for reorder_within
library(scales) # for application of common formats to scale labels (e.g., comma, percent, dollar)


# Create the url ----------------------------------------------------------


base_url <- "https://www.athensauthenticmarathon.gr/entries/index.php/start-list-marathon?orderby=bib&start="
end_url <- seq(0, 15200, 200)

url <- paste0(rep(base_url, length(end_url)), end_url)


# Prepare the vectors -----------------------------------------------------

bib <- vector()
Block <- vector()
Name <- vector()
Gender <- vector()
Birth <- vector()
Nationality <- vector()
Club <- vector()


# Read the html files -----------------------------------------------------


for(i in 1:length(url)) {
  html <- read_html(url[i])
  
  bib <- html %>% 
    html_nodes('[style*="240"]') %>%
    html_text2() %>% 
    .[2:length(.)] %>%
    as.numeric() %>%
    c(., bib)
  
  Block <- html %>% 
    html_nodes('[style*="290"]') %>%
    html_text2() %>% 
    .[2:length(.)] %>%
    as.numeric() %>%
    c(., Block)
  
  Name <- html %>% 
    html_nodes('[style*="330"]') %>%
    html_text2() %>% 
    .[2:length(.)] %>%
    c(., Name)
  
  Gender <- html %>% 
    html_nodes('[style*="590"]') %>%
    html_text2() %>% 
    .[2:length(.)] %>%
    c(., Gender)
  
  Birth <- html %>% 
    html_nodes('[style*="660"]') %>%
    html_text2() %>% 
    .[2:length(.)] %>%
    as.numeric() %>%
    c(., Birth)
  
  Nationality <- html %>% 
    html_nodes('[style*="740"]') %>%
    html_text2() %>% 
    .[2:length(.)] %>%
    c(., Nationality)
  
  Club <- html %>% 
    html_nodes('[style*="820"]') %>%
    html_text2() %>% 
    .[2:length(.)] %>%
    c(., Club)
  
}


# Create data frame -------------------------------------------------------


df <- data.frame(bib, Block, Name, Gender, Birth, Nationality, Club)

# write.csv(df, file="Athens_Marathon.csv", row.names = FALSE)

# Overall plots ------------------------------------------------------------
df %>%
  group_by(Nationality) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(Nationality = ifelse(Nationality == "GRE", "gr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "USA", "us", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ITA", "it", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "FRA", "fr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GBR", "gb", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "POL", "pl", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GER", "de", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "CYP", "cy", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ESP", "es", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "NED", "nl", Nationality)) %>%
  ggplot(., aes(x = n, y=reorder(Nationality, n))) +
  geom_col(alpha=0.2) +
  geom_flag(x=-200, aes(country = Nationality), size = 7) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank")
  ) +
  labs(title = "Athens Marathon 2022", x = NULL,
    y = NULL, subtitle = "Top-10 countries by number of participants")

df %>%
  filter(Nationality!="GRE") %>%
  group_by(Nationality) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(Nationality = ifelse(Nationality == "BEL", "be", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "USA", "us", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ITA", "it", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "FRA", "fr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GBR", "gb", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "POL", "pl", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GER", "de", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "CYP", "cy", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ESP", "es", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "NED", "nl", Nationality)) %>%
  ggplot(., aes(x = n, y=reorder(Nationality, n))) +
  geom_col(alpha=0.2) +
  geom_flag(x=-20, aes(country = Nationality), size = 7) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank")
  ) +
  labs(title = "Athens Marathon 2022", x = NULL,
       y = NULL, subtitle = "Top-10 countries by number of participants \n excluding Greece")

# Male plots ------------------------------------------------------------
df %>%
  filter(Gender %in% c("male", "Male", "MALE")) %>%
  group_by(Nationality) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(Nationality = ifelse(Nationality == "GRE", "gr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "USA", "us", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ITA", "it", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "FRA", "fr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GBR", "gb", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "POL", "pl", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GER", "de", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "CYP", "cy", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ESP", "es", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "NED", "nl", Nationality)) %>%
  ggplot(., aes(x = n, y=reorder(Nationality, n))) +
  geom_col(alpha=0.2) +
  geom_flag(x=-200, aes(country = Nationality), size = 7) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank")
  ) +
  labs(title = "Athens Marathon 2022", x = NULL,
       y = NULL, subtitle = "Top-10 countries by number of male participants")

df %>%
  filter(Nationality!="GRE", Gender %in% c("male", "Male", "MALE")) %>%
  group_by(Nationality) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(Nationality = ifelse(Nationality == "BEL", "be", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "USA", "us", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ITA", "it", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "FRA", "fr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GBR", "gb", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "POL", "pl", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GER", "de", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "CYP", "cy", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ESP", "es", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "NED", "nl", Nationality)) %>%
  ggplot(., aes(x = n, y=reorder(Nationality, n))) +
  geom_col(alpha=0.2) +
  geom_flag(x=-20, aes(country = Nationality), size = 7) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank")
  ) +
  labs(title = "Athens Marathon 2022", x = NULL,
       y = NULL, subtitle = "Top-10 countries by number of male participants \n excluding Greece")


# Female plots ------------------------------------------------------------

df %>%
  filter(Gender %in% c("female", "Female", "FEMALE")) %>%
  group_by(Nationality) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(Nationality = ifelse(Nationality == "GRE", "gr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "USA", "us", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ITA", "it", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "FRA", "fr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GBR", "gb", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "POL", "pl", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GER", "de", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "CYP", "cy", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ESP", "es", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "HUN", "hu", Nationality)) %>%
  ggplot(., aes(x = n, y=reorder(Nationality, n))) +
  geom_col(alpha=0.2) +
  geom_flag(x=-50, aes(country = Nationality), size = 7) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank")  ) +
  labs(title = "Athens Marathon 2022", x = NULL,
       y = NULL, subtitle = "Top-10 countries by number of female participants")

df %>%
  filter(Nationality!="GRE", Gender %in% c("female", "Female", "FEMALE")) %>%
  group_by(Nationality) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(Nationality = ifelse(Nationality == "USA", "us", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ITA", "it", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GBR", "gb", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "POL", "pl", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "FRA", "fr", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "GER", "de", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "CYP", "cy", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "HUN", "hu", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "ESP", "es", Nationality)) %>%
  mutate(Nationality = ifelse(Nationality == "NED", "nl", Nationality)) %>%
  ggplot(., aes(x = n, y=reorder(Nationality, n))) +
  geom_col(alpha=0.2) +
  geom_flag(x=-10, aes(country = Nationality), size = 7) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_line(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank")
  ) +
  labs(title = "Athens Marathon 2022", x = NULL,
       y = NULL, subtitle = "Top-10 countries by number of female participants \n excluding Greece")

