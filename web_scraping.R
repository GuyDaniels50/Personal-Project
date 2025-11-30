library(rvest)
library(tidyselect)
library(xml2)
library(tidyverse)
library(purrr)
library(readr)

link <- "https://nznbl.basketball/stats/competition-stats/?WHurl=%2Fteam%2F31805%3F"
page <- read_html(link)

players <- page %>%
  html_nodes("td") %>%
  html_attr("data-sort-value")