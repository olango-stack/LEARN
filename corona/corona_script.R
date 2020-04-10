#This is some analysis done on corona virus
install.packages('devtools')
library('devtools')
install.packages('StreamMetabolism')
install_github(repo = 'hilaryparker/hilary')
library('hilary')
create_sunset_cal() 
##covid-19 Api
library(httr)
library(dplyr)
library(jsonlite)
library(ggplot2)
#post to API
payload <- list(code = "ALL")
response <- httr::POST(url = "https://api.statworx.com/covid",
                       body = toJSON(payload, auto_unbox = TRUE), encode = "json")
## Convert to data frame
content <- rawToChar(response$content)
df <- data.frame(fromJSON(content))
# Make a cool plot
df %>%
  mutate(date = as.Date(date)) %>%
  filter(cases_cum > 100) %>%
  filter(code %in% c("US", "DE", "IT", "FR", "ES")) %>%
  group_by(code) %>%
  mutate(time = 1:n()) %>%
  ggplot(., aes(x = time, y = cases_cum, color = code)) +
  xlab("Days since 100 cases") + ylab("Cumulative cases") +
  geom_line() + theme_minimal()
##CODE above displays cumulative cases :days since 100 cases

