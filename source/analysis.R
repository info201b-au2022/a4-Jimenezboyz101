library(tidyverse)
library(dbplyr)
library(plotly)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

incarceration_records <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_records)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here 

# Beginning 1st calculation

modern_day <- incarceration_records %>% 
  select(year, state, county_name, total_jail_pop, white_jail_pop) %>% 
  drop_na() %>% 
  filter(year == max(year))

View(modern_day)

modern_by_state <- modern_day %>% 
  group_by(state) %>% 
  mutate(color_jail_pop = total_jail_pop - white_jail_pop) %>% 
  filter(color_jail_pop == max(color_jail_pop))

View(modern_by_state)
  
prop_color <- modern_by_state %>% 
  group_by(state) %>% 
  summarise(white_to_color = color_jail_pop/white_jail_pop)

View(prop_color)

# First value to be used in summary info
avg_prop <- prop_color %>% summarise(avg_prop = mean(white_to_color))


# Beginning 2nd calculation

View(modern_by_state)

colored_2008 <- incarceration_records %>% 
  select(year, state, county_name, total_jail_pop, white_jail_pop) %>% 
  drop_na() %>% 
  filter(year == "1990") %>% 
  group_by(state) %>% 
  mutate(color_jail_pop = total_jail_pop - white_jail_pop) %>% 
  filter(color_jail_pop == max(color_jail_pop))

View(colored_2008)


diff_colored <- data.frame(modern_by_state$color_jail_pop/colored_2008$color_jail_pop)
View(diff_colored)

# 2nd value to be used in summary info
avg_diff <- diff_colored %>% summarise(avg_diff = mean(diff_colored$modern_by_state.color_jail_pop.colored_2008.color_jail_pop))
avg_diff

# Beginning 3rd calculation

modern_black_pop <- incarceration_records %>% 
  select(year, state, county_name, total_jail_pop, black_jail_pop) %>% 
  drop_na() %>% 
  filter(year == max(year))

View(modern_black_pop)

black_pop_1990 <- incarceration_records %>% 
  select(year, state, county_name, total_jail_pop, black_jail_pop) %>% 
  drop_na() %>% 
  filter(year == "1990")

View(black_pop_1990)

# 3rd value to be used in data summary
diff_black <- mean(modern_black_pop$black_jail_pop - black_pop_1990$black_jail_pop)
diff_black


thing <- incarceration_records %>% 
  select(year, total_pop) %>% 
  drop_na() %>% 
  group_by(year)
  

View(thing)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>

get_year_jail_pop <- function() {
  pop_by_year <- incarceration_records %>%
    group_by(year) %>%
    drop_na() %>% 
    summarize(year_jail_pop = sum(total_jail_pop))
  return(pop_by_year)
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function() {
  pop_plot <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = year_jail_pop)) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year", y = "Prison Population", title = "Prison Population Trends")
    
  return(pop_plot)
}


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  pop_by_state <- incarceration_records %>%
    filter(state %in% states) %>%
    drop_na() %>% 
    group_by(state, year) %>%
    summarize(year_jail_pop = sum(total_pop))
  return(pop_by_state)
}

plot_jail_pop_by_states <- function(states) {
  jail_pop_chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = year_jail_pop, color = state)) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year", y = "Jail Population Per Year", title = "Prison Population Trends")
  return(jail_pop_chart)
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#




## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


