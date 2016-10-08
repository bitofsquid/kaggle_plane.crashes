library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

# Saved down raw data to local file separately from this link: 
# https://www.kaggle.com/saurograndi/airplane-crashes-since-1908

path <- "C:/Users/jtryker/Documents/R/practice_files/"

# Read in Kaggle dataset from local drive
data <- read.csv(paste0(path,"kaggle_planecrashes.csv"), 
                 stringsAsFactors = FALSE)

# Coerce Date column dates using lubridate package
# (not 100% necessary, but hoping to work in other elements later that will rely on Date format)
data2 <- data %>%
         separate(Date, c("Month", "Day", "Year"), sep = "/") %>%
         mutate(Day, Day = str_pad(Day, 2, side = "left", pad = 0)) %>%
         mutate(Month, Month = str_pad(Month, 2, side = "left", pad = 0)) %>%
         unite(Date, Month:Year, sep = "/")

data2$Date <- mdy(data2$Date)

# Answer question: How many planes crashed per year? How many people on board?
crashes <- data2 %>%
           group_by(year = year(Date)) %>%
           summarize(number = length(Date))

on_board <- data2 %>%
            group_by(year = year(Date)) %>%
            summarize(median = median(Aboard, na.rm = TRUE), 
                      percentile_30 = quantile(Aboard, 0.30, na.rm = TRUE), 
                      percentile_70 = quantile(Aboard, 0.70, na.rm = TRUE),
                      total = sum(Aboard))

# Graphs for the answers to these questions
combined_plot <- ggplot() +
                  geom_bar(data = crashes, aes(x = year, y = number, fill = number), 
                           stat = "identity", width = 1) +
                  geom_line(data = on_board, aes(x = year, y = median, color = "green")) +
                  geom_ribbon(data = on_board, 
                              aes(x = year, ymin = percentile_30, ymax = percentile_70, alpha = 0.2)) +
                  scale_fill_gradient(name = "Total Number of Crashes", low = "darkblue", high = "red") +
                  scale_color_identity(name = "Median Occupancy per Plane", guide = "legend") +
                  scale_alpha_continuous(name = "70/30th Percentile Occupancy per Plane", 
                                         guide = "legend") +
                  labs(title = "Global Airplane Crashes Since 1908", y = "", x = "") + 
                  theme(legend.position = "bottom")

combined_plot

# End for now...more to come later (hopefully)


