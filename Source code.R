# Library yang digunakan
library(tidyverse)
library(lubridate)
library(scales)
library(forcats)
library(plotly)

# Membaca file csv
budget <- read_csv2("January-august_2019.csv", col_types = cols(
  Date = col_date(format = "%m/%d/%Y"),
  Account = col_factor(),
  Amount = col_number()
))
head(budget)

# Merapihkan kolom
  # Mengubah nama kolom Main category
budget <- rename(budget, Main_category = 'Main category')

  # Menfilter kolom Main_category
  # Menghilangkan kolom 'Comment'
spending_data <- filter(budget, Main_category != 'Income' & Main_category != 'Saving')
spending_data <- select(spending_data, -Comment)
head(spending_data)

  # Menambah kolom 'wday' dan 'Month'
spending_data$Day <- wday(spending_data$Date, label = TRUE, abbr = FALSE)
spending_data$Month <- format(as_date(spending_data$Date), '%Y-%m')

# Mengambil data investasi dan menghilangkan kolom Subcategory
investment <- filter(spending_data, Main_category == 'Investment')
investment <- select(investment, -Subcategory)

# Mengambil data pemasukan 
income <- filter(budget, Main_category == "Income")

income$Day <- wday(income$Date, label = TRUE, abbr = FALSE)
income$Month <- format(as_date(income$Date), '%Y-%m')

# Plotting
  # Spending over time
spending_data %>%
  ggplot(mapping = aes(x = Date, y = abs(Amount))) +
  geom_line() +
  labs(y = "Amount") +
  scale_y_continuous(labels = comma) -> P

    # Convert to plotly to see detail metadata 
ggplotly(P)

  # Spending / month by category
spending_data %>%
  group_by(Month = month(Date), Year = year(Date), Main_category) %>%
  summarise(Amount = sum(abs(Amount))) %>%
  ggplot(mapping = aes(x = Month, y = Amount, fill = Main_category)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(x = "", title = "Spending/Month", subtitle =  "by Category", fill = "Category") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8), 
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")) +
  scale_y_continuous(labels = comma) -> P2
  
  # Mean Speding / category
spending_data %>%
  ggplot(mapping = aes(x = Main_category, y = abs(Amount))) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  coord_flip() -> P3

  # Balance adjustment category
spending_data %>%
  filter(Main_category %in% c("Balance Adjustment", "Compensation transactions")) %>%
  ggplot(mapping = aes(x = Month, y = Amount, fill = Title)) +
  geom_col(position = position_dodge2()) +
  labs(title = "Balance Adjustment") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "") -> P4

# investment
ggplot(investment, mapping = aes(x = Title, y = abs(Amount))) +
  geom_col(mapping = aes(fill = Title)) +
  labs(x = "", y = "Amount", title = "Invesment/Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "") + 
  theme(legend.position = 'none') +
  facet_wrap(. ~ Month) -> P5

# income
income %>%  
  group_by(Month = month(Date)) %>%
  summarise(Amount = sum(Amount)) %>%
  ggplot(mapping = aes(x = Month, y = Amount)) +
  geom_col(fill = 'darkgreen', position = position_identity()) +
  labs(x = "", title = 'Income/Month') +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")) +
  scale_y_continuous(labels = comma) -> P6

# Frequency plot
  # Main_category frequency
ggplot(budget, mapping = aes(x = fct_rev(fct_infreq(Main_category)))) +
  geom_bar() +
  labs(x = "Main Category") +
  coord_flip() -> P7

  # Subcategory frequency
ggplot(budget, mapping = aes(x = fct_rev(fct_infreq(Subcategory)))) +
  geom_bar() +
  labs(x = "Subcategory") +
  coord_flip() -> P8

  # Main_category w/ Subcategory 
budget %>%
  filter(Subcategory != is.na(NA)) %>%
  ggplot(mapping = aes(fct_rev(fct_infreq(Main_category)))) +
  geom_bar(aes(fill = Subcategory), position = position_identity()) +
  labs(x = "Main Category", 
       title = "Main Category Frequency", 
       subtitle = "with assigned Subcategory") +
  coord_flip() -> P9

  # Main_category wo/  Subcategory
budget %>%
  replace_na(list(Subcategory = "None")) %>%
  filter(Subcategory == "None") %>%
  ggplot(mapping = aes(fct_rev(fct_infreq(Main_category)))) +
  geom_bar(position = position_dodge2()) +
  labs(x = "Main Category",
       title = "Main Category Frequency", 
       subtitle = "with unassigned Subcategory") +
  coord_flip() -> P10