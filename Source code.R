# Library yang digunakan
library(tidyverse)
library(lubridate)
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

# Plotting
  # Spending over time
spending_data %>%
  ggplot(mapping = aes(x = Date, y = abs(Amount))) +
  geom_line() +
  labs(y = "Amount") -> P

    # Convert to plotly to see detail metadata 
ggplotly(P)

  # investment
ggplot(data = investment, mapping = aes(x = Title, y = abs(Amount))) +
  geom_col(mapping = aes(fill = Title)) +
  facet_wrap(~ Date)
