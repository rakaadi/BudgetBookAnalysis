# Library
library(tidyverse)
library(lubridate)
library(scales)
library(forcats)
library(TSclust)
library(reshape2)
library(plotly)

# Read the csv file, and process it
budget <- read_csv2("January-august_2019.csv", col_types = cols(
  Date = col_date(format = "%m/%d/%Y"),
  Amount = col_number()
))
head(budget)

  # Change column name
  # Add 'Month_Year' column
budget <- rename(budget, Main_category = 'Main category')
budget$Month_Year <- format(as_date(budget$Date), '%Y-%m')
head(budget)

  # Filter Main_category to create spending_data dataframe
  # Also remove 'Comment' column
budget %>%
  filter(Main_category != 'Income' & Main_category != 'Saving') %>%
  select(-Comment) -> spending_data
head(spending_data)

  # Get investment data, and make a dataframe
  # Also remove 'Subcategory' column
spending_data %>%
  filter(Main_category == 'Investment') %>%
  select(-Subcategory) -> investment
head(investment)

  # Get income data
income <- filter(budget, Main_category == 'Income')
head(income)

  # Get savings data
budget %>%
  filter(Main_category == 'Saving') %>%
  select(-Comment) -> savings
head(savings)

# Plotting
# Spending over time
spending_data %>%
  ggplot(mapping = aes(x = Date, y = abs(Amount))) +
  geom_line() +
  labs(y = "Amount") +
  scale_y_continuous(labels = comma) -> P1

  # Convert to plotly to see more detail 
ggplotly(P1)

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

# Average Speding / category
spending_data %>%
  ggplot(mapping = aes(x = Main_category, y = abs(Amount))) +
  geom_boxplot() +
  labs(x = "Main category", y = "Amount", title = "Average Spending", subtitle = "by Category") +
  scale_y_continuous(labels = comma) +
  coord_flip() -> P3

# Balance adjustment category
spending_data %>%
  filter(Main_category %in% c("Balance Adjustment", "Compensation transactions")) %>%
  ggplot(mapping = aes(x = Month_Year, y = Amount, fill = Title)) +
  geom_col(position = position_dodge2()) +
  labs(x = "Date", title = "Balance Adjustment") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "") -> P4

# Deeper dive into Expenses Main category
spending_data %>%
  filter(Main_category == "Expenses") %>%
  distinct(Title)

filter(spending_data, Main_category == "Expenses" & Title != "Weekly cash out") %>%
  ggplot(mapping = aes(x = Title, y = abs(Amount))) +
  geom_col() +
  labs(y = "Amount") +
  coord_flip() -> P5

# investment
ggplot(investment, mapping = aes(x = Title, y = abs(Amount))) +
  geom_col(mapping = aes(fill = Title)) +
  labs(x = "", y = "Amount", title = "Invesment/Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "") + 
  theme(legend.position = 'none') +
  facet_wrap(. ~ Month_Year) -> P6

# savings
savings %>%
  filter(Account != "Cash Money") %>%
  ggplot(mapping = aes(x = Month_Year, y = Amount)) +
  geom_col() + 
  labs(x = "Date", title = "Savings/Month") -> P7
  
# Coin vault
budget %>%
  filter(Account == "Coin Vault") %>%
  ggplot(mapping = aes(x = Date, y = Amount)) +
  geom_point(color = '#0c1f30' ) + 
  theme_minimal() -> P8
  
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
  scale_y_continuous(labels = comma) -> P9

# Frequency plot
  # Main_category frequency
ggplot(budget, mapping = aes(x = fct_rev(fct_infreq(Main_category)))) +
  geom_bar() +
  labs(x = "Main Category", y = "Count") +
  coord_flip() -> P10

  # Subcategory frequency
ggplot(budget, mapping = aes(x = fct_rev(fct_infreq(Subcategory)))) +
  geom_bar() +
  labs(x = "Subcategory", y = "Count") +
  coord_flip() -> P11

  # Main_category w/ Subcategory 
budget %>%
  filter(Subcategory != is.na(NA)) %>%
  ggplot(mapping = aes(fct_rev(fct_infreq(Main_category)))) +
  geom_bar(aes(fill = Subcategory), position = position_identity()) +
  labs(x = "Main Category",
       y = "Count",
       title = "Main Category", 
       subtitle = "with assigned Subcategory") +
  coord_flip() -> P12
  
  # Main_category wo/ Subcategory
budget %>%
  replace_na(list(Subcategory = "None")) %>%
  filter(Subcategory == "None") %>%
  ggplot(mapping = aes(fct_rev(fct_infreq(Main_category)))) +
  geom_bar(position = position_dodge2()) +
  labs(x = "Main Category",
       title = "Main Category", 
       subtitle = "with unassigned Subcategory") +
  coord_flip() -> P13

  # Account 
budget %>%
  ggplot(mapping = aes(x = Account)) +
  geom_bar() -> P14

# Time series clustering 
  # Reformatting the data
spending_data %>%
  select(Date, Main_category, Amount) %>%
  group_by(Main_category, Date) %>%
  summarise(Amount = sum(abs(Amount))) %>%
  spread(key = Main_category, value = Amount) -> spending_re

spending_re[is.na(spending_re)] <- 0

    # to matrix, 8x16
spending_re <- t(as.matrix(spending_re))
colnames(spending_re) <- spending_re[1,]
spending_re <- spending_re[-1,]
Main_category <- rownames(spending_re)

    # Time series plot
par(mfrow= c(3,5))
par(mar = c(2, 2, 1, 0))
for (i in 1:15) {
  plot(spending_re[i,], main=rownames(spending_re)[i], type = 'l')
}

# Correlation
spending_re <- apply(spending_re, 2, as.numeric)
rownames(spending_re) <- Main_category

diss_mat <- diss(spending_re, "COR")
summary(diss_mat)

melted_diss <- melt(as.matrix(diss_mat))
head(melted_diss)
  
  # Dissimilarity matrix plot
ggplot(melted_diss, aes(x= Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> P15
