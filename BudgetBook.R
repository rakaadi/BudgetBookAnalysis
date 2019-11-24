# Library
library(tidyverse)
library(lubridate)
library(scales)
library(forcats)
library(TSclust)
library(reshape2)
library(viridis)
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
  ggplot(aes(x = Date, y = abs(Amount))) +
  geom_line() +
  labs(y = "Amount") +
  scale_y_continuous(labels = comma) +
  theme_minimal() -> P1

  # Convert to plotly to see more detail 
ggplotly(P1)

# Spending / month by category
spending_data %>%
  group_by(Month = month(Date), Year = year(Date), Main_category) %>%
  summarise(Amount = sum(abs(Amount))) %>%
  ggplot(aes(x = Month, y = Amount, fill = Main_category)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(x = "", title = "Spending/Month", subtitle =  "by Category", fill = "Category") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8), 
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")) +
  scale_y_continuous(labels = comma) -> P2

# Deeper dive into Balance adjustment and Expenses category
spending_data %>%
  filter(Main_category %in% c("Balance Adjustment", "Compensation transactions")) %>%
  ggplot(aes(x = Month_Year, y = Amount, fill = Title)) +
  geom_col(position = position_dodge2()) +
  labs(x = "Date", title = "Balance Adjustment") +
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name = "") -> P3

  # Expenses Main category
spending_data %>%
  filter(Main_category == "Expenses") %>%
  distinct(Title)

filter(spending_data, Main_category == "Expenses" & Title != "Weekly cash out") %>%
  ggplot(aes(x = Title, y = abs(Amount))) +
  geom_col() +
  labs(y = "Amount") +
  coord_flip() -> P4

spending_data %>%
  filter(Title != "Weekly cash out") %>%
  group_by(Month = month(Date), Year = year(Date), Main_category) %>%
  summarise(Amount = sum(abs(Amount))) %>%
  ggplot(aes(x = Month, y = Amount, fill = Main_category)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(x = "", title = "Spending/Month", subtitle =  "by Category", fill = "Category") +
  scale_fill_viridis_d(option = 'inferno') +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8), 
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")) +
  scale_y_continuous(labels = comma) -> P5

  # Total spending per category
spending_data %>%
  filter(Title != "Weekly cash out") %>%
  group_by(Main_category) %>%
  summarise(Amount = sum(abs(Amount))) %>%
  ggplot(aes(x = Main_category, y = Amount)) +
  geom_point(size = 5, color = 'firebrick') +
  labs(x = "Main Category") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  coord_flip() -> P6

# Average Speding / category
spending_data %>%
  filter(Title != "Weekly cash out") %>%
  ggplot(aes(x = Main_category, y = abs(Amount))) +
  geom_boxplot(fill = 'lightblue') +
  labs(x = "Main category", y = "Amount", title = "Average Spending", subtitle = "by Category") +
  scale_y_continuous(labels = comma) +
  coord_flip() -> P7

# investment
ggplot(investment, aes(x = Title, y = abs(Amount), fill = factor(Title))) +
  geom_col() +
  labs(x = "", y = "Amount", title = "Invesment/Month") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ Month_Year) -> P8

# savings
savings %>%
  filter(Account != "Cash Money") %>%
  ggplot(aes(x = Month_Year, y = Amount)) +
  geom_col() +
  labs(x = "Date", title = "Savings/Month") +
  theme_minimal() -> P9
  
# Coin vault
budget %>%
  filter(Account == "Coin Vault") %>%
  ggplot(aes(x = Date, y = Amount)) +
  geom_point(color = 'firebrick' ) +
  theme_minimal() -> P10
  
# income
income %>%  
  group_by(Month = month(Date)) %>%
  summarise(Amount = sum(Amount)) %>%
  ggplot(aes(x = Month, y = Amount)) +
  geom_col(fill = 'darkgreen', position = position_identity()) +
  labs(x = "", title = 'Income/Month') +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
    label = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() -> P11

# Frequency of Occurence
  # Main_category frequency
ggplot(budget, aes(x = fct_rev(fct_infreq(Main_category)))) +
  geom_bar() +
  labs(x = "Main Category", y = "Count") +
  theme_minimal() +
  coord_flip() -> P12

  # Subcategory frequency
ggplot(budget, aes(x = fct_rev(fct_infreq(Subcategory)))) +
  geom_bar() +
  labs(x = "Subcategory", y = "Count") +
  theme_minimal() +
  coord_flip() -> P13

  # Main_category w/ Subcategory 
budget %>%
  filter(Subcategory != is.na(NA)) %>%
  ggplot(aes(fct_rev(fct_infreq(Main_category)))) +
  geom_bar(aes(fill = Subcategory), position = position_identity()) +
  scale_fill_viridis_d(option = "inferno") +
  labs(x = "Main Category",
       y = "Count",
       title = "Main Category", 
       subtitle = "with assigned Subcategory") +
  theme_minimal() + 
  coord_flip() -> P14
  
  # Main_category wo/ Subcategory
budget %>%
  replace_na(list(Subcategory = "None")) %>%
  filter(Subcategory == "None") %>%
  ggplot(aes(fct_rev(fct_infreq(Main_category)))) +
  geom_bar(position = position_dodge2()) +
  labs(x = "Main Category",
       title = "Main Category", 
       subtitle = "with unassigned Subcategory") +
  theme_minimal() +
  coord_flip() -> P15

  # Account 
budget %>%
  ggplot(mapping = aes(x = Account)) +
  geom_bar() + 
  theme_minimal() -> P16

# Time series clustering
  # Reformatting the data
budget %>%
  filter(Title != "Weekly cash out") %>%
  select(Month_Year, Main_category, Amount) %>%
  group_by(Main_category, Month_Year) %>%
  summarise(Amount = sum(abs(Amount))) %>%
  spread(key = Main_category, value = Amount) -> budget_re

budget_re[is.na(budget_re)] <- 0

  # to matrix format
budget_re <- t(as.matrix(budget_re))
colnames(budget_re) <- budget_re[1,]
budget_re <- budget_re[-1,]
Main_category <- rownames(budget_re)

  # Time series plot
par(mfrow= c(3,6))
par(mar = c(2, 2, 1, 0))
for (i in 1:17) {
  plot(budget_re[i,], main=rownames(budget_re)[i], type = 'l')
}

# Correlation
budget_re <- apply(budget_re, 2, as.numeric)
rownames(budget_re) <- Main_category

diss_mat <- diss(budget_re, "COR")
summary(diss_mat)

melted_diss <- melt(as.matrix(diss_mat))
head(melted_diss)
  
  # Dissimilarity matrix plot
ggplot(melted_diss, aes(x= Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis_c(option = "inferno") +
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) -> P17
