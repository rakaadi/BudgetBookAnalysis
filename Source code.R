# Library yang digunakan
library(tidyverse)

# Membaca file csv
budget <- read_csv2("January-august_2019.csv", col_types = cols(
  Date = col_date(format = "%m/%d/%Y"),
  Account = col_factor(),
  Amount = col_number()
))
head(budget)

# Merapihkan kolom
  # Menghilangkan kolom 'Comment'
spending_data <- select(budget, -Comment)
head(spending_data)

  # Mengubah nama kolom Main category
  # dan menghilangkan nilai NA di kolom Subcategory
spending_data <- rename(spending_data, Main_category = 'Main category')
spending_data <- replace_na(spending_data, list(Subcategory = "None"))

  # Menghitung nilai absolut dari kolom Amount,
  # untuk menghilangkan nilai negatif
spending_data$Amount <- abs(spending_data$Amount)

# Mengambil data investasi dan menghilangkan kolom Subcategory
investment <- filter(spending_data, Main_category == 'Investment')
investment <- select(investment, -Subcategory)

# Mengambil data pemasukan 
income <- filter(spending_data, Main_category == "Income")

# Mengubah isi dari kolom subcategory, mungkin tidak dipakai

# Mengelompokan data berdasarkan bulan
# Kode ini belum berhasil sesuai harapan
month_investment <- investment %>%
  mutate_at(investment, Date, format(investment$Date, "%m%Y")) %>%
  group_by(month_investment, Date) %>%
  summarise(Amount=sum(Amount))

# Menjumlahkan pengeluaran untuk persatu hari
gdate_spending <- group_by(spending_data, Date) %>%
  summarise_if(is.numeric, sum)

# Plotting
  # investment
ggplot(data = investment, mapping = aes(x = Title, y = abs(Amount))) +
  geom_col(mapping = aes(fill = Title)) +
  facet_wrap(~ Date)
