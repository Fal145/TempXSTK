suppressPackageStartupMessages({
  library(car)
  library(dplyr)
  library(stringr)
})

# 1. Đọc dữ liệu gốc và chuẩn hóa riêng cho bài toán ANOVA (Manufacturer vs Core_Speed)
df_raw <- read.csv("../data/All_GPUs.csv", na.strings = c("\n-", "-", "None", "", "NA"))

data_anova <- df_raw %>%
  select(Name, Manufacturer, Core_Speed) %>%
  mutate(
    Name = str_trim(as.character(Name)),
    Manufacturer = str_trim(as.character(Manufacturer)),
    Core_Speed = as.numeric(str_extract(as.character(Core_Speed), "[0-9.]+"))
  ) %>%
  filter(!is.na(Name), !is.na(Manufacturer), !is.na(Core_Speed)) %>%
  distinct(Name, .keep_all = TRUE) # Mỗi dòng là 1 mẫu GPU duy nhất

cat("--- TẦN SỐ CÁC NHÀ SẢN XUẤT TRƯỚC KHI LỌC ---\n")
print(table(data_anova$Manufacturer))

# Giữ lại các hãng có cỡ mẫu n >= 30 (Lúc này AMD, ATI, Nvidia đều đạt >= 30)
freq_table <- table(data_anova$Manufacturer)
valid_manufacturers <- names(freq_table[freq_table >= 30])

data_anova <- data_anova %>%
  filter(Manufacturer %in% valid_manufacturers) %>%
  mutate(Manufacturer = factor(Manufacturer))

cat("\n--- TẦN SỐ CÁC NHÀ SẢN XUẤT ĐƯA VÀO PHÂN TÍCH (n >= 30) ---\n")
print(table(data_anova$Manufacturer))

# 2. Thống kê mô tả theo từng hãng
cat("\n--- THỐNG KÊ MÔ TẢ CORE_SPEED THEO HÃNG ---\n")
desc_stats <- data_anova %>%
  group_by(Manufacturer) %>%
  summarise(
    n = n(),
    Mean = round(mean(Core_Speed), 2),
    SD = round(sd(Core_Speed), 2),
    Median = round(median(Core_Speed), 2),
    IQR = round(IQR(Core_Speed), 2)
  )
print(as.data.frame(desc_stats))

# 3. Kiểm tra giả định phân phối chuẩn (Shapiro-Wilk & xuất ảnh Q-Q Plot)
img_dir <- "../../img/statisticPros"

png(file.path(img_dir, "anova_qqplot.png"), width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
for (hang in levels(data_anova$Manufacturer)) {
  sub_data <- data_anova$Core_Speed[data_anova$Manufacturer == hang]
  qqnorm(sub_data, main = paste("Q-Q Plot:", hang))
  qqline(sub_data, col = "red", lwd = 2)
  cat(paste0("\n>> Shapiro-Wilk cho ", hang, " (n = ", length(sub_data), "):\n"))
  print(shapiro.test(sub_data))
}
par(mfrow = c(1, 1))
dev.off()

# Xuất ảnh Boxplot (Đủ 4 màu cho AMD, ATI, Intel, Nvidia)
png(file.path(img_dir, "anova_boxplot.png"), width = 8, height = 5.5, units = "in", res = 300)
boxplot(Core_Speed ~ Manufacturer, data = data_anova,
        main = "Phân phối Core_Speed theo Nhà sản xuất",
        xlab = "Nhà sản xuất", ylab = "Core Speed (MHz)",
        col = c("#ED1C24", "#990000", "#0071C5", "#76B900"))
dev.off()

# 4. Kiểm tra đồng nhất phương sai (Levene's Test)
cat("\n--- KIỂM ĐỊNH ĐỒNG NHẤT PHƯƠNG SAI (LEVENE'S TEST) ---\n")
print(leveneTest(Core_Speed ~ Manufacturer, data = data_anova))

# 5. Chạy ANOVA một nhân tố & Hậu kiểm Tukey HSD
cat("\n--- KẾT QUẢ PHÂN TÍCH PHƯƠNG SAI (ANOVA) ---\n")
model_aov <- aov(Core_Speed ~ Manufacturer, data = data_anova)
print(summary(model_aov))

cat("\n--- HẬU KIỂM TUKEY HSD ---\n")
print(TukeyHSD(model_aov))

# Dự phòng: Welch's ANOVA nếu Levene's Test có p-value < 0.05 (phương sai không đồng nhất)
cat("\n--- WELCH'S ANOVA (DÙNG KHI PHƯƠNG SAI KHÔNG ĐỒNG NHẤT) ---\n")
print(oneway.test(Core_Speed ~ Manufacturer, data = data_anova, var.equal = FALSE))