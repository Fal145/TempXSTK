# ==============================================================================
# BÀI TẬP LỚN XÁC SUẤT THỐNG KÊ - MÔ HÌNH HỒI QUY TUYẾN TÍNH BỘI
# ==============================================================================
# 1. ĐỌC DỮ LIỆU SẠCH VÀ BIẾN ĐỔI LOGARITHM
df_clean <- read.csv("../data/new_GPUs.csv")

# Kiểm tra an toàn giá trị 0 trước khi lấy log
if (any(df_clean == 0)) {
  df_log <- log(df_clean + 1)
} else {
  df_log <- log(df_clean)
}

cat("--- KÍCH THƯỚC BỘ DỮ LIỆU: ", nrow(df_clean), "dòng x", ncol(df_clean), "cột ---\n")

# 2. XÂY DỰNG MÔ HÌNH HỒI QUY
Model1 <- lm(Core_Speed ~ Memory_Bandwidth + Memory + Memory_Speed + Memory_Bus + Process + ROPs + TMUs, 
             data = df_clean)

Model2 <- lm(Core_Speed ~ Memory_Bandwidth + Memory + Memory_Bus + Process + ROPs + TMUs, 
             data = df_clean)

# Mô hình 3: Dạng Logarithm đầy đủ 8 biến độc lập
Model3 <- lm(Core_Speed ~ Max_Power + Memory_Bandwidth + Memory + Memory_Speed + 
               Memory_Bus + Process + ROPs + TMUs, 
             data = df_log)

cat("\n=================== KẾT QUẢ MÔ HÌNH 3 (LOGARITHM) ===================\n")
print(summary(Model3))

# 3. KIỂM ĐỊNH PHẦN DƯ
par(mfrow = c(2, 2))
plot(Model3)
par(mfrow = c(1, 1))

# 4. DỰ ĐOÁN GIÁ TRỊ
predicted_values <- predict(Model3, newdata = df_log)

comparison_table <- data.frame(
  Actual_log = df_log$Core_Speed,
  Predicted_log = predicted_values,
  Actual_MHz = df_clean$Core_Speed,
  Predicted_MHz = exp(predicted_values)
)

cat("\n--- BẢNG SO SÁNH (10 DÒNG ĐẦU) ---\n")
print(head(comparison_table, 10))

# 5. VẼ ĐỒ THỊ SO SÁNH
plot(predicted_values, df_log$Core_Speed,
     main = "Plot to predict: Observed vs Predicted Values",
     xlab = "Predicted values", 
     ylab = "Observed values",
     col = "black", pch = 1)
abline(lm(df_log$Core_Speed ~ predicted_values), col = "red", lwd = 2)

