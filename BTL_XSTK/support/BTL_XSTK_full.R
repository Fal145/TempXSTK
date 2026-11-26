# ==============================================================================
# BÀI TẬP LỚN XÁC SUẤT THỐNG KÊ (MT2013) - PHÂN TÍCH HIỆU NĂNG GPU
# Tổng hợp mã nguồn R của cả nhóm (đã hoàn thiện Phần 3 - Thống kê suy diễn):
#   Phần 1: Tiền xử lý số liệu (Data Preprocessing)
#   Phần 2: Thống kê mô tả & Biểu đồ (Descriptive Statistics)
#   Phần 3: Thống kê suy diễn (6.1 ANOVA, 6.2 Hồi quy & VIF,
#           6.3 Kiểm tra giả định, 6.4 Dự báo & Đánh giá Train/Test)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. NẠP THƯ VIỆN & CẤU HÌNH ĐƯỜNG DẪN
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(gridExtra)
  library(car)
  library(lmtest)
})

# Tự động nhận diện đường dẫn dữ liệu (chạy từ thư mục gốc hoặc thư mục support/)
raw_data_path <- if (file.exists("data/All_GPUs.csv")) {
  "data/All_GPUs.csv"
} else if (file.exists("support/data/All_GPUs.csv")) {
  "support/data/All_GPUs.csv"
} else {
  "../data/All_GPUs.csv"
}

data_dir <- dirname(raw_data_path)
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}
clean_data_path <- file.path(data_dir, "new_GPUs.csv")

img_des_dir <- if (dir.exists("img")) "img/statisticDes" else "../img/statisticDes"
if (!dir.exists(img_des_dir)) {
  dir.create(img_des_dir, recursive = TRUE)
}

img_pros_dir <- if (dir.exists("img")) "img/statisticPros" else "../img/statisticPros"
if (!dir.exists(img_pros_dir)) {
  dir.create(img_pros_dir, recursive = TRUE)
}

selected_cols <- c(
  "Core_Speed", "Max_Power", "Memory", "Memory_Bandwidth",
  "Memory_Bus", "Memory_Speed", "Process", "ROPs", "TMUs"
)


# ==============================================================================
# PHẦN 1: TIỀN XỬ LÝ SỐ LIỆU (DATA PREPROCESSING)
# ==============================================================================

# --- Bước 1.1: Đọc dữ liệu gốc và trích lọc 9 biến quan tâm ---
df_raw <- read.csv(raw_data_path, na.strings = c("\n-", "-", "None", "", "NA"))
df_step1 <- df_raw[, selected_cols]

cat(sprintf("Kích thước bộ dữ liệu ban đầu: %d dòng x %d cột\n\n",
            nrow(df_step1), ncol(df_step1)))
print(head(df_step1, 7))
cat("\n---------------------------------------------------------\n\n")

# --- Bước 1.2: Tách bỏ đơn vị và chuyển đổi sang kiểu số ---
extract_numeric <- function(val) {
  num <- str_extract(as.character(val), "[0-9.]+")
  return(as.numeric(num))
}

df_step2 <- df_step1
for (col in selected_cols) {
  # Tối ưu vector hóa trực tiếp (không cần sapply vì str_extract đã nhận vector)
  df_step2[[col]] <- extract_numeric(df_step2[[col]])
}

cat("Kiểu dữ liệu của các cột sau khi chuyển đổi:\n")
print(sapply(df_step2, class))

cat("\nHiển thị 7 dòng đầu tiên (đã xóa chữ đơn vị):\n")
print(head(df_step2, 7))
cat("\n---------------------------------------------------------\n\n")

# --- Bước 1.3: Chuẩn hóa ô trống và xử lý dữ liệu khuyết ---
cat("Thống kê số lượng ô trống ở từng cột trước khi xóa:\n")
print(colSums(is.na(df_step2)))

df_step3 <- na.omit(df_step2)
rownames(df_step3) <- NULL

cat(sprintf("\nKích thước dữ liệu sau khi xóa bỏ các dòng có NA: %d dòng x %d cột\n\n",
            nrow(df_step3), ncol(df_step3)))
print(head(df_step3, 7))
cat("\n---------------------------------------------------------\n\n")

# --- Bước 1.4: Loại bỏ quan sát trùng lặp và lưu dữ liệu sạch ---
duplicates_count <- sum(duplicated(df_step3))
cat(sprintf("Số lượng dòng bị trùng lặp hoàn toàn trên 9 thông số: %d\n",
            duplicates_count))

df_clean <- distinct(df_step3)
rownames(df_clean) <- NULL

cat(sprintf("Kích thước bộ dữ liệu SẠCH hoàn chỉnh: %d dòng x %d cột\n\n",
            nrow(df_clean), ncol(df_clean)))
cat("Bảng dữ liệu sạch 100% sẵn sàng cho bước Thống kê:\n")
print(head(df_clean, 7))

write.csv(df_clean, clean_data_path, row.names = FALSE)


# ==============================================================================
# PHẦN 2: THỐNG KÊ MÔ TẢ - BIỂU ĐỒ & ĐỒ THỊ (DESCRIPTIVE STATISTICS)
# ==============================================================================

df_clean <- read.csv(clean_data_path)
df_log <- log(df_clean)

# --- 2.1. Histogram dữ liệu gốc (Original) - 2 đồ thị / hàng ---
hist_orig_list <- list()

for (col in selected_cols) {
  p <- ggplot(df_clean, aes_string(x = col)) +
    geom_histogram(fill = "gray70", color = "black", bins = 30) +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      axis.title = element_text(size = 10)
    )
  hist_orig_list[[col]] <- p
}

png(file.path(img_des_dir, "histograms_original.png"),
    width = 10, height = 14, units = "in", res = 300)
grid.arrange(grobs = hist_orig_list, ncol = 2)
dev.off()

# --- 2.2. Histogram dữ liệu Log - 2 đồ thị / hàng ---
hist_log_list <- list()

for (col in selected_cols) {
  p <- ggplot(df_log, aes_string(x = col)) +
    geom_histogram(fill = "gray85", color = "black", bins = 30) +
    labs(
      title = paste0("Histogram of log(", col, ")"),
      x = paste0("log(", col, ")"),
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      axis.title = element_text(size = 10)
    )
  hist_log_list[[col]] <- p
}

png(file.path(img_des_dir, "histograms_log.png"),
    width = 10, height = 14, units = "in", res = 300)
grid.arrange(grobs = hist_log_list, ncol = 2)
dev.off()

# --- 2.3. Scatter Plots dạng Log (Core_Speed vs 8 biến còn lại) ---
feature_cols <- setdiff(selected_cols, "Core_Speed")
scatter_log_list <- list()

for (col in feature_cols) {
  p <- ggplot(df_log, aes_string(x = col, y = "Core_Speed")) +
    geom_point(color = "orange", alpha = 0.6, size = 1.5) +
    labs(
      title = paste0("Scatter Plot of log(", col, ") with log(Core_Speed)"),
      x = paste0("log(", col, ")"),
      y = "log(Core_Speed)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      axis.title = element_text(size = 10)
    )
  scatter_log_list[[col]] <- p
}

png(file.path(img_des_dir, "scatter_log.png"),
    width = 10, height = 12, units = "in", res = 300)
grid.arrange(grobs = scatter_log_list, ncol = 2)
dev.off()


# ==============================================================================
# PHẦN 3: THỐNG KÊ SUY DIỄN, KIỂM TRA GIẢ ĐỊNH & DỰ BÁO
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1. Phân tích phương sai một nhân tố (ANOVA) & hậu kiểm (Mục 6.1)
# ------------------------------------------------------------------------------
cat("\n================ MỤC 6.1: PHÂN TÍCH ANOVA ================\n")

# Lọc NA trước rồi loại trùng theo tên mẫu GPU (Name) -> N = 2,469 quan sát
df_anova <- df_raw %>%
  transmute(
    Name = as.character(Name),
    Manufacturer = as.factor(Manufacturer),
    Core_Speed = extract_numeric(Core_Speed)
  ) %>%
  filter(!is.na(Name), !is.na(Manufacturer), !is.na(Core_Speed)) %>%
  distinct(Name, .keep_all = TRUE) %>%
  mutate(Manufacturer = droplevels(Manufacturer))

cat(sprintf("Cỡ mẫu phân tích ANOVA: N = %d\n\n", nrow(df_anova)))

# Bảng thống kê mô tả Core_Speed theo từng nhà sản xuất
anova_desc <- df_anova %>%
  group_by(Manufacturer) %>%
  summarise(
    n = n(),
    Mean = round(mean(Core_Speed), 2),
    SD = round(sd(Core_Speed), 2),
    Median = round(median(Core_Speed), 2),
    IQR = round(IQR(Core_Speed), 2),
    .groups = "drop"
  )
print(as.data.frame(anova_desc))

# Kiểm định phân phối chuẩn Shapiro-Wilk cho từng nhóm
cat("\n--- Kiểm định Shapiro-Wilk theo nhà sản xuất ---\n")
print(tapply(df_anova$Core_Speed, df_anova$Manufacturer, shapiro.test))

# Kiểm định đồng nhất phương sai Levene's Test
cat("\n--- Kiểm định Levene (đồng nhất phương sai) ---\n")
print(leveneTest(Core_Speed ~ Manufacturer, data = df_anova))

# Biểu đồ Q-Q Plot theo 4 nhà sản xuất
p_anova_qq <- ggplot(df_anova, aes(sample = Core_Speed)) +
  stat_qq(color = "steelblue", alpha = 0.6, size = 1.2) +
  stat_qq_line(color = "red", linetype = "dashed") +
  facet_wrap(~ Manufacturer, scales = "free") +
  labs(
    title = "Q-Q Plot của Core_Speed theo từng nhà sản xuất",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles (MHz)"
  ) +
  theme_minimal()

ggsave(file.path(img_pros_dir, "anova_qqplot.png"),
       plot = p_anova_qq, width = 9, height = 6, dpi = 300)

# Biểu đồ Boxplot so sánh phân phối Core_Speed giữa các nhà sản xuất
p_anova_box <- ggplot(df_anova, aes(x = Manufacturer, y = Core_Speed, fill = Manufacturer)) +
  geom_boxplot(alpha = 0.75, show.legend = FALSE) +
  labs(
    title = "Phân phối Core_Speed (MHz) theo nhà sản xuất",
    x = "Nhà sản xuất",
    y = "Core_Speed (MHz)"
  ) +
  theme_minimal()

ggsave(file.path(img_pros_dir, "anova_boxplot.png"),
       plot = p_anova_box, width = 8, height = 5, dpi = 300)

# ANOVA tiêu chuẩn, Welch's ANOVA và hậu kiểm Tukey HSD
cat("\n--- ANOVA tiêu chuẩn ---\n")
anova_model <- aov(Core_Speed ~ Manufacturer, data = df_anova)
print(summary(anova_model))

cat("\n--- Welch's ANOVA (không giả định phương sai bằng nhau) ---\n")
print(oneway.test(Core_Speed ~ Manufacturer, data = df_anova, var.equal = FALSE))

cat("\n--- Phân tích hậu kiểm Tukey HSD ---\n")
print(TukeyHSD(anova_model))


# ------------------------------------------------------------------------------
# 3.2. Xây dựng, so sánh 3 mô hình hồi quy & kiểm tra VIF (Mục 6.2)
# ------------------------------------------------------------------------------
cat("\n================ MỤC 6.2: HỒI QUY TUYẾN TÍNH BỘI ================\n")

df <- df_clean

Model1 <- lm(
  Core_Speed ~ Memory_Bandwidth + Memory + Memory_Speed +
    Memory_Bus + Process + ROPs + TMUs,
  data = df
)

Model2 <- lm(
  Core_Speed ~ Memory_Bandwidth + Memory + Memory_Bus +
    Process + ROPs + TMUs,
  data = df
)

Model3 <- lm(
  Core_Speed ~ Max_Power + Memory_Bandwidth + Memory +
    Memory_Speed + Memory_Bus + Process + ROPs + TMUs,
  data = df_log
)

cat("\n--- Tóm tắt Mô hình 1 (7 biến, thang gốc) ---\n")
print(summary(Model1))

cat("\n--- Tóm tắt Mô hình 2 (6 biến, thang gốc) ---\n")
print(summary(Model2))

cat("\n--- Tóm tắt Mô hình 3 (8 biến, log-log) ---\n")
print(summary(Model3))

cat("\n--- Hệ số phóng đại phương sai VIF của Mô hình 3 ---\n")
print(round(vif(Model3), 2))


# ------------------------------------------------------------------------------
# 3.3. Kiểm tra các giả định hồi quy & điểm ảnh hưởng (Mục 6.3)
# ------------------------------------------------------------------------------
cat("\n================ MỤC 6.3: KIỂM TRA GIẢ ĐỊNH ================\n")

# Xuất 4 biểu đồ chẩn đoán phần dư của Mô hình 3
png(file.path(img_pros_dir, "model3_diagnostics.png"),
    width = 9, height = 7, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(Model3)
par(mfrow = c(1, 1))
dev.off()

cat(sprintf("Trung bình phần dư: %.4e\n", mean(residuals(Model3))))

cat("\n--- Kiểm định Shapiro-Wilk (phân phối chuẩn của phần dư) ---\n")
print(shapiro.test(residuals(Model3)))

cat("\n--- Kiểm định Breusch-Pagan (phương sai đồng nhất) ---\n")
print(bptest(Model3))

cat("\n--- Kiểm định Durbin-Watson (tự tương quan sai số) ---\n")
print(dwtest(Model3))

# Top 5 quan sát có khoảng cách Cook lớn nhất (Bảng 9)
cooks_d <- cooks.distance(Model3)
top5_idx <- order(cooks_d, decreasing = TRUE)[1:5]
top5_cook <- data.frame(
  Quan_sat = top5_idx,
  Cooks_D = round(cooks_d[top5_idx], 4),
  Leverage = round(hatvalues(Model3)[top5_idx], 4),
  Std_Resid = round(rstandard(Model3)[top5_idx], 4),
  Actual_MHz = round(df$Core_Speed[top5_idx], 2),
  Predicted_MHz = round(exp(fitted(Model3)[top5_idx]), 2)
)
cat("\n--- Top 5 quan sát có Cook's Distance lớn nhất ---\n")
print(top5_cook, row.names = FALSE)


# ------------------------------------------------------------------------------
# 3.4. Dự đoán toàn mẫu & đánh giá trên tập Train/Test 70:30 (Mục 6.4)
# ------------------------------------------------------------------------------
cat("\n================ MỤC 6.4: DỰ ĐOÁN & ĐÁNH GIÁ ================\n")

# Hàm tính các chỉ số đánh giá (MAE, MSE, RMSE, R2, MAPE)
calc_metrics <- function(actual, predicted, include_mape = FALSE) {
  err <- actual - predicted
  mae <- mean(abs(err))
  mse <- mean(err^2)
  rmse <- sqrt(mse)
  r2 <- 1 - sum(err^2) / sum((actual - mean(actual))^2)
  mape <- if (include_mape) mean(abs(err / actual)) * 100 else NA
  c(MAE = mae, MSE = mse, RMSE = rmse, R2 = r2, MAPE = mape)
}

# --- 3.4.1. Đối chiếu 10 quan sát đầu trên toàn mẫu (Bảng 10) ---
pred_all_log <- predict(Model3, newdata = df_log)
pred_all_mhz <- exp(pred_all_log)

comparison_table <- data.frame(
  STT = seq_len(nrow(df)),
  Actual_log = round(df_log$Core_Speed, 4),
  Predicted_log = round(pred_all_log, 4),
  Actual_MHz = df$Core_Speed,
  Predicted_MHz = round(pred_all_mhz, 4),
  Abs_Error_MHz = round(abs(df$Core_Speed - pred_all_mhz), 4),
  Pct_Error = round(abs(df$Core_Speed - pred_all_mhz) / df$Core_Speed * 100, 2)
)
cat("\n--- Đối chiếu 10 quan sát đầu (Bảng 10) ---\n")
print(head(comparison_table, 10), row.names = FALSE)

# --- 3.4.2. Chia tập Train/Test 70:30 với set.seed(99) ---
set.seed(99)
n <- nrow(df_log)

train_idx <- sample(1:n, size = 0.7 * n)
train_log <- df_log[train_idx, ]
test_log <- df_log[-train_idx, ]

Model3_tr <- lm(
  Core_Speed ~ Max_Power + Memory_Bandwidth + Memory +
    Memory_Speed + Memory_Bus + Process + ROPs + TMUs,
  data = train_log
)

pred_test_log <- predict(Model3_tr, newdata = test_log)
act_test_log <- test_log$Core_Speed
pred_test_mhz <- exp(pred_test_log)
act_test_mhz <- exp(act_test_log)

# Tổng hợp đầy đủ 4 dòng chỉ số đánh giá (Bảng 11)
eval_table <- rbind(
  `Toan mau (n=1420) - log` = round(calc_metrics(df_log$Core_Speed, pred_all_log, FALSE), 4),
  `Toan mau - MHz`          = round(calc_metrics(df$Core_Speed, pred_all_mhz, TRUE), 4),
  `Tap Test (n=427) - log`  = round(calc_metrics(act_test_log, pred_test_log, FALSE), 4),
  `Tap Test - MHz`          = round(calc_metrics(act_test_mhz, pred_test_mhz, TRUE), 4)
)
cat("\n--- Bảng chỉ số đánh giá hiệu quả dự báo (Bảng 11) ---\n")
print(as.data.frame(eval_table))

# Biểu đồ so sánh giá trị thực tế và dự báo trên tập Test (Hình 11)
png(file.path(img_pros_dir, "prediction_test.png"),
    width = 8, height = 6, units = "in", res = 300)
plot(
  act_test_mhz, pred_test_mhz,
  pch = 19,
  col = rgb(0, 0, 0, 0.55),
  xlab = "Core_Speed thuc te (MHz)",
  ylab = "Core_Speed du doan (MHz)",
  main = "So sanh gia tri thuc te va du bao tren tap Test (n = 427)"
)
abline(0, 1, lty = 2, col = "red", lwd = 2)
dev.off()
