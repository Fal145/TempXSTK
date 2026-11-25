# ==============================================================================
# BÀI TẬP LỚN XÁC SUẤT THỐNG KÊ (MT2013) - PHÂN TÍCH HIỆU NĂNG GPU
# Tổng hợp mã nguồn R của cả nhóm:
#   Phần 1: Tiền xử lý số liệu (Data Preprocessing)
#   Phần 2: Thống kê mô tả & Biểu đồ (Descriptive Statistics)
#   Phần 3: Thống kê suy diễn, Kiểm tra giả định & Dự báo (Inferential Statistics)
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

# Nhận diện dữ liệu khi chạy từ repo root, thư mục dự án hoặc support/.
data_candidates <- list(
  list(
    path = "data/All_GPUs.csv",
    project_dir = if (dir.exists("img") || dir.exists("support")) "." else if (dir.exists("../img")) ".." else "."
  ),
  list(path = "support/data/All_GPUs.csv", project_dir = "."),
  list(path = "../data/All_GPUs.csv", project_dir = "../.."),
  list(path = "BTL_XSTK/support/data/All_GPUs.csv", project_dir = "BTL_XSTK")
)
data_match <- Filter(function(candidate) file.exists(candidate$path), data_candidates)
if (length(data_match) == 0) {
  stop("Không tìm thấy data/All_GPUs.csv. Hãy chạy script từ repo root, thư mục BTL_XSTK hoặc support/.")
}
raw_data_path <- data_match[[1]]$path
project_dir <- data_match[[1]]$project_dir
data_dir <- dirname(raw_data_path)
clean_data_path <- file.path(data_dir, "new_GPUs.csv")

img_des_dir <- file.path(project_dir, "img", "statisticDes")
img_pros_dir <- file.path(project_dir, "img", "statisticPros")
dir.create(img_des_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(img_pros_dir, recursive = TRUE, showWarnings = FALSE)

selected_cols <- c(
  "Core_Speed", "Max_Power", "Memory", "Memory_Bandwidth",
  "Memory_Bus", "Memory_Speed", "Process", "ROPs", "TMUs"
)


# ==============================================================================
# PHẦN 1: TIỀN XỬ LÝ SỐ LIỆU (DATA PREPROCESSING)
# ==============================================================================

# --- Bước 1.1: Đọc dữ liệu gốc và trích lọc 9 biến quan tâm ---
df_raw <- read.csv(raw_data_path, na.strings = c("\n-", "-", "None", "", "NA"))
df_step1 <- df_raw[, c("Name", selected_cols)]

cat(sprintf("Kích thước bộ dữ liệu ban đầu: %d dòng x %d cột\n\n",
            nrow(df_step1), ncol(df_step1)))
print(head(df_step1, 7))
cat("\n---------------------------------------------------------\n\n")

# --- Bước 1.2: Tách bỏ đơn vị và chuyển đổi sang kiểu số ---
extract_numeric <- function(val) {
  num <- str_extract(as.character(val), "[0-9.]+")
  return(as.numeric(num))
}

parse_bandwidth <- function(val) {
  str_val <- as.character(val)
  num <- as.numeric(str_extract(str_val, "[0-9.]+"))
  is_mb <- str_detect(str_val, regex("MB", ignore_case = TRUE))
  ifelse(!is.na(num) & !is.na(is_mb) & is_mb, num / 1024, num)
}

parse_units_multiplier <- function(val) {
  str_val <- as.character(val)
  base_num <- as.numeric(str_extract(str_val, "^[0-9.]+"))
  mult_str <- str_extract(str_val, "(?<=\\(x)[0-9.]+(?=\\))")
  mult <- ifelse(is.na(mult_str), 1, as.numeric(mult_str))
  base_num * mult
}

df_step2 <- df_step1
for (col in selected_cols) {
  if (col == "Memory_Bandwidth") {
    df_step2[[col]] <- parse_bandwidth(df_step2[[col]])
  } else if (col %in% c("ROPs", "TMUs")) {
    df_step2[[col]] <- parse_units_multiplier(df_step2[[col]])
  } else {
    df_step2[[col]] <- sapply(df_step2[[col]], extract_numeric)
  }
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

# --- Bước 1.4: Loại bỏ quan sát trùng lặp theo tên mẫu GPU ---
df_step3$Name <- str_trim(as.character(df_step3$Name))
duplicates_count <- sum(duplicated(df_step3$Name))
cat(sprintf("Số lượng dòng bị trùng lặp theo tên mẫu GPU: %d\n",
            duplicates_count))

df_clean <- df_step3 %>%
  distinct(Name, .keep_all = TRUE) %>%
  select(all_of(selected_cols))
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

# --- 3.1. ANOVA: so sánh Core_Speed theo nhà sản xuất ---
data_anova <- df_raw %>%
  select(Name, Manufacturer, Core_Speed) %>%
  mutate(
    Name = str_trim(as.character(Name)),
    Manufacturer = str_trim(as.character(Manufacturer)),
    Core_Speed = as.numeric(str_extract(as.character(Core_Speed), "[0-9.]+"))
  ) %>%
  filter(!is.na(Name), !is.na(Manufacturer), !is.na(Core_Speed)) %>%
  distinct(Name, .keep_all = TRUE)

manufacturer_counts <- table(data_anova$Manufacturer)
valid_manufacturers <- names(manufacturer_counts[manufacturer_counts >= 30])
data_anova <- data_anova %>%
  filter(Manufacturer %in% valid_manufacturers) %>%
  mutate(Manufacturer = factor(Manufacturer))

cat("\n--- ANOVA: CORE_SPEED THEO NHÀ SẢN XUẤT ---\n")
print(data_anova %>%
        group_by(Manufacturer) %>%
        summarise(
          n = n(), Mean = mean(Core_Speed), SD = sd(Core_Speed),
          Median = median(Core_Speed), IQR = IQR(Core_Speed), .groups = "drop"
        ))

png(file.path(img_pros_dir, "anova_qqplot.png"),
    width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2))
for (manufacturer in levels(data_anova$Manufacturer)) {
  values <- data_anova$Core_Speed[data_anova$Manufacturer == manufacturer]
  qqnorm(values, main = paste("Q-Q Plot:", manufacturer))
  qqline(values, col = "red", lwd = 2)
  cat("\nShapiro-Wilk:", manufacturer, "\n")
  print(shapiro.test(values))
}
par(mfrow = c(1, 1))
dev.off()

png(file.path(img_pros_dir, "anova_boxplot.png"),
    width = 8, height = 5.5, units = "in", res = 300)
boxplot(Core_Speed ~ Manufacturer, data = data_anova,
        main = "Phân phối Core_Speed theo nhà sản xuất",
        xlab = "Nhà sản xuất", ylab = "Core Speed (MHz)",
        col = c("#ED1C24", "#990000", "#0071C5", "#76B900"))
dev.off()

print(car::leveneTest(Core_Speed ~ Manufacturer, data = data_anova))
model_aov <- aov(Core_Speed ~ Manufacturer, data = data_anova)
print(summary(model_aov))
print(TukeyHSD(model_aov))
print(oneway.test(Core_Speed ~ Manufacturer, data = data_anova, var.equal = FALSE))

# --- 3.2. Xây dựng và so sánh 3 mô hình hồi quy ---
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

summary(Model1)
summary(Model2)
summary(Model3)
vif(Model3)

# --- 3.3. Kiểm tra các giả định hồi quy ---
par(mfrow = c(2, 2))
plot(Model3)
par(mfrow = c(1, 1))

mean(residuals(Model3))
shapiro.test(residuals(Model3))
bptest(Model3)
dwtest(Model3)
sort(cooks.distance(Model3), decreasing = TRUE)[1:5]

# --- 3.4. Dự đoán trên toàn bộ mẫu ---
pred_log <- predict(Model3, newdata = df_log)

comparison_table <- data.frame(
  Actual_log = df_log$Core_Speed,
  Predicted_log = pred_log,
  Actual_MHz = df$Core_Speed,
  Predicted_MHz = exp(pred_log)
)

head(comparison_table, 10)

# --- 3.5. Đánh giá trên tập Train/Test (70:30) ---
set.seed(99)
n <- nrow(df_log)

train_idx <- sample(seq_len(n), size = floor(0.7 * n))
train_log <- df_log[train_idx, ]
test_log <- df_log[-train_idx, ]

Model3_tr <- lm(
  Core_Speed ~ Max_Power + Memory_Bandwidth + Memory +
    Memory_Speed + Memory_Bus + Process + ROPs + TMUs,
  data = train_log
)

pred_mhz <- exp(predict(Model3_tr, newdata = test_log))
act_mhz <- exp(test_log$Core_Speed)
err_mhz <- act_mhz - pred_mhz

MAE  <- mean(abs(err_mhz))
MSE  <- mean(err_mhz^2)
RMSE <- sqrt(MSE)
R2   <- 1 - sum(err_mhz^2) / sum((act_mhz - mean(act_mhz))^2)
MAPE <- mean(abs(err_mhz / act_mhz)) * 100

print(data.frame(MAE = MAE, MSE = MSE, RMSE = RMSE, R2 = R2, MAPE = MAPE))

plot(
  act_mhz, pred_mhz,
  pch = 19,
  xlab = "Core_Speed thuc te (MHz)",
  ylab = "Core_Speed du doan (MHz)",
  main = "Thuc te vs Du bao (Test)"
)
abline(0, 1, lty = 2, col = "red")
