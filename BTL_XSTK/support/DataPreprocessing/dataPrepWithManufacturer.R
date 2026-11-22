suppressPackageStartupMessages(library(dplyr))
library(stringr)

# =========================================================
# BƯỚC 1: Trích lọc tập biến quan tâm (kèm Name và Manufacturer)
# =========================================================
data_path <- "../data/All_GPUs.csv"
df_raw <- read.csv(data_path, na.strings = c("\n-", "-", "None", "", "NA"))

numeric_cols <- c("Core_Speed", "Max_Power", "Memory", "Memory_Bandwidth", 
                  "Memory_Bus", "Memory_Speed", "Process", "ROPs", "TMUs")
keep_cols <- c("Manufacturer", numeric_cols)

df_step1 <- df_raw[, c("Name", keep_cols)]

cat(sprintf("Kích thước bộ dữ liệu ban đầu: %d dòng x %d cột\n\n", nrow(df_step1), ncol(df_step1)))
print(head(df_step1, 7))
cat("\n---------------------------------------------------------\n\n")

# =========================================================
# BƯỚC 2: Tách bỏ đơn vị, quy đổi hệ số & chuyển kiểu numeric
# =========================================================
extract_numeric <- function(val) {
  as.numeric(str_extract(as.character(val), "[0-9.]+"))
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
  return(base_num * mult)
}

df_step2 <- df_step1
for (col in numeric_cols) {
  if (col == "Memory_Bandwidth") {
    df_step2[[col]] <- parse_bandwidth(df_step2[[col]])
  } else if (col %in% c("ROPs", "TMUs")) {
    df_step2[[col]] <- parse_units_multiplier(df_step2[[col]])
  } else {
    df_step2[[col]] <- sapply(df_step2[[col]], extract_numeric)
  }
}

cat("Kiểu dữ liệu của các cột sau khi chuyển đổi:\n")
print(sapply(df_step2[, keep_cols], class))
cat("\nHiển thị 7 dòng đầu tiên (đã chuẩn hóa đơn vị):\n")
print(head(df_step2, 7))
cat("\n---------------------------------------------------------\n\n")

# =========================================================
# BƯỚC 3: Chuẩn hóa ô trống và xử lý dữ liệu khuyết
# =========================================================
cat("Thống kê số lượng ô trống ở từng cột trước khi xóa:\n")
print(colSums(is.na(df_step2)))

df_step3 <- na.omit(df_step2)
rownames(df_step3) <- NULL

cat(sprintf("\nKích thước dữ liệu sau khi xóa bỏ các dòng có NA: %d dòng x %d cột\n\n", nrow(df_step3), ncol(df_step3)))

# =========================================================
# BƯỚC 4: Loại bỏ quan sát trùng lặp theo tên mẫu GPU (Name)
# =========================================================
df_step3$Name <- str_trim(as.character(df_step3$Name))
duplicates_count <- sum(duplicated(df_step3$Name))
cat(sprintf("Số lượng dòng bị trùng lặp theo tên mẫu GPU (Name): %d\n", duplicates_count))

df_clean <- df_step3 %>%
  distinct(Name, .keep_all = TRUE) %>%
  select(all_of(keep_cols))
rownames(df_clean) <- NULL

cat(sprintf("Kích thước bộ dữ liệu SẠCH hoàn chỉnh: %d dòng x %d cột\n\n", nrow(df_clean), ncol(df_clean)))
print(head(df_clean, 7))

write.csv(df_clean, "../data/new_GPUs_Manufacturer.csv", row.names = FALSE)