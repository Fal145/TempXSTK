library(stringr) 

# Đọc file CSV
sourcedata <- read.csv("../data/All_GPUs.csv")

# Tách bỏ đơn vị và chuyển đổi kiểu dữ liệu cho cột Memory_Bandwidth

extract_numeric <- function(val) {
  num <- str_extract(as.character(val), "[0-9.]+")
  return(as.numeric(num))
}

# Áp dụng hàm chuyển đổi trực tiếp cho cột Memory_Bandwidth
sourcedata$Memory_Bandwidth <- sapply(sourcedata$Memory_Bandwidth, extract_numeric)

# Loại bỏ các dòng bị thiếu dữ liệu (NA) ở cột Memory_Bandwidth để tính toán không bị lỗi
sourcedata <- subset(sourcedata, !is.na(Memory_Bandwidth))

cat("Kiểu dữ liệu của cột Memory_Bandwidth sau khi chuyển đổi:", class(sourcedata$Memory_Bandwidth), "\n")
cat("\nHiển thị 7 dòng đầu tiên (đã xóa chữ đơn vị):\n")
print(head(sourcedata$Memory_Bandwidth, 7))
cat("\n-----------------------------------------------------------------\n\n")

# Tách dữ liệu theo 2 nhóm: NVIDIA và AMD
nvidia_GPU <- subset(sourcedata, Manufacturer == "Nvidia")
amd_GPU <- subset(sourcedata, Manufacturer == "AMD")

# Tính các đặc trưng mẫu của từng nhóm
n1 <- length(nvidia_GPU$Memory_Bandwidth)
x_1 <- mean(nvidia_GPU$Memory_Bandwidth)
s1 <- sd(nvidia_GPU$Memory_Bandwidth)

n2 <- length(amd_GPU$Memory_Bandwidth)
x_2 <- mean(amd_GPU$Memory_Bandwidth)
s2 <- sd(amd_GPU$Memory_Bandwidth)

# Tổng hợp bảng thống kê mô tả 2 mẫu
summary_table <- data.frame(
  Hang = c("Nvidia", "AMD"),
  So_luong = c(n1, n2),
  Trung_binh = round(c(x_1, x_2), 4),
  Do_lech_chuan = round(c(s1, s2), 4)
)
print(summary_table)

# Kiểm tra giả định phân phối chuẩn (Q-Q Plot & Shapiro-Wilk test)
par(mfrow = c(1, 2)) # Chia khung hình vẽ làm 2 cột
qqnorm(nvidia_GPU$Memory_Bandwidth, main = "Q-Q Plot: Nvidia")
qqline(nvidia_GPU$Memory_Bandwidth, col = "red")
qqnorm(amd_GPU$Memory_Bandwidth, main = "Q-Q Plot: AMD")
qqline(amd_GPU$Memory_Bandwidth, col = "red")
par(mfrow = c(1, 1)) # Trả lại khung hình mặc định

print(shapiro.test(nvidia_GPU$Memory_Bandwidth))
print(shapiro.test(amd_GPU$Memory_Bandwidth))
# (Kết quả p-value < 0.05 -> không chuẩn, nhưng n1, n2 > 30 nên dùng Z-test)

# Thực hiện kiểm định Z hai mẫu độc lập (Kiểm định phía phải: H1: mu_1 > mu_2)
alpha <- 0.05
z0 <- (x_1 - x_2) / sqrt((s1^2 / n1) + (s2^2 / n2))
z_alpha <- qnorm(1 - alpha) # Giá trị tới hạn cho kiểm định 1 phía
p_val <- 1 - pnorm(z0)      # Tính p-value phía phải

# In kết quả kiểm định và kết luận tự động
cat("\n--- KẾT QUẢ KIỂM ĐỊNH 2 MẪU (Z-TEST) ---\n")
cat("Giá trị thống kê Z0    :", round(z0, 4), "\n")
cat("Ngưỡng tới hạn Z_alpha :", round(z_alpha, 4), "\n")
cat("p-value                :", round(p_val, 4), "\n")

if (z0 > z_alpha) {
  cat("=> Kết luận: Bác bỏ H0. Băng thông trung bình của Nvidia lớn hơn AMD.\n")
} else {
  cat("=> Kết luận: Chưa đủ cơ sở bác bỏ H0. Chưa thể khẳng định Nvidia lớn hơn AMD.\n")
}