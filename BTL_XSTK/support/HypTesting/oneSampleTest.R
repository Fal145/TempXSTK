# Đọc file CSV và gán vào biến sourcedata
sourcedata <- read.csv("../data/new_GPUs.csv")

# Trích xuất dữ liệu Memory_Bandwidth
x <- sourcedata$Core_Speed

# Thống kê mẫu
n <- length(x)
x_tb <- mean(x)
s <- sd(x)


# Kiểm tra phân phối chuẩn 
result <- shapiro.test(x) 
print(result)

# Thiết lập thông số kiểm định
alpha <- 0.05


# Tính giá trị kiểm định Z và p-value

z_th <- qnorm(1 - alpha / 2)
cat("z_toi_han = ", z_th)

e <- z_th * s / sqrt(n)
cat("e = ", e)

up_lim <- x_tb + e
low_lim <- x_tb - e
cat("Khoảng tin cậy: (", low_lim, ", ", up_lim, ")")

