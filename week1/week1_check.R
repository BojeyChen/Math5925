# 1. Packages
packages <- c("data.table", "ggplot2", "rstudioapi")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}
library(data.table)
library(ggplot2)



# 2. paths
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)
data_path <- file.path(
  script_dir,
  "1_min_SPY_2008-2021.csv"
)
out_dir <- script_dir
cat("Working folder:", script_dir, "\n")


# 3. Read
raw <- fread(data_path)

raw_rows <- nrow(raw)
raw_cols <- ncol(raw)
missing_n <- sum(is.na(raw))

cat("Raw rows:", raw_rows, "\n")
cat("Raw columns:", raw_cols, "\n")
cat("Missing values:", missing_n, "\n")


# 4. Parse time / 处理时间
raw[, datetime := as.POSIXct(
  gsub("\\s+", " ", trimws(date)),
  format = "%Y%m%d %H:%M:%S",
  tz = "UTC"
)]
raw[, trading_date := as.IDate(datetime)]
raw[, clock_time := format(datetime, "%H:%M:%S")]
raw[, minute_of_day :=
      as.integer(format(datetime, "%H")) * 60 +
      as.integer(format(datetime, "%M"))]


# 5. Data quality checks / 数据质量检查
content_cols <- c(
  "date",
  "open",
  "high",
  "low",
  "close",
  "volume",
  "barCount",
  "average"
)

# 重复
duplicate_n <- sum(
  duplicated(raw[, ..content_cols])
)

# 倒序
backward_n <- sum(
  diff(as.numeric(raw$datetime)) < 0,
  na.rm = TRUE
)

# 去重
dat <- unique(
  copy(raw),
  by = content_cols
)

# 按时间排序
setorder(dat, datetime)

# 开盘收盘高低点检查
bad_high <- dat[
  high < pmax(open, low, close)
]
bad_low <- dat[
  low > pmin(open, high, close)
]
bad_vwap <- dat[
  average < low | average > high
]



# 6. Daily coverage
daily_coverage <- dat[
  ,
  .(
    N_Observations = .N,
    First_Time = min(clock_time),
    Last_Time = max(clock_time)
  ),
  by = trading_date
][order(trading_date)]

fwrite(
  daily_coverage,
  file.path(out_dir, "daily_coverage.csv")
)



# 7. Common 390-minute window / 统一390分钟窗口
# 数据中最主要的共同时间窗口为07:30-13:59
# 这里只称为common observation window，
# 暂不假设原始timestamp的具体时区

common <- dat[
  minute_of_day >= 450 &
    minute_of_day <= 839
]

common_coverage <- common[
  ,
  .(N_Observations = .N),
  by = trading_date
]

# 只保留完整390分钟的交易日
complete_days <- common_coverage[
  N_Observations == 390,
  trading_date
]

balanced <- copy(
  common[
    trading_date %in% complete_days
  ]
)

setorder(
  balanced,
  trading_date,
  datetime
)



# 8. One-minute returns / 1分钟收益率
balanced[
  ,
  log_return :=
    log(close / shift(close)),
  by = trading_date
]



# 9. Daily data / 每日汇总数据
daily <- balanced[
  ,
  .(
    Close = last(close),
    
    Realised_Volatility =
      sqrt(
        sum(log_return^2, na.rm = TRUE)
      )
  ),
  by = trading_date
]

# 按日期排序
setorder(daily, trading_date)

fwrite(
  daily,
  file.path(out_dir, "daily_summary.csv")
)



# 10. Intraday pattern / 日内规律

intraday <- balanced[
  ,
  .(
    Mean_Volume =
      mean(as.numeric(volume), na.rm = TRUE),
    
    Mean_Abs_Return =
      if (all(is.na(log_return))) {
        NA_real_
      } else {
        mean(abs(log_return), na.rm = TRUE)
      }
  ),
  by = .(
    minute_of_day,
    clock_time
  )
][order(minute_of_day)]

fwrite(
  intraday,
  file.path(out_dir, "intraday_pattern.csv")
)



# 11. Figure 1: Price trend
p1 <- ggplot(
  daily,
  aes(
    x = trading_date,
    y = Close
  )
) +
  geom_line(linewidth = 0.35) +
  labs(
    title = "SPY Price Over Time",
    x = "Trading Date",
    y = "Last Price in Common Window"
  ) +
  theme_minimal()

ggsave(
  file.path(
    out_dir,
    "01_price_trend.png"
  ),
  p1,
  width = 10,
  height = 5,
  dpi = 300
)



# 12. Figure 2: Observations per trading day
p2 <- ggplot(
  daily_coverage,
  aes(
    x = trading_date,
    y = N_Observations
  )
) +
  geom_line(linewidth = 0.35) +
  labs(
    title = "Number of Observations per Trading Day",
    x = "Trading Date",
    y = "Number of Observations"
  ) +
  theme_minimal()

ggsave(
  file.path(
    out_dir,
    "02_daily_observation_count.png"
  ),
  p2,
  width = 10,
  height = 5,
  dpi = 300
)



# 13. Figure 3: Intraday volume
p3 <- ggplot(
  intraday,
  aes(
    x = minute_of_day,
    y = Mean_Volume
  )
) +
  geom_line(linewidth = 0.45) +
  scale_x_continuous(
    breaks = c(
      450, 510, 570, 630,
      690, 750, 810, 839
    ),
    labels = c(
      "07:30", "08:30", "09:30", "10:30",
      "11:30", "12:30", "13:30", "13:59"
    )
  ) +
  labs(
    title = "Average Intraday Volume Pattern",
    x = "Time of Day",
    y = "Average Volume"
  ) +
  theme_minimal()

ggsave(
  file.path(
    out_dir,
    "03_intraday_volume.png"
  ),
  p3,
  width = 10,
  height = 5,
  dpi = 300
)



# 14. Figure 4: Intraday volatility
p4 <- ggplot(
  intraday[
    !is.na(Mean_Abs_Return)
  ],
  aes(
    x = minute_of_day,
    y = Mean_Abs_Return
  )
) +
  geom_line(linewidth = 0.45) +
  scale_x_continuous(
    breaks = c(
      450, 510, 570, 630,
      690, 750, 810, 839
    ),
    labels = c(
      "07:30", "08:30", "09:30", "10:30",
      "11:30", "12:30", "13:30", "13:59"
    )
  ) +
  labs(
    title = "Average Intraday Absolute Return",
    x = "Time of Day",
    y = "Mean Absolute Log Return"
  ) +
  theme_minimal()

ggsave(
  file.path(
    out_dir,
    "04_intraday_volatility.png"
  ),
  p4,
  width = 10,
  height = 5,
  dpi = 300
)



# 15. Figure 5: Daily realised volatility
p5 <- ggplot(
  daily,
  aes(
    x = trading_date,
    y = Realised_Volatility
  )
) +
  geom_line(linewidth = 0.35) +
  labs(
    title = "Daily Realised Volatility",
    x = "Trading Date",
    y = "Realised Volatility"
  ) +
  theme_minimal()

ggsave(
  file.path(
    out_dir,
    "05_daily_realised_volatility.png"
  ),
  p5,
  width = 10,
  height = 5,
  dpi = 300
)



# 16. Save incomplete days / 保存非完整交易日
incomplete_days <- common_coverage[
  N_Observations != 390
]

fwrite(
  incomplete_days,
  file.path(
    out_dir,
    "incomplete_days.csv"
  )
)



# 17. Save Week 1 summary
complete_pct <- round(
  length(complete_days) /
    uniqueN(dat$trading_date) * 100,
  2
)

summary_text <- c(
  
  "MATH5925/MATH5926 Week 1 Data Summary",
  "=====================================",
  
  paste(
    "Raw rows:",
    raw_rows
  ),
  
  paste(
    "Raw columns:",
    raw_cols
  ),
  
  paste(
    "Missing values:",
    missing_n
  ),
  
  paste(
    "Duplicate market rows:",
    duplicate_n
  ),
  
  paste(
    "Rows after duplicate removal:",
    nrow(dat)
  ),
  
  paste(
    "Backward time jumps in raw data:",
    backward_n
  ),
  
  paste(
    "Earliest date:",
    min(dat$trading_date)
  ),
  
  paste(
    "Latest date:",
    max(dat$trading_date)
  ),
  
  paste(
    "Trading days:",
    uniqueN(dat$trading_date)
  ),
  
  paste(
    "Invalid HIGH rows:",
    nrow(bad_high)
  ),
  
  paste(
    "Invalid LOW rows:",
    nrow(bad_low)
  ),
  
  paste(
    "VWAP outside HIGH-LOW:",
    nrow(bad_vwap)
  ),
  
  paste(
    "Complete 390-minute days:",
    length(complete_days)
  ),
  
  paste(
    "Complete 390-minute days (%):",
    paste0(complete_pct, "%")
  )
)

writeLines(
  summary_text,
  file.path(
    out_dir,
    "week1_summary.txt"
  )
)



# 18. Final message
cat("\n")
cat("Week 1 analysis completed.\n")
cat("Output folder:", out_dir, "\n")
cat("Complete 390-minute days:", length(complete_days), "\n")
cat("Complete-day percentage:", complete_pct, "%\n")
