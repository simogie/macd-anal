# ============================================================
# MACD分析 - 日経225
# macdGRAPH.qmd の通常 R スクリプト版
# ============================================================


# ── 1. パッケージ・データ読み込み ──────────────────────────

library(TTR)
library(kableExtra)

Sys.setenv(LANGUAGE = "ja")

# df <- read.csv("D:/R/workspace/n225E.csv")

# このようにGitHub上のRaw URLに書き換えます
read.csv("https://raw.githubusercontent.com/simogie/macd-anal/refs/heads/main/n225E.csv")

colnames(df) <- c("date", "close")
df$date  <- as.Date(df$date)
df$close <- as.numeric(df$close)


# ── 2. MACD計算 ───────────────────────────────────────────
# MACDは短期EMA（12日）と長期EMA（26日）の差。
# シグナルはMACDの9日EMA。
# ヒストグラム = MACD − シグナル。

macd      <- MACD(df$close, nFast = 12, nSlow = 26, nSig = 9)
df$macd   <- macd[, 1]
df$signal <- macd[, 2]
df$hist   <- df$macd - df$signal

df <- na.omit(df)

# 2025年以降に絞る
df <- df[df$date >= as.Date("2025-01-01"), ]


# ── 3. スプライン平滑化と微分（全期間） ───────────────────

x   <- 1:nrow(df)
fit <- smooth.spline(x, df$hist, spar = 0.6)  # スプライン平滑化
d1  <- predict(fit, x, deriv = 1)$y           # 1階微分（変化速度）
d2  <- predict(fit, x, deriv = 2)$y           # 2階微分（変化加速度）

# 月ラインと月番号ラベルを描画するヘルパー（1月のみ強調色）
add_month_lines <- function(dates, jan_col = "green", other_col = "lightgray") {
  ms <- unique(as.Date(format(dates, "%Y-%m-01")))
  m  <- as.integer(format(ms, "%m"))
  abline(v = as.numeric(ms[m != 1]), col = other_col, lty = 1)
  abline(v = as.numeric(ms[m == 1]), col = jan_col,   lty = 1, lwd = 1.5)
  # 毎月15日に点線
  ms15 <- as.Date(format(ms, "%Y-%m-15"))
  abline(v = as.numeric(ms15), col = "lightgray", lty = 3)
  text(x = as.numeric(ms), y = par("usr")[3],
       labels = m, col = ifelse(m == 1, jan_col, "black"),
       xpd = TRUE, cex = 0.7, adj = c(0.5, 1.5))
}


# ── 4. グラフ（全期間） ────────────────────────────────────

# 終値
plot(df$date, df$close, type = "l", main = "Close Price",
     xlab = "日付", ylab = "終値")
add_month_lines(df$date)

# MACDヒストグラム＋スプライン
plot(df$date, df$hist, type = "l", main = "MACD Histogram",
     xlab = "日付", ylab = "MACD Histogram")
lines(df$date, predict(fit)$y, col = "red", lwd = 2)
add_month_lines(df$date)
abline(h = 0, col = "darkgoldenrod", lty = 1)
legend("topright", legend = c("Histogram", "Spline"), col = c("black", "red"), lty = 1,
       cex = 0.7, bty = "n")

# 1階微分（変化の速さ）
plot(df$date, d1, type = "l", main = "1階: MACDヒストグラムのスプライン曲線の変化速度",
     xlab = "日付", ylab = "d1")
add_month_lines(df$date)
abline(h = 0, col = "gray", lty = 2)

# 2階微分（変化の加速度）
plot(df$date, d2, type = "l", main = "2階: 変化の変化率",
     xlab = "日付", ylab = "d2")
add_month_lines(df$date)
abline(h = 0, col = "gray", lty = 2)


# ── 5. 微分グラフの読み方（メモ） ─────────────────────────
#
# | 記号    | 名称     | 意味                                           |
# |---------|----------|------------------------------------------------|
# | f(x)    | 元のグラフ | 現在の値（価格など）そのもの                 |
# | f'(x)   | 1階微分  | 変化の勢い（速度）。右上がり=プラス            |
# | f''(x)  | 2階微分  | 変化の勢いの変化（加速度）                     |
#
# 2階微分 > 0 → 下に凸（U字型）→ 勢いが増している
# 2階微分 < 0 → 上に凸（山型） → 勢いが衰えている（ピークアウトが近い）
# 2階微分 = 0 → 変曲点          → 加速↔減速の切り替わり


# ── 6. 直近N日の分析（2026/5/16追加） ─────────────────────
# 期間を変えたい場合はここだけ変更する

N <- 100

df_recent  <- tail(df, N)
x_recent   <- 1:nrow(df_recent)

# スプラインも直近N日で再計算
fit_recent <- smooth.spline(x_recent, df_recent$hist, spar = 0.6)
d1_recent  <- predict(fit_recent, x_recent, deriv = 1)$y
d2_recent  <- predict(fit_recent, x_recent, deriv = 2)$y

# 終値（直近N日）
plot(df_recent$date, df_recent$close, type = "l",
     main = paste0("終値（直近 ", N, " 日）"),
     xlab = "日付", ylab = "終値")
add_month_lines(df_recent$date)

# MACDヒストグラム＋スプライン（直近N日）
plot(df_recent$date, df_recent$hist, type = "l",
     main = paste0("MACDヒストグラム（直近 ", N, " 日）"),
     xlab = "日付", ylab = "MACD Histogram")
lines(df_recent$date, predict(fit_recent)$y, col = "red", lwd = 2)
abline(h = 0, col = "darkgoldenrod", lty = 2)
add_month_lines(df_recent$date)
legend("topleft", legend = c("Histogram", "Spline"), col = c("black", "red"), lty = 1, cex = 0.7)

# 1階微分（直近N日）
plot(df_recent$date, d1_recent, type = "l",
     main = paste0("1階微分（直近 ", N, " 日）"),
     xlab = "日付", ylab = "d1")
add_month_lines(df_recent$date)
abline(h = 0, col = "gray", lty = 2)

# 2階微分（直近N日）
plot(df_recent$date, d2_recent, type = "l",
     main = paste0("2階微分（直近 ", N, " 日）"),
     xlab = "日付", ylab = "d2")
add_month_lines(df_recent$date)
abline(h = 0, col = "gray", lty = 2)


# ── 7. 頂点・谷底マーカー（直近N日） ──────────────────────
# d1のゼロクロス（符号が変わるインデックスを検出）
sign_changes <- which(diff(sign(d1_recent)) != 0)

# 2階微分の符号で山・谷を分類
peak_idx   <- sign_changes[d2_recent[sign_changes] < 0]  # 山
valley_idx <- sign_changes[d2_recent[sign_changes] > 0]  # 谷

# スプライン＋マーカーのプロット
plot(df_recent$date, df_recent$hist, type = "l",
     main = paste0("MACDヒストグラム 頂点・谷底（直近 ", N, " 日）"),
     xlab = "日付", ylab = "MACD Histogram")
lines(df_recent$date, predict(fit_recent)$y, col = "red", lwd = 2)
points(df_recent$date[peak_idx],   predict(fit_recent)$y[peak_idx],
       pch = 25, col = "blue",   bg = "blue",   cex = 1.2)  # 山 下矢印
points(df_recent$date[valley_idx], predict(fit_recent)$y[valley_idx],
       pch = 24, col = "orange", bg = "orange", cex = 1.2)  # 谷 上矢印
add_month_lines(df_recent$date)
abline(h = 0, col = "darkgoldenrod", lty = 1)
legend("topleft",
       legend = c("Histogram", "Spline", "山（頂点）", "谷（底）"),
       col    = c("black", "red", "blue", "orange"),
       lty    = c(1, 1, NA, NA),
       pch    = c(NA, NA, 25, 24),
       pt.bg  = c(NA, NA, "blue", "orange"),
       cex = 0.7, bty = "n")


# ── 8. 直近マーカー表示 ────────────────────────────────────
# ※ kableExtra はHTMLレンダリング環境（RStudio Viewer / Quarto）で色付き表示される

spline_y   <- predict(fit_recent)$y
marker_idx <- sort(c(peak_idx, valley_idx))

if (length(marker_idx) > 0) {
  latest_idx <- tail(marker_idx, 1)

  type <- if (latest_idx %in% peak_idx) "山（頂点）" else "谷（底）"

  latest_marker <- data.frame(
    種別         = type,
    日付         = format(df_recent$date[latest_idx], "%Y-%m-%d"),
    スプライン値 = round(spline_y[latest_idx], 3)
  )

  bg_col  <- if (type == "山（頂点）") "#cce5ff" else "#ffe6cc"
  txt_col <- if (type == "山（頂点）") "#0066cc" else "#ff8800"

  print(
    knitr::kable(latest_marker, align = c("c", "c", "r"), row.names = FALSE) |>
      kableExtra::kable_styling(full_width = FALSE) |>
      kableExtra::row_spec(1, background = bg_col, color = txt_col)
  )
} else {
  message("マーカーが検出されませんでした")
}


# ── 9. 全期間マーカー一覧表 ───────────────────────────────

種別_vec <- rep("", length(df_recent$date))
種別_vec[peak_idx]   <- "▲ 山（頂点）"
種別_vec[valley_idx] <- "▼ 谷（底）"

delta <- c(NA, diff(spline_y))

result_tbl <- data.frame(
  種別         = 種別_vec,
  日付         = df_recent$date,
  スプライン値 = round(spline_y, 3),
  前日差       = round(delta, 3)
)

blue_rows <- which(!is.na(delta) & delta < 0)
red_rows  <- which(!is.na(delta) & delta > 0)

print(
  knitr::kable(result_tbl, align = c("c", "c", "r")) |>
    kableExtra::kable_styling(full_width = FALSE) |>
    kableExtra::row_spec(blue_rows, background = "#d0e8ff", color = "#003399") |>
    kableExtra::row_spec(red_rows,  background = "#ffe0e0", color = "#cc0000")
)


# ── 補足 ──────────────────────────────────────────────────
# - N <- 200 のように変数を変えるだけで期間を変更できる
# - 直近N日は「営業日ベース」のため、土日祝は自動的に除外される
# - スプラインの spar は期間が短いほど影響が大きいため、適宜調整する
#
# 今後追加できる機能：
# - MACDのゴールデンクロス／デッドクロス検出
# - 微分のゼロクロス点の自動抽出とグラフへのマーカー表示
# - 売買シグナルの自動生成
# - ggplot2 での高品質グラフ化
