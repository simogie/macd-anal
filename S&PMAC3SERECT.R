
#******* s&--S&P


# S&P 500のデータを使って、先ほどのR言語によるMACDヒストグラムの頂点検出
# （findpeaks法と平滑化＋微分法）を実行するコードを作成しました。
# 💡 S&P 500（指数）で扱う際のポイント
# 個別株（Appleなど）に比べると、S&P 500のような市場平均指数は
# カクカクとした突発的なスパイクが少なく、なだらかな波を描きやすいです。

# そのため、手法1の minpeakheight や、手法2のフィルタ窓幅 n
# （上記のコードでは n = 9 に拡張しています）を少し大きめに調整してあげると、
# だまし（偽のシグナル）を減らして「大局的なトレンドの天井と底」を綺麗に
# 近似できるようになります。
# 
# S&P 500のティッカーシンボルである ^GSPC を指定してデータを取得します。



# 1. 準備：S&P 500データの取得とMACD計算 **************************

# 必要なパッケージのロード

 # install.packages("signal")
 # install.packages("quantmod")
 # install.packages("pracma")

 
# 2. ライブラリの読み込み
library(quantmod)
library(pracma)
library(signal)

# 日本時間で統一する場合
Sys.setenv(TZ = "Asia/Tokyo")

# ========================================
# 前回のデータをクリア
# ========================================
rm(list = ls())  # グローバル環境の全変数を削除
gc()             # ガベージコレクション（メモリ解放）



# 指標よみこみ（直近１００日）

# === 指標選択メニュー ===
cat("========================================\n")
cat("指標を選択してください（番号を入力）\n")
cat("========================================\n")
cat("1 - 日経225\n")
cat("2 - TOPIX\n")
cat("3 - ダウ平均\n")
cat("4 - S&P500\n")
cat("5 - ナスダック\n")
cat("========================================\n")

# ユーザーに番号を入力させる
#(ここまで実行して選択後次へ進む。）

index_choice <- as.numeric(readline(prompt = "選択してください (1-5): "))




# 選択した指標に対応するシンボルと名前を設定
index_symbols <- list(
  list(symbol = "^N225", name = "日経225"),
  list(symbol = "^TOPIX", name = "TOPIX"),
  list(symbol = "^DJI", name = "ダウ平均"),
  list(symbol = "^GSPC", name = "S&P500"),
  list(symbol = "^IXIC", name = "ナスダック")
)

# 入力値のバリデーション
if (is.na(index_choice) || index_choice < 1 || index_choice > 5) {
  cat("エラー：1から5の数値を入力してください\n")
  index_choice <- 4  # デフォルトはS&P500
  cat("デフォルト(4 - S&P500)を使用します\n")
}

# 選択された指標の情報を取得
selected_index <- index_symbols[[index_choice]]
symbol <- selected_index$symbol
index_name <- selected_index$name

cat("\n選択した指標：", index_name, "\n")
cat("ティッカーシンボル：", symbol, "\n\n")

# 指標データの取得
today <- Sys.Date()
from_date <- today - 100

# getSymbols() で取得したデータを直接変数に格納
index_data <- getSymbols(symbol, from = format(from_date, "%Y-%m-%d"), to = format(today, "%Y-%m-%d"), auto.assign = FALSE)

# MACDの計算
# データのクリーニング（NA行を削除）
index_data <- index_data[!is.na(Cl(index_data)), ]

# MACDの計算
macd_data <- MACD(Cl(index_data), nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")

#macd_data <- MACD(Cl(index_data), nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")

# ヒストグラム（MACD - Signal）の抽出とNA除外
macd_hist <- as.numeric(na.omit(macd_data$macd - macd_data$signal))

# MACDのインデックス（日付）を抽出
macd_dates <- index(na.omit(macd_data))




# 2. 手法1：findpeaks による頂点検出して描画 *********************************

# S&P 500は個別株に比べて値動きがマイルドになる傾向があるため、
# 山の高さ（minpeakheight）の閾値を少し調整できるようにしておくと便利です。

# 山（ピーク）の検出
# 指標に応じて minpeakheight を動的に調整（最大値の10%を基準）
max_hist <- max(macd_hist)
min_hist <- min(macd_hist)
peak_threshold <- (max_hist - min_hist) * 0.1  # 範囲の10%を閾値とする

peaks <- findpeaks(macd_hist, minpeakheight = peak_threshold, minpeakdistance = 3)

# 谷（ボトム）の検出
troughs <- findpeaks(-macd_hist, minpeakheight = -peak_threshold, minpeakdistance = 3)




# --- ２のグラフ描画 ---**************************************

plot(macd_hist, type = "l", main = paste(index_name, "MACD Hist Peak Detection"),
     col = "darkgray", lwd = 2, xaxt = "n")

# 年始・月初・6月の位置を特定
year_labels_all  <- format(macd_dates, "%Y")
month_labels_all <- format(macd_dates, "%Y-%m")
month_num_all    <- as.integer(format(macd_dates, "%m"))

year_starts  <- which(!duplicated(year_labels_all))
month_starts <- which(!duplicated(month_labels_all))
june_starts  <- month_starts[month_num_all[month_starts] == 6]
other_months <- setdiff(month_starts, june_starts)

# 月初：灰色点線（6月以外）
abline(v = other_months, col = "gray", lty = 3, lwd = 1)
# 6月初：灰色実線
abline(v = june_starts, col = "gray", lty = 1, lwd = 1)
# 年始：緑実線（月初線の上に重ねて描画）
abline(v = year_starts, col = "green", lty = 1, lwd = 1)

# 年ラベル（年始位置）
axis(1, at = year_starts, labels = year_labels_all[year_starts], las = 2, cex.axis = 0.75)
# 月ラベル（年始以外の月初位置）
month_only_starts  <- setdiff(month_starts, year_starts)
month_only_labels  <- as.integer(format(macd_dates[month_only_starts], "%m"))
axis(1, at = month_only_starts, labels = month_only_labels,
     las = 2, cex.axis = 0.55, tick = FALSE, col.axis = "gray40")

abline(h = 0, col = "red", lty = 3) # ゼロライン（赤点線）

# 頂点をプロット
points(peaks[, 2], peaks[, 1], col = "red", pch = 19, cex = 1.5)
points(troughs[, 2], -troughs[, 1], col = "blue", pch = 19, cex = 1.5)






# 3. 手法2：Savitzky-Golayフィルタ（平滑化）＋微分法 *****************

# インデックス（指数）特有の細かな揉み合いノイズを綺麗に落として、
# 大きなトレンドの転換点を近似するのに向いています。

# Savitzky-Golay フィルタで平滑化
smoothed_hist <- sgolayfilt(macd_hist, p = 2, n = 9) # 窓幅を少し広めの9に調整

# 1階微分（前日との差分）
diff_hist <- diff(smoothed_hist)

# 傾きがプラスからマイナスに切り替わる点（山の頂点）
peak_indices <- which(diff_hist[-length(diff_hist)] > 0 & diff_hist[-1] < 0) + 1

# 傾きがマイナスからプラスに切り替わる点（谷の底）
trough_indices <- which(diff_hist[-length(diff_hist)] < 0 & diff_hist[-1] > 0) + 1




# --- 3のグラフ描画 ---**********************************

plot(macd_hist, type = "l", col = "lightgray", main = paste(index_name, "Smoothing & Differentiation"))
lines(smoothed_hist, col = "darkgreen", lwd = 2) # 平滑化曲線

# 近似された頂点をプロット
points(peak_indices, macd_hist[peak_indices], col = "darkred", pch = 4, lwd = 2, cex = 1.5)
points(trough_indices, macd_hist[trough_indices], col = "darkblue", pch = 4, lwd = 2, cex = 1.5)

n_total    <- length(macd_hist)

# 直近100日間に絞る
last_n    <- min(100, n_total)
idx_range <- (n_total - last_n + 1):n_total

hist_100   <- macd_hist[idx_range]
smooth_100 <- smoothed_hist[idx_range]
dates_100  <- macd_dates[idx_range]
x_pos      <- seq_along(hist_100)   # 1 〜 100 の整数インデックス

# ---- 縦線位置の計算 ----
day_of_month <- as.integer(format(dates_100, "%d"))

# 月初（1日）が取引日にない場合は月が切り替わる最初の取引日を使う
month_num   <- as.integer(format(dates_100, "%m"))
month_start_pos <- which(c(TRUE, diff(month_num) != 0))   # 月が変わる位置

# 10日・20日・30日に近い取引日（灰色点線）
ten_day_pos <- which(day_of_month %in% c(10, 20, 30))

# ---- 山・谷のうち直近100日内のものを抽出 ----
peak_in100   <- peak_indices[peak_indices %in% idx_range] - idx_range[1] + 1
trough_in100 <- trough_indices[trough_indices %in% idx_range] - idx_range[1] + 1

# 直近の山・谷（最後の1点）
latest_peak_x   <- if (length(peak_in100)   > 0) tail(peak_in100,   1) else NULL
latest_trough_x <- if (length(trough_in100) > 0) tail(trough_in100, 1) else NULL

# ---- 描画 ----
plot(x_pos, hist_100, type = "l", col = "lightgray",
     main = paste(index_name, "Smoothing & Differentiation（直近100日）"),
     xlab = "", ylab = "MACD Histogram",
     xaxt = "n")

# x軸：月初ラベル
axis(1, at = month_start_pos,
     labels = format(dates_100[month_start_pos], "%m/%d"),
     las = 2, cex.axis = 0.75)

# 月初：緑の実線
abline(v = month_start_pos, col = "green3", lwd = 1.5)

# 10日・20日・30日：灰色の点線
abline(v = ten_day_pos, col = "gray60", lty = 3, lwd = 1)

# ゼロライン：赤
abline(h = 0, col = "red", lwd = 1.5)

# 平滑化曲線
lines(x_pos, smooth_100, col = "darkgreen", lwd = 2)

# 山・谷のマーカー
if (length(peak_in100)   > 0)
  points(peak_in100,   hist_100[peak_in100],   col = "darkred",  pch = 4, lwd = 2, cex = 1.5)
if (length(trough_in100) > 0)
  points(trough_in100, hist_100[trough_in100], col = "darkblue", pch = 4, lwd = 2, cex = 1.5)

# 直近の山の日付ラベル
if (!is.null(latest_peak_x)) {
  text(latest_peak_x, hist_100[latest_peak_x],
       labels = format(dates_100[latest_peak_x], "%m/%d"),
       col = "darkred", pos = 3, cex = 0.85, font = 2)
}

# 直近の谷の日付ラベル
if (!is.null(latest_trough_x)) {
  text(latest_trough_x, hist_100[latest_trough_x],
       labels = format(dates_100[latest_trough_x], "%m/%d"),
       col = "darkblue", pos = 1, cex = 0.85, font = 2)
}

