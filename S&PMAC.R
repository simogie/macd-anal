

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

# 1. 準備：S&P 500データの取得とMACD計算

# 必要なパッケージのロード

 # install.packages("signal")
 # install.packages("quantmod")
 # install.packages("pracma")
 
# 2. ライブラリの読み込み
library(quantmod)
library(pracma)
library(signal)

# S&P 500（^GSPC）のデータを取得
getSymbols("^GSPC", from = "2025-01-01", to = "2026-01-01", auto.assign = TRUE)

# MACDの計算
macd_data <- MACD(Cl(GSPC), nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")

# ヒストグラム（MACD - Signal）の抽出とNA除外
macd_hist <- as.numeric(na.omit(macd_data$macd - macd_data$signal))


# 2. 手法1：findpeaks による頂点検出
# S&P 500は個別株に比べて値動きがマイルドになる傾向があるため、
# 山の高さ（minpeakheight）の閾値を少し調整できるようにしておくと便利です。

# 山（ピーク）の検出
# S&P 500のヒストグラムのスケールに合わせて minpeakheight を調整（例: 0.5）
peaks <- findpeaks(macd_hist, minpeakheight = 0.5, minpeakdistance = 5)

# 谷（ボトム）の検出
troughs <- findpeaks(-macd_hist, minpeakheight = 0.5, minpeakdistance = 5)

# --- グラフ描画 ---
plot(macd_hist, type = "l", main = "S&P 500 MACD Hist Peak Detection", col = "darkgray", lwd = 2)
abline(h = 0, col = "gray", lty = 2) # ゼロライン

# 頂点をプロット
points(peaks[, 2], peaks[, 1], col = "red", pch = 19, cex = 1.5)
points(troughs[, 2], -troughs[, 1], col = "blue", pch = 19, cex = 1.5)


# 3. 手法2：Savitzky-Golayフィルタ（平滑化）＋微分法
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

# --- グラフ描画 ---
plot(macd_hist, type = "l", col = "lightgray", main = "S&P 500 Smoothing & Differentiation")
lines(smoothed_hist, col = "darkgreen", lwd = 2) # 平滑化曲線

# 近似された頂点をプロット
points(peak_indices, macd_hist[peak_indices], col = "darkred", pch = 4, lwd = 2, cex = 1.5)
points(trough_indices, macd_hist[trough_indices], col = "darkblue", pch = 4, lwd = 2, cex = 1.5)

