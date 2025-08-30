#' Word用の高品質画像テーブルを作成（修正版）
#' 
#' @param data データフレーム
#' @param filename 出力ファイル名（拡張子なし）
#' @param width 画像幅（ピクセル、デフォルト: 1400）
#' @param height 画像高さ（ピクセル、デフォルト: 800）
#' @param zoom ズーム倍率（デフォルト: 2）
#' @param col_names 列名のベクター（NULL の場合は元の列名を使用）
#' @param use_webshot2 webshot2を使用するか（FALSEの場合はwebshot使用）
#' @return ファイルパスを返す
#' @export
create_image_table <- function(data, filename = "table", width = 1400, height = 800, 
                              zoom = 2, col_names = NULL, use_webshot2 = TRUE) {
  
  if (is.null(col_names)) {
    col_names <- names(data)
  }
  
  # 高品質HTML表を作成
  html_table <- data %>%
    knitr::kable(
      format = "html", 
      escape = FALSE,
      table.attr = 'style="font-family: Arial, sans-serif; border-collapse: collapse; margin: 20px;"',
      col.names = col_names
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "condensed"),
      full_width = FALSE,
      font_size = 12
    ) %>%
    kableExtra::row_spec(0, bold = TRUE, background = "#343a40", color = "white") %>%
    kableExtra::add_footnote("***p<.001, **p<.01, *p<.05, .p<.1", notation = "none")
  
  # 一時HTMLファイルを作成
  temp_html <- tempfile(fileext = ".html")
  
  html_content <- paste0(
    "<html><head><style>
    body { margin: 40px; background: white; font-family: Arial, sans-serif; }
    table { border-collapse: collapse; margin: 0 auto; }
    th, td { padding: 8px 12px; border: 1px solid #ddd; text-align: left; }
    th { background-color: #343a40; color: white; font-weight: bold; }
    tr:nth-child(even) { background-color: #f8f9fa; }
    .footnote { margin-top: 15px; font-size: 0.9em; color: #666; }
    </style></head><body>",
    html_table,
    "</body></html>"
  )
  
  # HTMLファイルに書き込み（エラーハンドリング付き）
  tryCatch({
    writeLines(html_content, temp_html)
  }, error = function(e) {
    stop(paste("HTMLファイルの作成に失敗しました:", e$message))
  })
  
  # 出力ファイルパス
  output_file <- paste0(filename, ".png")
  
  # webshot2またはwebshotを使用して画像出力
  tryCatch({
    if (use_webshot2 && requireNamespace("webshot2", quietly = TRUE)) {
      # webshot2を使用
      webshot2::webshot(
        temp_html, 
        file = output_file,
        vwidth = width,
        vheight = height,
        zoom = zoom,
        delay = 1
      )
    } else if (requireNamespace("webshot", quietly = TRUE)) {
      # webshotにフォールバック
      message("webshot2が利用できないため、webshotを使用します")
      webshot::webshot(
        temp_html, 
        file = output_file,
        vwidth = width,
        vheight = height,
        zoom = zoom,
        delay = 1
      )
    } else {
      stop("webshotまたはwebshot2パッケージが必要です")
    }
  }, error = function(e) {
    # 一時ファイル削除
    if (file.exists(temp_html)) unlink(temp_html)
    
    # エラー詳細を表示
    cat("画像出力エラーの詳細:\n")
    cat("エラーメッセージ:", e$message, "\n")
    
    # 代替案を提示
    cat("\n=== 代替案 ===\n")
    cat("1. HTMLファイルが作成されました:", temp_html, "\n")
    cat("2. ブラウザで開いてスクリーンショットを取ることができます\n")
    cat("3. または以下のコマンドを試してください:\n")
    cat("   webshot::install_phantomjs()\n")
    cat("   webshot2::find_chrome()  # Chromeのパスを確認\n")
    
    # HTMLファイルのパスを返す
    return(temp_html)
  })
  
  # 成功時は一時ファイル削除
  if (file.exists(temp_html)) unlink(temp_html)
  
  if (file.exists(output_file)) {
    message(paste("画像ファイルを出力しました:", output_file))
    return(invisible(output_file))
  }
}

#' HTMLテーブルのみ作成（画像出力なし）
#' 
#' @param data データフレーム
#' @param filename 出力ファイル名（拡張子なし）
#' @param col_names 列名のベクター（NULL の場合は元の列名を使用）
#' @return HTMLファイルパスを返す
#' @export
create_html_table <- function(data, filename = "table", col_names = NULL) {
  
  if (is.null(col_names)) {
    col_names <- names(data)
  }
  
  # 高品質HTML表を作成
  html_table <- data %>%
    knitr::kable(
      format = "html", 
      escape = FALSE,
      col.names = col_names
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "condensed", "hover"),
      full_width = FALSE
    ) %>%
    kableExtra::row_spec(0, bold = TRUE, background = "#343a40", color = "white")
  
  # HTMLファイル作成
  output_file <- paste0(filename, ".html")
  
  html_content <- paste0(
    "<!DOCTYPE html>
    <html><head>
    <meta charset='utf-8'>
    <title>Research Table</title>
    <style>
    body { margin: 40px; background: white; font-family: Arial, sans-serif; }
    table { border-collapse: collapse; margin: 0 auto; }
    th, td { padding: 8px 12px; border: 1px solid #ddd; text-align: left; }
    th { background-color: #343a40; color: white; font-weight: bold; }
    tr:nth-child(even) { background-color: #f8f9fa; }
    </style></head><body>",
    html_table,
    "</body></html>"
  )
  
  writeLines(html_content, output_file)
  
  message(paste("HTMLファイルを出力しました:", output_file))
  message("ブラウザで開いてスクリーンショットを取ることができます")
  
  return(invisible(output_file))
}
