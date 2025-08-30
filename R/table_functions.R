#' PDF用のテーブルを作成
#'
#' @param data データフレーム
#' @param font_size フォントサイズ（デフォルト: 9）
#' @param col_names 列名のベクター（NULL の場合は元の列名を使用）
#' @return kableExtraオブジェクト
#' @export
create_pdf_table <- function(data, font_size = 9, col_names = NULL) {

  if (is.null(col_names)) {
    col_names <- names(data)
  }

  data %>%
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE,
      col.names = col_names
    ) %>%
    kableExtra::kable_styling(
      latex_options = c("repeat_header", "hold_position", "scale_down"),
      font_size = font_size
    ) %>%
    kableExtra::column_spec(1, bold = TRUE, width = "1.5cm") %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::footnote(
      general = "***p<.001, **p<.01, *p<.05, .p<.1",
      general_title = "注：",
      footnote_as_chunk = TRUE
    )
}

#' Word用の高品質画像テーブルを作成
#'
#' @param data データフレーム
#' @param filename 出力ファイル名（拡張子なし）
#' @param width 画像幅（ピクセル、デフォルト: 1400）
#' @param height 画像高さ（ピクセル、デフォルト: 800）
#' @param zoom ズーム倍率（デフォルト: 2）
#' @param col_names 列名のベクター（NULL の場合は元の列名を使用）
#' @return ファイルパスを返す
#' @export
create_image_table <- function(data, filename = "table", width = 1400, height = 800,
                               zoom = 2, col_names = NULL) {

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

  writeLines(
    paste0(
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
    ),
    temp_html
  )

  # 出力ファイルパス
  output_file <- paste0(filename, ".png")

  # 画像として出力
  webshot2::webshot(
    temp_html,
    file = output_file,
    vwidth = width,
    vheight = height,
    zoom = zoom,
    delay = 1
  )

  # 一時ファイル削除
  unlink(temp_html)

  message(paste("画像ファイルを出力しました:", output_file))
  return(invisible(output_file))
}

#' 研究仮説テーブル用のプリセット（日英対応）
#'
#' @param data データフレーム
#' @param filename 出力ファイル名
#' @param lang 言語（"ja" または "en"）
#' @export
create_hypothesis_table <- function(data, filename = "hypotheses", lang = "ja") {

  col_names <- if (lang == "ja") {
    c("仮説ID", "仮説", "モデル", "結果（日本）", "結果（タイ）", "メモ")
  } else {
    c("Hypothesis ID", "Hypothesis", "Model", "Results (Japan)", "Results (Thailand)", "Notes")
  }

  create_image_table(data, filename, col_names = col_names)
}
