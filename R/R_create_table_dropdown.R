#' Create a dropdown to switch among multiple DataTables in HTML output
#'
#' 在 R Markdown/Quarto 的 HTML 输出中，用一个下拉菜单在多个 DataTable 间切换。
#' - 使用 DT/htmlwidgets 管理依赖（不重复注入 jQuery），减少与 plotly 等冲突。
#' - 每个表按你提供的 v9 参数创建（Buttons、filter='top'、style='bootstrap4'、caption=表名、pageLength=10、scrollX=TRUE、scrollY='400px'、dom='Bfrtip'、buttons=c('copy','csv','excel','pdf')）。
#' - 为避免隐藏状态初始化的列宽为 0，前端先让所有表完成初始化，再只显示选中的表，并在切换后进行轻量 columns.adjust()。
#'
#' @param dfs 一个命名的 list，每个元素是 data.frame；list 的名称将用于下拉选项和 DataTable 的 caption。
#' @param style 传给 DT::datatable 的 style（默认 "bootstrap4"）。
#' @param filter 传给 DT::datatable 的 filter（默认 "top"）。
#' @param pageLength 每页显示行数（默认 10）。
#' @param scrollY 传给 DataTables 的纵向滚动高度（默认 "400px"；设为 NULL 可禁用竖向滚动）。
#' @param scrollX 是否启用横向滚动（默认 TRUE）。
#' @param dom DataTables 的 dom 布局字符串（默认 "Bfrtip"）。
#' @param buttons Buttons 扩展的按钮列表（默认 c("copy","csv","excel","pdf")）。
#' @param dt_rownames 是否在 DT 左侧显示行号列（默认 TRUE）。注意：若 \code{use_rowname_col=TRUE}，将自动把 \code{dt_rownames} 置为 FALSE 以避免重复。
#' @param use_rowname_col 是否把 data.frame 的行名作为第一列 "Row" 插入（默认 FALSE）。
#' @param select_label 下拉菜单前的标签文本（默认 "Choose table:"）。
#'
#' @return 一个 htmltools 可渲染对象，可直接在 Rmd/Quarto HTML 输出中显示。
#' @examples
#' \dontrun{
#' library(DT); library(htmltools)
#' dfs <- list(
#'   C1 = head(mtcars),
#'   C2 = head(iris)
#' )
#' # v9 默认（含竖向滚动）
#' create_table_dropdown(dfs)
#'
#' # 不要竖向滚动，一页 10 行
#' create_table_dropdown(dfs, scrollY = NULL, pageLength = 10)
#' }
#' @export
create_table_dropdown <- function(
  dfs,
  style           = "bootstrap4",
  filter          = "top",
  pageLength      = 10,
  scrollY         = "400px",          # 设为 NULL 可去掉竖向滚动
  scrollX         = TRUE,
  dom             = "Bfrtip",
  buttons         = c("copy","csv","excel","pdf"),
  dt_rownames     = TRUE,
  use_rowname_col = FALSE,
  select_label    = "Choose table:"
) {
  if (!requireNamespace("DT", quietly = TRUE)) stop("Please install DT: install.packages('DT')")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("Please install htmltools: install.packages('htmltools')")
  if (!is.list(dfs) || length(dfs) == 0) stop("dfs must be a non-empty named list of data.frames")
  if (is.null(names(dfs)) || any(names(dfs) == "")) names(dfs) <- paste0("Table_", seq_along(dfs))

  # 安全 ID，避免选择器冲突；保留原标签用于显示
  sanitize_id <- function(x) gsub("[^A-Za-z0-9_-]", "_", as.character(x))
  labels <- names(dfs)
  ids_raw <- sanitize_id(labels)
  ids     <- make.unique(ids_raw, sep = "_")
  names(dfs) <- ids

  # 构建 DT widgets（保留 htmlwidgets 依赖）
  widgets <- lapply(seq_along(ids), function(i) {
    id <- ids[i]; label <- labels[i]
    df <- as.data.frame(dfs[[id]], stringsAsFactors = FALSE, check.names = FALSE)
    # 处理 Row 列与 DT 行号列的关系：如果使用 Row，则禁用 DT 的行号列
    this_dt_rownames <- isTRUE(dt_rownames)
    rn <- rownames(df)
    if (isTRUE(use_rowname_col) && !is.null(rn) && any(nzchar(rn))) {
      df <- cbind(Row = rn, df)
      rownames(df) <- NULL
      this_dt_rownames <- FALSE
    }

    opts <- list(
      pageLength   = pageLength,
      scrollX      = isTRUE(scrollX),
      dom          = dom,
      buttons      = as.list(buttons),
      autoWidth    = TRUE,
      deferRender  = TRUE
    )
    if (!is.null(scrollY)) opts$scrollY <- scrollY

    DT::datatable(
      df,
      extensions = "Buttons",
      filter     = filter,
      style      = style,
      caption    = label,
      options    = opts,
      rownames   = this_dt_rownames,
      elementId  = paste0("dt_", id)
    ) |>
      htmltools::as.tags()
  })
  names(widgets) <- ids

  root_id <- paste0("ctd_v9_", as.integer(Sys.time()), "_", sample(10000, 1))

  # 下拉选项：value 用安全 id，显示原始 label
  select_opts <- lapply(seq_along(ids), function(i) {
    htmltools::tags$option(value = ids[i], htmltools::htmlEscape(labels[i]))
  })

  # 容器：初始全部可见（init-visible），完成初始化后只保留第一个为 active
  table_divs <- lapply(ids, function(id) {
    htmltools::tags$div(
      id = paste0("container__", id),
      class = "ctd-dt-container init-visible",
      widgets[[id]]
    )
  })

  # 作用域 CSS（避免额外空白；表格尽量占满宽度）
  css <- paste0(
    "#", root_id, " { margin: 6px 0; }",
    "#", root_id, " #controls { margin-bottom: 8px; }",
    "#", root_id, " #tables_wrap { margin-top: 8px; }",
    "#", root_id, " #select_plot { min-width: 420px; padding: 6px 8px; font-size: 15px; }",
    "#", root_id, " .ctd-dt-container { display: none; }",
    "#", root_id, " .ctd-dt-container.init-visible { display: block; }",
    "#", root_id, " .ctd-dt-container.active { display: block; }",
    "#", root_id, " .html-widget, #", root_id, " .datatables, #", root_id, " .dataTables_wrapper { margin: 0 !important; padding: 0 !important; width: 100% !important; }",
    "#", root_id, " table { width: 100% !important; }",
    "@media (max-width: 480px) { #", root_id, " #select_plot { width: 100% !important; } }"
  )

  # 初始化后隐藏非选中的；切换时轻量调整列宽，避免空白
  js <- paste0(
    "(function(){",
    "var root=document.getElementById('", root_id, "');",
    "if(!root) return;",
    "var sel=root.querySelector('#select_plot');",
    "function adjustActive(){",
    " var active=root.querySelector('.ctd-dt-container.active'); if(!active) return;",
    " try{ if(window.HTMLWidgets&&HTMLWidgets.find){ HTMLWidgets.find(active).forEach(function(w){ if(w&&w.resize) w.resize(); }); } }catch(e){}",
    " var tbl=active.querySelector('table.dataTable, table.display, table');",
    " if(tbl && window.jQuery && window.jQuery.fn && window.jQuery.fn.dataTable){",
    "   try{ if(window.jQuery.fn.dataTable.isDataTable(tbl)){ window.jQuery(tbl).DataTable().columns.adjust().draw(false);} }catch(e){}",
    " }",
    " try{ window.dispatchEvent(new Event('resize')); }catch(e){}",
    "}",
    "function activate(id){",
    " var all=root.querySelectorAll('.ctd-dt-container');",
    " for(var i=0;i<all.length;i++) all[i].classList.remove('active');",
    " var cont=root.querySelector('#container__'+id); if(cont) cont.classList.add('active');",
    " setTimeout(adjustActive,20); setTimeout(adjustActive,120);",
    "}",
    "function initialHide(){",
    " var all=root.querySelectorAll('.ctd-dt-container');",
    " var firstId=(sel&&sel.options&&sel.options.length>0)?sel.options[0].value:null;",
    " for(var i=0;i<all.length;i++) all[i].classList.remove('init-visible');",
    " if(firstId){ activate(firstId);} else if(all.length>0){ all[0].classList.add('active'); setTimeout(adjustActive,80);} ",
    "}",
    "setTimeout(initialHide,120);",
    "if(sel) sel.addEventListener('change', function(e){ activate(e.target.value); });",
    "})();"
  )

  htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(css)),
      htmltools::tags$div(
        id = root_id,
        htmltools::tags$div(
          id = "controls",
          htmltools::tags$label(paste0(select_label, " "), `for` = "select_plot"),
          htmltools::tags$select(id = "select_plot", select_opts)
        ),
        htmltools::tags$div(id = "tables_wrap", table_divs)
      ),
      htmltools::tags$script(htmltools::HTML(js))
    )
  )
}

#' @export
#' @rdname create_table_dropdown
create_table_dropdown_v9 <- create_table_dropdown