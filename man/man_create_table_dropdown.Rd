\\name{create_table_dropdown}
\\alias{create_table_dropdown}
\\alias{create_table_dropdown_v9}
\\title{在 HTML 输出中用下拉菜单切换多个 DataTable（v9 配置）}
\\description{
在 R Markdown/Quarto 的 HTML 输出中，用一个下拉菜单在多个 DataTable 间切换。每个表按 v9 配置创建：
\\code{extensions='Buttons'}, \\code{filter='top'}, \\code{style='bootstrap4'}, \\code{caption=表名}，
\\code{pageLength=10}, \\code{scrollX=TRUE}, \\code{scrollY='400px'}, \\code{dom='Bfrtip'},
\\code{buttons=c('copy','csv','excel','pdf')}。依赖由 \\pkg{DT}/htmlwidgets 管理，避免与 plotly 等冲突。
为避免隐藏状态初始化导致列宽为 0，前端先让所有表完成初始化，再只显示选中的表，并在切换后进行轻量列宽调整。
}
\\usage{
create_table_dropdown(dfs,
  style = "bootstrap4",
  filter = "top",
  pageLength = 10,
  scrollY = "400px",
  scrollX = TRUE,
  dom = "Bfrtip",
  buttons = c("copy","csv","excel","pdf"),
  dt_rownames = TRUE,
  use_rowname_col = FALSE,
  select_label = "Choose table:"
)

create_table_dropdown_v9(dfs,
  style = "bootstrap4",
  filter = "top",
  pageLength = 10,
  scrollY = "400px",
  scrollX = TRUE,
  dom = "Bfrtip",
  buttons = c("copy","csv","excel","pdf"),
  dt_rownames = TRUE,
  use_rowname_col = FALSE,
  select_label = "Choose table:"
)
}
\\arguments{
  \\item{dfs}{命名的 \\code{list}，每个元素是 \\code{data.frame}；list 的名称用于下拉选项与每个 DataTable 的 \\code{caption}。}
  \\item{style}{传给 \\code{DT::datatable} 的 \\code{style}（默认 \\code{"bootstrap4"}）。}
  \\item{filter}{传给 \\code{DT::datatable} 的 \\code{filter}（默认 \\code{"top"}）。}
  \\item{pageLength}{每页显示的行数（默认 10）。}
  \\item{scrollY}{表格纵向滚动高度（默认 \\code{"400px"}；设为 \\code{NULL} 可禁用竖向滚动）。}
  \\item{scrollX}{是否启用横向滚动（默认 \\code{TRUE}）。}
  \\item{dom}{DataTables 的 DOM 布局（默认 \\code{"Bfrtip"}）。}
  \\item{buttons}{Buttons 扩展按钮（默认 \\code{c("copy","csv","excel","pdf")}）。}
  \\item{dt_rownames}{是否在左侧显示 DT 行号列（默认 \\code{TRUE}）。若 \\code{use_rowname_col=TRUE}，为了避免重复会被自动置为 \\code{FALSE}。}
  \\item{use_rowname_col}{是否将 \\code{data.frame} 的行名作为第一列 \\code{Row} 插入（默认 \\code{FALSE}）。}
  \\item{select_label}{下拉菜单文字标签（默认 \\code{"Choose table:"}）。}
}
\\details{
- 依赖由 \\pkg{DT}/htmlwidgets 自动注入，不显式加载 jQuery，减少与 \\pkg{plotly} 等 htmlwidgets 的冲突。
- 为避免“隐藏状态初始化”导致列宽为 0，函数会在前端先让所有表处于可见状态完成一次初始化，随后只显示选中的表，并在切换后调用 \\code{columns.adjust().draw(FALSE)} 进行轻量列宽调整。
- 若不需要纵向滚动，调用时把 \\code{scrollY = NULL}。
}
\\value{
返回一个 \\pkg{htmltools} 可渲染对象，可直接在 Rmd/Quarto 的 HTML 输出中显示。
}
\\examples{
\\dontrun{
library(DT); library(htmltools)
dfs <- list(
  C1 = head(mtcars),
  C2 = head(iris)
)

# v9 默认参数（含竖向滚动）
create_table_dropdown(dfs)

# 不要竖向滚动，每页 10 行
create_table_dropdown(dfs, scrollY = NULL, pageLength = 10)

# 使用行名作为第一列，且不显示 DT 的行号列
create_table_dropdown(dfs, use_rowname_col = TRUE, dt_rownames = FALSE)
}
}
\\author{Anbunensi}
\\keyword{datatable}
\\keyword{htmlwidgets}