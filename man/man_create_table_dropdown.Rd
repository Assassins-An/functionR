\name{create_table_dropdown}
\alias{create_table_dropdown}
\title{在 R Markdown (HTML 输出) 中用下拉菜单切换多个 DataTable}
\description{
在 R Markdown 的 HTML 输出中嵌入多个使用 DataTables 渲染的表格，并提供一个简单的下拉菜单用于切换要显示的表格。此函数不依赖 Shiny，生成一个 htmltools 可渲染的片段，可直接在 Rmd 的 chunk 中调用。
}
\usage{
create_table_dropdown(dfs,
                      pageLength = 10,
                      scrollY = "400px",
                      use_rowname_col = TRUE)
}
\arguments{
  \item{dfs}{一个命名的 \code{list}，每个元素是一个 \code{data.frame}（若未命名，函数会自动命名为 \code{Table_1}, \code{Table_2}, ...）。}
  \item{pageLength}{每页默认显示的行数（传给 DataTables 的 \code{pageLength}，默认 10）。}
  \item{scrollY}{表格垂直滚动区域高度（传给 DataTables 的 \code{scrollY}，例如 \code{"400px"}）。}
  \item{use_rowname_col}{逻辑值，是否把每个 data.frame 的行名（若存在且非空）作为第一列插入表格（默认 \code{TRUE}）。}
}
\details{
该函数将每个传入的 \code{data.frame} 转换为 JSON 并嵌入到返回的 HTML 片段中，然后使用 CDN 上的 jQuery 与 DataTables 库在浏览器中渲染交互式表格。页面内包含一个下拉菜单（元素 id 为 \code{select_plot}），用户通过下拉选择不同表格，函数会在页面中显示对应的 DataTable 并隐藏其他表格。

本函数仅生成在 HTML 输出环境中可用的前端代码（依赖浏览器），不适用于非 HTML 输出（如 PDF / Word）。若数据量非常大（数万行），客户端渲染会较慢，建议在 R 端先筛选或改为服务器端分页（Shiny）。
}
\value{
返回一个 \pkg{htmltools} 可渲染对象（\code{htmltools::browsable} tagList），在 R Markdown 的 HTML 输出中直接显示交互式下拉表格控件。
}
\note{
函数内部使用 \pkg{jsonlite} 将数据框编码为 JSON，并使用 \pkg{htmltools} 生成 HTML 片段。确保在运行环境中已安装这两个包（\code{install.packages("jsonlite"); install.packages("htmltools")}）。函数使用 DataTables/Buttons 的 CDN 链接；若希望离线使用，请将这些资源替换为本地静态文件并调整生成的 HTML。
}
\examples{
## 在 R Markdown 的 chunk 中示例用法
## (在 setup chunk 中先 source 本函数定义，或将函数直接放入 Rmd)
library(htmltools)  # 用于渲染（Rmd 在 knit 时会自动渲染）
dfs <- list(
  Cars = head(mtcars),
  Iris = head(iris)
)
# 在 Rmd 的 chunk 中执行：
create_table_dropdown(dfs)

## 如需调整每页显示行数或滚动高度：
create_table_dropdown(dfs, pageLength = 5, scrollY = "300px", use_rowname_col = TRUE)
}
\author{函数作者: 由 Copilot 协助生成 文档: Anbunensi}
\keyword{html interface datatables dropdown}