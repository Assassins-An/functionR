# create_table_dropdown_rmd_simple.R
# Minimal Rmd-friendly table dropdown — adjusted so the dropdown looks like the one in
# create_plot_dropdown (same id and simple styling). Only the dropdown appearance/ID changed;
# other behavior is unchanged (no page title, no extra download-all or per-table download buttons).
#
# Usage in an Rmd (html_document):
# source("create_table_dropdown_rmd_simple.R")
# create_table_dropdown(dfs)
#
# dfs must be a named list of data.frames (if unnamed, names will be assigned).

create_table_dropdown <- function(dfs,
                                  pageLength = 10,
                                  scrollY = "400px",
                                  use_rowname_col = TRUE) {
  if (!is.list(dfs) || length(dfs) == 0) stop("dfs must be a non-empty named list of data.frames")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install package 'jsonlite'")
  if (!requireNamespace("htmltools", quietly = TRUE)) stop("Please install package 'htmltools'")
  # ensure names
  if (is.null(names(dfs)) || any(names(dfs) == "")) {
    names(dfs) <- paste0("Table_", seq_along(dfs))
  }

  # prepare JSON for each table
  json_list <- list()
  for (nm in names(dfs)) {
    df <- as.data.frame(dfs[[nm]], stringsAsFactors = FALSE, check.names = FALSE)
    rn <- rownames(df)
    if (use_rowname_col && !is.null(rn) && any(nzchar(rn))) {
      df <- cbind(Row = rn, df)
      rownames(df) <- NULL
    }
    json_list[[nm]] <- jsonlite::toJSON(df, dataframe = "rows", na = "null", auto_unbox = TRUE)
  }

  # head tags (CSS) -- minimal styling, dropdown appearance modeled after create_plot_dropdown
  head_tags <- htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/1.13.6/css/jquery.dataTables.min.css"),
    htmltools::tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/buttons/2.4.1/css/buttons.dataTables.min.css"),
    htmltools::tags$style(HTML("
      /* Minimal styling: only dropdown control, no title or extra buttons */
      body{font-family: Arial, Helvetica, sans-serif; margin: 20px;}
      #controls { margin-bottom: 8px; }
      .dt-container { display: none; margin-top: 6px; }
      .dt-container .table-title { display: none; } /* hide per-table title */
      /* dropdown appearance matched to create_plot_dropdown: larger padding and readable font */
      #select_plot {
        padding: 6px 8px;
        font-size: 14px;
        line-height: 1.2;
      }
      @media (max-width: 480px) {
        #select_plot { width: 100% !important; }
      }
    "))
  )

  # select options (use same id name as create_plot_dropdown: select_plot)
  select_opts <- lapply(names(dfs), function(nm) {
    htmltools::tags$option(value = nm, htmltools::htmlEscape(nm))
  })

  # table placeholders
  table_containers <- lapply(names(dfs), function(nm) {
    htmltools::tags$div(
      class = "dt-container", id = paste0("container__", nm),
      htmltools::tags$table(id = paste0("table__", nm), class = "display", style = "width:100%")
    )
  })

  # embed JSON into JS
  js_lines <- c("var TABLES = {};")
  for (nm in names(json_list)) {
    js_lines <- c(js_lines, paste0('TABLES["', nm, '"] = ', json_list[[nm]], ';'))
  }

  # JS init (no per-table download or global download)
  js_init <- sprintf('
    function initTable(name, data) {
      var tableId = "#table__" + name;
      var columns = [];
      if (data && data.length > 0) {
        var keys = Object.keys(data[0]);
        for (var i=0;i<keys.length;i++){
          columns.push({ title: keys[i], data: keys[i] });
        }
      }
      $(tableId).DataTable({
        data: data,
        columns: columns,
        dom: "Bfrtip",
        buttons: ["copy", "csv", "excel", "pdf", "print"],
        scrollX: true,
        scrollY: "%s",
        pageLength: %d,
        destroy: true
      });
    }

    document.addEventListener("DOMContentLoaded", function(){
      // initialize all DataTables (hidden by default)
      for (var nm in TABLES) {
        initTable(nm, TABLES[nm]);
      }
      // show first table
      var first = document.querySelector("#select_plot option").value;
      if (first) document.getElementById("container__" + first).style.display = "block";

      // dropdown change handler
      document.getElementById("select_plot").addEventListener("change", function(e){
        var v = e.target.value;
        var conts = document.getElementsByClassName("dt-container");
        for (var i=0;i<conts.length;i++) conts[i].style.display = "none";
        var sel = document.getElementById("container__" + v);
        if (sel) {
          sel.style.display = "block";
          try { document.querySelector("#table__" + v).DataTable().columns.adjust().draw(); } catch(err) {}
        }
      });
    });
  ', scrollY, pageLength)

  script_tags <- list(
    htmltools::tags$script(src = "https://code.jquery.com/jquery-3.7.1.min.js"),
    htmltools::tags$script(src = "https://cdn.datatables.net/1.13.6/js/jquery.dataTables.min.js"),
    htmltools::tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js"),
    htmltools::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js"),
    htmltools::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.2.7/pdfmake.min.js"),
    htmltools::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.2.7/vfs_fonts.js"),
    htmltools::tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.html5.min.js"),
    htmltools::tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.print.min.js")
  )

  data_script <- htmltools::tags$script(HTML(paste(c(js_lines, js_init), collapse = "\n")))

  # assemble tagList (no title, only dropdown + tables), using select_plot id
  html_out <- htmltools::tagList(
    head_tags,
    htmltools::tags$div(
      id = "controls",
      # label text kept minimal; dropdown id matches create_plot_dropdown
      htmltools::tags$label("Choose table: ", `for` = "select_plot"),
      htmltools::tags$select(id = "select_plot", select_opts)
    ),
    htmltools::tags$div(id = "tables_wrap", table_containers),
    script_tags,
    data_script
  )

  htmltools::browsable(html_out)
}