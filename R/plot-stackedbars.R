#' @title Plot a stacked bar chart of RNAi gene expression by category
#'
#' @description
#' This function generates a stacked bar chart representing the summed expression
#' of RNAi machinery genes across experimental groups. Optionally, it can save
#' the chart as an interactive HTML file and export a summary table of counts by category.
#'
#' @param rnai_hits A data frame containing RNAi hits, typically obtained from
#'   \code{\link{search.rnai}} or \code{\link{expr.filter}}. Must contain the columns
#'   "GeneID", "ProteinAnnotation", "Category", and "Function".
#' @param expression_df A data frame of raw counts with contigs as rows and sample names as columns.
#' @param groups_df A data frame specifying sample grouping. Must contain the columns "SAMPLE" and "REP".
#' @param save_table Logical; if \code{TRUE}, a summary table (.xlsx) of counts by category will be saved.
#'   Default is \code{FALSE}.
#' @param table_path Character string specifying the path to save the summary table.
#'   If \code{NULL}, the table will be saved in the current working directory with
#'   the default name "categories-vs-sum.xlsx".
#' @param save_plot Logical; if \code{TRUE}, the stacked bar chart will be saved as an HTML file.
#'   Default is \code{FALSE}.
#' @param plot_path Character string specifying the path to save the HTML plot.
#'   If \code{NULL}, the plot will be saved in the current working directory with
#'   the default name "stackedbars_plot.html".
#'
#' @return A \code{plotly} object of the stacked bar chart.
#'
#' @examples
#' \dontrun{
#' # Generate a stacked bar chart for filtered RNAi hits:
#' raw_rnai_hits <- search.rnai(annotation_df = annotation, column = "Top_BLASTX_hit")
#' expression_df <- read.table("raw_counts.txt")
#' groups_df <- read.table("samples.txt")
#'
#' filtered_rnai_hits <- expr.filter(raw_rnai_hits, expression_df, groups_df)
#' stackedbars_plot <- stackedbars.plot(filtered_rnai_hits, expression_df, groups_df)
#' }
#'
#' @importFrom plotly plot_ly layout %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom htmlwidgets saveWidget
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom stats setNames
#' @importFrom dplyr group_by summarise across ungroup where
#'
#' @export

stackedbars.plot <- function(rnai_hits, expression_df, groups_df, save_table = FALSE, table_path = NULL, save_plot = FALSE, plot_path = NULL) {
  # Input validation
  validate.inputs(rnai_hits = rnai_hits,
                  expression_df = expression_df,
                  groups_df = groups_df,
                  save = c(save_table, save_plot),
                  path = c(table_path, plot_path)
  )

  # Catalog of colors by category
  category_colors <- read.csv(system.file("extdata", "category_colors.txt", package = "RNAiMachinerySearch"), stringsAsFactors = FALSE)
  category_colors <- setNames(category_colors$Color, category_colors$Category)

  # Filtering expression matrix to keep only hits from filtered
  expr_sub <- expression_df[rownames(expression_df) %in% rnai_hits$GeneID, , drop = FALSE]

  # Mapping GeneID by category
  gene_categories <- rnai_hits$Category
  names(gene_categories) <- rnai_hits$GeneID

  # Add category column in expression data
  expr_sub$Category <- gene_categories[rownames(expr_sub)]

  # Merge replicates expression by sample group
  grouped_expr <- sapply(unique(groups_df$SAMPLE), function(g) {
    # Select columns that exactly match the sample/group name
    cols_to_sum <- grep(paste0("^", g), colnames(expr_sub), value = TRUE)
    if (length(cols_to_sum) == 0) stop(paste("No columns found for group", g))
    rowSums(expr_sub[, cols_to_sum, drop = FALSE])
  })

  # Convert to data frame
  grouped_expr <- as.data.frame(grouped_expr)

  # Add category column in groups
  grouped_expr$Category <- expr_sub$Category

  # Merge by category (add all contigs by categories)
  Category <- NULL
  agg_df <- grouped_expr %>%
    dplyr::group_by(Category) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, .names = "{.col}")) %>%
    dplyr::ungroup()

  # Adapt data to long format input
  agg_long <- tidyr::pivot_longer(
    agg_df,
    cols = -Category,
    names_to = "Group",
    values_to = "Expression"
  )

  # Transform Group in factor to plot in correct order
  group_order <- unique(groups_df$SAMPLE)
  agg_long$Group <- factor(agg_long$Group, levels = group_order)

  # Ploting with plotly
  plot <- plotly::plot_ly(
    agg_long,
    x = ~.data$Group,
    y = ~.data$Expression,
    color = ~.data$Category,
    colors = category_colors,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "Experimental groups"),
      yaxis = list(title = "Summed expression"),
      legend = list(title = list(text = "RNAi categories"))
    )

  # Export category x summed table
  if (isTRUE(save_table)) {
    if (is.null(table_path)) {
      table_path <- file.path(getwd(), "categories-vs-sum.xlsx")
    }
    openxlsx::write.xlsx(agg_df, table_path, rowNames = FALSE)
    message("Table saved in: ", table_path)
  }
  # Export plot
  if (isTRUE(save_plot)) {
    if (is.null(plot_path)) {
      plot_path <- file.path(getwd(), "stackedbars_plot.html")
    }
    htmlwidgets::saveWidget(plot, plot_path)
    message("Stacked bards plot saved in: ", plot_path)
  }

  return(plot)
}
