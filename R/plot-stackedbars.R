# This function plots a stacked bar chart to represent the expression by category within each sample group

library(plotly)
library(openxlsx)

stackedbars.plot <- function(rnai_hits, expression_df, groups_df, save_table = FALSE, table_path = NULL) {
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
  agg_df <- aggregate(. ~ Category, data = grouped_expr, sum)

  # Normalize by log
  #agg_df[,-1] <- log1p(agg_df[,-1])

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
  p <- plotly::plot_ly(
    agg_long,
    x = ~Group,
    y = ~Expression,
    color = ~Category,
    colors = category_colors,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "Experimental groups"),
      yaxis = list(title = "Summed expression"),
      legend = list(title = list(text = "RNAi categories"))
    )

  # Export category x summeds table
  if (isTRUE(save_table)) {
    if (is.null(table_path)) {
      table_path <- file.path(getwd(), "categories-vs-sum.xlsx")
    }
    openxlsx::write.xlsx(agg_df, table_path, rowNames = FALSE)
    message("Table saved in: ", table_path)
  }
  return(p)
}
