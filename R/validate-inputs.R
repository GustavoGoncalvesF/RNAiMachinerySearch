# Internal function to do a validation in inputs

# @keywords internal

validate.inputs <- function(annotation_df = NULL,
                            column = NULL,
                            expression_df = NULL,
                            groups_df = NULL,
                            rnai_hits = NULL,
                            cpm_cut = NULL,
                            save = NULL,
                            path = NULL,
                            normalize = NULL) {

# Helper for erros prefix
err <- function(msg) stop(paste0("RNAiMachinerySearch Error: ", msg), call. = FALSE)

# ---- annotation_df ----
if (!is.null(annotation_df)) {
  if (!is.data.frame(annotation_df)) {
    err("'annotation_df' must be a data frame.")
  }
}

# ---- column ----
if (!is.null(column)) {
  if (!is.character(column) || length(column) != 1) {
    err("'column' must be a single character string.")
  }
  if (!is.null(annotation_df) && !(column %in% colnames(annotation_df))) {
    err(paste0("Column '", column, "' not found in 'annotation_df'."))
  }
  if (!is.null(annotation_df) && all(is.na(annotation_df[[column]]))) {
    err(paste0("Column '", column, "' contains only missing values."))
  }
}

# ---- expression_df ----
if (!is.null(expression_df)) {
  if (!is.data.frame(expression_df)) {
    err("'expression_df' must be a data frame.")
  }
  if (nrow(expression_df) == 0 || ncol(expression_df) == 0) {
    err("'expression_df' cannot be empty.")
  }
}

# ---- groups_df ----
if (!is.null(groups_df)) {
  if (!is.data.frame(groups_df)) {
    err("'groups_df' must be a data frame.")
  }
  required_cols <- c("SAMPLE", "REP")
  if (!all(required_cols %in% colnames(groups_df))) {
    err(paste0("'groups_df' must contain the columns: ", paste(required_cols, collapse = ", "), "."))
  }
}

# ---- rnai_hits ----
if (!is.null(rnai_hits)) {
  if (!is.data.frame(rnai_hits)) {
    err("'rnai_hits' must be a data frame.")
  }
  required_cols <- c("GeneID", "ProteinAnnotation", "Category")
  if (!all(required_cols %in% colnames(rnai_hits))) {
    err(paste0("'rnai_hits' must contain the columns: ", paste(required_cols, collapse = ", "), "."))
  }
}

# ---- CPM cutoffs ----
if (!is.null(cpm_cut)) {
  if (!is.numeric(cpm_cut) || any(cpm_cut < 0)) {
    err("'cpm_cut' must contain only non-negative numeric values.")
  }
}

# ---- save ----
if (!is.null(save)) {
  if (!is.logical(save) || any(is.na(save))) {
    err("'save' must contain only TRUE or FALSE values.")
  }
}

# ---- path ----
if (!is.null(path)) {
  if (!is.character(path)) {
    err("'path' must contain only character strings representing directory paths.")
  }
  invalid_paths <- path[!dir.exists(path)]
  if (length(invalid_paths) > 0) {
    err(paste0("The following directories do not exist: ", paste(invalid_paths, collapse = ", ")))
  }
}

# ---- normalize ----
if (!missing(normalize)) {
  valid_opts <- c("logCPM", "zscore", "none")
  if (is.null(normalize)) {
    err(paste0("'normalize' argument is required. Please choose one of: ", paste(valid_opts, collapse = ", "), "."))
  } else if (any(!normalize %in% valid_opts)) {
    err(paste0("'normalize' must contain only values among: ", paste(valid_opts, collapse = ", "), "."))
  }
}

# ---- success ----
invisible(TRUE)
}
