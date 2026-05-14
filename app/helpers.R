# app/helpers.R

# Normalize coverage data for a group
normalize_coverage <- function(df) {
  original_names <- colnames(df)

  # Convert to numeric matrix with decimal fix
  mat <- as.matrix(df)
  mode(mat) <- "character"
  mat <- matrix(as.numeric(gsub(",", ".", mat)), nrow = nrow(df))

  # Normalize by column median
  col_medians <- apply(mat, 2, median, na.rm = TRUE)
  norm <- sweep(mat, 2, col_medians, "/")

  # Center by row means
  row_means <- rowMeans(norm, na.rm = TRUE)
  centered <- sweep(norm, 1, row_means, "-")

  colnames(centered) <- original_names

  return(as.data.frame(centered))
}

# Load OMIM reference file safely
# load_omim_file <- function(path = "../reference/omim-phenptype-2024-upr-sl67.txt") { # nolint
#   if (!file.exists(path)) return(NULL)

#   lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
#   parsed <- strsplit(lines, "\t")

#   clean_df <- lapply(parsed, function(parts) {
#     gene <- parts[1]
#     phenotypes <- paste(Filter(function(x) x != "" && x != " ", parts[-1]), collapse = "; ")

#     return(data.frame(gene = gene, phenotyp = phenotypes, stringsAsFactors = FALSE)) # nolint
#   })

#   df <- do.call(rbind, clean_df)
#   df[] <- lapply(df, trimws)
#   return(df)
# }

load_omim_file <- function(path = "../reference/gen-phenotyp2-uniq.txt") { #nolint
  if (!file.exists(path)) return(NULL)
  # df <- tryCatch({
  #   read.table(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE,
  #             fill = TRUE, colClasses = c("character", "character"))
  # }, error = function(e) {
  df <- read.delim(
    path,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    quote = "",
    comment.char = "",
    check.names = FALSE,
    fileEncoding = "UTF-8"
  # }, error = function(e) {
  #   showNotification("Chyba při načítání OMIM souboru.", type = "error")
  #   return(NULL)
  # })
  )

  # cat("pocet radku OMIM:", nrow(df), "\n")
  # cat("DVL1 v readLines:", any(grepl("^DVL1\\t", readLines(path))), "\n")
  # cat("DVL1 v df$gene:", any(df$gene == "DVL1"), "\n")
  # print(df[grep("DVL1", df$gene), ])

  # cat("OMIM path used by Shiny:\n")
  # print(normalizePath(path, mustWork = FALSE))

  # cat("getwd():\n")
  # print(getwd())

  # cat("DVL1 grep after loading:\n")
  # print(grep("DVL1", df$gene, value = TRUE))

  df[] <- lapply(df, trimws)
  return(df)
}

# Apply OMIM annotation to result
annotate_with_omim <- function(result_df, omim_df) {
  if (is.null(omim_df) || !"name" %in% tolower(colnames(result_df))) {
    result_df$OMIM <- "NA"
    return(result_df)
  }

  # Convert to uppercase and trim whitespace for matching
  query_genes <- toupper(trimws(result_df$name))
  ref_genes <- toupper(trimws(omim_df$gene))


  # cat("TEST DVL1\n")
  # cat("result:", dput(unique(result_df$name[grepl("DVL1", result_df$name)])), "\n")
  # cat("omim:", dput(unique(omim_df$gene[grepl("DVL1", omim_df$gene)])), "\n")

  # cat("match DVL1:", match("DVL1", toupper(trimws(omim_df$gene))), "\n")
  # cat("phenotyp DVL1:", omim_df$phenotyp[match("DVL1", toupper(trimws(omim_df$gene)))], "\n")


  # Use match to find corresponding phenotypes
  match_idx <- match(query_genes, ref_genes)
  result_df$OMIM <- ifelse(!is.na(match_idx), omim_df$phenotyp[match_idx], "NA")

  # remove trailing semicolons and extra spaces
  result_df$OMIM <- gsub("(;\\s*)+$", "", result_df$OMIM)
  result_df$OMIM <- gsub("\\s*;\\s*;", ";", result_df$OMIM)
  return(result_df)
}
