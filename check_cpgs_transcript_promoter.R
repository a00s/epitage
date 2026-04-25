#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript check_cpg_transcript_signed_distance.R ENST00000456616.2.csv")
}

input_file <- args[1]

suppressPackageStartupMessages({
  library(minfi)
  library(GenomicRanges)
  library(GenomeInfoDb)
  library(GenomicFeatures)
  library(ensembldb)
  library(AnnotationFilter)
  library(EnsDb.Hsapiens.v75)
  library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
  library(httr)
  library(jsonlite)
})

# --------------------------------------------------
# Safe GET helper
# --------------------------------------------------
safe_get_json <- function(url) {
  resp <- try(httr::GET(
    url,
    httr::add_headers("Accept" = "application/json")
  ), silent = TRUE)

  if (inherits(resp, "try-error")) return(NULL)
  if (httr::status_code(resp) != 200) return(NULL)

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(txt)) return(NULL)

  out <- try(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)

  out
}

# --------------------------------------------------
# Current Ensembl REST annotation
# Returns transcript info + true parent gene info
# --------------------------------------------------
get_transcript_from_rest <- function(tx_id_input) {

  tx_obj <- safe_get_json(
    paste0("https://rest.ensembl.org/lookup/id/", tx_id_input, "?expand=1")
  )

  if (is.null(tx_obj)) return(NULL)

  strand_symbol <- ifelse(tx_obj$strand == 1, "+", "-")

  tx_gr <- GRanges(
    seqnames = paste0("chr", tx_obj$seq_region_name),
    ranges = IRanges(start = tx_obj$start, end = tx_obj$end),
    strand = strand_symbol
  )

  tx_id_found <- if (!is.null(tx_obj$id)) as.character(tx_obj$id) else NA_character_
  parent_gene_id <- if (!is.null(tx_obj$Parent)) as.character(tx_obj$Parent) else NA_character_
  tx_biotype <- if (!is.null(tx_obj$biotype)) as.character(tx_obj$biotype) else NA_character_
  tx_display_name <- if (!is.null(tx_obj$display_name)) as.character(tx_obj$display_name) else NA_character_

  # true parent gene lookup
  gene_name_found <- NA_character_
  gene_biotype_found <- NA_character_

  if (!is.na(parent_gene_id)) {
    gene_obj <- safe_get_json(
      paste0("https://rest.ensembl.org/lookup/id/", parent_gene_id)
    )

    if (!is.null(gene_obj)) {
      if (!is.null(gene_obj$display_name)) {
        gene_name_found <- as.character(gene_obj$display_name)
      }
      if (!is.null(gene_obj$biotype)) {
        gene_biotype_found <- as.character(gene_obj$biotype)
      }
    }
  }

  mcols(tx_gr)$tx_id <- tx_id_found
  mcols(tx_gr)$gene_id <- parent_gene_id
  mcols(tx_gr)$gene_name <- gene_name_found
  mcols(tx_gr)$tx_biotype <- tx_biotype
  mcols(tx_gr)$gene_biotype <- gene_biotype_found
  mcols(tx_gr)$tx_display_name <- tx_display_name

  # exons
  exon_obj <- safe_get_json(
    paste0("https://rest.ensembl.org/overlap/id/", tx_id_input, "?feature=exon")
  )

  exon_gr <- GRanges()

  if (!is.null(exon_obj) && is.data.frame(exon_obj) && nrow(exon_obj) > 0) {
    exon_strand <- ifelse(exon_obj$strand == 1, "+", "-")

    exon_gr <- GRanges(
      seqnames = paste0("chr", exon_obj$seq_region_name),
      ranges = IRanges(start = exon_obj$start, end = exon_obj$end),
      strand = exon_strand
    )
  }

  list(
    tx_gr = tx_gr,
    tx_exons = exon_gr,
    tx_id_found = tx_id_found,
    gene_id_found = parent_gene_id,
    gene_name_found = gene_name_found,
    tx_biotype_found = tx_biotype,
    gene_biotype_found = gene_biotype_found,
    tx_display_name_found = tx_display_name,
    source = "Ensembl_REST_current"
  )
}

# --------------------------------------------------
# Transcript from filename
# --------------------------------------------------
file_base <- basename(input_file)

tx_versioned <- sub("\\.csv$", "", file_base)
tx_unversioned <- sub("\\.[0-9]+$", "", tx_versioned)

cat("Input file:", input_file, "\n")
cat("Transcript:", tx_versioned, "\n\n")

# --------------------------------------------------
# Read input
# --------------------------------------------------
header_df <- read.csv(
  input_file,
  nrows = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

all_cols <- colnames(header_df)
cpgs <- grep("^cg[0-9]+$", all_cols, value = TRUE)

full_df <- read.csv(
  input_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

n_samples <- nrow(full_df)

cat("CpGs found in file:", length(cpgs), "\n")
cat("Samples found in file:", n_samples, "\n\n")

# --------------------------------------------------
# 450k annotation
# --------------------------------------------------
ann <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

present_cpgs <- intersect(cpgs, rownames(ann))
missing_cpgs <- setdiff(cpgs, rownames(ann))

cat("CpGs found in 450k annotation:", length(present_cpgs), "\n")
cat("CpGs missing from 450k annotation:", length(missing_cpgs), "\n\n")

if (length(present_cpgs) == 0) {
  stop("No CpGs found in annotation.")
}

ann_sub <- ann[present_cpgs, ]

# --------------------------------------------------
# EnsDb v75 lookup
# --------------------------------------------------
edb <- EnsDb.Hsapiens.v75
seqlevelsStyle(edb) <- "UCSC"

tx_gr_v75 <- transcripts(
  edb,
  filter = TxIdFilter(tx_versioned),
  columns = c("tx_id", "gene_id", "gene_name", "tx_biotype")
)

if (length(tx_gr_v75) == 0) {
  tx_gr_v75 <- transcripts(
    edb,
    filter = TxIdFilter(tx_unversioned),
    columns = c("tx_id", "gene_id", "gene_name", "tx_biotype")
  )
}

ensdb_found <- length(tx_gr_v75) > 0

tx_exons_v75 <- GRanges()
tx_id_v75 <- NA_character_
gene_id_v75 <- NA_character_
gene_name_v75 <- NA_character_
tx_biotype_v75 <- NA_character_

if (ensdb_found) {
  tx_gr_v75 <- tx_gr_v75[1]

  tx_id_v75 <- as.character(mcols(tx_gr_v75)$tx_id[1])
  gene_id_v75 <- as.character(mcols(tx_gr_v75)$gene_id[1])
  gene_name_v75 <- as.character(mcols(tx_gr_v75)$gene_name[1])
  tx_biotype_v75 <- as.character(mcols(tx_gr_v75)$tx_biotype[1])

  exons_list <- exonsBy(
    edb,
    by = "tx",
    filter = TxIdFilter(tx_id_v75)
  )

  if (tx_id_v75 %in% names(exons_list)) {
    tx_exons_v75 <- exons_list[[tx_id_v75]]
  }
}

# --------------------------------------------------
# Current Ensembl REST lookup
# --------------------------------------------------
rest_info <- get_transcript_from_rest(tx_unversioned)

# --------------------------------------------------
# Print annotations side by side
# --------------------------------------------------
cat("========== Annotation summary ==========\n")

if (ensdb_found) {
  cat("[EnsDb.Hsapiens.v75]\n")
  cat("Transcript:", tx_id_v75, "\n")
  cat("Gene ID:", gene_id_v75, "\n")
  cat("Gene name:", gene_name_v75, "\n")
  cat("Transcript biotype:", tx_biotype_v75, "\n\n")
} else {
  cat("[EnsDb.Hsapiens.v75]\n")
  cat("Transcript not found\n\n")
}

if (!is.null(rest_info)) {
  cat("[Ensembl REST current]\n")
  cat("Transcript:", rest_info$tx_id_found, "\n")
  cat("Gene ID:", rest_info$gene_id_found, "\n")
  cat("Gene name:", rest_info$gene_name_found, "\n")
  cat("Transcript biotype:", rest_info$tx_biotype_found, "\n")
  cat("Gene biotype:", rest_info$gene_biotype_found, "\n")
  cat("Transcript display_name:", rest_info$tx_display_name_found, "\n\n")
} else {
  cat("[Ensembl REST current]\n")
  cat("Transcript not found\n\n")
}

# --------------------------------------------------
# Choose coordinates source for distances
# Prefer EnsDb v75 to remain compatible with pipeline
# --------------------------------------------------
if (ensdb_found) {
  tx_gr <- tx_gr_v75
  tx_exons <- tx_exons_v75
  annotation_source <- "EnsDb.Hsapiens.v75"
} else if (!is.null(rest_info)) {
  tx_gr <- rest_info$tx_gr
  tx_exons <- rest_info$tx_exons
  annotation_source <- rest_info$source
} else {
  stop("Transcript not found in EnsDb v75 nor Ensembl REST.")
}

cat("Coordinate source used for TSS/exon overlap:", annotation_source, "\n")

tx_strand <- as.character(strand(tx_gr))[1]
tx_start <- start(tx_gr)[1]
tx_end <- end(tx_gr)[1]
tss_pos <- if (tx_strand == "+") tx_start else tx_end

cat("Transcript strand:", tx_strand, "\n")
cat("TSS:", tss_pos, "\n\n")

# --------------------------------------------------
# CpGs as ranges
# --------------------------------------------------
cpg_gr <- GRanges(
  seqnames = ann_sub$chr,
  ranges = IRanges(start = ann_sub$pos, end = ann_sub$pos),
  strand = "*"
)

names(cpg_gr) <- rownames(ann_sub)
seqlevelsStyle(cpg_gr) <- "UCSC"

# --------------------------------------------------
# Overlaps
# --------------------------------------------------
hits_tx <- subsetByOverlaps(
  cpg_gr,
  tx_gr,
  ignore.strand = TRUE
)

hits_exon <- subsetByOverlaps(
  cpg_gr,
  tx_exons,
  ignore.strand = TRUE
)

inside_transcript <- names(cpg_gr) %in% names(hits_tx)
inside_exon <- names(cpg_gr) %in% names(hits_exon)

# --------------------------------------------------
# Signed biological distance to TSS
# negative = upstream
# positive = downstream
# --------------------------------------------------
cpg_pos <- start(cpg_gr)

signed_distance_to_tss <- if (tx_strand == "+") {
  cpg_pos - tss_pos
} else {
  tss_pos - cpg_pos
}

# --------------------------------------------------
# Final table
# --------------------------------------------------
res <- data.frame(
  cpg = names(cpg_gr),
  signed_distance_to_tss = signed_distance_to_tss,
  inside_transcript = inside_transcript,
  inside_exon = inside_exon,
  stringsAsFactors = FALSE
)

res$flags <- ifelse(
  res$inside_transcript & res$inside_exon, "TE",
  ifelse(res$inside_transcript, "T", "")
)

res <- res[order(res$signed_distance_to_tss, res$cpg), ]

# --------------------------------------------------
# Print result
# --------------------------------------------------
cat("cpg\tsigned_distance_to_tss\tflags\n")
for (i in seq_len(nrow(res))) {
  cat(
    res$cpg[i], "\t",
    res$signed_distance_to_tss[i], "\t",
    res$flags[i], "\n",
    sep = ""
  )
}

# --------------------------------------------------
# Missing CpGs
# --------------------------------------------------
cat("\n")
cat("Missing CpGs from 450k annotation:\n")

if (length(missing_cpgs) == 0) {
  cat("None\n")
} else {
  cat(paste(missing_cpgs, collapse = "\n"), "\n")
}