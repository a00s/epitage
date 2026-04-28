#!/usr/bin/env Rscript

# Uso:
# Rscript check_cpg_transcript.R cg02709407

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Uso: Rscript check_cpg_transcript.R cg02709407")
}

cpg_id <- args[1]

suppressPackageStartupMessages({
  library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
  library(EnsDb.Hsapiens.v75)
  library(GenomicRanges)
  library(ensembldb)
  library(AnnotationFilter)
})

# =====================================
# [ Input CpG ] ⇒ [ Annotation ]
# =====================================

ann <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

if (!(cpg_id %in% rownames(ann))) {
  stop(paste("CpG não encontrado:", cpg_id))
}

cpg_row <- ann[cpg_id, ]

cat("\n=============================\n")
cat("[ CPG ENCONTRADO ]\n")
cat("=============================\n")

print(cpg_row[, c(
  "Name",
  "chr",
  "pos",
  "strand",
  "UCSC_RefGene_Name",
  "UCSC_RefGene_Accession",
  "Relation_to_UCSC_CpG_Island"
)])

# =====================================
# [ CpG ] ⇒ [ GRanges ]
# =====================================

cpg_gr <- GRanges(
  seqnames = cpg_row$chr,
  ranges = IRanges(start = cpg_row$pos, end = cpg_row$pos),
  strand = "*"
)

# =====================================
# [ Ensembl transcripts ]
# =====================================

edb <- EnsDb.Hsapiens.v75

tx <- transcripts(
  edb,
  return.type = "GRanges",
  columns = c("tx_id", "gene_id", "gene_name")
)

seqlevelsStyle(cpg_gr) <- seqlevelsStyle(tx)

common_seqlevels <- intersect(seqlevels(cpg_gr), seqlevels(tx))

cpg_gr <- keepSeqlevels(cpg_gr, common_seqlevels, pruning.mode = "coarse")
tx     <- keepSeqlevels(tx, common_seqlevels, pruning.mode = "coarse")

# =====================================
# [ Overlap direto ]
# =====================================

hits <- findOverlaps(cpg_gr, tx, ignore.strand = TRUE)

cat("\n=============================\n")
cat("[ TRANSCRIPTS QUE CONTÊM O CPG ]\n")
cat("=============================\n")

if (length(hits) == 0) {

  cat("Nenhum transcript contém esse CpG.\n")

} else {

  res <- data.frame(
    cpg       = cpg_id,
    tx_id     = mcols(tx)$tx_id[subjectHits(hits)],
    gene_id   = mcols(tx)$gene_id[subjectHits(hits)],
    gene_name = mcols(tx)$gene_name[subjectHits(hits)],
    tx_start  = start(tx)[subjectHits(hits)],
    tx_end    = end(tx)[subjectHits(hits)],
    strand    = as.character(strand(tx))[subjectHits(hits)]
  )

  print(res)
}

# =====================================
# [ Transcript mais próximo ]
# =====================================

cat("\n=============================\n")
cat("[ TRANSCRIPT MAIS PRÓXIMO ]\n")
cat("=============================\n")

nearest_idx <- nearest(cpg_gr, tx, ignore.strand = TRUE)

if (!is.na(nearest_idx)) {

  near_tx <- tx[nearest_idx]

  dist_bp <- distance(cpg_gr, near_tx, ignore.strand = TRUE)

  near_res <- data.frame(
    cpg         = cpg_id,
    tx_id       = mcols(near_tx)$tx_id,
    gene_id     = mcols(near_tx)$gene_id,
    gene_name   = mcols(near_tx)$gene_name,
    tx_start    = start(near_tx),
    tx_end      = end(near_tx),
    strand      = as.character(strand(near_tx)),
    distance_bp = dist_bp
  )

  print(near_res)

}