# Epitage

**A transcript-level DNA methylation ranking resource for human ageing research**

Epitage is an open scientific resource that assigns scores to gene transcripts based on the association between their DNA methylation profiles and chronological age in human blood, allowing them to be ranked for further investigation.

## Related research paper

**A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage)**  
DOI: https://doi.org/10.64898/2026.02.09.704870

## Generated with ugPlot

This table was generated using **ugPlot**, an open-source R/Shiny framework for machine-learning testing, model comparison, and methylation data exploration.  
Zenodo DOI: https://doi.org/10.5281/zenodo.19824653

## Legend

| Type | Description |
|------|-------------|
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | Entries are mainly explained by a dominant CpG signal. |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | Entries show stronger transcript-level performance when multiple CpGs are analysed together. |
| ![CpG & ML](https://img.shields.io/badge/CpG_&_ML-purple?style=flat-square) | Entries combining both CpG-centered and ML-centered signals. |
| ![Processing](https://img.shields.io/badge/Processing-006400?style=flat-square) | Transcript currently being processed. |

## Epitage transcript rankings GEO GSE87571

| Type | Gene | Transcripts | Best CpG Spearman | Median ML R² | CpGs | Samples |
|------|------|-------------|-------------------|--------------|------|---------|
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | ELOVL2 | ENST00000456616.2<br>ENST00000667154.1<br>ENST00000657744.1 | 0.95 | 0.93 | 7 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | ELOVL2 | ENST00000354666.4 | 0.95 | 0.92 | 7 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | ELOVL2 | ENST00000667435.1<br>ENST00000607275.6 | 0.95 | 0.92 | 8 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | FHL2 | ENST00000409177.6 | 0.93 | 0.9 | 9 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | FHL2 | ENST00000393353.7<br>ENST00000344213.9<br>ENST00000530340.6<br>ENST00000358129.8<br>ENST00000408995.5<br>ENST00000322142.13 | 0.93 | 0.9 | 18 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | TRIM59 | ENST00000471155.5<br>ENST00000483754.1<br>ENST00000471396.1<br>ENST00000479460.5<br>ENST00000309784.9 | 0.89 | 0.83 | 14 | 636 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | TRIM59 | ENST00000543469.1 | 0.89 | 0.81 | 13 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | TRIM59 | ENST00000494486.1 | 0.89 | 0.81 | 13 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | TRIM59 | ENST00000496222.1 | 0.89 | 0.81 | 14 | 636 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | CALB1 | ENST00000518457.5 | 0.88 | 0.79 | 8 | 671 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | CALB1 | ENST00000476853.1<br>ENST00000473670.1 | 0.88 | 0.79 | 8 | 710 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | CALB1 | ENST00000265431.7 | 0.88 | 0.78 | 5 | 671 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | CALB1 | ENST00000482702.5 | 0.88 | 0.79 | 7 | 710 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | VTRNA1-2 | ENST00000689319.1 | 0.87 | 0.78 | 6 | 703 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | MIR29B2CHG | ENST00000655169.1 | -0.86 | 0.83 | 7 | 687 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | MIR29B2CHG | ENST00000487977.2<br>ENST00000652846.1<br>ENST00000702741.1 | -0.86 | 0.82 | 5 | 687 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | OBSCN | ENST00000422127.5<br>ENST00000680850.1<br>ENST00000570156.7 | 0.76 | 0.84 | 69 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | OTUD7A | ENST00000560598.2 | 0.84 | 0.79 | 11 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | OTUD7A | ENST00000678495.1 | 0.84 | 0.78 | 4 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | KLF14 | ENST00000583337.4 | 0.83 | 0.8 | 13 | 685 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | PRRT1 | ENST00000472641.1 | 0.75 | 0.83 | 46 | 678 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | PRRT1 | ENST00000211413.10 | 0.76 | 0.83 | 60 | 660 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | PRRT1 | ENST00000495191.5 | 0.76 | 0.83 | 62 | 660 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | OBSCN | ENST00000636476.2 | 0.76 | 0.82 | 57 | 714 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | PRRT1 | ENST00000375150.6 | 0.76 | 0.82 | 76 | 663 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | SYNGR3 | ENST00000248121.7 | 0.81 | 0.77 | 12 | 714 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | OBSCN | ENST00000284548.16 | 0.76 | 0.8 | 65 | 714 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | OBSCN | ENST00000662438.1 | 0.76 | 0.8 | 55 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | KCNS1 | ENST00000537075.3 | 0.8 | 0.78 | 12 | 713 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | SLC1A6 | ENST00000430939.6 | 0.78 | 0.72 | 14 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | TEKTL1 | ENST00000292574.4 | 0.78 | 0.72 | 13 | 714 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | PRRT1 | ENST00000467780.5 | 0.74 | 0.77 | 41 | 686 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | RNF180 | ENST00000504296.1 | 0.63 | 0.77 | 8 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | FOXG1-AS1 | ENST00000546560.5 | 0.77 | 0.71 | 12 | 683 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | TP73 | ENST00000354437.8 | 0.74 | 0.71 | 27 | 714 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | SPTBN4 | ENST00000352632.7 | 0.69 | 0.73 | 42 | 682 |
| ![ML centered](https://img.shields.io/badge/ML_centered-orange?style=flat-square) | CACNA1G | ENST00000359106.10 | 0.7 | 0.75 | 41 | 714 |
| ![CpG & ML](https://img.shields.io/badge/CpG_&_ML-purple?style=flat-square) | PPM1N | ENST00000415077.1 | 0.66 | 0.66 | 11 | 714 |
| ![CpG centered](https://img.shields.io/badge/CpG_centered-blue?style=flat-square) | ZIK1 | ENST00000597219.1 | 0.82 | 0.70 | 10 | 714 |
| ![Processing](https://img.shields.io/badge/Processing-006400?style=flat-square) | CELF6 | ENST00000567083.2 | 0.81 | — | — | — |
| — | CACNA1G | ENST00000442258.6 | 0.7 | — | — | — |

## Waiting list

| Gene | Transcript |
|------|-----------|
| MARCHF11-DT | ENST00000509037.1 |
| KCNS1 | ENST00000306117.5 |
| SYNGR3 | ENST00000248121.7 |
| CACNA1G | ENST00000352832.9 |
| CACNA1G | ENST00000352832.9 |
| ADRB1 | ENST00000369295.4 |
| ZIC1 | ENST00000282928.5 |
| OTUD7A | ENST00000560598.2 |
| OTUD7A | ENST00000678495.1 |
| TP73 | ENST00000604074.5 |
| FOXG1-AS1 | ENST00000549487.1 |
| CACNA1G | ENST00000354983.8 |
| SLC1A6 | ENST00000599636.1 |
| ZIK1 | ENST00000597219.1 |
| LRTM2 | ENST00000543818.5 |
|  | ENST00000587412.1 |
| F5 | ENST00000367797.9 |
| LRTM2 | ENST00000535041.5 |
| SPTBN4 | ENST00000598249.6 |
| OBSCN | ENST00000366704.2 |
| B3GNT9 | ENST00000449549.4 |
| LINC01551 | ENST00000675861.1 |
|  | ENST00000346387.8 |
| OTUD7A | ENST00000307050.6 |
| F5 | ENST00000367796.3 |
|  | ENST00000357733.7 |
| LRTM2 | ENST00000299194.6 |
| TP73 | ENST00000378295.9 |
| THCAT155 | ENST00000656838.1 |
| WHRN | ENST00000265134.10 |
| ZIK1 | ENST00000307468.4 |
| RPA2 | ENST00000444045.1 |
| SPTBN4 | ENST00000352632.7 |
| THCAT155 | ENST00000668740.1 |
| ZIK1 | ENST00000597850.2 |
| FOXG1-AS1 | ENST00000551395.5 |
| ZIK1 | ENST00000598689.1 |
| RFX5-AS1 | ENST00000609583.1 |
| FOXG1-AS1 | ENST00000658593.1 |
| TBR1 | ENST00000389554.8 |
| FAM163B | ENST00000673969.1 |
| ZIK1 | ENST00000599456.1 |
| RPA2 | ENST00000373912.8 |
| ZIK1 | ENST00000536878.6 |
| EPHX3 | ENST00000221730.8 |
| COL1A1 | ENST00000495677.1 |
| RNF180 | ENST00000389100.9 |
| ZYG11A | ENST00000371532.5 |
| NEFM | ENST00000518131.5 |
| CELF6 | ENST00000287202.10 |
|  | ENST00000612017.4 |
| CELF6 | ENST00000437872.7 |
|  | ENST00000481848.6 |
| RNF180 | ENST00000296615.10 |
| MTCL3 | ENST00000465909.2 |
| PRRT4 | ENST00000446477.6 |
| NEFM | ENST00000521540.1 |
| MTCL3 | ENST00000525778.5 |
| LIN28B-AS1 | ENST00000659952.1 |
| FOXG1 | ENST00000313071.7 |
| RPA2 | ENST00000313433.11 |
| BCAN | ENST00000457777.6 |
| NEFM | ENST00000523467.1 |
|  | ENST00000701512.1 |
|  | ENST00000607058.2 |
| NEFM | ENST00000437366.2 |
| PEARL1 | ENST00000689319.1 |
| MARCHF11 | ENST00000507111.1 |
| BCAN | ENST00000479949.5 |
| GRM2 | ENST00000464585.1 |
|  | ENST00000569547.1 |
| CELF6 | ENST00000567083.2 |

## Citation

*A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage).*  
https://doi.org/10.64898/2026.02.09.704870
