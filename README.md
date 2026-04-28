# Epitage

**Epitage** is a transcript-ranking resource for ageing research based on DNA methylation patterns in human blood.

This repository contains the complete CSV-derived table related to the study:

**A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage)**

**Research DOI:**  
https://doi.org/10.64898/2026.02.09.704870

## Generated with ugPlot

The table below was generated using **ugPlot**, an open-source R/Shiny framework for machine-learning testing, model comparison, and methylation data exploration.

**ugPlot Zenodo DOI:**  
https://doi.org/10.5281/zenodo.19824653

## Epitage table
| Best result | Gene | Transcripts | Spearman | cpgs | samples | Model (best seed) | Median R² | Min R² | Max R² | Median MAE | Max R² scramble age |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CpG centered | ELOVL2 | ENST00000456616.2<br>ENST00000667154.1<br>ENST00000657744.1 | cg16867657 0.95<br>cg21572722 0.92<br>cg24724428 0.87 | 7 | 714 | bagEarthGCV(243:1) | 0.93 (IQR 0.01) | 0.88 | 0.95 | 4.41 (IQR 0.36) | 0.06 |
| CpG centered | ELOVL2 | ENST00000354666.4 | cg16867657 0.95<br>cg24724428 0.87<br>cg21572722 0.92 | 7 | 714 | bagEarth(1186:1231) | 0.92 (IQR 0.01) | 0.88 | 0.95 | 4.46 (IQR 0.35) | 0.05 |
| CpG centered | ELOVL2 | ENST00000667435.1<br>ENST00000607275.6 | cg16867657 0.95<br>cg24724428 0.87<br>cg21572722 0.92 | 8 | 714 | cubist(243:3) | 0.92 (IQR 0.01) | 0.87 | 0.95 | 4.46 (IQR 0.37) | 0.04 |
| CpG centered | FHL2 | ENST00000409177.6 | cg06639320 0.93<br>cg24079702 0.90<br>cg22454769 0.92 | 9 | 714 | gamLoess(3157:8479) | 0.9 (IQR 0.02) | 0.85 | 0.94 | 5.06 (IQR 0.44) | 0.04 |
| CpG centered | FHL2 | ENST00000393353.7<br>ENST00000344213.9<br>ENST00000530340.6<br>ENST00000358129.8<br>ENST00000408995.5<br>ENST00000322142.13 | cg24079702 0.90<br>cg22454769 0.92<br>cg06639320 0.93 | 18 | 714 | bagEarth(2123:1849) | 0.9 (IQR 0.03) | 0.45 | 0.94 | 5.2 (IQR 0.51) | 0.04 |
| CpG centered | TRIM59 | ENST00000471155.5<br>ENST00000483754.1<br>ENST00000471396.1<br>ENST00000479460.5<br>ENST00000309784.9 | cg07553761 0.89 | 14 | 636 | bstTree(1351:1) | 0.83 (IQR 0.03) | 0.72 | 0.89 | 6.91 (IQR 0.58) | 0.06 |
| ML centered | OBSCN | ENST00000422127.5<br>ENST00000680850.1<br>ENST00000570156.7 | cg04193160 0.76<br>cg00251125 -0.73<br>cg19261426 -0.72 | 69 | 714 | gamboost(3928:1) |  0.84 (IQR 0.03) | 0.73 | 0.9 | 6.36 (IQR 0.54) | 0.06 |
| CpG centered | MIR29B2CHG | ENST00000655169.1 | cg10501210 -0.86<br>(negative) | 7 | 687 | ranger(3687:1) | 0.83 (IQR 0.03) | 0.74 | 0.9 | 6.64 (IQR 0.58) | 0.04 |
| CpG centered | TRIM59 | ENST00000543469.1 | cg07553761 0.89 | 13 | 714 | rf(2233:1) | 0.81 (IQR 0.03) | 0.73 | 0.88 | 7.15 (IQR 0.57) | 0.04 |
| CpG centered | TRIM59 | ENST00000494486.1 | cg07553761 0.89 | 13 | 714 | bagEarth(3082:1) |  0.81 (IQR 0.04) | 0.39 | 0.89 | 7.15 (IQR 0.71) | 0.05 |
| CpG centered | TRIM59 | ENST00000496222.1 | cg07553761 0.89 | 14 | 636 | bagEarth(3598:1) | 0.81 (IQR 0.05) | 0.32 | 0.89 | 7.19 (IQR 0.85) | 0.03 |
| CpG centered | MIR29B2CHG | ENST00000487977.2<br>ENST00000652846.1<br>ENST00000702741.1 | cg10501210 -0.86<br>(negative) | 5 | 687 | ranger(3687:1) | 0.82 (IQR 0.03) | 0.73 | 0.89 | 6.73 (IQR 0.58) | 0.03 |
| ML centered | OBSCN | ENST00000636476.2 | cg04193160 0.76<br>cg19261426 -0.72 | 57 | 714 | gamboost(3928:1) | 0.82 (IQR 0.04) | 0.68 | 0.89 | 0.82 (IQR 0.04) | 0.04 |
| CpG centered | KLF14 | ENST00000583337.4 | cg08097417 0.83<br>cg07955995 0.83 | 13 | 685 | ppr(2155:1) | 0.8 (IQR 0.04) | 0.69 | 0.89 | 6.99 (IQR 0.66) | 0.05 |
| ML centered | PRRT1 | ENST00000472641.1 | cg24125828 0.75<br>cg13782301 0.74<br>cg18501647 0.73<br>cg14757228 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70 | 46 | 678 | brnn(2245:1) | 0.83 (IQR 0.03) | 0.73 | 0.89 | 6.71 (IQR 0.59) | 0.04 |
| ML centered | PRRT1 | ENST00000375150.6 | cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70 | 76 | 663 | brnn(2973:1) | 0.82 (IQR 0.03) | 0.72 | 0.88 | 6.96 (IQR 0.61) | 0.05 |
| ML centered | OBSCN | ENST00000662438.1 | cg04193160 0.76<br>cg19261426 -0.72 | 55 | 714 | ppr(3526:1) | 0.8 (IQR 0.04) | 0.64 | 0.88 | 0.8 (IQR 0.04) | 0.07 |
| CpG centered | CALB1 | ENST00000518457.5 | cg26290632 0.88 | 8 | 671 | ppr(1412:1) | 0.79 (IQR 0.04) | 0.65 | 0.87 | 7.35 (IQR 0.64) | 0.05 |
| CpG centered | CALB1 | ENST00000476853.1<br>ENST00000473670.1 | cg26290632 0.88 | 8 | 710 | ppr(2556:1) | 0.79 (IQR 0.04) | 0.68 | 0.87 | 7.35 (IQR 0.64) | 0.04 |
| CpG centered | CALB1 | ENST00000265431.7 | cg26290632 0.88 | 5 | 671 | ppr(2053:1) | 0.78 (IQR 0.04) | 0.65 | 0.87 | 7.44 (IQR 0.68) | 0.02 |
| CpG centered | CALB1 | ENST00000482702.5 | cg26290632 0.88 | 7 | 710 | ppr(2556:1) | 0.79 (IQR 0.04) | 0.67 | 0.86 | 7.41 (IQR 0.64) | 0.03 |
| ML centered | PRRT1 | ENST00000211413.10 | cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70 | 60 | 660 | bagEarthGCV(3125:1) | 0.83 (IQR 0.03) | 0.74 | 0.9 | 6.63 (IQR 0.58) | 0.05 |
| ML centered | PRRT1 | ENST00000495191.5 | cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70 | 62 | 660 | bagEarthGCV(3125:1) | 0.83 (IQR 0.03) | 0.74 | 0.9 | 6.6 (IQR 0.58) | 0.04 |
| CpG centered | KCNS1 | ENST00000537075.3 | cg19702785 0.80 | 12 | 713 | ppr(947:1) | 0.78 (IQR 0.04) | 0.65 | 0.88 | 7.44 (IQR 0.65) | 0.06 |
| CpG centered | VTRNA1-2 | ENST00000689319.1 | cg23500537 0.87 | 6 | 703 | brnn(2065:1) | 0.78 (IQR 0.03) | 0.68 | 0.86 | 7.83 (IQR 0.61) | 0.03 |
| CpG centered | OTUD7A | ENST00000560598.2 | cg04875128 0.84<br>cg01763090 0.81 | 11 | 714 | bstTree(130:1) | 0.79 (IQR 0.04) | 0.65 | 0.87 | 7.43 (IQR 0.63) | 0.05 |
| ML centered | PRRT1 | ENST00000467780.5 | cg24125828 0.74<br>cg13782301 0.74<br>cg18501647 0.73<br>cg14757228 0.73<br>cg27067781 0.72<br>cg11945824 0.71 | 41 | 686 | brnn(3178:1) |  0.77 (IQR 0.04) | 0.65 | 0.86 | 7.82 (IQR 0.64) | 0.06 |
| CpG centered | SYNGR3 | ENST00000248121.7 | cg11220950 0.81 | 12 | 714 | cubist(268:45) | 0.77 (IQR 0.05) | 0.63 | 0.85 | 7.8 (IQR 0.71) | 0.03 |
| ML centered | SPTBN4 | ENST00000352632.7 | cg05764628 0.69<br>cg02997982 0.68<br>cg17768491 0.68<br>cg09381003 0.68<br>cg02576468 0.68 | 42 | 682 | bagEarthGCV(3523:1) |  0.73 (IQR 0.04) | 0.6 | 0.83 | 8.66 (IQR 0.67) | 0.03 |
## Citation

*A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage).*  
https://doi.org/10.64898/2026.02.09.704870

If you use the generated table or workflow, please also cite ugPlot:  
https://doi.org/10.5281/zenodo.19824653
