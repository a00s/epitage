# Epitage

**A transcript-level DNA methylation ranking resource for human ageing research**

Epitage is an open scientific resource created to identify **gene transcripts whose DNA methylation profiles are strongly associated with chronological age in human blood**.

## Related research paper

**A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage)**  
DOI: https://doi.org/10.64898/2026.02.09.704870

## Generated with ugPlot

This table was generated using **ugPlot**, an open-source R/Shiny framework for machine-learning testing, model comparison, and methylation data exploration.  
Zenodo DOI: https://doi.org/10.5281/zenodo.19824653

## Card legend

<img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"> entries are mainly explained by a dominant CpG signal.  
<img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"> entries show stronger transcript-level performance when multiple CpGs are analysed together.

## Epitage transcript cards

<table>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>ELOVL2</b><br><sub>ENST00000456616.2<br>ENST00000667154.1<br>ENST00000657744.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg16867657 0.95<br>cg21572722 0.92<br>cg24724428 0.87</td></tr>
<tr><td><b>CpGs</b></td><td>7</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarthGCV(243:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.93 (IQR 0.01)</td></tr>
<tr><td><b>R² range</b></td><td>0.88–0.95</td></tr>
<tr><td><b>Median MAE</b></td><td>4.41 (IQR 0.36)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.06</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>ELOVL2</b><br><sub>ENST00000354666.4</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg16867657 0.95<br>cg24724428 0.87<br>cg21572722 0.92</td></tr>
<tr><td><b>CpGs</b></td><td>7</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarth(1186:1231)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.92 (IQR 0.01)</td></tr>
<tr><td><b>R² range</b></td><td>0.88–0.95</td></tr>
<tr><td><b>Median MAE</b></td><td>4.46 (IQR 0.35)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.05</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>ELOVL2</b><br><sub>ENST00000667435.1<br>ENST00000607275.6</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg16867657 0.95<br>cg24724428 0.87<br>cg21572722 0.92</td></tr>
<tr><td><b>CpGs</b></td><td>8</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>cubist(243:3)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.92 (IQR 0.01)</td></tr>
<tr><td><b>R² range</b></td><td>0.87–0.95</td></tr>
<tr><td><b>Median MAE</b></td><td>4.46 (IQR 0.37)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>FHL2</b><br><sub>ENST00000409177.6</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg06639320 0.93<br>cg24079702 0.90<br>cg22454769 0.92</td></tr>
<tr><td><b>CpGs</b></td><td>9</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>gamLoess(3157:8479)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.9 (IQR 0.02)</td></tr>
<tr><td><b>R² range</b></td><td>0.85–0.94</td></tr>
<tr><td><b>Median MAE</b></td><td>5.06 (IQR 0.44)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>FHL2</b><br><sub>ENST00000393353.7<br>ENST00000344213.9<br>ENST00000530340.6<br>ENST00000358129.8<br>ENST00000408995.5<br>ENST00000322142.13</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg24079702 0.90<br>cg22454769 0.92<br>cg06639320 0.93</td></tr>
<tr><td><b>CpGs</b></td><td>18</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarth(2123:1849)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.9 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.45–0.94</td></tr>
<tr><td><b>Median MAE</b></td><td>5.2 (IQR 0.51)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>TRIM59</b><br><sub>ENST00000471155.5<br>ENST00000483754.1<br>ENST00000471396.1<br>ENST00000479460.5<br>ENST00000309784.9</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg07553761 0.89</td></tr>
<tr><td><b>CpGs</b></td><td>14</td></tr>
<tr><td><b>Samples</b></td><td>636</td></tr>
<tr><td><b>Best model</b></td><td><code>bstTree(1351:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.83 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.72–0.89</td></tr>
<tr><td><b>Median MAE</b></td><td>6.91 (IQR 0.58)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.06</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>OBSCN</b><br><sub>ENST00000422127.5<br>ENST00000680850.1<br>ENST00000570156.7</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg04193160 0.76<br>cg00251125 -0.73<br>cg19261426 -0.72</td></tr>
<tr><td><b>CpGs</b></td><td>69</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>gamboost(3928:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td> 0.84 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.73–0.9</td></tr>
<tr><td><b>Median MAE</b></td><td>6.36 (IQR 0.54)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.06</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>MIR29B2CHG</b><br><sub>ENST00000655169.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg10501210 -0.86<br>(negative)</td></tr>
<tr><td><b>CpGs</b></td><td>7</td></tr>
<tr><td><b>Samples</b></td><td>687</td></tr>
<tr><td><b>Best model</b></td><td><code>ranger(3687:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.83 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.74–0.9</td></tr>
<tr><td><b>Median MAE</b></td><td>6.64 (IQR 0.58)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>TRIM59</b><br><sub>ENST00000543469.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg07553761 0.89</td></tr>
<tr><td><b>CpGs</b></td><td>13</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>rf(2233:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.81 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.73–0.88</td></tr>
<tr><td><b>Median MAE</b></td><td>7.15 (IQR 0.57)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>TRIM59</b><br><sub>ENST00000494486.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg07553761 0.89</td></tr>
<tr><td><b>CpGs</b></td><td>13</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarth(3082:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td> 0.81 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.39–0.89</td></tr>
<tr><td><b>Median MAE</b></td><td>7.15 (IQR 0.71)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.05</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>TRIM59</b><br><sub>ENST00000496222.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg07553761 0.89</td></tr>
<tr><td><b>CpGs</b></td><td>14</td></tr>
<tr><td><b>Samples</b></td><td>636</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarth(3598:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.81 (IQR 0.05)</td></tr>
<tr><td><b>R² range</b></td><td>0.32–0.89</td></tr>
<tr><td><b>Median MAE</b></td><td>7.19 (IQR 0.85)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.03</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>MIR29B2CHG</b><br><sub>ENST00000487977.2<br>ENST00000652846.1<br>ENST00000702741.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg10501210 -0.86<br>(negative)</td></tr>
<tr><td><b>CpGs</b></td><td>5</td></tr>
<tr><td><b>Samples</b></td><td>687</td></tr>
<tr><td><b>Best model</b></td><td><code>ranger(3687:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.82 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.73–0.89</td></tr>
<tr><td><b>Median MAE</b></td><td>6.73 (IQR 0.58)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.03</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>OBSCN</b><br><sub>ENST00000636476.2</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg04193160 0.76<br>cg19261426 -0.72</td></tr>
<tr><td><b>CpGs</b></td><td>57</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>gamboost(3928:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.82 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.68–0.89</td></tr>
<tr><td><b>Median MAE</b></td><td>0.82 (IQR 0.04)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>KLF14</b><br><sub>ENST00000583337.4</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg08097417 0.83<br>cg07955995 0.83</td></tr>
<tr><td><b>CpGs</b></td><td>13</td></tr>
<tr><td><b>Samples</b></td><td>685</td></tr>
<tr><td><b>Best model</b></td><td><code>ppr(2155:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.8 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.69–0.89</td></tr>
<tr><td><b>Median MAE</b></td><td>6.99 (IQR 0.66)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.05</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>PRRT1</b><br><sub>ENST00000472641.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg24125828 0.75<br>cg13782301 0.74<br>cg18501647 0.73<br>cg14757228 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td></tr>
<tr><td><b>CpGs</b></td><td>46</td></tr>
<tr><td><b>Samples</b></td><td>678</td></tr>
<tr><td><b>Best model</b></td><td><code>brnn(2245:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.83 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.73–0.89</td></tr>
<tr><td><b>Median MAE</b></td><td>6.71 (IQR 0.59)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>PRRT1</b><br><sub>ENST00000375150.6</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td></tr>
<tr><td><b>CpGs</b></td><td>76</td></tr>
<tr><td><b>Samples</b></td><td>663</td></tr>
<tr><td><b>Best model</b></td><td><code>brnn(2973:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.82 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.72–0.88</td></tr>
<tr><td><b>Median MAE</b></td><td>6.96 (IQR 0.61)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.05</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>OBSCN</b><br><sub>ENST00000662438.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg04193160 0.76<br>cg19261426 -0.72</td></tr>
<tr><td><b>CpGs</b></td><td>55</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>ppr(3526:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.8 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.64–0.88</td></tr>
<tr><td><b>Median MAE</b></td><td>0.8 (IQR 0.04)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.07</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>CALB1</b><br><sub>ENST00000518457.5</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg26290632 0.88</td></tr>
<tr><td><b>CpGs</b></td><td>8</td></tr>
<tr><td><b>Samples</b></td><td>671</td></tr>
<tr><td><b>Best model</b></td><td><code>ppr(1412:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.79 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.65–0.87</td></tr>
<tr><td><b>Median MAE</b></td><td>7.35 (IQR 0.64)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.05</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>CALB1</b><br><sub>ENST00000476853.1<br>ENST00000473670.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg26290632 0.88</td></tr>
<tr><td><b>CpGs</b></td><td>8</td></tr>
<tr><td><b>Samples</b></td><td>710</td></tr>
<tr><td><b>Best model</b></td><td><code>ppr(2556:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.79 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.68–0.87</td></tr>
<tr><td><b>Median MAE</b></td><td>7.35 (IQR 0.64)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>CALB1</b><br><sub>ENST00000265431.7</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg26290632 0.88</td></tr>
<tr><td><b>CpGs</b></td><td>5</td></tr>
<tr><td><b>Samples</b></td><td>671</td></tr>
<tr><td><b>Best model</b></td><td><code>ppr(2053:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.78 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.65–0.87</td></tr>
<tr><td><b>Median MAE</b></td><td>7.44 (IQR 0.68)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.02</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>CALB1</b><br><sub>ENST00000482702.5</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg26290632 0.88</td></tr>
<tr><td><b>CpGs</b></td><td>7</td></tr>
<tr><td><b>Samples</b></td><td>710</td></tr>
<tr><td><b>Best model</b></td><td><code>ppr(2556:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.79 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.67–0.86</td></tr>
<tr><td><b>Median MAE</b></td><td>7.41 (IQR 0.64)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.03</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>PRRT1</b><br><sub>ENST00000211413.10</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td></tr>
<tr><td><b>CpGs</b></td><td>60</td></tr>
<tr><td><b>Samples</b></td><td>660</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarthGCV(3125:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.83 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.74–0.9</td></tr>
<tr><td><b>Median MAE</b></td><td>6.63 (IQR 0.58)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.05</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>PRRT1</b><br><sub>ENST00000495191.5</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td></tr>
<tr><td><b>CpGs</b></td><td>62</td></tr>
<tr><td><b>Samples</b></td><td>660</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarthGCV(3125:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.83 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.74–0.9</td></tr>
<tr><td><b>Median MAE</b></td><td>6.6 (IQR 0.58)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.04</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>KCNS1</b><br><sub>ENST00000537075.3</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg19702785 0.80</td></tr>
<tr><td><b>CpGs</b></td><td>12</td></tr>
<tr><td><b>Samples</b></td><td>713</td></tr>
<tr><td><b>Best model</b></td><td><code>ppr(947:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.78 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.65–0.88</td></tr>
<tr><td><b>Median MAE</b></td><td>7.44 (IQR 0.65)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.06</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>VTRNA1-2</b><br><sub>ENST00000689319.1</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg23500537 0.87</td></tr>
<tr><td><b>CpGs</b></td><td>6</td></tr>
<tr><td><b>Samples</b></td><td>703</td></tr>
<tr><td><b>Best model</b></td><td><code>brnn(2065:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.78 (IQR 0.03)</td></tr>
<tr><td><b>R² range</b></td><td>0.68–0.86</td></tr>
<tr><td><b>Median MAE</b></td><td>7.83 (IQR 0.61)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.03</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>OTUD7A</b><br><sub>ENST00000560598.2</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg04875128 0.84<br>cg01763090 0.81</td></tr>
<tr><td><b>CpGs</b></td><td>11</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>bstTree(130:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.79 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.65–0.87</td></tr>
<tr><td><b>Median MAE</b></td><td>7.43 (IQR 0.63)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.05</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>PRRT1</b><br><sub>ENST00000467780.5</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg24125828 0.74<br>cg13782301 0.74<br>cg18501647 0.73<br>cg14757228 0.73<br>cg27067781 0.72<br>cg11945824 0.71</td></tr>
<tr><td><b>CpGs</b></td><td>41</td></tr>
<tr><td><b>Samples</b></td><td>686</td></tr>
<tr><td><b>Best model</b></td><td><code>brnn(3178:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td> 0.77 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.65–0.86</td></tr>
<tr><td><b>Median MAE</b></td><td>7.82 (IQR 0.64)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.06</td></tr>
</table>

</td>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="CpG centered" src="https://img.shields.io/badge/CpG_centered-blue?style=flat-square"></td></tr>
<tr><td colspan="2"><b>SYNGR3</b><br><sub>ENST00000248121.7</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg11220950 0.81</td></tr>
<tr><td><b>CpGs</b></td><td>12</td></tr>
<tr><td><b>Samples</b></td><td>714</td></tr>
<tr><td><b>Best model</b></td><td><code>cubist(268:45)</code></td></tr>
<tr><td><b>Median R²</b></td><td>0.77 (IQR 0.05)</td></tr>
<tr><td><b>R² range</b></td><td>0.63–0.85</td></tr>
<tr><td><b>Median MAE</b></td><td>7.8 (IQR 0.71)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.03</td></tr>
</table>

</td>
</tr>
<tr>
<td valign="top" width="50%">

<table>
<tr><td colspan="2"><img alt="ML centered" src="https://img.shields.io/badge/ML_centered-orange?style=flat-square"></td></tr>
<tr><td colspan="2"><b>SPTBN4</b><br><sub>ENST00000352632.7</sub></td></tr>
<tr><td><b>Best CpG / Spearman</b></td><td>cg05764628 0.69<br>cg02997982 0.68<br>cg17768491 0.68<br>cg09381003 0.68<br>cg02576468 0.68</td></tr>
<tr><td><b>CpGs</b></td><td>42</td></tr>
<tr><td><b>Samples</b></td><td>682</td></tr>
<tr><td><b>Best model</b></td><td><code>bagEarthGCV(3523:1)</code></td></tr>
<tr><td><b>Median R²</b></td><td> 0.73 (IQR 0.04)</td></tr>
<tr><td><b>R² range</b></td><td>0.6–0.83</td></tr>
<tr><td><b>Median MAE</b></td><td>8.66 (IQR 0.67)</td></tr>
<tr><td><b>Max R² scramble age</b></td><td>0.03</td></tr>
</table>

</td>
<td valign="top" width="50%">



</td>
</tr>
</table>

## Citation

*A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage).*  
https://doi.org/10.64898/2026.02.09.704870
