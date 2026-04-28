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
<table style="font-size:8px; line-height:1; border-collapse:collapse;">
  <thead>
    <tr>
      <th>Type</th>
      <th>Gene</th>
      <th>Transcript</th>
      <th>Best CpG (spearman)</th>
      <th>CpGs</th>
      <th>Samples</th>
      <th>Best model</th>
      <th>Median R²</th>
      <th>Min R²</th>
      <th>Max R²</th>
      <th>Median MAE</th>
      <th>RMSE IQR</th>
    </tr>
  </thead>
  <tbody>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>TRIM59</td><td>ENST00000494486.1</td><td>cg07553761 0.89</td><td>13</td><td>714</td><td>bagEarth(3082:1)</td><td>0.81 (IQR 0.04)</td><td>0.39</td><td>0.89</td><td>7.15 (IQR 0.71)</td><td>0.05</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>TRIM59</td><td>ENST00000496222.1</td><td>cg07553761 0.89</td><td>14</td><td>636</td><td>bagEarth(3598:1)</td><td>0.81 (IQR 0.05)</td><td>0.32</td><td>0.89</td><td>7.19 (IQR 0.85)</td><td>0.03</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>MIR29B2CHG</td><td>ENST00000487977.2<br>ENST00000652846.1<br>ENST00000702741.1</td><td>cg10501210 -0.86<br>(negative)</td><td>5</td><td>687</td><td>ranger(3687:1)</td><td>0.82 (IQR 0.03)</td><td>0.73</td><td>0.89</td><td>6.73 (IQR 0.58)</td><td>0.03</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>OBSCN</td><td>ENST00000636476.2</td><td>cg04193160 0.76<br>cg19261426 -0.72</td><td>57</td><td>714</td><td>gamboost(3928:1)</td><td>0.82 (IQR 0.04)</td><td>0.68</td><td>0.89</td><td>0.82 (IQR 0.04)</td><td>0.04</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>KLF14</td><td>ENST00000583337.4</td><td>cg08097417 0.83<br>cg07955995 0.83</td><td>13</td><td>685</td><td>ppr(2155:1)</td><td>0.8 (IQR 0.04)</td><td>0.69</td><td>0.89</td><td>6.99 (IQR 0.66)</td><td>0.05</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>PRRT1</td><td>ENST00000472641.1</td><td>cg24125828 0.75<br>cg13782301 0.74<br>cg18501647 0.73<br>cg14757228 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td><td>46</td><td>678</td><td>brnn(2245:1)</td><td>0.83 (IQR 0.03)</td><td>0.73</td><td>0.89</td><td>6.71 (IQR 0.59)</td><td>0.04</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>PRRT1</td><td>ENST00000375150.6</td><td>cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td><td>76</td><td>663</td><td>brnn(2973:1)</td><td>0.82 (IQR 0.03)</td><td>0.72</td><td>0.88</td><td>6.96 (IQR 0.61)</td><td>0.05</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>OBSCN</td><td>ENST00000662438.1</td><td>cg04193160 0.76<br>cg19261426 -0.72</td><td>55</td><td>714</td><td>ppr(3526:1)</td><td>0.8 (IQR 0.04)</td><td>0.64</td><td>0.88</td><td>0.8 (IQR 0.04)</td><td>0.07</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>CALB1</td><td>ENST00000518457.5</td><td>cg26290632 0.88</td><td>8</td><td>671</td><td>ppr(1412:1)</td><td>0.79 (IQR 0.04)</td><td>0.65</td><td>0.87</td><td>7.35 (IQR 0.64)</td><td>0.05</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>CALB1</td><td>ENST00000476853.1<br>ENST00000473670.1</td><td>cg26290632 0.88</td><td>8</td><td>710</td><td>ppr(2556:1)</td><td>0.79 (IQR 0.04)</td><td>0.68</td><td>0.87</td><td>7.35 (IQR 0.64)</td><td>0.04</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>CALB1</td><td>ENST00000265431.7</td><td>cg26290632 0.88</td><td>5</td><td>671</td><td>ppr(2053:1)</td><td>0.78 (IQR 0.04)</td><td>0.65</td><td>0.87</td><td>7.44 (IQR 0.68)</td><td>0.02</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>CALB1</td><td>ENST00000482702.5</td><td>cg26290632 0.88</td><td>7</td><td>710</td><td>ppr(2556:1)</td><td>0.79 (IQR 0.04)</td><td>0.67</td><td>0.86</td><td>7.41 (IQR 0.64)</td><td>0.03</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>PRRT1</td><td>ENST00000211413.10</td><td>cg24125828 0.76<br>cg13782301 0.74<br>cg14757228 0.73<br>cg18501647 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td><td>60</td><td>660</td><td>bagEarthGCV(3125:1)</td><td>0.83 (IQR 0.03)</td><td>0.74</td><td>0.9</td><td>6.63 (IQR 0.58)</td><td>0.05</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>PRRT1</td><td>ENST00000495191.5</td><td>cg24125828 0.76<br>cg13782301 0.74<br>cg18501647 0.73<br>cg14757228 0.73<br>cg27067781 0.73<br>cg11945824 0.72<br>cg22897615 0.70</td><td>62</td><td>660</td><td>bagEarthGCV(3125:1)</td><td>0.83 (IQR 0.03)</td><td>0.74</td><td>0.9</td><td>6.6 (IQR 0.58)</td><td>0.04</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>KCNS1</td><td>ENST00000537075.3</td><td>cg19702785 0.80</td><td>12</td><td>713</td><td>ppr(947:1)</td><td>0.78 (IQR 0.04)</td><td>0.65</td><td>0.88</td><td>7.44 (IQR 0.65)</td><td>0.06</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>VTRNA1-2</td><td>ENST00000689319.1</td><td>cg23500537 0.87</td><td>6</td><td>703</td><td>brnn(2065:1)</td><td>0.78 (IQR 0.03)</td><td>0.68</td><td>0.86</td><td>7.83 (IQR 0.61)</td><td>0.03</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>OTUD7A</td><td>ENST00000560598.2</td><td>cg04875128 0.84<br>cg01763090 0.81</td><td>11</td><td>714</td><td>bstTree(130:1)</td><td>0.79 (IQR 0.04)</td><td>0.65</td><td>0.87</td><td>7.43 (IQR 0.63)</td><td>0.05</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>PRRT1</td><td>ENST00000467780.5</td><td>cg24125828 0.74<br>cg13782301 0.74<br>cg18501647 0.73<br>cg14757228 0.73<br>cg27067781 0.72<br>cg11945824 0.71</td><td>41</td><td>686</td><td>brnn(3178:1)</td><td>0.77 (IQR 0.04)</td><td>0.65</td><td>0.86</td><td>7.82 (IQR 0.64)</td><td>0.06</td>
    </tr>
    <tr style="background-color:#eef7ff;">
      <td><b>CpG centered</b></td><td>SYNGR3</td><td>ENST00000248121.7</td><td>cg11220950 0.81</td><td>12</td><td>714</td><td>cubist(268:45)</td><td>0.77 (IQR 0.05)</td><td>0.63</td><td>0.85</td><td>7.8 (IQR 0.71)</td><td>0.03</td>
    </tr>
    <tr style="background-color:#fff4e6;">
      <td><b>ML centered</b></td><td>SPTBN4</td><td>ENST00000352632.7</td><td>cg05764628 0.69<br>cg02997982 0.68<br>cg17768491 0.68<br>cg09381003 0.68<br>cg02576468 0.68</td><td>42</td><td>682</td><td>bagEarthGCV(3523:1)</td><td>0.73 (IQR 0.04)</td><td>0.6</td><td>0.83</td><td>8.66 (IQR 0.67)</td><td>0.03</td>
    </tr>
  </tbody>
</table>

## Citation

*A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage).*  
https://doi.org/10.64898/2026.02.09.704870

If you use the generated table or workflow, please also cite ugPlot:  
https://doi.org/10.5281/zenodo.19824653
