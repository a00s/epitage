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

## Epitage transcript rankings

<style>
  .epitage-table {
    width: 100%;
    border-collapse: collapse;
    margin: 20px 0;
    font-size: 14px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    border-radius: 8px;
    overflow: hidden;
  }
  .epitage-table thead {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }
  .epitage-table thead th {
    padding: 16px;
    text-align: left;
    border: none;
  }
  .epitage-table tbody tr {
    border-bottom: 1px solid #e0e0e0;
    transition: background-color 0.2s ease;
  }
  .epitage-table tbody tr:hover {
    background-color: #f8f9ff;
  }
  .epitage-table tbody td {
    padding: 14px 16px;
    border: none;
  }
  .epitage-table tbody tr:last-child {
    border-bottom: none;
  }
  .badge-cpg {
    display: inline-block;
    background: #3b82f6;
    color: white;
    padding: 4px 12px;
    border-radius: 20px;
    font-size: 12px;
    font-weight: 600;
    margin-right: 8px;
  }
  .badge-ml {
    display: inline-block;
    background: #f97316;
    color: white;
    padding: 4px 12px;
    border-radius: 20px;
    font-size: 12px;
    font-weight: 600;
    margin-right: 8px;
  }
  .badge-both {
    display: inline-block;
    background: #8b5cf6;
    color: white;
    padding: 4px 12px;
    border-radius: 20px;
    font-size: 12px;
    font-weight: 600;
    margin-right: 8px;
  }
  .gene-name {
    font-weight: 600;
    color: #1f2937;
  }
  .transcript-list {
    font-size: 12px;
    color: #6b7280;
    margin-top: 4px;
    font-family: monospace;
  }
  .metric-excellent {
    color: #059669;
    font-weight: 600;
  }
  .metric-good {
    color: #2563eb;
    font-weight: 600;
  }
  .metric-fair {
    color: #f59e0b;
    font-weight: 600;
  }
</style>

<table class="epitage-table">
  <thead>
    <tr>
      <th>Type</th>
      <th>Gene</th>
      <th>Transcripts</th>
      <th>Best CpG Spearman</th>
      <th>Median ML R²</th>
      <th>CpGs</th>
      <th>Samples</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">ELOVL2</td>
      <td><div class="transcript-list">ENST00000456616.2<br>ENST00000667154.1<br>ENST00000657744.1</div></td>
      <td><span class="metric-excellent">0.95</span></td>
      <td><span class="metric-excellent">0.93</span></td>
      <td>7</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">ELOVL2</td>
      <td><div class="transcript-list">ENST00000354666.4</div></td>
      <td><span class="metric-excellent">0.95</span></td>
      <td><span class="metric-excellent">0.92</span></td>
      <td>7</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">ELOVL2</td>
      <td><div class="transcript-list">ENST00000667435.1<br>ENST00000607275.6</div></td>
      <td><span class="metric-excellent">0.95</span></td>
      <td><span class="metric-excellent">0.92</span></td>
      <td>8</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">FHL2</td>
      <td><div class="transcript-list">ENST00000409177.6</div></td>
      <td><span class="metric-excellent">0.93</span></td>
      <td><span class="metric-excellent">0.9</span></td>
      <td>9</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">FHL2</td>
      <td><div class="transcript-list">ENST00000393353.7<br>ENST00000344213.9<br>ENST00000530340.6<br>ENST00000358129.8<br>ENST00000408995.5<br>ENST00000322142.13</div></td>
      <td><span class="metric-excellent">0.93</span></td>
      <td><span class="metric-excellent">0.9</span></td>
      <td>18</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">TRIM59</td>
      <td><div class="transcript-list">ENST00000471155.5<br>ENST00000483754.1<br>ENST00000471396.1<br>ENST00000479460.5<br>ENST00000309784.9</div></td>
      <td><span class="metric-good">0.89</span></td>
      <td><span class="metric-good">0.83</span></td>
      <td>14</td>
      <td>636</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">TRIM59</td>
      <td><div class="transcript-list">ENST00000543469.1</div></td>
      <td><span class="metric-good">0.89</span></td>
      <td><span class="metric-good">0.81</span></td>
      <td>13</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">TRIM59</td>
      <td><div class="transcript-list">ENST00000494486.1</div></td>
      <td><span class="metric-good">0.89</span></td>
      <td><span class="metric-good">0.81</span></td>
      <td>13</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">TRIM59</td>
      <td><div class="transcript-list">ENST00000496222.1</div></td>
      <td><span class="metric-good">0.89</span></td>
      <td><span class="metric-good">0.81</span></td>
      <td>14</td>
      <td>636</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">CALB1</td>
      <td><div class="transcript-list">ENST00000518457.5</div></td>
      <td><span class="metric-good">0.88</span></td>
      <td><span class="metric-good">0.79</span></td>
      <td>8</td>
      <td>671</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">CALB1</td>
      <td><div class="transcript-list">ENST00000476853.1<br>ENST00000473670.1</div></td>
      <td><span class="metric-good">0.88</span></td>
      <td><span class="metric-good">0.79</span></td>
      <td>8</td>
      <td>710</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">CALB1</td>
      <td><div class="transcript-list">ENST00000265431.7</div></td>
      <td><span class="metric-good">0.88</span></td>
      <td><span class="metric-good">0.78</span></td>
      <td>5</td>
      <td>671</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">CALB1</td>
      <td><div class="transcript-list">ENST00000482702.5</div></td>
      <td><span class="metric-good">0.88</span></td>
      <td><span class="metric-good">0.79</span></td>
      <td>7</td>
      <td>710</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">VTRNA1-2</td>
      <td><div class="transcript-list">ENST00000689319.1</div></td>
      <td><span class="metric-good">0.87</span></td>
      <td><span class="metric-good">0.78</span></td>
      <td>6</td>
      <td>703</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">MIR29B2CHG</td>
      <td><div class="transcript-list">ENST00000655169.1</div></td>
      <td><span class="metric-good">-0.86</span></td>
      <td><span class="metric-good">0.83</span></td>
      <td>7</td>
      <td>687</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">MIR29B2CHG</td>
      <td><div class="transcript-list">ENST00000487977.2<br>ENST00000652846.1<br>ENST00000702741.1</div></td>
      <td><span class="metric-good">-0.86</span></td>
      <td><span class="metric-good">0.82</span></td>
      <td>5</td>
      <td>687</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">OBSCN</td>
      <td><div class="transcript-list">ENST00000422127.5<br>ENST00000680850.1<br>ENST00000570156.7</div></td>
      <td><span class="metric-good">0.76</span></td>
      <td><span class="metric-excellent">0.84</span></td>
      <td>69</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">OTUD7A</td>
      <td><div class="transcript-list">ENST00000560598.2</div></td>
      <td><span class="metric-good">0.84</span></td>
      <td><span class="metric-good">0.79</span></td>
      <td>11</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">KLF14</td>
      <td><div class="transcript-list">ENST00000583337.4</div></td>
      <td><span class="metric-good">0.83</span></td>
      <td><span class="metric-good">0.8</span></td>
      <td>13</td>
      <td>685</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">PRRT1</td>
      <td><div class="transcript-list">ENST00000472641.1</div></td>
      <td><span class="metric-good">0.75</span></td>
      <td><span class="metric-excellent">0.83</span></td>
      <td>46</td>
      <td>678</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">PRRT1</td>
      <td><div class="transcript-list">ENST00000211413.10</div></td>
      <td><span class="metric-good">0.76</span></td>
      <td><span class="metric-excellent">0.83</span></td>
      <td>60</td>
      <td>660</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">PRRT1</td>
      <td><div class="transcript-list">ENST00000495191.5</div></td>
      <td><span class="metric-good">0.76</span></td>
      <td><span class="metric-excellent">0.83</span></td>
      <td>62</td>
      <td>660</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">OBSCN</td>
      <td><div class="transcript-list">ENST00000636476.2</div></td>
      <td><span class="metric-good">0.76</span></td>
      <td><span class="metric-good">0.82</span></td>
      <td>57</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">PRRT1</td>
      <td><div class="transcript-list">ENST00000375150.6</div></td>
      <td><span class="metric-good">0.76</span></td>
      <td><span class="metric-good">0.82</span></td>
      <td>76</td>
      <td>663</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">SYNGR3</td>
      <td><div class="transcript-list">ENST00000248121.7</div></td>
      <td><span class="metric-good">0.81</span></td>
      <td><span class="metric-good">0.77</span></td>
      <td>12</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">OBSCN</td>
      <td><div class="transcript-list">ENST00000662438.1</div></td>
      <td><span class="metric-good">0.76</span></td>
      <td><span class="metric-good">0.8</span></td>
      <td>55</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">KCNS1</td>
      <td><div class="transcript-list">ENST00000537075.3</div></td>
      <td><span class="metric-good">0.8</span></td>
      <td><span class="metric-good">0.78</span></td>
      <td>12</td>
      <td>713</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">SLC1A6</td>
      <td><div class="transcript-list">ENST00000430939.6</div></td>
      <td><span class="metric-good">0.78</span></td>
      <td><span class="metric-fair">0.72</span></td>
      <td>14</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">TEKTL1</td>
      <td><div class="transcript-list">ENST00000292574.4</div></td>
      <td><span class="metric-good">0.78</span></td>
      <td><span class="metric-fair">0.72</span></td>
      <td>—</td>
      <td>—</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">RNF180</td>
      <td><div class="transcript-list">ENST00000504296.1</div></td>
      <td><span class="metric-fair">0.63</span></td>
      <td><span class="metric-good">0.77</span></td>
      <td>8</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">PRRT1</td>
      <td><div class="transcript-list">ENST00000467780.5</div></td>
      <td><span class="metric-good">0.74</span></td>
      <td><span class="metric-good">0.77</span></td>
      <td>41</td>
      <td>686</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">FOXG1-AS1</td>
      <td><div class="transcript-list">ENST00000546560.5</div></td>
      <td><span class="metric-good">0.77</span></td>
      <td><span class="metric-fair">0.71</span></td>
      <td>12</td>
      <td>683</td>
    </tr>
    <tr>
      <td><span class="badge-cpg">CpG</span></td>
      <td class="gene-name">TP73</td>
      <td><div class="transcript-list">ENST00000354437.8</div></td>
      <td><span class="metric-good">0.74</span></td>
      <td><span class="metric-fair">0.71</span></td>
      <td>27</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">SPTBN4</td>
      <td><div class="transcript-list">ENST00000352632.7</div></td>
      <td><span class="metric-fair">0.69</span></td>
      <td><span class="metric-good">0.73</span></td>
      <td>42</td>
      <td>682</td>
    </tr>
    <tr>
      <td><span class="badge-ml">ML</span></td>
      <td class="gene-name">CACNA1G</td>
      <td><div class="transcript-list">ENST00000359106.10</div></td>
      <td><span class="metric-good">0.7</span></td>
      <td><span class="metric-good">0.75</span></td>
      <td>41</td>
      <td>714</td>
    </tr>
    <tr>
      <td><span class="badge-both">CpG & ML</span></td>
      <td class="gene-name">PPM1N</td>
      <td><div class="transcript-list">ENST00000415077.1</div></td>
      <td><span class="metric-fair">0.66</span></td>
      <td><span class="metric-fair">0.66</span></td>
      <td>—</td>
      <td>—</td>
    </tr>
    <tr>
      <td>—</td>
      <td class="gene-name">ZIK1</td>
      <td><div class="transcript-list">ENST00000597219.1</div></td>
      <td><span class="metric-good">0.82</span></td>
      <td>—</td>
      <td>—</td>
      <td>—</td>
    </tr>
    <tr>
      <td>—</td>
      <td class="gene-name">CELF6</td>
      <td><div class="transcript-list">ENST00000567083.2</div></td>
      <td><span class="metric-good">0.81</span></td>
      <td>—</td>
      <td>—</td>
      <td>—</td>
    </tr>
    <tr>
      <td>—</td>
      <td class="gene-name">CACNA1G</td>
      <td><div class="transcript-list">ENST00000442258.6</div></td>
      <td><span class="metric-good">0.7</span></td>
      <td>—</td>
      <td>—</td>
      <td>—</td>
    </tr>
  </tbody>
</table>

## Citation

*A machine learning approach to identify key Epigenetic Transcripts for Ageing research in human blood (Epitage).*  
https://doi.org/10.64898/2026.02.09.704870
