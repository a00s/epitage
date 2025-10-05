SELECT
  tb.cpg,
  ROUND(AVG(CASE WHEN ts.age BETWEEN 14 AND 30 THEN tb.value END), 4) AS avg_G1,
  ROUND(AVG(CASE WHEN ts.age BETWEEN 31 AND 60 THEN tb.value END), 4) AS avg_G2,
  ROUND(AVG(CASE WHEN ts.age > 60 THEN tb.value END), 4) AS avg_G3,
  ROUND((AVG(CASE WHEN ts.age > 60 THEN tb.value END) - AVG(CASE WHEN ts.age < 31 THEN tb.value END)), 4) AS diff_G3_G1
FROM
  tatu_betas tb
JOIN
  tatu_samples ts ON ts.sample = tb.sample
GROUP BY
  tb.cpg
HAVING
  ABS(diff_G3_G1) > 0.2
ORDER BY
  diff_G3_G1 DESC;
