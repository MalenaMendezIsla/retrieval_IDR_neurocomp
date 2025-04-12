SELECT DISTINCT LR.*, LD.EDiscipline, LD.ESpecialite
INTO #neuro_journals
from wos.pex.Liste_Revue as LR
LEFT JOIN wos.pex.Liste_Discipline as LD on LD.Code_Discipline=LR.Code_discipline
WHERE Revue like '%BRAIN%' OR
Revue like '%NEURO%'
UNION ALL
SELECT DISTINCT LR.*,  LD.EDiscipline, LD.ESpecialite
from wos.pex.Liste_Revue as LR
LEFT JOIN wos.pex.Liste_Discipline as LD on LD.Code_Discipline=LR.Code_discipline
WHERE LR.Code_discipline = 51;

SELECT  j.Code_Revue, j.Revue, COUNT(DISTINCT OST_BK) as n
FROM #neuro_journals j
INNER JOIN wos.pex.Article a on a.Code_Revue=j.Code_Revue
GROUP BY j.Code_Revue, j.Revue
ORDER BY n desc

-- save to neuro_journals.csv