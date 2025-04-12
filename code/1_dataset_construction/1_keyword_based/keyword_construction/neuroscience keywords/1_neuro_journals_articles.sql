-- Step 1 
-- Construct a table with all the journals in Neuroscience
SELECT DISTINCT LR.*, LD.EDiscipline, LD.ESpecialite
from pex.Liste_Revue as LR
LEFT JOIN pex.Liste_Discipline as LD on LD.Code_Discipline=LR.Code_discipline
WHERE Revue like '%BRAIN%' OR
Revue like '%NEURO%'
UNION ALL
SELECT DISTINCT LR.*,  LD.EDiscipline, LD.ESpecialite
from pex.Liste_Revue as LR
LEFT JOIN pex.Liste_Discipline as LD on LD.Code_Discipline=LR.Code_discipline
WHERE LR.Code_discipline = 51
-- save to neuroscience_journals.csv

-- Step 2
-- Construct a table with all the articles in Neuroscience
SELECT DISTINCT A.OST_BK
INTO BDMauro.dbo.neuroscience_OST_BK
FROM WoS.pex.Article as A
INNER JOIN BDMauro.dbo.neuroscience_journals as N ON A.Code_Revue = N.Code_Revue
-- saved to neuroscience_OST_BK.csv
