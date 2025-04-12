--e2

-- Construct a table with all the articles in Neuroscience
IF OBJECT_ID('tempdb..#temp_neuroscience_articles') IS NOT NULL
    DROP TABLE #temp_neuroscience_articles;

SELECT DISTINCT A.OST_BK
INTO #temp_neuroscience_articles
FROM WoS.pex.Article AS A
INNER JOIN BDMendez.dbo.neuro_journals AS N ON A.Code_Revue = N.Code_Revue;

-- Drop temporal table if it exists
IF OBJECT_ID('tempdb..#temp_computer_science_articles') IS NOT NULL
    DROP TABLE #temp_computer_science_articles;

-- Construct a table with all the articles in Computer Science
SELECT DISTINCT OST_BK
INTO #temp_computer_science_articles
FROM WoS.pex.Article
WHERE Code_Discipline = '77';

-- Drop temporal table if it exists
IF OBJECT_ID('tempdb..#TempCompuxNeuro') IS NOT NULL
    DROP TABLE #TempCompuxNeuro;

SELECT DISTINCT kw.OST_BK
INTO #TempCompuxNeuro
FROM WoS.dbo.Keyword AS kw
INNER JOIN #temp_computer_science_articles AS ca ON kw.OST_BK = ca.OST_BK
INNER JOIN BDMendez.dbo.selected_100_neuro_keywords AS nk ON kw.Keyword = nk.Keyword;

-- Drop temporal table if it exists
IF OBJECT_ID('tempdb..#TempNeuroxCompu') IS NOT NULL
    DROP TABLE #TempNeuroxCompu;

SELECT DISTINCT kw.OST_BK
INTO #TempNeuroxCompu
FROM WoS.dbo.Keyword AS kw
INNER JOIN #temp_neuroscience_articles AS ca ON kw.OST_BK = ca.OST_BK
INNER JOIN BDMendez.dbo.selected_100_compu_keywords AS nk ON kw.Keyword = nk.Keyword;

-- Drop temporal table if it exists
IF OBJECT_ID('tempdb..#temp_strategy_table') IS NOT NULL
    DROP TABLE #temp_strategy_table;

-- Construct a table combining Computer Science and Neuroscience articles
SELECT *
INTO #temp_strategy_table
FROM #TempCompuxNeuro
UNION ALL
SELECT *
FROM #TempNeuroxCompu;

-- Retrieve the final strategy table
SELECT *
FROM #temp_strategy_table;
