--e4 with code discipline 77 in neuro_journals

-- Construct a table with all the articles in Neuroscience
IF OBJECT_ID('tempdb..#temp_neuroscience_articles') IS NOT NULL
    DROP TABLE #temp_neuroscience_articles;

SELECT DISTINCT A.OST_BK
INTO #temp_neuroscience_articles
FROM WoS.pex.Article AS A
INNER JOIN BDMendez.dbo.neuro_journals AS N ON A.Code_Revue = N.Code_Revue;

-- Construct a table with all the articles in Computer Science
IF OBJECT_ID('tempdb..#temp_computer_science_articles') IS NOT NULL
    DROP TABLE #temp_computer_science_articles;

SELECT DISTINCT OST_BK, 'computer_science' AS [group]
INTO #temp_computer_science_articles
FROM WoS.pex.Article
WHERE Code_Discipline='77';

-- Selects computer science articles that reference neuroscience articles and counts the references
IF OBJECT_ID('tempdb..#temp_REFcscitant_nscite') IS NOT NULL
    DROP TABLE #temp_REFcscitant_nscite;

SELECT CS.OST_BK, COUNT(distinct cit.OST_BK_Cite) AS [Nb_references_from_CS_to_NS] 
INTO #temp_REFcscitant_nscite
FROM [WoS].[pex].[Citations] AS cit
INNER JOIN #temp_computer_science_articles AS CS ON cit.OST_BK_Citant = CS.OST_BK
INNER JOIN #temp_neuroscience_articles AS NS ON cit.OST_BK_Cite = NS.OST_BK
GROUP BY CS.OST_BK;

--SELECT *
--FROM #temp_REFcscitant_nscite
--ORDER BY Nb_references_from_CS_to_NS DESC

-- Selects neuroscience articles that reference computer science articles and counts the references
IF OBJECT_ID('tempdb..#temp_REFnscitant_cscite') IS NOT NULL
    DROP TABLE #temp_REFnscitant_cscite;

SELECT NS.OST_BK, COUNT(distinct cit.OST_BK_Cite) AS [Nb_references_from_NS_to_CS] 
INTO #temp_REFnscitant_cscite
FROM [WoS].[pex].[Citations] AS cit
INNER JOIN #temp_neuroscience_articles AS NS ON cit.OST_BK_Citant = NS.OST_BK
INNER JOIN #temp_computer_science_articles AS CS ON cit.OST_BK_Cite = CS.OST_BK
GROUP BY NS.OST_BK

--SELECT *
--FROM #temp_REFnscitant_cscite
--ORDER BY Nb_references_from_NS_to_CS DESC;

-- Combines the citations counts from the two previous queries into a union table
IF OBJECT_ID('tempdb..#temp_strategy_table') IS NOT NULL
    DROP TABLE #temp_strategy_table;

SELECT OST_BK, 'CS' AS [origin], [Nb_references_from_CS_to_NS] as References_from_other
INTO #temp_strategy_table
FROM #temp_REFcscitant_nscite 
WHERE [Nb_references_from_CS_to_NS]>= 1
UNION ALL
SELECT OST_BK, 'NS' AS [origin], [Nb_references_from_NS_to_CS] as References_from_other
FROM #temp_REFnscitant_cscite
WHERE [Nb_references_from_NS_to_CS]>= 1

SELECT*
FROM #temp_strategy_table
ORDER BY References_from_other DESC

-- That's the end of the strategy. To add metadata select the appropriate query.