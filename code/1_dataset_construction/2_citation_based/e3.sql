--e3 code with discipline 77 in neuro_journals

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

--SELECT *
--FROM #temp_computer_science_articles
--ORDER BY OST_BK DESC

--check duplicated papers
--SELECT OST_BK
--FROM #temp_neuroscience_articles
--WHERE OST_BK IN (
 --SELECT OST_BK
    --FROM #temp_computer_science_articles
--);


-- Create temporal table with CS articles cited by NS articles
IF OBJECT_ID('tempdb..#temp_CITcscite_nscitant') IS NOT NULL
    DROP TABLE #temp_CITcscite_nscitant;

SELECT CS.OST_BK, COUNT(distinct cit.OST_BK_Citant) AS [Nb_citations_from_NS_to_CS] 
INTO #temp_CITcscite_nscitant
FROM [WoS].[pex].[Citations] AS cit
INNER JOIN #temp_computer_science_articles AS CS ON cit.OST_BK_Cite = CS.OST_BK
INNER JOIN #temp_neuroscience_articles AS NS ON cit.OST_BK_Citant = NS.OST_BK
GROUP BY CS.OST_BK;

--SELECT *
--FROM #temp_CITcscite_nscitant
--ORDER BY Nb_citations_from_NS_to_CS DESC

-- Create temporal table with NS articles cited by CS articles
IF OBJECT_ID('tempdb..#temp_CITnscite_cscitant') IS NOT NULL
    DROP TABLE #temp_CITnscite_cscitant;

SELECT NS.OST_BK, COUNT(distinct cit.OST_BK_Citant) AS [Nb_citations_from_CS_to_NS] 
INTO #temp_CITnscite_cscitant
FROM [WoS].[pex].[Citations] AS cit
INNER JOIN #temp_neuroscience_articles AS NS ON cit.OST_BK_Cite = NS.OST_BK
INNER JOIN #temp_computer_science_articles AS CS ON cit.OST_BK_Citant = CS.OST_BK
GROUP BY NS.OST_BK

--SELECT *
--FROM #temp_CITnscite_cscitant
--ORDER BY Nb_citations_from_CS_to_NS DESC;

-- Combines the citations counts from the two previous queries into a union table

IF OBJECT_ID('tempdb..#temp_strategy_table') IS NOT NULL
    DROP TABLE #temp_strategy_table;

SELECT DISTINCT OST_BK, 'CS' AS [origin], [Nb_citations_from_NS_to_CS] as Citations_from_other
INTO #temp_strategy_table
FROM #temp_CITcscite_nscitant
WHERE [Nb_citations_from_NS_to_CS]>= 1
UNION ALL
SELECT DISTINCT OST_BK, 'NS' AS [origin], [Nb_citations_from_CS_to_NS] as Citations_from_other
FROM #temp_CITnscite_cscitant
WHERE [Nb_citations_from_CS_to_NS]>= 1

SELECT*
FROM #temp_strategy_table
ORDER BY Citations_from_other DESC

--SELECT OST_BK, COUNT(*) AS count_duplicates
--FROM #temp_strategy_table
--GROUP BY OST_BK
--HAVING COUNT(*) > 1;

-- That's the end of the strategy. To add metadata select the appropriate query.