-- Make sure the final table of the estrategy is named #temp_strategy_table

--Check in final table construction lines 115: --Uncoment for E3 and E4
--E3 ,	E.Citations_from_other as CitRef_other,
--E4  , E.References_from_other as CitRef_other,
--	E.origin

--You have to run the strategy part first, and then this chunk, because if you don't, the temp tables (which construct journals and articles tables here) will be repeated in E3 and E4, and dropping the table won't work.


-- Construct a table with all the articles in Neuroscience
IF OBJECT_ID('tempdb..#temp_neuroscience_articles') IS NOT NULL
    DROP TABLE #temp_neuroscience_articles;

SELECT DISTINCT A.OST_BK
INTO #temp_neuroscience_articles
FROM WoS.pex.Article as A
INNER JOIN BDMendez.dbo.neuro_journals as N ON A.Code_Revue = N.Code_Revue

-- Construct a table with all the articles in Computer Science
IF OBJECT_ID('tempdb..#temp_computer_science_articles') IS NOT NULL
    DROP TABLE #temp_computer_science_articles;

SELECT DISTINCT OST_BK, 'computer_science' AS [group]
INTO #temp_computer_science_articles
FROM WoS.pex.Article
WHERE Code_Discipline='77';

-- Number of NEURO CITATIONS
IF OBJECT_ID('tempdb..#temp_neuro_citations') IS NOT NULL
    DROP TABLE #temp_neuro_citations;
    
SELECT E.OST_BK,
       CASE WHEN COUNT(NS.OST_BK) IS NULL THEN 0 ELSE COUNT(distinct NS.OST_BK) END AS Nb_Citations_NS
INTO #temp_neuro_citations
FROM #temp_strategy_table AS E
LEFT JOIN WoS.pex.Citations AS C ON C.OST_BK_Cite = E.OST_BK
INNER JOIN #temp_neuroscience_articles AS NS ON C.OST_BK_Citant = NS.OST_BK
GROUP BY E.OST_BK;

-- Number of COMPU CITATIONS
IF OBJECT_ID('tempdb..#temp_compu_citations') IS NOT NULL
    DROP TABLE #temp_compu_citations;

SELECT E.OST_BK,
       CASE WHEN COUNT(distinct CS.OST_BK) IS NULL THEN 0 ELSE COUNT(distinct CS.OST_BK) END AS Nb_Citations_CS
INTO #temp_compu_citations
FROM #temp_strategy_table AS E
LEFT JOIN WoS.pex.Citations AS C ON C.OST_BK_Cite = E.OST_BK
INNER JOIN #temp_computer_science_articles AS CS ON C.OST_BK_Citant = CS.OST_BK
GROUP BY E.OST_BK;

--SELECT * FROM #temp_compu_citations
--ORDER BY Nb_Citations_CS DESC


-- Number of NEURO REFERENCES
IF OBJECT_ID('tempdb..#temp_neuro_references') IS NOT NULL
    DROP TABLE #temp_neuro_references;

SELECT E.OST_BK,
       CASE WHEN COUNT(NS.OST_BK) IS NULL THEN 0 ELSE COUNT(distinct NS.OST_BK) END AS Nb_References_NS
INTO #temp_neuro_references
FROM #temp_strategy_table AS E
LEFT JOIN wos.pex.Citations AS C ON C.OST_BK_Citant = E.OST_BK
INNER JOIN #temp_neuroscience_articles AS NS ON C.OST_BK_Cite = NS.OST_BK
GROUP BY E.OST_BK;

-- Number of COMPU REFERENCES
IF OBJECT_ID('tempdb..#temp_compu_references') IS NOT NULL
    DROP TABLE #temp_compu_references;
	
SELECT E.OST_BK,
       CASE WHEN COUNT(CS.OST_BK) IS NULL THEN 0 ELSE COUNT(distinct CS.OST_BK) END AS Nb_References_CS
INTO #temp_compu_references
FROM #temp_strategy_table AS E
LEFT JOIN wos.pex.Citations AS C ON C.OST_BK_Citant = E.OST_BK
INNER JOIN #temp_computer_science_articles AS CS ON C.OST_BK_Cite = CS.OST_BK
GROUP BY E.OST_BK;

-- Create final table
IF OBJECT_ID('tempdb..#E_metadata') IS NOT NULL
    DROP TABLE #E_metadata

SELECT DISTINCT 
    E.OST_BK, 
    A.Annee_Bibliographique AS Year, 
	A.Code_Revue , 
	R.Revue , 
    A.Code_Discipline,
    D.EDiscipline AS Discipline,
    D.ESpecialite AS Specialty,
    C.Cit_ALL_iac,
    TNC.Nb_Citations_NS,
    TCC.Nb_Citations_CS,
    A.Nb_Reference, 
    TNR.Nb_References_NS,
    TCR.Nb_References_CS
--Uncoment for E3 and E4
--E3 ,	E.Citations_from_other as CitRef_other,
--E4  , E.References_from_other as CitRef_other,
--	E.origin
INTO #E_metadata
FROM #temp_strategy_table AS E
INNER JOIN wos.pex.article AS A ON E.OST_BK = A.OST_BK
INNER JOIN wos.pex.Liste_discipline AS D ON A.Code_discipline = D.Code_Discipline
LEFT JOIN wos.pex.citations_relatives AS C ON E.OST_BK = C.OST_BK
LEFT JOIN wos.pex.Liste_revue as R ON A.Code_revue=R.Code_Revue
LEFT JOIN #temp_neuro_citations AS TNC ON E.OST_BK = TNC.OST_BK
LEFT JOIN #temp_compu_citations AS TCC ON E.OST_BK = TCC.OST_BK
LEFT JOIN #temp_neuro_references AS TNR ON E.OST_BK = TNR.OST_BK
LEFT JOIN #temp_compu_references AS TCR  ON E.OST_BK = TCR.OST_BK;

SELECT *
FROM #E_metadata
ORDER BY Cit_ALL_iac DESC;