-- Drop temporal tables
IF OBJECT_ID('tempdb..#temp_computer_science_articles') IS NOT NULL
    DROP TABLE #temp_computer_science_articles;
IF OBJECT_ID('tempdb..#keywords_CS') IS NOT NULL
    DROP TABLE #keywords_CS;
IF OBJECT_ID('tempdb..#keywords_freqCS') IS NOT NULL
    DROP TABLE #keywords_freqCS;


-- Construct a table with all the articles in Computer Science
SELECT DISTINCT OST_BK
INTO #temp_computer_science_articles
FROM WoS.pex.Article
WHERE Code_Discipline='77';

-- Find computer science keywords
SELECT Keyword , K.OST_BK 
INTO #keywords_CS
FROM [WoS].[dbo].[Keyword] as K
INNER JOIN #temp_computer_science_articles as CS ON CS.OST_BK = K.OST_BK 

-- Count frequency of each keyword
SELECT Keyword, COUNT(DISTINCT [OST_BK]) as N
INTO #keywords_freqCS
FROM #keywords_CS
GROUP BY Keyword
ORDER BY N desc

SELECT TOP 1000 *
FROM #keywords_freqCS
order by N desc

