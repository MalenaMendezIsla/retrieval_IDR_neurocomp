--e1

-- Create temporal table with all articles that use CS keywords
IF OBJECT_ID('tempdb..#TempCompu') IS NOT NULL
    DROP TABLE #TempCompu;

SELECT DISTINCT C.OST_BK
INTO #TempCompu
FROM WoS.dbo.Keyword AS C
INNER JOIN BDMendez.dbo.selected_100_compu_keywords AS CK ON UPPER(C.Keyword) = UPPER(CK.Keyword)
--ORDER BY OST_BK;

--SELECT *
--FROM #TempCompu

-- Create temporal table with all articles that use NS keywords
IF OBJECT_ID('tempdb..#TempNeuro') IS NOT NULL
    DROP TABLE #TempNeuro;

SELECT DISTINCT N.OST_BK
INTO #TempNeuro
FROM WoS.dbo.Keyword AS N
INNER JOIN BDMendez.dbo.selected_100_neuro_keywords AS NK ON UPPER(N.Keyword) = UPPER(NK.Keyword)

--SELECT *
--FROM #TempNeuro

-- Create the intersection of those tables
IF OBJECT_ID('tempdb..#temp_strategy_table') IS NOT NULL
    DROP TABLE #temp_strategy_table;

SELECT DISTINCT N.OST_BK
INTO #temp_strategy_table
FROM #TempNeuro AS N
INNER JOIN #TempCompu AS C ON N.OST_BK = C.OST_BK;

--SELECT *
--FROM #temp_strategy_table

-- That's the end of the strategy. To add metadata select the appropriate query.