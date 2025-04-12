--Drop temporal tables
IF OBJECT_ID('tempdb..#keywords_NS') IS NOT NULL
    DROP TABLE #keywords_NS;
IF OBJECT_ID('tempdb..#keywords_plus_NS') IS NOT NULL
    DROP TABLE #keywords_plus_NS;

IF OBJECT_ID('tempdb..#keywords_freqNS') IS NOT NULL
    DROP TABLE #keywords_freqNS;
IF OBJECT_ID('tempdb..#keywords_plus_freqNS') IS NOT NULL
    DROP TABLE #keywords_plus_freqNS;


-- Find neuroscience keywords and keywords plus
SELECT Keyword , K.OST_BK 
INTO #keywords_NS
  FROM [WoS].[dbo].[Keyword] as K
  INNER JOIN BDMauro.dbo.neuroscience_OST_BK as NS ON NS.OST_BK = K.OST_BK 

SELECT Keyword, KP.OST_BK 
INTO #keywords_plus_NS
  FROM [WoS].[dbo].[Keyword_Plus] as KP
  INNER JOIN BDMauro.dbo.neuroscience_OST_BK as NS ON NS.OST_BK = KP.OST_BK 


-- Count frequency of each keyword
SELECT Keyword, COUNT(DISTINCT [OST_BK]) as N
INTO #keywords_freqNS
  FROM #keywords_NS
  GROUP BY Keyword
  ORDER BY N desc

SELECT TOP 1000 *
	FROM #keywords_freqNS
	order by N desc

-- saved to: https://www.dropbox.com/scl/fi/8h1hcghbt0tftwic0dv2e/neuro_top1000_keywords.csv?rlkey=31nkjmdttq4gshnakepsa66pr&st=i8vkhwv1&dl=0
-- 100 of this keywords where selected for the keyword based strategies
-- saved to: https://www.dropbox.com/scl/fi/7a9l4skkov2jtyrm3qmdm/selected_top_100_neuro_keywords.csv?rlkey=0ftjd8fojwoyxmnch16hx1rxo&st=q4cag5ed&dl=0


-- Count frequency of each keyword plus
SELECT Keyword, COUNT(DISTINCT [OST_BK]) as N
INTO #keywords_plus_freqNS
  FROM #keywords_plus_NS
  GROUP BY Keyword
  ORDER BY N desc

SELECT TOP 1000 *
	FROM #keywords_plus_freqNS
	order by N desc

-- saved to: https://www.dropbox.com/scl/fi/dibfqncr3zf6xtodcnn1s/neuro_top1000_keywords_plus.csv?rlkey=dolhydpbciyfing2l6xzfbxcx&st=qaov40sw&dl=0
-- 100 of this keywords where selected for the keyword based strategies
-- saved to: https://www.dropbox.com/scl/fi/kbtzj2qi6rc4rlluwdwee/selected_top_100_neuro_keywords_plus.csv?rlkey=a4g1lin3m8jx7q3rtvs2jkfuj&st=q0pozlnu&dl=0