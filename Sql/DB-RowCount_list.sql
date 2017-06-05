IF OBJECT_ID(N'#counts', N'U') IS NOT NULL DROP TABLE #counts;

CREATE TABLE #counts (TableName VARCHAR(255), TableRows INT);

EXEC sp_MSForEachTable @command1='INSERT #counts (TableName, TableRows) SELECT ''?'', COUNT(*) FROM ?';

SELECT  TableName
        , TableRows
FROM    #counts
ORDER BY
        TableRows DESC
;
