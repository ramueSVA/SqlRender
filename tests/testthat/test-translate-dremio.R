library("testthat")

# For debugging: force reload of patterns:
# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')

expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equivalent(string1, string2)
}

expect_match_ignore_spaces <- function(string1, regexp) {
  string1 <- gsub(" +", " ", string1)
  expect_match(string1, regexp)
}

test_that("translate sql server -> Dremio select random row using hash", {
  sql <- translate("SELECT column1 FROM (SELECT column1, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) FROM mytable) tmp WHERE rn <= 1",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column1 FROM (SELECT column1, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) FROM mytable) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Dremio SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER(ORDER BY CONVERT(VARBINARY, val, 1)) rn FROM mytable WHERE rn <= 1",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT ROW_NUMBER() OVER(ORDER BY CAST(CONCAT('x', val) AS VARBINARY)) rn FROM mytable WHERE rn <= 1"
  )
})

test_that("translate sql server -> Dremio clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "-- dremio does not support indexes")
})

test_that("translate sql server -> Dremio index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "-- dremio does not support indexes")
})

test_that("translate sql server -> Dremio USE", {
  sql <- translate("USE schema;", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "USE schema;")
})

test_that("translate sql server -> Dremio INSERT INTO WITH ", {
  sql <- translate("WITH a AS (SELECT * FROM b) INSERT INTO c SELECT * FROM a;", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "INSERT INTO c WITH a AS (SELECT * FROM b) SELECT * FROM a;")
})

test_that("translate sql server -> Dremio WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d \nAS\nWITH cte1  AS (SELECT a FROM b)  SELECT\nc\nFROM\ncte1;"
  )
})

test_that("translate sql server -> Dremio WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;", targetDialect = "dremio")
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d AS\nSELECT\nc ;"
  )
})

test_that("translate sql server -> Dremio location reserved word", {
  sql <- translate("select count(1) from omop_cdm.location;", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "select count(1) from omop_cdm.location;")
})

test_that("translate sql server -> Dremio TOP in subqueries", {
  sql <- translate("select statistic_value from achilles_results join (SELECT TOP 1 count(*) as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003)",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "select statistic_value from achilles_results join (SELECT  count(*) as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003)"
  )
})

# Also added type conversion: STRING to VARCHAR, SMALLINT to INT, INYINT to INT, REAL to DOUBLE
test_that("translate sql server -> Dremio CREATE TABLE with NOT NULL", {
  sql <- translate("CREATE TABLE a (c1 BIGINT NOT NULL, c2 BOOLEAN NOT NULL, c3 CHAR NOT NULL, c4 DECIMAL NOT NULL, c5 DOUBLE NOT NULL, c6 FLOAT NOT NULL, c7 INT NOT NULL, c8 REAL NOT NULL, c9 SMALLINT NOT NULL, c10 STRING NOT NULL, c11 TIMESTAMP NOT NULL, c12 TINYINT NOT NULL, c13 VARCHAR(10) NOT NULL, c14 DATE NOT NULL, c15 DATETIME NOT NULL, c16 INTEGER NOT NULL)",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a (c1 BIGINT NOT NULL, c2 BOOLEAN NOT NULL, c3 CHAR NOT NULL, c4 DECIMAL NOT NULL, c5 DOUBLE NOT NULL, c6 FLOAT NOT NULL, c7 INT NOT NULL, c8 DOUBLE NOT NULL, c9 INT NOT NULL, c10 VARCHAR NOT NULL, c11 TIMESTAMP NOT NULL, c12 INT NOT NULL, c13 VARCHAR(10) NOT NULL, c14 DATE NOT NULL, c15 TIMESTAMP NOT NULL, c16 INTEGER NOT NULL)"
  )
})

test_that("translate sql server -> Dremio CREATE TABLE with NULL", {
  sql <- translate("CREATE TABLE a (c1 BIGINT NULL, c2 BOOLEAN NULL, c3 CHAR NULL, c4 DECIMAL NULL, c5 DOUBLE NULL, c6 FLOAT NULL, c7 INT NULL, c8 REAL NULL, c9 SMALLINT NULL, c10 STRING NULL, c11 TIMESTAMP NULL, c12 TINYINT NULL, c13 VARCHAR(10) NULL, c14 DATE NULL, c15 DATETIME NULL)",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a (c1 BIGINT NULL, c2 BOOLEAN NULL, c3 CHAR NULL, c4 DECIMAL NULL, c5 DOUBLE NULL, c6 FLOAT NULL, c7 INT NULL, c8 DOUBLE NULL, c9 INT NULL, c10 VARCHAR NULL, c11 TIMESTAMP NULL, c12 INT NULL, c13 VARCHAR(10) NULL, c14 DATE NULL, c15 TIMESTAMP NULL)"
  )
})

test_that("translate sql server -> Dremio clause with NOT NULL", {
  sql <- translate("SELECT * FROM x WHERE y IS NOT NULL", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT * FROM x WHERE y IS NOT NULL")
})

test_that("translate sql server -> Dremio CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 TIMESTAMP CONSTRAINT a_c1_def DEFAULT NOW())",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP)")
})

test_that("translate sql server -> Dremio CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 TIMESTAMP DEFAULT NOW())", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP)")
})

test_that("translate sql server -> Dremio DATEFROMPARTS()", {
  sql <- translate("SELECT DATEFROMPARTS(1977, 10, 12)", targetDialect = "dremio")
  expect_equal_ignore_spaces(
    sql,
    "SELECT TO_DATE(TO_CHAR(1977,'0000') || '-' || TO_CHAR (10, '00') || '-' || TO_CHAR (12 , '00') , 'YYYY-MM-DD') "
  )
})

test_that("translate sql server -> Dremio EOMONTH()", {
  sql <- translate("SELECT eomonth(payer_plan_period_start_date) AS obs_month_end",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT LAST_DAY(payer_plan_period_start_date) AS obs_month_end"
  )
})

test_that("translate sql server -> Dremio ISNUMERIC", {
  sql <- translate("SELECT ISNUMERIC(a) FROM b", targetDialect = "dremio")
  expect_equal_ignore_spaces(
    sql,
    "SELECT ISNUMERIC(a) FROM b"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "dremio")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE ISNUMERIC(a) = 1"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "dremio")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE ISNUMERIC(a) = 0"
  )
})

test_that("translate sql server -> Dremio CEILING", {
  sql <- translate("SELECT CEILING(0.1);", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT CEILING(0.1);")
})

test_that("translate sql server -> Dremio TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT  * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> Dremio NEWID()", {
  sql <- translate("SELECT *, NEWID() FROM my_table;", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT *, UUID() FROM my_table;")
})

test_that("translate sql server -> Dremio drvd()", {
  sql <- translate("SELECT
      TRY_CAST(name AS VARCHAR(MAX)) AS name,
      TRY_CAST(speed AS FLOAT) AS speed
    FROM (  VALUES ('A', 1.0), ('B', 2.0)) AS drvd(name, speed);", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT\n      CAST(name AS VARCHAR) AS name,\n      CAST(speed AS FLOAT) AS speed\n    FROM (SELECT NULL AS name, NULL AS speed WHERE (0 = 1) UNION ALL VALUES ('A', 1.0), ('B', 2.0)) AS values_table;")
})

test_that("translate sql server -> Dremio data types", {
  sql <- translate("CREATE TABLE a (c1 DOUBLE PRECISION)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "CREATE TABLE a (c1 DOUBLE)")
})

test_that("translate sql server -> Dremio DELETE FROM WHERE", {
  sql <- translate("DELETE FROM ACHILLES_results WHERE analysis_id IN (1, 2, 3)",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "DELETE FROM ACHILLES_results WHERE analysis_id IN (1, 2, 3)"
  )
})

test_that("translate sql server -> Dremio RIGHT functions", {
  sql <- translate("SELECT RIGHT(x,4)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT RIGHT(x,4)")
})

test_that("translate sql server -> Dremio DELETE FROM", {
  sql <- translate("DELETE FROM ACHILLES_results", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "DELETE FROM ACHILLES_results")
})

test_that("translate sql server -> Dremio UNION ORDER BY", {
  sql <- translate("SELECT * (SELECT a FROM b UNION SELECT a FROM c) ORDER BY a", targetDialect = "dremio")
  expect_equal_ignore_spaces(
    sql,
    "SELECT * (SELECT a FROM b UNION SELECT a FROM c) ORDER BY a"
  )
})

test_that("translate sql server -> Dremio DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS cohort;")
})

test_that("translate sql server -> Dremio create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translate sql server -> Dremio CAST(AS DATE)", {
  sql <- translate("SELECT CAST('2000-01-01' AS DATE)", targetDialect = "dremio")
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST('2000-01-01' AS DATE)"
  )
})

test_that("translate sql server -> Dremio DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(CAST(drug_era_end_date AS DATE),CAST(drug_era_start_date AS DATE)) FROM drug_era"
  )
})

test_that("translate sql server -> Dremio DATEDIFF (MONTH)", {
  sql <- translate("SELECT DATEDIFF(month,drug_era_start_date,drug_era_end_date) FROM drug_era",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "SELECT CAST(MONTHS_BETWEEN(CAST(drug_era_end_date AS DATE),CAST(drug_era_start_date AS DATE)) AS INTEGER) FROM drug_era")
})

test_that("translate sql server -> Dremio DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATE_ADD(CAST(drug_era_end_date AS DATE),30) FROM drug_era"
  )
})

test_that("translate sql server -> Dremio CHARINDEX from position", {
  sql <- translate("SELECT CHARINDEX('test','abctest') FROM table", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT INSTR('abctest','test') FROM table")
})

test_that("translate sql server -> Dremio COUNT", {
  sql <- translate("SELECT COUNT_BIG('test') FROM table", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT COUNT('test') FROM table")
})

test_that("translate sql server -> Dremio left SUBSTR", {
  sql <- translate("SELECT LEFT('test',3)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT LEFT('test',3)")
})

test_that("translate sql server -> Dremio right SUBSTR", {
  sql <- translate("SELECT RIGHT('test',3)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT RIGHT('test',3)")
})

test_that("translate sql server -> Dremio LENGTH", {
  sql <- translate("SELECT LEN('test')", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT LENGTH('test')")
})

test_that("translate sql server -> Dremio LN", {
  sql <- translate("SELECT LOG(10)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT LN(CAST((10) AS DOUBLE))")
})

test_that("translate sql server -> Dremio ROUND", {
  sql <- translate("SELECT ROUND('100.2564', 2)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT ROUND(CAST('100.2564' AS DOUBLE),2)")
})

test_that("translate sql server -> Dremio SQUARE", {
  sql <- translate("SELECT SQUARE(2)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT ((2)*(2))")
})

test_that("translate sql server -> Dremio STDDEV", {
  sql <- translate("SELECT STDEV(4)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT STDDEV_POP(4)")
})

test_that("translate sql server -> Dremio VARIANCE", {
  sql <- translate("SELECT VAR(4)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT VARIANCE(4)")
})

test_that("translate sql server -> Dremio DATE_ADD month", {
  sql <- translate("SELECT DATEADD(month,3,CAST(drug_era_end_date AS DATE)) FROM drug_era",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATE_ADD(drug_era_end_date, CAST(3 AS INTERVAL MONTH)) FROM drug_era"
  )
})

test_that("translate sql server -> Dremio DROP with definition", {
  sql <- translate("DROP TABLE IF EXISTS test.testing (id int)", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test.testing ")
})

test_that("translate sql server -> Dremio IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table", targetDialect = "dremio")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN a>b THEN 1 ELSE b END AS max_val FROM table")
})

test_that("translate sql server -> Dremio cross join", {
  sql <- translate(
    sql = "SELECT a from (select b) x, (select c) y",
    targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "SELECT a from (select b) x, (select c) y")
  #expect_equal_ignore_spaces(sql, "SELECT a  FROM (select b) x cross join (select c) y")
})

test_that("translate sql server -> Dremio CTAS with distribute_on_key", {
  sql <- translate(
    sql = "--HINT DISTRIBUTE_ON_KEY(key)
                          SELECT a INTO b FROM c;",
    targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, " CREATE TABLE b AS \n SELECT \n a \n FROM \n c;")
})


test_that("translate sql server -> Dremio varchar", {
  sql <- translate("VARCHAR(MAX)",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "VARCHAR")

  sql <- translate("VARCHAR",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "VARCHAR")

  sql <- translate("VARCHAR(100)",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "VARCHAR(100)")
})

test_that("translate sql server -> Dremio datetime", {
  sql <- translate("DATETIME",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "TIMESTAMP")

  sql <- translate("DATETIME2",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Dremio table admin", {
  sql <- translate("CREATE CLUSTERED INDEX index_name ON some_table (variable);",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "-- dremio does not support indexes")

  sql <- translate("CREATE UNIQUE CLUSTERED INDEX index_name ON some_table (variable);",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "-- dremio does not support indexes")

  sql <- translate("PRIMARY KEY NONCLUSTERED",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "-- dremio does not support primary keys")

  sql <- translate("UPDATE STATISTICS test;",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> Dremio datetime from parts", {
  sql <- translate("select datetimefromparts('2019', '01', '01', '12', '15', '30', '01')",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "select TO_TIMESTAMP(TO_CHAR('2019','0000') || '-' || TO_CHAR('01','00') || '-' || TO_CHAR( '01','00') || ' ' || TO_CHAR('12','00') || ':' || TO_CHAR('15','00') || ':' || TO_CHAR('30','00'),'YYYY-MM-DD HH24:MI:SS')")
})


test_that("translate sql server -> Dremio concat", {
  sql <- translate("select 'oh' + 'dsi'",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "select 'oh' || 'dsi'")
})

test_that("translate sql server -> Dremio cast varchar and concat", {
  sql <- translate("select cast('test' as VARCHAR(10)) + 'ing'",
                   targetDialect = "dremio"
  )
  expect_equal_ignore_spaces(sql, "select cast('test' as VARCHAR(10)) || 'ing'")
})

#
# test_that("translate sql server -> Dremio temp", {
#   sql <- translate("CREATE TABLE #temp  (speed DOUBLE, dist DOUBLE)",
#                    targetDialect = "dremio"
#   )
#   expect_equal_ignore_spaces(sql, "CREATE TABLE abc  (speed DOUBLE, dist DOUBLE)")
# })


# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')