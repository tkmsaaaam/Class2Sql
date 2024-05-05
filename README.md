# Class2Sql

- This convert from Java Class to SQL.

- from

```java
package class2Sql;

public record Table(boolean isOk, String name, Integer id) {}

```

- to

```sql
SELECT
 A.IS_OK, A.NAME, A.ID
FROM
 TABLE A
```

## How to use

```bash
  dotnet run --filePath=example/example.java --fileOutput=true --shortenedTableName=A
```
