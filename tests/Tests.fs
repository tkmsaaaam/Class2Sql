module Tests

open Xunit

open Program

[<Fact>]
let ``makeMetaDataFromRecord is ok, lines is single line.`` () =
    let lines = [| "public record Table(String arg1){}" |]
    let actual = makeMetaDataFromRecord (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)

[<Fact>]
let ``makeMetaDataFromRecord is ok, lines is multiple lines.`` () =
    let lines = [| "public record Table(String arg1){"; "}" |]
    let actual = makeMetaDataFromRecord (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)

[<Fact>]
let ``makeMetaDataFromRecord is ok, args is multiple lines.`` () =
    let lines = [| "public record Table(String arg1,"; "Integer arg2"; "){"; "}" |]
    let actual = makeMetaDataFromRecord (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null
    let mutable arg2Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
        arg2Name <- o.fields[1].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)
    Assert.Equal("arg2", arg2Name)

[<Fact>]
let ``makeMetaDataFromClass is ok, class has a private arg.`` () =
    let lines = [| "public class Table {private String arg1;}" |]
    let actual = makeMetaDataFromClass (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)

[<Fact>]
let ``makeMetaDataFromClass is ok, class has a public arg.`` () =
    let lines = [| "public class Table {public String arg1;}" |]
    let actual = makeMetaDataFromClass (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)

[<Fact>]
let ``makeMetaDataFromClass is ok, args is multiple lines.`` () =
    let lines =
        [| "public class Table {"
           "\tprivate String arg1;"
           "\tpublic Integer arg2;"
           "}" |]

    let actual = makeMetaDataFromClass (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null
    let mutable arg2Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
        arg2Name <- o.fields[1].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)
    Assert.Equal("arg2", arg2Name)

[<Fact>]
let ``makeMetaData is error`` () =
    let lines = [||]
    let actual = makeMetaData (lines)
    Assert.True(Result.isError (actual))

    match actual with
    | Error e -> Assert.Equal("record or class is not contained.", e)
    | Ok(resultValue) -> failwith "Not Implemented"

[<Fact>]
let ``makeMetaData is ok, when record.`` () =
    let lines =
        [| "package example;"
           "public record Table(String arg1,"
           "Integer arg2"
           "){"
           "}" |]

    let actual = makeMetaDataFromRecord (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null
    let mutable arg2Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
        arg2Name <- o.fields[1].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)
    Assert.Equal("arg2", arg2Name)

[<Fact>]
let ``makeMetaData is ok, when class.`` () =
    let lines =
        [| "pakcage example;"
           "public class Table {"
           "\tprivate String arg1;"
           "\tpublic Integer arg2;"
           "}" |]

    let actual = makeMetaDataFromClass (lines)
    Assert.True(Result.isOk (actual))
    let mutable tableName = null
    let mutable arg1Name = null
    let mutable arg2Name = null

    match actual with
    | Ok o ->
        tableName <- o.name
        arg1Name <- o.fields[0].name
        arg2Name <- o.fields[1].name
    | Error(errorValue) -> failwith "Not Implemented"

    Assert.Equal("Table", tableName)
    Assert.Equal("arg1", arg1Name)
    Assert.Equal("arg2", arg2Name)

[<Fact>]
let ``makeQuery`` () =
    let tableName = "table"
    let column1 = Field("columnOne", "String")
    let column2 = Field("column2", "Integer")
    let list = List.append [ column1 ] [ column2 ]
    let classInformartion = ClassMetaData(tableName, list)
    let actual = makeQuery (classInformartion)
    Assert.Equal("SELECT\n\tA.COLUMN_ONE, A.COLUMN2\nFROM\n\tTABLE A", actual)
