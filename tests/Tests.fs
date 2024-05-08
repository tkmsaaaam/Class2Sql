module Tests

open Xunit

open Program

[<Fact>]
let ``makeClassInformationFromRecord is ok, lines is single line.`` () =
    let lines = [| "public record Table(String arg1){}" |]
    let actual = makeClassInformationFromRecord (lines)
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
let ``makeClassInformationFromRecord is ok, lines is multiple lines.`` () =
    let lines = [| "public record Table(String arg1){"; "}" |]
    let actual = makeClassInformationFromRecord (lines)
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
let ``makeClassInformationFromRecord is ok, args is multiple lines.`` () =
    let lines = [| "public record Table(String arg1,"; "Integer arg2"; "){"; "}" |]
    let actual = makeClassInformationFromRecord (lines)
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
let ``makeClassInformationFromClass is ok, class has a private arg.`` () =
    let lines = [| "public class Table {private String arg1;}" |]
    let actual = makeClassInformationFromClass (lines)
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
let ``makeClassInformationFromClass is ok, class has a public arg.`` () =
    let lines = [| "public class Table {public String arg1;}" |]
    let actual = makeClassInformationFromClass (lines)
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
let ``makeClassInformationFromClass is ok, args is multiple lines.`` () =
    let lines =
        [| "public class Table {"
           "\tprivate String arg1;"
           "\tpublic Integer arg2;"
           "}" |]

    let actual = makeClassInformationFromClass (lines)
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
let ``makeClassInformation is error`` () =
    let lines = [||]
    let actual = makeClassInformation (lines)
    Assert.True(Result.isError (actual))
