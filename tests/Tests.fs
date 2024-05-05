module Tests

open Xunit

open Program

[<Fact>]
let ``makeClassInformation is error`` () =
    let lines = [||]
    let actual = makeClassInformation (lines)
    Assert.True(Result.isError (actual))

[<Fact>]
let ``makeClassInformation is ok, lines is single line.`` () =
    let record = "public record Table(String arg1){}"
    let lines = List.toArray [ record ]
    let actual = makeClassInformation (lines)
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
let ``makeClassInformation is ok, lines is multiple lines.`` () =
    let record = "package example;\n\npublic record Table(String arg1){\n}\n"
    let lines = List.toArray [ record ]
    let actual = makeClassInformation (lines)
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
let ``makeClassInformation is ok, args is multiple lines.`` () =
    let record =
        "package example;\n\npublic record Table(String arg1,\nInteger arg2\n){\n}\n"

    let lines = List.toArray [ record ]
    let actual = makeClassInformation (lines)
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
