let readLines =
    let args = System.Environment.GetCommandLineArgs()

    let mutable file = null
    let key = "--filePath="

    for arg in args do
        if arg.StartsWith(key) then
            file <- arg[key.Length ..]

    match file with
    | null -> Error "file name is null"
    | _ -> Ok(System.IO.File.ReadAllLines(file))

[<Struct>]
type Field =
    val name: string
    val columnType: string
    new(name: string, columnType: string) = { name = name; columnType = columnType }

[<Struct>]
type ClassInformation =
    val name: string
    val fields: Field list
    new(name: string, fields: Field list) = { name = name; fields = fields }

let makeClassInformationFromRecord (lines: string array) =
    let mutable isStarted = false
    let mutable isEnded = false
    let mutable baseStr = ""

    let recordNamePrefix = "record"
    let separator = "("
    let argsSuffix = ")"

    for line in lines do
        if not isEnded then
            if line.Contains(recordNamePrefix) || isStarted then
                isStarted <- true

                baseStr <- baseStr + line

                if line.Contains(argsSuffix) then
                    isEnded <- true


    match baseStr.Length with
    | 0 -> Error "record is not found."
    | _ ->
        let spaceLength = " ".Length

        let classNameStartAt =
            baseStr.IndexOf(recordNamePrefix) + recordNamePrefix.Length + spaceLength

        let classNameEndAt = baseStr.IndexOf(separator) - separator.Length
        let argsStartAt = baseStr.IndexOf(separator) + separator.Length
        let argsEndAt = baseStr.IndexOf(argsSuffix) - argsSuffix.Length

        let argsStr = baseStr[argsStartAt..argsEndAt]

        let mutable fields = []

        let separatedFieldsArray = argsStr.Split(",")

        for field in separatedFieldsArray do
            let trimmedField = field.Trim()
            let name = trimmedField.Split(" ")[1]
            let columnType = trimmedField.Split(" ")[0]
            fields <- List.append fields [ Field(name, columnType) ]

        Ok(ClassInformation(baseStr[classNameStartAt..classNameEndAt].Trim(), fields))

let makeClassInformationFromClass (lines: string array) =
    let mutable isStarted = false
    let mutable isEnded = false
    let mutable baseStr = ""

    let classNamePrefix = "class"
    let separator = "{"
    let argsSuffix = "}"
    let delimiter = "---"

    for line in lines do
        if not isEnded then
            if line.Contains(" " + classNamePrefix + " ") || isStarted then
                isStarted <- true

                baseStr <- baseStr + line + delimiter

                if line.Contains(argsSuffix) then
                    isEnded <- true


    match baseStr.Length with
    | 0 -> Error "record is not found."
    | _ ->
        let spaceLength = " ".Length

        let classNameStartAt =
            baseStr.IndexOf(classNamePrefix) + classNamePrefix.Length + spaceLength

        let classNameEndAt = baseStr.IndexOf(separator) - separator.Length - spaceLength
        let argsStartAt = baseStr.IndexOf(separator) + separator.Length
        let argsEndAt = baseStr.IndexOf(argsSuffix) - argsSuffix.Length

        let argsStr = baseStr[argsStartAt..argsEndAt]

        let mutable fields = []

        let separatedFieldsArray = argsStr.Split(delimiter)

        for field in separatedFieldsArray do
            let str = field.Trim()

            if str.StartsWith("private") || str.StartsWith("public") then
                let trimmedField = str[0 .. str.Length - 2].TrimEnd() // remove tailing ";"
                let name = trimmedField.Split(" ")[2]
                let columnType = trimmedField.Split(" ")[1]
                fields <- List.append fields [ Field(name, columnType) ]

        Ok(ClassInformation(baseStr[classNameStartAt..classNameEndAt].Trim(), fields))

let makeClassInformation (lines: string array) =
    let mutable result = Error "record or class is not contained."

    for i in 0 .. lines.Length - 1 do
        let line = lines[i]

        if line.Contains(" record ") then
            result <- makeClassInformationFromRecord lines[i..]
        else if line.Contains(" class ") then
            result <- makeClassInformationFromClass lines[i..]

    result

let makeQuery (classInformartion: ClassInformation) =

    let mutable selectClause = "SELECT\n\t"

    let mutable shortenedTableName = "A"
    let args = System.Environment.GetCommandLineArgs()

    let mutable file = null
    let key = "--shortenedTableName="

    for arg in args do
        if arg.StartsWith(key) then
            shortenedTableName <- arg[key.Length ..]


    for column in classInformartion.fields do

        selectClause <- selectClause + shortenedTableName + "."

        for c in column.name.ToCharArray() do
            if System.Char.IsUpper(c) then
                selectClause <- selectClause + "_" + c.ToString()
            else
                selectClause <- selectClause + c.ToString()

        selectClause <- selectClause + ", "

    selectClause[0 .. selectClause.Length - 3].ToUpper()
    + "\nFROM\n\t"
    + classInformartion.name.ToUpper()
    + " "
    + shortenedTableName

let export (query: string) =
    let args = System.Environment.GetCommandLineArgs()

    let target = "--fileOutput=true"

    for arg in args do
        if arg.Equals(target) then
            System.IO.File.WriteAllText("result.sql", query)

let main =
    let lines = readLines

    match lines with
    | Error err -> printfn "can not read lines: %s" err
    | Ok ok ->
        let classInformartion = makeClassInformation ok

        match classInformartion with
        | Error e -> printfn "can not get class information: %s" e
        | Ok o ->
            let query = makeQuery o
            query |> printfn "---\nquery\n---\n%s\n---"
            export query

    0
