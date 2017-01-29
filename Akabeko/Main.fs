module Main

open System.IO
open Pdf

type Main() =
    member this.TypeSet(xslFo: string): string =
        let pageBuilder = PageBuilder(IndirectReferenceGenerator())
        let emptyPage = { resources = Dictionary(Map.empty); mediaBox = Array([Integer(0); Integer(0); Integer(595); Integer(842)]) }
        pageBuilder.AddPage(emptyPage)
        let writer = new StringWriter()
        let body = pageBuilder.BuildPageTree()
        let file = File(body, Seq.last body)
        file.WriteTo(writer)
        writer.ToString()

    member this.WriteToPdf(fileName: string, contents: string): unit =
        File.WriteAllText(System.Environment.CurrentDirectory + Path.DirectorySeparatorChar.ToString() + fileName, contents)

[<EntryPointAttribute>]
let main (args: string []) =
    //assert (args.Length > 0)
    //let xslFo = File.ReadAllText(args.[0])
    let xslFo = ""
    let main = new Main()
    main.WriteToPdf("test.pdf", main.TypeSet(xslFo))
    0