module Main

open System.IO
open System.Xml
open System.Xml.Linq
open Pdf
open XslFo
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open FParsec
open System.Collections.Generic

[<Measure>]
type point

type Main() =
    let parseLength (length: string): float<m> =
        let parser = pfloat .>>. restOfLine false
        match CharParsers.run parser length with
        | Success((value, unitSymbol), _, _) ->
            match unitSymbol with
            | "mm" -> LanguagePrimitives.FloatWithMeasure (value / 1000.)
            | _ -> raise (System.NotImplementedException())
        | Failure(errorMsg, _, _) -> failwith "Failed to parse a float"

    let convertLength (input: float<m>): float<point> = input * 2834.6472<point/m>

    member this.TypeSet(xslFoStream: System.IO.Stream): string =
        let xslFo = XDocument.Load(xslFoStream)
        let foNamespace = XNamespace.Get("http://www.w3.org/1999/XSL/Format")
        let root = xslFo.Element(foNamespace + "root")
        let layoutMasterSet = root.Element(foNamespace + "layout-master-set")
        let pageMasterMap =
            layoutMasterSet.Elements(foNamespace + "simple-page-master")
            |> Seq.map
                begin fun simplePageMaster ->
                    let masterName = simplePageMaster.Attribute(XName.Get("master-name")).Value
                    (masterName, simplePageMaster)
                end
            |> Map.ofSeq
        let indirectReferenceGenerator = IndirectReferenceGenerator()
        let pageTreeBuilder = PageTreeBuilder()
        let pdfBuilder = PdfBuilder(indirectReferenceGenerator)
        let fontReference = pdfBuilder.AddIndirect(Pdf.helvetica)
        let procSetReference = pdfBuilder.AddIndirect(Array(seq [Name("PDF"); Name("Text")]))
        for pageSequence in root.Elements(foNamespace + "page-sequence") do
            let masterReference = pageSequence.Attribute(XName.Get("master-reference")).Value
            let pageMaster = Map.find masterReference pageMasterMap
            let pageHeight = parseLength (pageMaster.Attribute(XName.Get("page-height")).Value)
            let pageWidth = parseLength (pageMaster.Attribute(XName.Get("page-width")).Value)
            let contentsReference = pdfBuilder.AddIndirect(Stream("BT /F13 12 Tf 0 100 Td (Hello World) Tj ET"))
            let pageBuilder = PageBuilder()
            pageBuilder.Resources <-
                Some(
                    Dictionary(
                        (Name("Font"), Dictionary((Name("F13"), fontReference.Object)).Object),
                        (Name("ProcSet"), procSetReference.Object)
                    )
                )
            pageBuilder.MediaBox <- Some(Array([ Integer(0)
                                                 Integer(0)
                                                 Integer((int)(convertLength pageWidth))
                                                 Integer((int)(convertLength pageHeight)) ]))
            pageBuilder.Contents <- Some contentsReference
            pageTreeBuilder.AddPageBuilder(pageBuilder)
        let writer = new StringWriter()
        pageTreeBuilder.BuildPageTree(pdfBuilder)
        pdfBuilder.BuildPdf(writer)
        writer.ToString()

    member this.WriteToPdf(fileName: string, contents: string): unit =
        File.WriteAllText(System.Environment.CurrentDirectory + Path.DirectorySeparatorChar.ToString() + fileName, contents)

[<EntryPointAttribute>]
let main (args: string []) =
    //assert (args.Length > 0)
    //let xslFo = File.ReadAllText(args.[0])
    let xslFo = new FileStream(@"..\..\xslfo\empty_a4.fo.xml", FileMode.Open)
    let main = new Main()
    main.WriteToPdf("test.pdf", main.TypeSet(xslFo))
    0
    