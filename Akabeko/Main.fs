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
            | _ -> LanguagePrimitives.FloatWithMeasure value
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
        let fontIndirectReference = indirectReferenceGenerator.GenerateNext()
        let procSetIndirectReference = indirectReferenceGenerator.GenerateNext()
        let pageBuilder = PageBuilder(indirectReferenceGenerator)
        let objects = List<Indirect>()
        for pageSequence in root.Elements(foNamespace + "page-sequence") do
            let masterReference = pageSequence.Attribute(XName.Get("master-reference")).Value
            let pageMaster = Map.find masterReference pageMasterMap
            let pageHeight = parseLength (pageMaster.Attribute(XName.Get("page-height")).Value)
            let pageWidth = parseLength (pageMaster.Attribute(XName.Get("page-width")).Value)
            let contentsIndirectReference = indirectReferenceGenerator.GenerateNext()
            let contents = Indirect(contentsIndirectReference, Stream("BT /F13 12 Tf 0 0 Td (Hello World) Tj ET"))
            objects.Add(contents)
            let page = {
                resources =
                    Dictionary(
                        Map.ofList
                            [
                                (Name("Font"), Dictionary(Map.ofList [(Name("F13"), fontIndirectReference :> Object)]) :> Object);
                                (Name("ProcSet"), procSetIndirectReference :> Object)
                            ]
                    );
                mediaBox = Array([Integer(0); Integer(0); Integer((int) (convertLength pageWidth)); Integer((int) (convertLength pageHeight))]);
                contents = Some contentsIndirectReference
            }
            pageBuilder.AddPage(page)
        let fontResource =
            let content = Dictionary(Map.ofList [(Name("Type"), Name("Font") :> Object); (Name("Subtype"), Name("Type1") :> Object); (Name("BaseFont"), Name("Helvetica") :> Object)])
            Indirect(fontIndirectReference, content)
        let procSet =
            Indirect(procSetIndirectReference, Array(seq [Name("PDF"); Name("Text")]))
        let writer = new StringWriter()
        let body = pageBuilder.BuildPageTree()
        let file = File(Seq.concat (seq [ seq [fontResource; procSet]; objects :> seq<Indirect>; body]), Seq.last body)
        file.WriteTo(writer)
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