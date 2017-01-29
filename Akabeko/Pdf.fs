module Pdf

open System
open System.IO
open System.Collections.Generic
open System.Text
open Printf

let undefined() = System.NotImplementedException() |> raise

let eol = Environment.NewLine

type Object() = class end

type IndirectReference(objectNumber: int, generationNumber: int) =
    inherit Object()

    member this.ObjectNumber = objectNumber
    member this.GenerationNumber = generationNumber

    override this.ToString(): string = sprintf "%d %d R" objectNumber generationNumber

type IndirectReferenceGenerator() =

    let mutable objectNumber = 1

    let generateObjectNumber() =
        let oldObjectNumber = objectNumber
        objectNumber <- objectNumber + 1
        oldObjectNumber
    
    member this.GenerateNext(): IndirectReference = IndirectReference(generateObjectNumber(), 0)

type Indirect(ref: IndirectReference, content: Object) =
    inherit Object()

    member this.IndirectReference: IndirectReference = ref

    override this.ToString(): string =
        sprintf "%d %d obj%s%O%sendobj%s" ref.ObjectNumber ref.GenerationNumber eol content eol eol

type Integer(value: int) =
    inherit Object()

    override this.ToString(): string = sprintf "%d" value

type Real(value: double) =
    inherit Object()

type String(value: string) =
    inherit Object()

    override this.ToString(): string = sprintf "(%s)" value

type Array(items: seq<Object>) =
    inherit Object()

    override this.ToString(): string =
        String.Join(" ", items)
        |> sprintf "[%s]"

type Name(name: string) =
    inherit Object()

    let isValidName name = true // TODO: implement

    do
        assert (isValidName(name))

    member this.Name = name

    override this.Equals(obj: obj): bool =
        match obj with
        | :? Name as that -> name = that.Name
        | _ -> false

    override this.GetHashCode(): int = hash name

    override this.ToString(): string = sprintf "/%s" name

    interface IComparable with
        member this.CompareTo(obj: obj): int =
            match obj with
            | :? Name as that -> name.CompareTo(that.Name)
            | _ -> raise (ArgumentException("Object is not a Name"))

type Dictionary(map: Map<Name, Object>) =
    inherit Object()

    member private this.ToString(indentWidth: int): string =
        let indent = String.replicate indentWidth " "
        let builder = StringBuilder()
        bprintf builder "<<%s" eol
        for (KeyValue(key, value)) in map do
            let newLine = sprintf "%s%O " indent key
            let rest =
                match value with
                | :? Dictionary as dict -> dict.ToString (indentWidth + newLine.Length)
                | _ -> value.ToString()
            bprintf builder "%s%s%s" newLine rest eol
        bprintf builder "%s>>" indent
        builder.ToString()

    override this.ToString(): string = this.ToString(0)

let createCatalog(rootPageTreeNode: Indirect): Dictionary =
    Dictionary(
        Map.ofList [
            (Name("Type"), Name("Catalog") :> Object);
            (Name("Pages"), rootPageTreeNode.IndirectReference :> Object)
        ]
    )

type PageInfo = { resources: Dictionary; mediaBox: Array }

type PageBuilder(indirectReferenceGenerator: IndirectReferenceGenerator) =

    let pages = List<PageInfo>()
    
    member this.AddPage(pageInfo: PageInfo) = pages.Add(pageInfo)

    member this.BuildPageTree(): seq<Indirect> =
        let kidsRefs: seq<IndirectReference> = Seq.map (fun _ -> indirectReferenceGenerator.GenerateNext()) pages |> Seq.toList |> List.toSeq
        let root: Indirect =
            let content =
                Dictionary(
                    Map.ofList [
                        (Name("Type"), Name("Pages") :> Object);
                        (Name("Kids"), Array(Seq.map (fun k -> k :> Object) kidsRefs) :> Object);
                        (Name("Count"), Integer(Seq.length pages) :> Object)
                    ]
                )
            Indirect(indirectReferenceGenerator.GenerateNext(), content)
        let catalog: Indirect =
            let content =
                Dictionary(
                    Map.ofList [
                        (Name("Type"), Name("Catalog") :> Object);
                        (Name("Pages"), root.IndirectReference :> Object)
                    ]
                )
            Indirect(indirectReferenceGenerator.GenerateNext(), content)
        let kids: seq<Indirect> =
            seq {
                for (p, r) in Seq.zip pages kidsRefs do
                let content =
                    Dictionary(
                        Map.ofList [
                            (Name("Type"), Name("Page") :> Object);
                            (Name("Parent"), root.IndirectReference :> Object);
                            (Name("Resources"), p.resources :> Object);
                            (Name("MediaBox"), p.mediaBox :> Object)
                        ]
                    )
                yield Indirect(r, content)
            }
        Seq.append kids (seq [root; catalog])
            
        

type CrossReferenceSubsection(offset: seq<int64>) =

    let eol = (if Environment.NewLine.Length = 1 then " " else "") + Environment.NewLine

    member this.Size: int = Seq.length offset

    member this.WriteTo(writer: TextWriter): unit =
        fprintf writer "0 %d%s" (Seq.length offset + 1) Environment.NewLine
        fprintf writer "0000000000 65536 f%s" eol
        for o in offset do
            assert (o < 10000000000L)
            fprintf writer "%010d 00000 n%s" o eol

type CrossReferenceTable(crossReferenceSubsections: seq<CrossReferenceSubsection>) =

    member this.Size: int =
        crossReferenceSubsections
        |> Seq.map (fun c -> c.Size)
        |> Seq.sum

    member this.WriteTo(writer: TextWriter): unit =
        fprintf writer "xref%s" eol
        for c in crossReferenceSubsections do
            c.WriteTo(writer)

type Trailer(xrefOffset: int64, size: int, root: Indirect, id: string) =

    member this.WriteTo(writer: TextWriter): unit =
        fprintf writer "trailer%s" eol
        fprintf writer "%O%s" (Dictionary(Map.ofList [(Name("Size"), Integer(size) :> Object); (Name("Root"), root.IndirectReference :> Object)])) eol
        fprintf writer "startxref%s" eol
        fprintf writer "%d%s" xrefOffset eol

type File(body: seq<Indirect>, root: Indirect) =

    let header: string = "%PDF-1.7"
    let eof: string = "%%EOF"

    member this.WriteTo(writer: TextWriter): unit =
        use subWriter = new StringWriter()
        fprintf subWriter "%s%s" header eol
        let offset =
            List.scan
                (fun state b ->
                    let s = b.ToString()
                    fprintf subWriter "%s" s
                    state + ((int64) s.Length))
                (((int64) header.Length) + 2L)
                (Seq.toList body)
        let crossReferenceTable: CrossReferenceTable =
            CrossReferenceTable([CrossReferenceSubsection(offset)])
        let trailer: Trailer = Trailer((int64) (subWriter.ToString().Length), crossReferenceTable.Size, root, "aaaaa")
        fprintf writer "%s" (subWriter.ToString())
        crossReferenceTable.WriteTo(writer)
        trailer.WriteTo(writer)
        fprintf writer "%s%s" eof eol
