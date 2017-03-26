module Pdf

open System
open System.IO
open System.Collections.Generic
open System.Text
open Printf

let undefined() = System.NotImplementedException() |> raise

let eol = Environment.NewLine

[<AbstractClass>]
type Object() =
    abstract member Object: Object

type Nothing() =
    inherit Object()

    override this.Object = this :> Object
    override this.ToString(): string = failwith "Nothing is not a true PDF object."

type IndirectReference(objectNumber: int, generationNumber: int) =
    inherit Object()

    member this.ObjectNumber = objectNumber
    member this.GenerationNumber = generationNumber

    override this.Object = this :> Object
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

    override this.Object = this :> Object
    override this.ToString(): string =
        sprintf "%d %d obj%s%O%sendobj%s" ref.ObjectNumber ref.GenerationNumber eol content eol eol

type Integer(value: int) =
    inherit Object()

    override this.Object = this :> Object
    override this.ToString(): string = sprintf "%d" value

type Real(value: double) =
    inherit Object()

    override this.Object = this :> Object

type String(value: string) =
    inherit Object()

    override this.Object = this :> Object
    override this.ToString(): string = sprintf "(%s)" value

type Array(items: seq<Object>) =
    inherit Object()

    override this.Object = this :> Object
    override this.ToString(): string =
        String.Join(" ", items)
        |> sprintf "[%s]"

type Name(name: string) =
    inherit Object()

    let isValidName name = true // TODO: implement

    do
        assert (isValidName(name))

    member this.Name = name

    override this.Object = this :> Object

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

    new ([<ParamArray>] entries: (Name * Object) array) = Dictionary(Map.ofArray entries)

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

    override this.Object = this :> Object
    override this.ToString(): string = this.ToString(0)

type Stream(content: string) =
    inherit Object()

    let metadata = Dictionary(Map.ofList [(Name("Length"), Integer(content.Length) :> Object)])

    override this.Object = this :> Object
    override this.ToString(): string =
        let builder = StringBuilder()
        bprintf builder "%O%sstream%s%O%sendstream" metadata eol eol content eol
        builder.ToString()
        
let createCatalog(rootPageTreeNode: Indirect): Dictionary =
    Dictionary(
        Map.ofList [
            (Name("Type"), Name("Catalog") :> Object);
            (Name("Pages"), rootPageTreeNode.IndirectReference :> Object)
        ]
    )

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


type PdfBuilder(generator: IndirectReferenceGenerator) =
    let header: string = "%PDF-1.7"
    let eof: string = "%%EOF"
    let objects = Dictionary<IndirectReference, Indirect>()

    member val Root: IndirectReference option = None with get, set

    member this.AddIndirect(obj: Object): IndirectReference =
        let reference = generator.GenerateNext()
        objects.[reference] <- Indirect(reference, obj)
        reference
    
    member this.SetIndirect(reference: IndirectReference, obj: Object) =
        objects.[reference] <- Indirect(reference, obj)
   
    member this.BuildPdf(writer: TextWriter): unit =
        use subWriter = new StringWriter()
        fprintf subWriter "%s%s" header eol
        let offset =
            List.scan
                (fun state b ->
                    let s = b.ToString()
                    fprintf subWriter "%s" s
                    state + ((int64) s.Length))
                (((int64) header.Length) + 2L)
                (Seq.toList objects.Values)
        let crossReferenceTable: CrossReferenceTable =
            CrossReferenceTable([CrossReferenceSubsection(offset)])
        let trailer: Trailer = Trailer((int64) (subWriter.ToString().Length), crossReferenceTable.Size, objects.[this.Root.Value], "aaaaa")
        fprintf writer "%s" (subWriter.ToString())
        crossReferenceTable.WriteTo(writer)
        trailer.WriteTo(writer)
        fprintf writer "%s%s" eof eol

let presentEntries (candidates: (Name * (Object option)) list): (Name * Object) list =
    [
        for (name, optional)in candidates do
            match optional with
            | Some obj -> yield (name, obj.Object)
            | None -> ()
    ]
   
let up<'T when 'T :> Object> (option: 'T option): Object option = Option.map (fun o -> o :> Object) option

type PageBuilder() =
    
    member val Parent: IndirectReference option = None with get, set
    member val Resources: Dictionary option = None with get, set
    member val MediaBox: Array option = None with get, set
    member val Contents: IndirectReference option = None with get, set

    member this.BuildPage(): Dictionary = 
        let required = 
            [ (Name("Type"), Name("Page").Object)
              (Name("Parent"), this.Parent.Value.Object)
              (Name("Resources"), this.Resources.Value.Object)
              (Name("MediaBox"), this.MediaBox.Value.Object) ]
        let optional = presentEntries [ (Name("Contents"), up this.Contents) ]
        Dictionary(Map.ofList (List.append required optional))

type PageTreeBuilder() =

    let pages = List<PageBuilder>()
    
    member this.AddPageBuilder(pageBuilder: PageBuilder) = pages.Add(pageBuilder)

    member this.BuildPageTree(pdfBuilder: PdfBuilder): unit =
        let rootReference = pdfBuilder.AddIndirect(Nothing())
        let kidsReferences = [
            for page in pages do
                let content =
                    page.Parent <- Some rootReference
                    page.BuildPage()
                yield pdfBuilder.AddIndirect(content).Object
        ]
        let root =
            Dictionary(
                Map.ofList [
                    (Name("Type"), Name("Pages").Object);
                    (Name("Kids"), Array(kidsReferences).Object);
                    (Name("Count"), Integer(Seq.length pages).Object)
                ]
            )
        let catalog =
            Dictionary(
                Map.ofList [
                    (Name("Type"), Name("Catalog").Object);
                    (Name("Pages"), rootReference.Object)
                ]
            )
        pdfBuilder.SetIndirect(rootReference, root)
        let catalogReference = pdfBuilder.AddIndirect(catalog)
        pdfBuilder.Root <- Some catalogReference
 
        
let helvetica =
    Dictionary(
        (Name("Type"), Name("Font").Object),
        (Name("Subtype"), Name("Type1").Object),
        (Name("BaseFont"), Name("Helvetica").Object)
    )


type Resources =
    inherit Dictionary

    new (?font: Dictionary, ?procSet: IndirectReference) =
        { inherit Dictionary(Map.ofList (presentEntries [ (Name("Font"), up font); (Name("ProcSet"), up procSet) ])) }
        