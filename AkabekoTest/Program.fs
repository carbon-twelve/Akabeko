module AkabekoTest

open Persimmon
open UseTestNameByReflection

let ``TypeSet outputs an empty PDF`` = test {
    let expected =
        "%PDF-1.2\n" +
        // catalog
        "1 0 obj\n" +
        "<<\n" +
        "   /Type /Catalog\n" +
        "   /Pages 2 0 R\n" +
        ">>\n" +
        "endobj\n" +
        // pages
        "2 0 obj\n" +
        "<<\n" +
        "   /Type /Pages\n" +
        "   /Kids [ 3 0 R ]\n" +
        "   /Count 1\n" +
        "   /MediaBox [0 0 595 842]\n" +
        ">>\n" +
        "endobj\n" +
        // page
        "3 0 obj\n" +
        "<<\n" +
        "   /Type /Page\n" +
        "   /Parent 2 0 R\n" +
        ">>\n" +
        "endobj\n" +
        //xref
        "\n" +
        "xref\n" +
        "0 4\n" +
        "0000000000 65535 f\n" +
        "0000000010 0 f\n" +
        "0000000071 0 f\n" +
        "0000000174 0 f\n" +
        // trailer
        "trailer\n" +
        "<<\n" +
        "   /Root 1 0 R\n" +
        "   /Size 4\n" +
        ">>\n" +
        //startxref
        "startxref\n" +
        "235\n" +
        "%%EOF\n"

    let main = Main.Main()
    let actual = main.TypeSet("")

    do! assertEquals expected actual
}
