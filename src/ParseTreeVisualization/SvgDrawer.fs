namespace LelekParser

module SvgDrawer =
    open ParseTree
    open ParseTreeVisualization
    open System.Xml

    [<Literal>]
    let private XMARGIN = 10.0f
    [<Literal>]
    let private YMARGIN = 25.0f
    [<Literal>]
    let private YFACTOR = 3.5f
    [<Literal>]
    let private fontXSize = 10.0f
    [<Literal>]
    let private fontYSize = 10.0f

    let drawAST (embedding: Embedding) (ast: ParseTree) (fileName: string): unit =
        
        let scaleY (y: int) =
            (y |> float32) * fontYSize * YFACTOR + YMARGIN

        let scaleX (x: int) =
            (x |> float32) * fontXSize + XMARGIN

        let measureString (str: string) =
            (str.Length |> float32) * fontXSize


        let writer = XmlWriter.Create(fileName)
        writer.WriteProcessingInstruction("xml", "version='1.0' encoding='UTF-16'")
        writer.WriteStartElement(null, "svg", "http://www.w3.org/2000/svg")
        writer.WriteAttributeString("version", "1.1")
        writer.WriteAttributeString("lang", "en")

        let stringFont = "font-family: 'Courier'; font-style: normal"
        let terminalFont = "font-family: 'Courier'; font-weight: bold; font-style: normal"

        let rec _drawAST ((ParseTree(Children=children)) as ast) =
            let data = embedding.Item(ast)

            let font = if data.IsTerminal then terminalFont else stringFont
            let szx = measureString(data.Name)
            let x = scaleX(data.XCenter) - szx / 2.0f
            let y = scaleY(data.YOrder)
            writer.WriteStartElement("text")
            writer.WriteAttributeString("x", (sprintf "%f" x))
            writer.WriteAttributeString("y", (sprintf "%f" y))
            writer.WriteAttributeString("style", font)
            writer.WriteString(data.Name)
            writer.WriteEndElement()
            children
            |> List.iter (fun child ->
                let childData = embedding.Item(child)
                writer.WriteStartElement("line")
                writer.WriteAttributeString("x1", (sprintf "%f" (scaleX(data.XCenter))))
                writer.WriteAttributeString("y1", (sprintf "%f" (y + fontYSize)))
                writer.WriteAttributeString("x2", (sprintf "%f" (scaleX(childData.XCenter))))
                writer.WriteAttributeString("y2", (sprintf "%f" (scaleY(childData.YOrder) - fontYSize)))
                writer.WriteAttributeString("stroke", "black")
                writer.WriteEndElement()
                _drawAST child;
            )

        let treeDepth = (embedding.Values |> Seq.maxBy (fun vd -> vd.YOrder)).YOrder
        let imgHeight = scaleY (treeDepth + 1) |> int
        let treeWidth = embedding.Item(ast).XExtendWithChildren
        let imgWidth = scaleX treeWidth |> int
        writer.WriteAttributeString("width", (sprintf "%d" imgWidth))
        writer.WriteAttributeString("height", (sprintf "%d" imgHeight))
        
        // Draw on a white rectangle to be visible also on black backgrounds.
        writer.WriteStartElement("rect")
        writer.WriteAttributeString("x", "0")
        writer.WriteAttributeString("y", "0")
        writer.WriteAttributeString("width", (sprintf "%d" imgWidth))
        writer.WriteAttributeString("height", (sprintf "%d" imgHeight))
        writer.WriteAttributeString("fill", "white")
        writer.WriteEndElement()

        _drawAST ast
        
        writer.WriteEndElement()
        writer.Flush()
        writer.Close()
