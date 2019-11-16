namespace LelekParser

module PngDrawer =
    open ParseTree
    open ParseTreeVisualization
    open System.Drawing

    [<Literal>]
    let private XMARGIN = 10.0f
    [<Literal>]
    let private YMARGIN = 10.0f
    [<Literal>]
    let private YFACTOR = 2.0f

    let drawAST (embedding: Embedding) (ast: ParseTree) (fileName: string): unit =
        let stringFont = new Font("Courier", 8.0f);
        let terminalFont = new Font("Courier", 8.0f, FontStyle.Bold);
        let normalBrush = new SolidBrush(Color.Black);
        let img = new Bitmap(100, 100)
        let g = Graphics.FromImage(img)
        let sz = g.MeasureString("A", stringFont)
        let fontXSize = sz.Width / 1.8f
        let fontYSize = sz.Height

        let scaleY (y: int) =
            (y |> float32) * fontYSize * YFACTOR + YMARGIN

        let scaleX (x: int) =
            (x |> float32) * fontXSize + XMARGIN

        let rec _drawAST (g: Graphics) ((ParseTree(Children=children)) as ast) =
            let data = embedding.Item(ast)

            let font = if data.IsTerminal then terminalFont else stringFont
            let sz = g.MeasureString(data.Name, font)
            let x = scaleX(data.XCenter) - sz.Width / 2.0f
            let y = scaleY(data.YOrder)
            let brush = normalBrush
            g.DrawString(data.Name, font, brush, PointF(x, y))
            children
            |> List.iter (fun child ->
                let childData = embedding.Item(child)
                g.DrawLine(Pens.Black, scaleX(data.XCenter), y + fontYSize, scaleX(childData.XCenter), scaleY(childData.YOrder))                
                _drawAST g child;
            )

        let treeDepth = (embedding.Values |> Seq.maxBy (fun vd -> vd.YOrder)).YOrder
        let imgHeight = scaleY (treeDepth + 1) |> int
        let treeWidth = embedding.Item(ast).XExtendWithChildren
        let imgWidth = scaleX treeWidth |> int
        let img = new Bitmap(imgWidth, imgHeight);
        let g = Graphics.FromImage(img);
        g.FillRectangle(new SolidBrush(Color.White), 0, 0, imgWidth, imgHeight);
        _drawAST g ast
        img.Save(fileName, Imaging.ImageFormat.Png);


