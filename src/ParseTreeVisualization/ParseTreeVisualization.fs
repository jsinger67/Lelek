namespace LelekParser

module ParseTreeVisualization =
    open ParseTree
    open System.Collections.Generic

    type VisData = {
        YOrder: int
        XCenter: int
        XExtend: int
        XExtendChildren: int
        XExtendWithChildren: int
        Name: string
        IsTerminal: bool
        Parent: ParseTree option
    }

    type Embedding = Dictionary<ParseTree, VisData>

    type Drawer = Embedding -> ParseTree -> string -> unit

    module VisData =

        let empty = {
            YOrder = -1
            XCenter = 0
            XExtend = 0
            XExtendChildren = 0
            XExtendWithChildren = 0
            Name = ""
            IsTerminal = false
            Parent = None
        }


        let toString ({XCenter = xc; YOrder = yo; Name = na}) =
            sprintf "%d,%d: '%s'" xc yo na


    let visualize (drawer: Drawer) (fileName: string) (ast: ParseTree): unit =
        let embedding = Embedding(HashIdentity.Reference)

        let rec visitForDecorations (depth: int) (parent: ParseTree option) (ParseTree(Item=data; Children=children) as ast) =
            children
            |> List.iter (visitForDecorations (depth + 1) (Some ast))

            let isTerminal = data |> PTItem.isToken
            let name = data |> PTItem.toShortString
            let xE = name.Length + 1
            let xEC = 
                    children
                    |> List.sumBy (fun c -> embedding.Item(c).XExtendWithChildren)

            let thisData = {
                YOrder = depth
                XCenter = 0
                XExtend = xE
                XExtendChildren = xEC
                XExtendWithChildren = [xE; xEC] |> List.max
                Name = name
                IsTerminal = isTerminal
                Parent = parent
            }

            embedding.Item(ast) <- thisData

        let xCenterLayer (layer: int) =
            let nodesInLayer =
                embedding |> Seq.filter (fun kvp -> kvp.Value.YOrder = layer)

            let nodesInLayerPerParent =
                nodesInLayer |> Seq.groupBy (fun kvp -> kvp.Value.Parent)

            nodesInLayerPerParent
            |> Seq.iter (fun (parent, nodesOfParent) ->
                let xCenter =
                    match parent with
                    | Some p ->
                        let dataOfParent = embedding.Item(p)
                        dataOfParent.XCenter - dataOfParent.XExtendChildren / 2
                    | None -> 0
                nodesOfParent
                |> Seq.fold (fun acc kvp ->
                    let dataOfNodeInLayer = kvp.Value
                    embedding.Item(kvp.Key) <-
                        { dataOfNodeInLayer with XCenter = acc + dataOfNodeInLayer.XExtendWithChildren / 2 }
                    acc + dataOfNodeInLayer.XExtendWithChildren;
                ) xCenter
                |> ignore
            )

        let xCenterNodes () =
            let depth = (embedding.Values |> Seq.maxBy (fun vd -> vd.YOrder)).YOrder
            List.init (depth + 1) id
            |> List.iter xCenterLayer

        visitForDecorations 0 None ast
        xCenterNodes ()
        drawer embedding ast fileName
            


