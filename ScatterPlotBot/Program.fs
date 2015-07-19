open System;

let mutable config = Map.empty.
                        Add("dataFile", "data.dat").
                        Add("colorFile", "").
                        Add("plotLabel", "ScatterPlotBot FTW").
                        Add("yAxisLabel", "").
                        Add("xAxisLabel", "").
                        Add("w", "620").
                        Add("fontName", "Arial").
                        Add("fontSize", "10").
                        Add("xAxisTicks", "").
                        Add("xAxisTickNames", "").
                        Add("yAxisTicks", "").
                        Add("yAxisTickNames", "").
                        Add("outFileName", "out.png")

let phi = (1.0+sqrt(5.0))/2.0

let mutable colorTable = [| [|0.0;0.0;0.0|]
                            [|1.0;1.0;1.0|] |]

let mutable font = new Drawing.Font("Arial", 10.0f)
let textBrush = new Drawing.SolidBrush(Drawing.Color.FromArgb(32, 32, 32))

let getTextSize (g:Drawing.Graphics) s =
    g.MeasureString(s, font, new Drawing.PointF(), Drawing.StringFormat.GenericTypographic)

let parseLine (str:String) =
    let a = str.Split([|' '|])
    (Double.Parse(a.[0]), Double.Parse(a.[1]))

let plotPoint pt minX maxX minY maxY w h (a:int[]) =
    if fst pt > minX && fst pt < maxX && snd pt > minY && snd pt < maxY then
        let pX = ((fst pt)-minX)/(maxX-minX)
        let pY = 1.0-((snd pt)-minY)/(maxY-minY)
        let x = int (round (pX*(float (w-1))))
        let y = int (round (pY*(float (h-1))))
        a.[x+y*w] <- a.[x+y*w] + 1

let plotPixel (b:Drawing.Bitmap) i v maxV w h =
    if v > 0 then
        let x = i % w
        let y = i / w
        let p = (float v)/(float maxV)
        let i = int (p*float (colorTable.Length-1))
        let red = int (colorTable.[i].[0]*255.0)
        let green = int (colorTable.[i].[1]*255.0)
        let blue = int (colorTable.[i].[2]*255.0)
        b.SetPixel(x, y, Drawing.Color.FromArgb(red, green, blue))

let plot (w:int) (h:int) (minX:float) (maxX:float) (minY:float) (maxY:float) d =
    let a = Array.init (w*h) (fun _ -> 0)
    Seq.iter (fun pt -> plotPoint pt minX maxX minY maxY w h a) d
    let maxV = Array.max a
    let b = new Drawing.Bitmap(w, h)
    Array.iteri (fun i v -> plotPixel b i v maxV w h) a
    b

let parseTicks (s:string) min max =
    if s = "" then
        [|min; max|]
    else
        let a = s.Split([|' '|])
        Array.map (fun f -> Double.Parse f) a

let parseTickNames (s:string) =
    if s = "" then
        [||]
    else
        s.Split([|' '|])

// Pass in a width
let scatterPlotBot (w:int) (data:seq<float*float>) =
    let minX = fst (Seq.minBy fst data)
    let maxX = fst (Seq.maxBy fst data)
    let minY = snd (Seq.minBy snd data)
    let maxY = snd (Seq.maxBy snd data)
    let backBuffer = new Drawing.Bitmap(w, w)
    let backBufferGraphics = Drawing.Graphics.FromImage(backBuffer);
    backBufferGraphics.TextRenderingHint <- Drawing.Text.TextRenderingHint.AntiAliasGridFit
    // left margin is max(half the y-axis label width, largest y-tick label width, 16)
    let yAxisLabel = config.["yAxisLabel"]
    let yAxisLabelSize = getTextSize backBufferGraphics yAxisLabel
    let yAxisLabelWidth = yAxisLabelSize.Width
    let yAxisTicks = parseTicks config.["yAxisTicks"] minY maxY
    let yAxisTickNames = parseTickNames config.["yAxisTickNames"]
    let yAxisTickPadding = 4
    let yAxisTickWidths = Seq.map (fun tick -> (getTextSize backBufferGraphics (sprintf "%g" tick)).Width + (float32 yAxisTickPadding)) yAxisTicks
    let leftMargin = int (ceil (max (max (yAxisLabelWidth/2.0f) 16.0f) (Seq.max yAxisTickWidths)))
    // 1px for y-axis
    // right margin is width of x-axis label
    let xAxisLabel = config.["xAxisLabel"]
    let xAxisLabelSize = getTextSize backBufferGraphics xAxisLabel
    let xAxisLabelWidth = xAxisLabelSize.Width
    let xAxisLabelHeight = xAxisLabelSize.Height
    let rightMargin = int (ceil xAxisLabelWidth)
    // so, plot width is: width - leftMargin - rightMargin - 1
    let plotWidth = w - leftMargin - rightMargin - 1
    // and, plot height is: plot width / aspect ratio (default: phi)
    let plotHeight = int (ceil ((float plotWidth) / phi))
    let plotBitmap = plot plotWidth plotHeight minX maxX minY maxY data
    // total height is plotLabelHeight + y-axis label height + plotHeight + 1 (for x axis) + pad? + x-tick label height
    let plotLabel = config.["plotLabel"]
    let plotLabelSize = getTextSize backBufferGraphics plotLabel
    let plotLabelWidth = plotLabelSize.Width
    let plotLabelHeight = int (ceil plotLabelSize.Height)
    let yAxisLabelHeight = int (ceil yAxisLabelSize.Height)
    let xAxisTicks = parseTicks config.["xAxisTicks"] minX maxX
    let xAxisTickNames = parseTickNames config.["xAxisTickNames"]
    let xTickHeight =
        if xAxisTicks.Length = 0 then
            0
        else
            int ((getTextSize backBufferGraphics (sprintf "%g" xAxisTicks.[0])).Height)
    let totalHeight = plotLabelHeight + yAxisLabelHeight + plotHeight + 1 + xTickHeight + 1 // 1 SLUSH
    
    let frontBuffer = new Drawing.Bitmap( w, totalHeight )
    let frontBufferGraphics = Drawing.Graphics.FromImage(frontBuffer)
    frontBufferGraphics.TextRenderingHint <- Drawing.Text.TextRenderingHint.AntiAliasGridFit
    let plotLabelX = ((float32 w)/2.0f)-(plotLabelWidth/2.0f)
    frontBufferGraphics.DrawString(plotLabel, font, textBrush, plotLabelX, 0.0f)
    let yAxisLabelX = float32 leftMargin-(yAxisLabelWidth/2.0f)
    frontBufferGraphics.DrawString(yAxisLabel, font, textBrush, yAxisLabelX, float32 plotLabelHeight)
    frontBufferGraphics.DrawString(xAxisLabel, font, textBrush, float32 (leftMargin+plotWidth), float32 (plotLabelHeight+yAxisLabelHeight+plotHeight)-(xAxisLabelHeight/2.0f)-1.0f) // 1 SLUSH
    let axisPen = new Drawing.Pen(Drawing.Color.FromArgb(32,32,32))
    let y1 = plotLabelHeight+yAxisLabelHeight
    let y2 = y1+plotHeight
    frontBufferGraphics.DrawLine(axisPen, leftMargin, y1, leftMargin, y2)
    frontBufferGraphics.DrawLine(axisPen, leftMargin, y2, leftMargin+plotWidth, y2)
    frontBufferGraphics.DrawImage(plotBitmap, leftMargin, y1)
    Seq.iteri (fun i tick -> 
        let s = if yAxisTickNames.Length > i then yAxisTickNames.[i] else (sprintf "%g" tick)
        let tickSize = (getTextSize frontBufferGraphics s)
        let tickWidth = tickSize.Width
        let tickHeight = tickSize.Height
        let tickY = int (round ((float plotHeight) * ((tick-minY)/(maxY-minY))))
        frontBufferGraphics.DrawLine(axisPen, leftMargin, y2-tickY, leftMargin+3, y2-tickY)
        frontBufferGraphics.DrawString(s, font, textBrush, float32 (leftMargin-yAxisTickPadding)-tickWidth, float32 (y2-tickY) - (tickHeight/2.0f))
    ) yAxisTicks
    Seq.iteri (fun i tick -> 
        let s = if xAxisTickNames.Length > i then xAxisTickNames.[i] else (sprintf "%g" tick)
        let tickSize = (getTextSize frontBufferGraphics s)
        let tickWidth = tickSize.Width
        let tickHeight = tickSize.Height
        let tickX = int (round ((float leftMargin + float plotWidth * ((tick-minX)/(maxX-minX)))))
        frontBufferGraphics.DrawLine(axisPen, tickX, y2, tickX, y2-3)
        frontBufferGraphics.DrawString(s, font, textBrush, float32 (float32 tickX-(tickWidth/2.0f)), float32 y2+2.0f)
    ) xAxisTicks
    // TODO: watermark?
    frontBuffer.Save(config.["outFileName"])

let parseColorFile path =
    if path <> "" then
        let lines = Array.ofSeq (IO.File.ReadLines(path))
        colorTable <- Array.map (fun (l:string) -> Array.map Double.Parse (l.Split([|' '|]))) lines

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 0 -> printfn "No config provided, using default..."
    | 1 -> printfn "Using config: %A" argv.[0]
    | _ as n -> 
        printfn "Exactly one command line parameter expected, %A given." n
        exit 1

    if argv.Length > 0 then
        let path = argv.[0]
        let xml = Xml.XmlReader.Create(path)
        while xml.Read() do
            match xml.NodeType with
            | Xml.XmlNodeType.Element ->
                if xml.Name = "attr" then
                    config <- config.Add(xml.GetAttribute("key"), xml.GetAttribute("val"))
            | _ -> ()
    printfn "%A" config
    parseColorFile config.["colorFile"]
    font <- new Drawing.Font(config.["fontName"], float32 (Double.Parse config.["fontSize"]))
    let lines = IO.File.ReadLines(config.["dataFile"])
    let w = Int32.Parse config.["w"]
    scatterPlotBot w (Seq.map parseLine lines)
    0 // return an integer exit code
