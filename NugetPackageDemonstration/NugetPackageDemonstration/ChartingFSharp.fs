module ChartingFSharp


open FSharp.Charting;
open MathNet.Numerics.LinearAlgebra
// https://fslab.org/FSharp.Charting/PointAndLineCharts.html

// Add reference to System.Drawing
// Add reference to Windows.Forms

let SimplePlotGraph (left: double) right diff f = 
    let numPoints =
        (right - left) / diff 
        |> int
        |> (+) 1    // lamppost error.
    let result =
        [1 .. numPoints]
        |> List.map (double)
        |> List.map (fun x -> x / (double numPoints))
        |> List.map (fun x -> (x,f x))
        |> Chart.Line
    result.ShowChart()

    //Chart.Line [ for x in 1.0 .. 100.0 -> (x, x ** 2.0) ]

let SimplePlotPoints (xySeq:seq<(double * double)>) =
    let xSmall, xLarge =
        xySeq
        |> Seq.map (fst)
        |> fun xSeq -> (Seq.min xSeq, Seq.max xSeq)
        
    let ySmall, yLarge =
        xySeq
        |> Seq.map (snd)
        |> fun ySeq -> (Seq.min ySeq, Seq.max ySeq)

    let xMin = xSmall - 0.05 * (xLarge - xSmall)
    let xMax = xLarge + 0.05 * (xLarge - xSmall)
    let yMin = ySmall - 0.05 * (yLarge - ySmall)
    let yMax = yLarge + 0.05 * (yLarge - ySmall)
    Chart.Point(xySeq).WithXAxis(Max = xMax, Min = xMin).WithYAxis(Max = yMax, Min = yMin).ShowChart()

let ScatterPlot (xSeq: seq<double>) (ySeq: seq<double>) =
    Seq.zip xSeq ySeq
    |> SimplePlotPoints

let PlotTwoColumnMatrix (xyMatrix: Matrix<double>) =
    if xyMatrix.ColumnCount <> 2 then failwith "expected 2 column Matrix."
    let xyColArrArr =
        xyMatrix
        |> Matrix.toColArrays
    let xArr =
        xyColArrArr.[0]
    let yArr = xyColArrArr.[1]
    ScatterPlot xArr yArr

let SimpleTimeSeries ySeq =
    let yLength = ySeq |> Seq.length
    [1 .. yLength]
    |> List.map (double)
    |> Seq.zip <| ySeq
    |> Chart.Line
    |> fun result -> result.ShowChart()