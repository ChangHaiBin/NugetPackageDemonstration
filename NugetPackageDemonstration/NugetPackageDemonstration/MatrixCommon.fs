module MatrixCommon

open MathNet.Numerics.LinearAlgebra

let RowSeqSeqToMatrix xSeqSeq =
    xSeqSeq
    |> Matrix.Build.DenseOfRows

let ColSeqSeqToMatrix xSeqSeq =
    xSeqSeq
    |> Matrix.Build.DenseOfColumns

let MatrixToRowSeqSeq xMatrix =
    xMatrix
    |> Matrix.toRowSeq
    |> Seq.map (Vector.toSeq)

let MatrixToColSeqSeq xMatrix =
    xMatrix
    |> Matrix.toColSeq
    |> Seq.map (Vector.toSeq)