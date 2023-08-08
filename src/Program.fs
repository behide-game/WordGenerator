open System
open System.IO
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging

let flip f x y = f y x
let charToInt = Char.GetNumericValue >> int
let getCharIndex (array: char list) char = array |> List.tryFindIndex (fun c -> char = c)
let flatten2DArray (array2d: 'A array2d) : 'A array =
    array2d
    |> Seq.cast<'A>
    |> Seq.toArray

let getMedian (array: int array) =
    let sortedArray = array |> Array.sort
    let length = sortedArray |> Array.length

    match length % 2 with
    | 1 ->
        sortedArray
        |> Array.item ((length + 1) / 2 |> int)
        |> float
    | 0 ->
        let val1 = sortedArray |> Array.item (length / 2)     |> float
        let val2 = sortedArray |> Array.item (length / 2 + 1) |> float

        (val1 + val2) / 2. |> Math.Round
    | _ -> 0

let getUpperQuartile (array: int array) =
    let sortedArray = array |> Array.sort
    let length = sortedArray |> Array.length

    match length % 2 with
    | 1 ->
        sortedArray
        |> Array.item ((length + 1) * 3/4 |> int)
        |> float
    | 0 ->
        let val1 = sortedArray |> Array.item (length / 2)     |> float
        let val2 = sortedArray |> Array.item (length / 2 + 1) |> float

        (val1 + val2) * 3./4. |> Math.Round
    | _ -> 0

let getXQuartile x (array: int array) =
    let sortedArray = array |> Array.sort
    let length = sortedArray |> Array.length

    match length % 2 with
    | 1 ->
        sortedArray
        |> Array.item ((length + 1) * (x |> int) |> int)
        |> float
    | 0 ->
        let val1 = sortedArray |> Array.item (length / 2)     |> float
        let val2 = sortedArray |> Array.item (length / 2 + 1) |> float

        (val1 + val2) * x |> Math.Round
    | _ -> 0

type Language = FR | EN
module Language =
    let toString = function
        | FR -> "fr"
        | EN -> "en"

let wantedChars = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

let generateDigraph (language: Language) =
    let mutable array: int array2d = Array2D.zeroCreate wantedChars.Length wantedChars.Length

    let fileLines =
        language
        |> Language.toString
        |> sprintf "%s/../words_%s.txt" __SOURCE_DIRECTORY__
        |> File.ReadAllLines

    let analyzeLine (line: char []) =
        line
        |> Array.pairwise
        |> Array.iter (fun (current, follower) ->
            let x = current |> getCharIndex wantedChars
            let y = follower |> getCharIndex wantedChars

            match x, y with
            | Some x, Some y -> array[x, y] <- array[x, y] + 1
            | _ -> ()
        )

    fileLines
    |> Array.map (fun line -> line.ToCharArray())
    |> Array.iter analyzeLine

    array

let generateImage (intArray: int array2d) =
    let array = intArray |> Array2D.map float
    let length1 = array |> Array2D.length1
    let length2 = array |> Array2D.length2

    let scaleFactor = 100
    printfn "Scale factor: %i" scaleFactor

    let image = new Bitmap(length1 * scaleFactor, length2 * scaleFactor)

    let limitValue =
        intArray
        |> flatten2DArray
        |> getXQuartile (9./10.)

    printfn "Limit value %i" (limitValue |> int)

    array
    |> Array2D.mapi (fun x y value ->
        let value = value / (limitValue / 255.)

        let r = Math.Clamp(value, 0, 255)
        let g = Math.Clamp(value - 255., 0, 255)
        let b = Math.Clamp(value - 510., 0, 255)

        let color =
            Color.FromArgb(
                r |> int,
                g |> int,
                b |> int
            )

        Array.init scaleFactor (fun i -> x * scaleFactor + i)
        |> Array.collect (fun x -> Array.init scaleFactor (fun i -> x, (y * scaleFactor + i)))
        |> Array.iter (fun (x, y) -> image.SetPixel(x, y, color))
    )
    |> ignore

    image.Save (__SOURCE_DIRECTORY__ + "/../output.png")

[<EntryPoint>]
let main args =

    let trigram = generateDigraph EN

    trigram |> generateImage

    0