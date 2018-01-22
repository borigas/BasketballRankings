// Learn more about F# at http://fsharp.org

open System
open FSharp.Data


type SeasonResult = {
    Record : string
}
type Team = {
    Seasons : List<SeasonResult>
    SizeClass : string
}
type School = {
    Name : string
    Boys : Team
    Girls : Team
}

let sports = [| "basketball"; "girls-basketball" |]
let seasons = [| DateTime.Now.Year - 3 .. DateTime.Now.Year |]
let states = [| "nebraska"; "iowa" |]

printf "%A" sports
printf "%A" seasons

let getRankingsPage (sport: string) (state: string) (pageNum: int) : string = String.Format("http://www.maxpreps.com/rankings/{0}/{1}/state/{2}.htm", sport, pageNum, state)

type MaxPrepsRankingPage = HtmlProvider<"http://www.maxpreps.com/rankings/basketball/1/state/nebraska.htm">

let getTeams (state:string) =
    let pageIndices = Seq.initInfinite(fun i -> i + 1)
    let teams = pageIndices
                |> Seq.map (fun i -> getRankingsPage "basketball" state i)
                |> Seq.map (fun url -> MaxPrepsRankingPage.Load url)
                |> Seq.map (fun page -> page.Tables.Html.Descendants("tbody")
                                        |> Seq.collect (fun rankings -> rankings.Descendants("a"))
                                        |> Seq.map (fun a -> a.AttributeValue("href"))
                                        |> Seq.map (fun url -> url.Split("/").[2]))
                |> Seq.takeWhile (fun pageResults -> Seq.length pageResults <> 0)
                |> Seq.collect (fun x -> x)
                
    let schoolCount = Seq.length(teams)
    printfn "Teams in %s: %i" state schoolCount

    teams

let allSchoolNames = states 
                        |> Seq.collect getTeams

let schoolCount = Seq.length(allSchoolNames)
printfn "%A" allSchoolNames
printfn "All schools: %i" schoolCount

//let pageUrl = getRankingsPage "basketball" "1" "nebraska"
//let page = MaxPrepsRankingPage.Load(pageUrl)

//let schoolLinks = page.Tables.Html.Descendants("tbody")
//                    |> Seq.collect (fun rankings -> rankings.Descendants("a"))
//                    |> Seq.map (fun a -> a.AttributeValue("href"))
//                    |> Seq.map (fun url -> url.Split("/").[2])
//printfn "%A" schoolLinks

//let firstRow = table.Rows.[0]
//let school = firstRow.School
//printfn "%s" school
//    //|> printfn "%A"

printfn "Done"


    

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Console.ReadKey() |> ignore;
    0 // return an integer exit code