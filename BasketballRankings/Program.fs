// Learn more about F# at http://fsharp.org

open System
open FSharp.Data



type Gender =
    | Boys
    | Girls
type SeasonResult = {
    Record : string
    NationalRanking : int
    StateRanking : int
    Year : int
}
type Team = {
    Seasons : SeasonResult[]
    //SizeClass : string
    Gender : Gender
}
type School = {
    Name : string
    State : string
    //DriveTime : TimeSpan
    Boys : Team
    Girls : Team
}
let genderToSportName gender =
    match gender with
    | Gender.Boys -> "basketball"
    | Gender.Girls -> "girls-basketball"
    
    
let yearToSeason (year:int) : string =
    let thisYear = DateTime.Now.Year
    match year with
    | y when (int y) = thisYear -> ""
    | _ -> String.Format("-winter-{0}-{1}", (int)year-2001, (int)year-2000)


let years = [| DateTime.Now.Year - 3 .. DateTime.Now.Year |] |> Seq.map(fun y -> y ) |> Seq.toArray
let seasons = years |> Seq.map(fun year -> yearToSeason year)
let states = [| "nebraska"; "iowa" |]

printf "%A" seasons

type MaxPrepsTeamRankingPage = HtmlProvider<"http://www.maxpreps.com/high-schools/college-view-academy-eagles-(lincoln,ne)/basketball/rankings.htm">
let getTeamRankingsPageUrl (teamId:string) (gender:Gender) (year:int) : string =
    let sport = genderToSportName gender
    let season = yearToSeason year
    let seasonSportName = sport + season
    String.Format("http://www.maxpreps.com/high-schools/{0}/{1}/rankings.htm", teamId, seasonSportName)

let getSeasonResult (teamId:string) (gender:Gender) (year:int) : SeasonResult =
    let pageUrl = getTeamRankingsPageUrl teamId gender year
    let resultsPage = MaxPrepsTeamRankingPage.Load(pageUrl)
    let rowItems = resultsPage.DefinitionLists.Html.CssSelect("dl#ctl00_NavigationWithContentOverRelated_ContentOverRelated_PageHeader_TeamRecord dd")
    let seasonResult = {
        Record = rowItems.[0].InnerText()
        NationalRanking = rowItems.[2].InnerText() |> int
        StateRanking = rowItems.[3].InnerText() |> int
        Year = year
    }
    seasonResult

let getTeam (teamId:string) gender =
    let results = years
                    |> Seq.map (fun year -> getSeasonResult teamId gender year)
                    |> Seq.toArray
    {
        Seasons = results
        Gender = gender
    }

let getSchool (aNode:HtmlNode) (state:string) : School=
    let teamId = aNode.AttributeValue("href").Split("/").[2]
    let schoolName = aNode.InnerText()

    let boys = getTeam teamId Gender.Boys
    let girls = getTeam teamId Gender.Girls

    {
        Name = schoolName
        State = state
        Boys = boys
        Girls = girls
    }


let getRankingsPage (sport: string) (state: string) (pageNum: int) : string = String.Format("http://www.maxpreps.com/rankings/{0}/{1}/state/{2}.htm", sport, pageNum, state)
type MaxPrepsRankingPage = HtmlProvider<"http://www.maxpreps.com/rankings/basketball/1/state/nebraska.htm">
let getTeams (state:string) =
    let pageIndices = Seq.initInfinite(fun i -> i + 1)
    pageIndices
        |> Seq.map (fun i -> getRankingsPage "basketball" state i)
        |> Seq.map (fun url -> MaxPrepsRankingPage.Load url)
        |> Seq.map (fun page -> page.Tables.Html.CssSelect("tbody a"))
        |> Seq.takeWhile (fun pageResults -> Seq.length pageResults <> 0)
        |> Seq.collect (fun x -> x)
        |> Seq.map (fun a -> getSchool a state)

let allSchoolNames = states 
                        |> Seq.collect getTeams
                        |> Seq.take 2
                        |> Seq.toArray

let schoolCount = Seq.length(allSchoolNames)
printfn "%A" allSchoolNames
printfn "All schools: %i" schoolCount

printfn "Done"


    

[<EntryPoint>]
let main argv =
    Console.ReadKey() |> ignore;
    0 // return an integer exit code