// Learn more about F# at http://fsharp.org

open System
open FSharp.Data
open System.Net



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
    Seasons : option<SeasonResult>[]
    //SizeClass : string
    Gender : Gender
}
type School = {
    Name : string
    State : string
    DriveTime : TimeSpan
    Boys : Team
    Girls : Team
    TeamUrl : string
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

let getSeasonResult (teamId:string) (gender:Gender) (year:int) : option<SeasonResult> =
    let pageUrl = getTeamRankingsPageUrl teamId gender year
    try
        let resultsPage = MaxPrepsTeamRankingPage.Load(pageUrl)
        let rowItems = resultsPage.DefinitionLists.Html.CssSelect("dl#ctl00_NavigationWithContentOverRelated_ContentOverRelated_PageHeader_TeamRecord dd")

        match rowItems with
            | [] -> None
            | _ -> Some {
                        Record = rowItems.[0].InnerText()
                        NationalRanking = rowItems.[2].InnerText() |> int
                        StateRanking = rowItems.[3].InnerText() |> int
                        Year = year
                    }
    with
        | :? WebException -> None
        | :? Exception -> None

let getTeam (teamId:string) gender =
    let results = years
                    |> Seq.map (fun year -> getSeasonResult teamId gender year)
                    |> Seq.toArray
    {
        Seasons = results
        Gender = gender
    }

type MapQuestDistanceResult = JsonProvider<"https://www.mapquestapi.com/directions/v2/route?key=LkN9DI5p6PB0cXLTLqbm1ykPAjICEJJV&from=College+View+Academy+Lincoln+NE&to=Omaha+NE&outFormat=json&ambiguities=ignore&routeType=fastest&doReverseGeocode=false&enhancedNarrative=false&avoidTimedConditions=false">
let getMapQuestDirectionsUrl (teamId:string) : string = String.Format("https://www.mapquestapi.com/directions/v2/route?key=LkN9DI5p6PB0cXLTLqbm1ykPAjICEJJV&from=College+View+Academy+Lincoln+NE&to={0}&outFormat=json&ambiguities=ignore&routeType=fastest&doReverseGeocode=false&enhancedNarrative=false&avoidTimedConditions=false", teamId.Replace("-", "+"))
let getSchoolDistance (teamId:string) : TimeSpan = 
    let url = getMapQuestDirectionsUrl teamId
    let results = MapQuestDistanceResult.Load(url)
    let seconds = results.Route.Time
    TimeSpan.FromSeconds(float seconds)
    

let getSchool (aNode:HtmlNode) (state:string) : option<School> =
    let teamUrl = aNode.AttributeValue("href")
    let fullLink = "https://www.maxpreps.com" + teamUrl
    let teamId = teamUrl.Split("/").[2]
    let schoolName = aNode.InnerText()
    
    try
        let distance = getSchoolDistance teamId

        let cutoffDistance = TimeSpan.FromHours(2.5)
        match distance with
            | x when x < cutoffDistance ->
                printfn "Getting %s" schoolName
                let boys = getTeam teamId Gender.Boys
                let girls = getTeam teamId Gender.Girls

                Some {
                    Name = schoolName
                    State = state
                    Boys = boys
                    Girls = girls
                    DriveTime = distance
                    TeamUrl = fullLink
                }
            | _ -> 
                printfn "Skipping %s" schoolName
                None
    with _ -> None


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
        |> Seq.sortBy (fun a -> a.InnerText())
        |> Seq.map (fun a -> getSchool a state)
        |> Seq.where (fun a -> a.IsSome)
        |> Seq.map (fun a -> a.Value)

let allSchools = states 
                        |> Seq.collect getTeams
                        //|> Seq.take 10
                        |> Seq.toArray

let schoolCount = Seq.length(allSchools)
printfn "%A" allSchools
printfn "All schools: %i" schoolCount

let teamResultsString (team:Team) : string =
    let seasonRankings = team.Seasons 
                            |> Seq.map (fun s -> match s with
                                                    | Some i -> String.Format("=\"{0}\",{1}", i.Record, i.NationalRanking.ToString())
                                                    | None -> " ,")
    String.Join(",", seasonRankings)

let toCsv (schools: School[]) =
    let header = ",,,,Boys,,,,,,,Girls" + Environment.NewLine
                + "Team,Link,Distance,BoysAvg,GirlsAvg,17-18,,16-17,,15-16,,14-15,,17-18,,16-17,,15-16,,14-15" + Environment.NewLine
                + ",,,,,Record,Rank,Record,Rank,Record,Rank,Record,Rank,Record,Rank,Record,Rank,Record,Rank,Record,Rank"
    let rows = schools
                |> Seq.map (fun school -> String.Format("{0},\"{4}\",{1},,,{2},{3}", school.Name, school.DriveTime.TotalMinutes, teamResultsString(school.Boys), teamResultsString(school.Girls), school.TeamUrl))
    let body = String.Join(Environment.NewLine, rows)
    let doc = header + Environment.NewLine + body
    doc

let result = toCsv allSchools
printfn "%s" result

System.IO.File.WriteAllText("output.csv", result)

printfn "Done"


    

[<EntryPoint>]
let main argv =
    Console.ReadKey() |> ignore;
    0 // return an integer exit code