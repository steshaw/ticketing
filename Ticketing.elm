module Ticketing exposing (main)

import Basics.Extra exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Http
import Json.Decode exposing ((:=))
import Pagination exposing (Paginated, Page)
import Platform.Cmd exposing (Cmd, batch)
import Process
import Task
import Time

githubOrg = "concourse"
githubUser = "concourse"

main =
  Html.program
    { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

type alias Model =
  { repos : Maybe (List Repo)
  , currentRepos : Maybe (List Repo)
  , openPullRequests : Maybe Int
  }

type alias Repo =
  { name : String
  , openIssues : Int
  }

type Msg
  = ReposFetched (Result Http.Error (Paginated Repo))
  | PullRequestsFetched (Result Http.Error Int)

init : (Model, Cmd Msg)
init =
  let
    model =
      { repos = Nothing
      , currentRepos = Nothing
      , openPullRequests = Nothing
      }
  in
    (model, batch [fetchRepos 0 Nothing, fetchPRs 0])

view : Model -> Html Msg
view model =
  case (model.repos, model.openPullRequests) of
    (Just repos, Just prCount) ->
      let
        allIssues = List.sum <| List.map .openIssues repos
        issueCount = allIssues - prCount
      in
        Html.div [class "main"]
          [ viewBox issueCount "issues"
          , viewBox prCount "pull requests"
          ]

    (_, _) ->
      Html.text "Loading..."

viewBox : Int -> String -> Html Msg
viewBox count noun =
  Html.div [class "box"]
    [ Html.div [class "count"]
      [ Html.text (toString count) ]
    , Html.div [class "noun"]
      [ Html.text noun ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ReposFetched (Ok repos) ->
      handleReposFetched repos model

    ReposFetched (Err err) ->
      Debug.crash (toString err)

    PullRequestsFetched (Ok count) ->
      handlePRsFetched count model

    PullRequestsFetched (Err err) ->
      Debug.crash (toString err)

repollInterval : Time.Time
repollInterval = 5 * Time.minute

handleReposFetched : Paginated Repo -> Model -> (Model, Cmd Msg)
handleReposFetched repoPage model =
  let
    repos = List.append (Maybe.withDefault [] model.currentRepos) repoPage.content
  in
    case repoPage.pagination.nextPage of
      Nothing ->
        ( { model | currentRepos = Nothing, repos = Just repos } , fetchRepos repollInterval Nothing)

      Just page ->
        ( { model | currentRepos = Just repos }, fetchRepos 0 (Just page))

handlePRsFetched : Int -> Model -> (Model, Cmd Msg)
handlePRsFetched count model =
  ( { model | openPullRequests = Just count } , fetchPRs repollInterval)

fetchRepos : Time.Time -> Maybe Page -> Cmd Msg
fetchRepos delay page =
  let
    url = "https://api.github.com/orgs/" ++ githubOrg ++ "/repos"
  in
    (Process.sleep delay `Task.andThen` (always <| Pagination.fetch decodeRepo url page))
      |> Task.toResult
      |> Task.perform never ReposFetched

decodeRepo : Json.Decode.Decoder Repo
decodeRepo = Json.Decode.object2 Repo
               ("name" := Json.Decode.string)
               ("open_issues" := Json.Decode.int)

decodeResults : Json.Decode.Decoder Int
decodeResults = ("total_count" := Json.Decode.int)

fetchPRs : Time.Time -> Cmd Msg
fetchPRs delay =
  let
    url =
      "https://api.github.com/search/issues?q=user:" ++ githubUser ++ "+state:open+type:pr"
    get =
      Http.send
        Http.defaultSettings
        { verb = "GET"
        , headers = [("Authorization", "token TOKEN-GOES-HERE")]
        , url = url
        , body = Http.empty
        }
  in
    (Process.sleep delay `Task.andThen` (\x -> Task.mapError promoteHttpError get) `Task.andThen` parsePRs decodeResults)
      |> Task.toResult
      |> Task.perform never PullRequestsFetched

promoteHttpError : Http.RawError -> Http.Error
promoteHttpError rawError =
  case rawError of
    Http.RawTimeout -> Http.Timeout
    Http.RawNetworkError -> Http.NetworkError

parsePRs : Json.Decode.Decoder a -> Http.Response -> Task.Task Http.Error a
parsePRs decode response =
  let
    decoded =
      handleResponse response `Result.andThen` \body ->
        Json.Decode.decodeString decode body
          |> Result.formatError Http.UnexpectedPayload
  in
    case decoded of
      Err err ->
        Task.fail err

      Ok count ->
        Task.succeed count

handleResponse : Http.Response -> Result Http.Error String
handleResponse response =
  if 200 <= response.status && response.status < 300 then
    case response.value of
      Http.Text str ->
        Ok str

      _ ->
        Err (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    Err (Http.BadResponse response.status response.statusText)
