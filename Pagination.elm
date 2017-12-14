module Pagination exposing (Paginated, Pagination, Page, Direction(..), fetch, parseLinks)

import Token exposing (githubToken)

import Dict exposing (Dict)
import Http
import Json.Decode
import Regex exposing (Regex)
import String
import Task exposing (Task)

type alias Paginated a =
  { content : List a
  , pagination : Pagination
  }

type alias Pagination =
  { previousPage : Maybe Page
  , nextPage : Maybe Page
  }

type alias Page = Int

type Direction
  = Since Int
  | Until Int

previousRel : String
previousRel = "prev"

nextRel : String
nextRel = "next"

linkHeaderRegex : Regex
linkHeaderRegex =
  Regex.regex ("<([^>]+)>; rel=\"(" ++ previousRel ++ "|" ++ nextRel ++ ")\"")

fetch : Json.Decode.Decoder a -> String -> Maybe Page -> Task Http.Error (Paginated a)
fetch decode url page =
  let
    get =
      Http.send
        Http.defaultSettings
        { verb = "GET"
        , headers = [("Authorization", "token " ++ githubToken)]
        , url = addParams url page
        , body = Http.empty
        }
  in
    Task.mapError promoteHttpError get `Task.andThen` parsePagination decode

parsePagination : Json.Decode.Decoder a -> Http.Response -> Task Http.Error (Paginated a)
parsePagination decode response =
  let
    pagination =
      parseLinks response

    decoded =
      handleResponse response `Result.andThen` \body ->
        Json.Decode.decodeString (Json.Decode.list decode) body
          |> Result.formatError Http.UnexpectedPayload
  in
    case decoded of
      Err err ->
        Task.fail err

      Ok content ->
        Task.succeed { content = content, pagination = pagination }

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

promoteHttpError : Http.RawError -> Http.Error
promoteHttpError rawError =
  case rawError of
    Http.RawTimeout -> Http.Timeout
    Http.RawNetworkError -> Http.NetworkError

parseLinks : Http.Response -> Pagination
parseLinks response =
  case Dict.get "Link" response.headers of
    Nothing ->
      Pagination Nothing Nothing

    Just commaSeparatedCraziness ->
      let
        headers = String.split ", " commaSeparatedCraziness
        parsed = Dict.fromList <| List.filterMap parseLinkTuple headers
      in
        Pagination
          (Dict.get previousRel parsed `Maybe.andThen` parseParams)
          (Dict.get nextRel parsed `Maybe.andThen` parseParams)

parseLinkTuple : String -> Maybe (String, String)
parseLinkTuple header =
  case Regex.find (Regex.AtMost 1) linkHeaderRegex header of
    [] ->
      Nothing

    {submatches} :: _ ->
      case submatches of
        (Just url :: Just rel :: _) ->
          Just (rel, url)

        _ ->
          Nothing

parseParams : String -> Maybe Page
parseParams =
  fromQuery << snd << extractQuery

extractQuery : String -> (String, Dict String String)
extractQuery url =
  case String.split "?" url of
    baseURL :: query :: _ ->
      (baseURL, parseQuery query)

    _ ->
      (url, Dict.empty)

setQuery : String -> Dict String String -> String
setQuery baseURL query =
  let
    params =
      String.join "&" <|
        List.map (\(k, v) -> k ++ "=" ++ v) (Dict.toList query)
  in
    if params == "" then
      baseURL
    else
      baseURL ++ "?" ++ params

parseQuery : String -> Dict String String
parseQuery query =
  let
    parseParam p =
      case String.split "=" p of
        k :: vs ->
          (k, String.join "=" vs)

        [] ->
          ("", "")
  in
    Dict.fromList <|
      List.map parseParam <|
        String.split "&" query

addParams : String -> Maybe Page -> String
addParams url page =
  let
    (baseURL, query) = extractQuery url
  in
    setQuery baseURL (Dict.union query (toQuery page))

fromQuery : Dict String String -> Maybe Page
fromQuery query =
  let
    num = Maybe.withDefault 1 <|
      Dict.get "page" query `Maybe.andThen` parseNum
  in
    Just num

toQuery : Maybe Page -> Dict String String
toQuery page =
  case page of
    Nothing ->
      Dict.empty

    Just num ->
      Dict.singleton "page" (toString num)

parseNum : String -> Maybe Int
parseNum = Result.toMaybe << String.toInt
