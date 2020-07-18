module Test.Generated.Main2805904964 exposing (main)

import PhotoGrooveTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "PhotoGrooveTests" [PhotoGrooveTests.decoderTest] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 71064698613932, processes = 12, paths = ["C:\\Users\\Braxton\\IdeaProjects\\PhotoGroove\\tests\\PhotoGrooveTests.elm"]}