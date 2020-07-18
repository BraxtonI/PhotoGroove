module Test.Generated.Main1056730544 exposing (main)

import PhotoGrooveTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "PhotoGrooveTests" [PhotoGrooveTests.decoderTest,
    PhotoGrooveTests.noPhotosNoThumbnails,
    PhotoGrooveTests.sliders] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 183034806426682, processes = 12, paths = ["C:\\Users\\Braxton\\IdeaProjects\\PhotoGroove\\tests\\PhotoGrooveTests.elm"]}