module Base32StringTest exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz)
import TOTP.Base32String exposing (stringToBase32, bytesFromBase32)
import String.Extra
import Bytes.Extra


suite : Test
suite =
    describe "Base32String"
        [ fuzz Fuzz.string "stringToBase32 and bytesFromBase32 are reversible for any string" <|
            \str ->
                str
                    |> stringToBase32
                    |> Result.andThen bytesFromBase32
                    |> Result.map Bytes.Extra.toByteValues
                    |> Result.map String.Extra.fromCodePoints
                    |> Expect.equal (Ok str)
        ]
