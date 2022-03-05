module TOTP exposing
    ( generateTOTP
    , test
    )

{-| Based on specification in <https://datatracker.ietf.org/doc/html/rfc6238>


## Main function

@docs generateTOTP


## Helper functions

@docs test

-}

import Array exposing (Array)
import Base16
import Base32
import Bytes exposing (Bytes)
import Bytes.Extra
import Crypto.HMAC
import Hex
import String.Extra
import TOTP.Algorithm
import TOTP.Base32String exposing (Base32String(..))
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


{-| <https://datatracker.ietf.org/doc/html/rfc6238#appendix-B>

    import TOTP.Algorithm
    import TOTP.Base32String exposing (Base32String(..))
    import Time

    seed : Base32String
    seed =
        "12345678901234567890"
            |> TOTP.Base32String.stringToBase32
            |> Result.withDefault (TOTP.Base32String.test.base32String "error")

    seed32 : Base32String
    seed32 =
        "12345678901234567890123456789012"
            |> TOTP.Base32String.stringToBase32
            |> Result.withDefault (TOTP.Base32String.test.base32String "error")

    seed64 : Base32String
    seed64 =
        "1234567890123456789012345678901234567890123456789012345678901234"
            |> TOTP.Base32String.stringToBase32
            |> Result.withDefault (TOTP.Base32String.test.base32String "error")

    testTable : List (Int, String, (TOTP.Algorithm.Algorithm, Base32String))
    testTable =
        [ ( 59,         "94287082",  ( TOTP.Algorithm.SHA1,   seed ) )
        , ( 59,         "46119246",  ( TOTP.Algorithm.SHA256, seed32 ) )
        , ( 59,         "90693936",  ( TOTP.Algorithm.SHA512, seed64 ) )
        , ( 1111111109, "07081804",  ( TOTP.Algorithm.SHA1,   seed ) )
        , ( 1111111109, "68084774",  ( TOTP.Algorithm.SHA256, seed32 ) )
        , ( 1111111109, "25091201",  ( TOTP.Algorithm.SHA512, seed64 ) )
        , ( 1111111111, "14050471",  ( TOTP.Algorithm.SHA1,   seed ) )
        , ( 1111111111, "67062674",  ( TOTP.Algorithm.SHA256, seed32 ) )
        , ( 1111111111, "99943326",  ( TOTP.Algorithm.SHA512, seed64 ) )
        , ( 1234567890, "89005924",  ( TOTP.Algorithm.SHA1,   seed ) )
        , ( 1234567890, "91819424",  ( TOTP.Algorithm.SHA256, seed32 ) )
        , ( 1234567890, "93441116",  ( TOTP.Algorithm.SHA512, seed64 ) )
        , ( 2000000000, "69279037",  ( TOTP.Algorithm.SHA1,   seed ) )
        , ( 2000000000, "90698825",  ( TOTP.Algorithm.SHA256, seed32 ) )
        , ( 2000000000, "38618901",  ( TOTP.Algorithm.SHA512, seed64 ) )
        , ( 20000000000, "65353130", ( TOTP.Algorithm.SHA1,   seed ) )
        , ( 20000000000, "77737706", ( TOTP.Algorithm.SHA256, seed32 ) )
        , ( 20000000000, "47863826", ( TOTP.Algorithm.SHA512, seed64 ) )
        ]

    testTable
        |> List.map (\(secs, _, (alg, secret)) -> generateTOTP alg { outputLength = 8, periodSeconds = 30 } secret (Time.millisToPosix (secs * 1000)))
    --> List.map (\(_, answer, _) -> Ok answer) testTable

-}
generateTOTP : TOTP.Algorithm.Algorithm -> { outputLength : Int, periodSeconds : Int } -> Base32String -> Time.Posix -> Result String String
generateTOTP algo { outputLength, periodSeconds } base32Secret now =
    Base16.decode (hexCounter (valueT periodSeconds now))
        |> Result.map (\counter -> generateHOTP algo outputLength counter base32Secret)
        |> Result.andThen (Result.fromMaybe "dynamicTruncation failed")


{-| <https://datatracker.ietf.org/doc/html/rfc4226#section-5.4>

TOTP is the time-based variant of this algorithm, where a value T,
derived from a time reference and a time step, replaces the counter C
in the HOTP computation.

-}
generateHOTP : TOTP.Algorithm.Algorithm -> Int -> List Int -> Base32String -> Maybe String
generateHOTP algo outputLength counter base32Secret =
    TOTP.Base32String.bytesFromBase32 base32Secret
        |> Result.toMaybe
        |> Maybe.map (\secret -> TOTP.Algorithm.digestBytes algo secret (Bytes.Extra.fromByteValues counter))
        |> Maybe.map Array.fromList
        |> Maybe.andThen (dynamicTruncation outputLength)


{-| Exposed functions to facilitate unit testing of this library
-}
test :
    { valueT : Int -> Time.Posix -> ValueT
    , newValueT : Int -> ValueT
    , hexCounter : ValueT -> String
    , dynamicTruncation : Int -> Array Int -> Maybe String
    , base32String : String -> Base32String
    }
test =
    { valueT = valueT
    , newValueT = ValueT
    , hexCounter = hexCounter
    , dynamicTruncation = dynamicTruncation
    , base32String = Base32String
    }


type ValueT
    = ValueT Int


{-| <https://datatracker.ietf.org/doc/html/rfc6238#section-4.2>

For example, with T0 = 0 and Time Step X = 30, T = 1 if the current
Unix time is 59 seconds, and T = 2 if the current Unix time is
60 seconds.

    import Time

    test.valueT 30 (Time.millisToPosix 59000)
    --> test.newValueT 1

    test.valueT 30 (Time.millisToPosix 60000)
    --> test.newValueT 2

-}
valueT : Int -> Time.Posix -> ValueT
valueT timeStepSeconds now =
    let
        t =
            Time.posixToMillis now
                -- float, otherwise overflow at `20000000000` test
                |> (\millis -> round (toFloat millis / 1000))
                |> (\secs -> secs // timeStepSeconds)
    in
    ValueT t


{-|

    test.hexCounter (test.newValueT 1)
    --> "0000000000000001"

    test.hexCounter (test.newValueT 59)
    --> "000000000000003B"

-}
hexCounter : ValueT -> String
hexCounter (ValueT t) =
    Hex.toString t
        |> String.toUpper
        |> String.padLeft 16 '0'


{-| <https://datatracker.ietf.org/doc/html/rfc4226#section-5.4>

    import Hex
    import Array exposing (Array)

    input : Array Int
    input =
        ["1f","86","98","69","0e","02","ca","16","61","85","50","ef","7f","19","da","8e","94","5b","55","5a"]
            |> List.map Hex.fromString
            |> List.filterMap Result.toMaybe
            |> Array.fromList


    test.dynamicTruncation 6 input
    --> Just "872921"

-}
dynamicTruncation : Int -> Array Int -> Maybe String
dynamicTruncation outputLength bytes =
    let
        resultOffset =
            Array.get (Array.length bytes - 1) bytes
                |> Maybe.map (\i -> modBy 16 i)

        value offset =
            [ Array.get offset bytes
                |> Maybe.map (\i -> modBy 128 i * (2 ^ 24))
            , Array.get (offset + 1) bytes
                |> Maybe.map (\i -> modBy 255 i * (2 ^ 16))
            , Array.get (offset + 2) bytes
                |> Maybe.map (\i -> modBy 255 i * (2 ^ 8))
            , Array.get (offset + 3) bytes
                |> Maybe.map (\i -> modBy 255 i)
            ]
                |> List.filterMap identity
                |> List.foldl (+) 0
                |> modBy (10 ^ outputLength)
                |> String.fromInt
                |> String.padLeft outputLength '0'
    in
    resultOffset
        |> Maybe.map value
