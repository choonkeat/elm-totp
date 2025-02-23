module TOTP.Base32String exposing (Base32String(..), bytesFromBase32, stringToBase32, test)

{-| A wrapper for `String` to denote that its value is encoded in base32
-}

import Base32
import Bytes exposing (Bytes)
import Bytes.Extra
import String.Extra


type Base32String
    = Base32String String


{-| Attempt to convert a `String` value to `Base32String`

    import String.Extra
    import Base32
    import Bytes.Extra
    
    -- Converting a simple string to Base32String and back should work
    "Hello, World!"
        |> stringToBase32
        |> Result.andThen bytesFromBase32
        |> Result.map Bytes.Extra.toByteValues
        |> Result.map String.Extra.fromCodePoints
    --> Ok "Hello, World!"
    
    -- Empty string should also work
    ""
        |> stringToBase32
        |> Result.andThen bytesFromBase32
        |> Result.map Bytes.Extra.toByteValues
        |> Result.map String.Extra.fromCodePoints
    --> Ok ""
    
    -- Unicode characters should be preserved
    "🌍👋🏼"
        |> stringToBase32
        |> Result.andThen bytesFromBase32
        |> Result.map Bytes.Extra.toByteValues
        |> Result.map String.Extra.fromCodePoints
    --> Ok "🌍👋🏼"
-}
stringToBase32 : String -> Result String Base32String
stringToBase32 rawString =
    String.Extra.toCodePoints rawString
        |> Base32.encode
        |> Result.map (String.replace "=" "")
        |> Result.map Base32String


bytesFromBase32 : Base32String -> Result String Bytes
bytesFromBase32 (Base32String s) =
    let
        padLength =
            (8 - modBy 8 (String.length s)) |> modBy 8

        padded =
            s ++ String.repeat padLength "="
    in
    Base32.decode padded
        |> Result.map Bytes.Extra.fromByteValues


test =
    { base32String = Base32String
    }
