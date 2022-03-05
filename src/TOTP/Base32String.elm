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
-}
stringToBase32 : String -> Result String Base32String
stringToBase32 rawString =
    String.Extra.toCodePoints rawString
        |> Base32.encode
        |> Result.map Base32String


bytesFromBase32 : Base32String -> Result String Bytes
bytesFromBase32 (Base32String s) =
    Base32.decode s
        |> Result.map Bytes.Extra.fromByteValues


test =
    { base32String = Base32String
    }
