module TOTP.Algorithm exposing
    ( Algorithm(..)
    , digestBytes, fromString, toString
    )

{-|


## Types

@docs Algorithm


## Helper functions

@docs digestBytes, fromString, toString

-}

import Bytes exposing (Bytes)
import Bytes.Extra
import Crypto.HMAC
import HmacSha1
import HmacSha1.Key


{-| <https://github.com/google/google-authenticator/wiki/Key-Uri-Format#algorithm>

Currently, the algorithm parameter is ignored by the Google Authenticator implementations.

Default is SHA1

-}
type Algorithm
    = SHA1
    | SHA256
    | SHA512


{-| String representation of `Algorithm` value
-}
toString : Algorithm -> String
toString alg =
    case alg of
        SHA1 ->
            "SHA1"

        SHA256 ->
            "SHA256"

        SHA512 ->
            "SHA512"


{-| Function to resolve the `Algorithm` from a `String` value.

This is useful when working with HTML inputs, allowing users to choose
which algorithm to use.

-}
fromString : String -> Maybe Algorithm
fromString str =
    case str of
        "SHA1" ->
            Just SHA1

        "SHA256" ->
            Just SHA256

        "SHA512" ->
            Just SHA512

        _ ->
            Nothing


{-| We need this function to route calls between underlying libraries

  - [rlopzc/elm-hmac-sha1](https://package.elm-lang.org/packages/rlopzc/elm-hmac-sha1/latest/)
  - [ktonon/elm-crypto](https://package.elm-lang.org/packages/ktonon/elm-crypto/latest)

because [ktonon/elm-crypto does not support SHA1](https://github.com/ktonon/elm-crypto/issues/3)

-}
digestBytes : Algorithm -> Bytes.Bytes -> Bytes.Bytes -> List Int
digestBytes alg keyBytes msgBytes =
    case alg of
        SHA1 ->
            HmacSha1.fromBytes
                (HmacSha1.Key.fromBytes keyBytes)
                msgBytes
                |> HmacSha1.toByteValues

        SHA256 ->
            Crypto.HMAC.digestBytes Crypto.HMAC.sha256
                (Bytes.Extra.toByteValues keyBytes)
                (Bytes.Extra.toByteValues msgBytes)

        SHA512 ->
            Crypto.HMAC.digestBytes Crypto.HMAC.sha512
                (Bytes.Extra.toByteValues keyBytes)
                (Bytes.Extra.toByteValues msgBytes)
