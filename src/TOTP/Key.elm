module TOTP.Key exposing
    ( Key
    , code, expiresIn, fromString, init, toString
    )

{-|


# Type

@docs Key


# Helper functions

@docs code, expiresIn, fromString, init, toString

-}

import Array exposing (Array)
import Base16
import Base32
import Bytes.Extra
import Crypto.HMAC
import Hex
import String.Extra
import TOTP
import TOTP.Algorithm
import TOTP.Base32String exposing (Base32String(..))
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


{-| An opaque type to hold the configuration value for a TOTP.

This is the value to store in your database, by calling `toString`

<https://github.com/google/google-authenticator/wiki/Key-Uri-Format#algorithm>

-}
type Key
    = Key
        { issuer : String
        , user : String
        , base32Secret : Base32String
        , outputLength : Maybe Int
        , periodSeconds : Maybe Int
        , algorithm : TOTP.Algorithm.Algorithm
        }


{-| Builds a `Key` value from the parameters
-}
init : { issuer : String, user : String, rawSecret : String, outputLength : Maybe Int, periodSeconds : Maybe Int, algorithm : TOTP.Algorithm.Algorithm } -> Result String Key
init { issuer, user, rawSecret, outputLength, periodSeconds, algorithm } =
    TOTP.Base32String.stringToBase32 rawSecret
        |> Result.map
            (\base32Secret ->
                Key
                    { user = user
                    , base32Secret = base32Secret
                    , issuer = issuer
                    , outputLength = outputLength
                    , periodSeconds = periodSeconds
                    , algorithm = algorithm
                    }
            )


{-| Attempt to return the expected OTP code at the given time.

Compare this value against the user input to verify if their OTP is correct.

-}
code : Time.Posix -> Key -> Result String String
code now (Key { issuer, user, base32Secret, outputLength, periodSeconds, algorithm }) =
    TOTP.generateTOTP algorithm
        { outputLength = Maybe.withDefault 6 outputLength
        , periodSeconds = Maybe.withDefault 30 periodSeconds
        }
        base32Secret
        now


{-| <https://rootprojects.org/authenticator/>

    import Base32
    import String.Extra
    import TOTP.Algorithm
    import TOTP

    keyResult : Result String Key
    keyResult =
        init
            { issuer = "ACME Co"
            , user = "john@example.com"
            , rawSecret = "HXDMVJECJJWSRB3HWIZR4IFUGFTMXBOZ"
                |> Base32.decode
                |> Result.map String.Extra.fromCodePoints
                |> Result.withDefault "<fail>"
            , outputLength = Just 6
            , periodSeconds = Just 30
            , algorithm = TOTP.Algorithm.SHA1
            }

    -- expected String output
    Result.map toString keyResult
    --> Ok "otpauth://totp/ACME%20Co:john@example.com?secret=HXDMVJECJJWSRB3HWIZR4IFUGFTMXBOZ&issuer=ACME%20Co&algorithm=SHA1&digits=6&period=30"


    -- toString and fromString are reversible
    Result.map toString keyResult
    |> Result.map fromString
    --> Ok (Result.toMaybe keyResult)

-}
toString : Key -> String
toString (Key { issuer, user, base32Secret, outputLength, periodSeconds, algorithm }) =
    let
        optionalParams =
            List.filterMap (\( k, v ) -> Maybe.map (String.fromInt >> Url.Builder.string k) v)
                [ ( "digits", outputLength )
                , ( "period", periodSeconds )
                ]

        percentEncodeExcept except str =
            -- ??
            Url.percentEncode str
                |> String.replace (Url.percentEncode except) except

        (Base32String secret) =
            base32Secret
    in
    { -- because Url.Protocol is Http | Https, we have to
      -- use a placeholder here, then String.replace below
      protocol = Url.Http
    , host = "totp"
    , port_ = Nothing

    --
    -- Secret keys may be encoded in QR codes as a URI with the following format:
    --
    --      otpauth://TYPE/LABEL?PARAMETERS
    --
    -- STRONGLY RECOMMENDED: ... If both issuer parameter and issuer label prefix are present, they should be equal.
    -- https://github.com/google/google-authenticator/wiki/Key-Uri-Format#issuer
    --
    , path = "/" ++ Url.percentEncode issuer ++ ":" ++ percentEncodeExcept "@" user
    , query =
        ([ Url.Builder.string "secret" secret
         , Url.Builder.string "issuer" issuer
         , Url.Builder.string "algorithm" (TOTP.Algorithm.toString algorithm)
         ]
            ++ optionalParams
        )
            |> Url.Builder.toQuery
            |> String.dropLeft 1
            |> Just
    , fragment = Nothing
    }
        |> Url.toString
        |> String.replace "http://" "otpauth://"


{-| Attempt to parse a String representation of `Key`
back into a `Key` value
-}
fromString : String -> Maybe Key
fromString str =
    str
        |> String.replace "otpauth://" "http://"
        |> Url.fromString
        |> Maybe.andThen (Url.Parser.parse urlParser)


urlParser : Url.Parser.Parser (Key -> b) b
urlParser =
    let
        splitKeyNameUser s =
            case Maybe.map (String.split ":") s of
                Just (head :: tail) ->
                    ( head, String.join ":" tail )

                _ ->
                    ( "error", "error" )

        buildKey ( keyName, user ) secret issuer algorithm outputLength periodSeconds =
            Key
                { user = user
                , base32Secret =
                    Maybe.withDefault "error" secret
                        |> Base32String
                , issuer = Maybe.withDefault keyName issuer
                , algorithm =
                    Maybe.andThen TOTP.Algorithm.fromString algorithm
                        |> Maybe.withDefault TOTP.Algorithm.SHA1
                , outputLength = outputLength
                , periodSeconds = periodSeconds
                }
    in
    Url.Parser.map buildKey
        ((Url.Parser.string |> Url.Parser.map (Url.percentDecode >> splitKeyNameUser))
            <?> Url.Parser.Query.string "secret"
            <?> Url.Parser.Query.string "issuer"
            <?> Url.Parser.Query.string "algorithm"
            <?> Url.Parser.Query.int "digits"
            <?> Url.Parser.Query.int "period"
        )


{-| Return the number of seconds the OTP from `code` is valid for
-}
expiresIn : Key -> Time.Posix -> Int
expiresIn (Key { periodSeconds }) now =
    let
        secs =
            Maybe.withDefault 30 periodSeconds
    in
    (Time.posixToMillis now // 1000)
        |> modBy secs
        |> (\i -> secs - i)
