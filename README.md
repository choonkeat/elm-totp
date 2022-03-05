# TOTP

Elm implementation of [RFC6238](https://datatracker.ietf.org/doc/html/rfc6238).

You can use this library with [pablohirafuji/elm-qrcode](https://package.elm-lang.org/packages/pablohirafuji/elm-qrcode/latest/) to generate a QR code for your user to setup their OTP authenticator devices.

You can also use this library in the backendn (for example [choonkeat/elm-webapp](https://github.com/choonkeat/elm-webapp)) to verify if a user input matches their OTP.

1. Initialize a Key

    ```elm
    keyResult : Result String TOTP.Key.Key
    keyResult =
        TOTP.Key.init
            { issuer = "ACME Co"
            , user = "john@example.com"
            , rawSecret = "topsecret"
            , outputLength = Nothing
            , periodSeconds = Nothing
            , algorithm = TOTP.Algorithm.SHA1
            }
    ```

2. And generate a QR code using [pablohirafuji/elm-qrcode](https://package.elm-lang.org/packages/pablohirafuji/elm-qrcode/latest/)

    ```elm
    view : TOTP.Key.Key -> Html msg
    view key =
        QRCode.fromString (TOTP.Key.toString key)
            |> Result.map (QRCode.toSvg [])
            |> Result.withDefault (text "Error while encoding to QRCode.")
    ```

3. Check if a user input String matches the OTP code

    ```elm
    TOTP.Key.code now key == inputString
    ```

4. Save `Key` into your database by converting it into a `String` first

    ```elm
    TOTP.Key.toString key
    ```

5. Load `Key` from your database,
    ```elm
    TOTP.Key.fromString key
    ```

See the code in [example](https://github.com/choonkeat/elm-totp/tree/main/example). The running example instance can be found at [elm-totp.netlify.app](https://elm-totp.netlify.app)