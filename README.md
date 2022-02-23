# TOTP

### Getting started

Initialize a Key

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

And generate a QR code using [pablohirafuji/elm-qrcode](https://package.elm-lang.org/packages/pablohirafuji/elm-qrcode/latest/)

```elm
view : TOTP.Key.Key -> Html msg
view key =
    QRCode.fromString (TOTP.Key.toString key)
        |> Result.map (QRCode.toSvg [])
        |> Result.withDefault (text "Error while encoding to QRCode.")
```