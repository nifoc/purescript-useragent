# Module Documentation

## Module Web.UAParser

### Types

    data UserAgent where
      UserAgent :: { vendor :: String, version :: Number, majorVersion :: Number, name :: String } -> UserAgent


### Type Class Instances

    instance eqUserAgent :: Eq UserAgent

    instance showUserAgent :: Show UserAgent


### Values

    isChrome :: String -> Boolean

    isFirefox :: String -> Boolean

    isMSIE :: String -> Boolean

    isMobileSafari :: String -> Boolean

    isOpera :: String -> Boolean

    isOperaMini :: String -> Boolean

    isSafari :: String -> Boolean

    parse :: String -> Maybe UserAgent