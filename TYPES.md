# Module Documentation

## Module Web.UAParser

### Types

    data UserAgent where
      UserAgent :: { vendor :: String, platform :: String, version :: Number, majorVersion :: Number, name :: String } -> UserAgent


### Type Class Instances

    instance eqUserAgent :: Eq UserAgent

    instance showUserAgent :: Show UserAgent


### Values

    isAndroid :: String -> Boolean

    isChrome :: String -> Boolean

    isFirefox :: String -> Boolean

    isIOS :: String -> Boolean

    isLinux :: String -> Boolean

    isMSIE :: String -> Boolean

    isMacOSX :: String -> Boolean

    isMobileSafari :: String -> Boolean

    isOpera :: String -> Boolean

    isOperaMini :: String -> Boolean

    isSafari :: String -> Boolean

    isWindows :: String -> Boolean

    majorVersion :: String -> Maybe Number

    name :: String -> Maybe String

    parse :: String -> Maybe UserAgent

    platform :: String -> Maybe String

    vendor :: String -> Maybe String

    version :: String -> Maybe Number