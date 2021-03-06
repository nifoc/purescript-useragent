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

    isAndroidBrowser :: String -> Boolean

    isChrome :: String -> Boolean

    isEkioh :: String -> Boolean

    isFirefox :: String -> Boolean

    isFreeBSD :: String -> Boolean

    isIOS :: String -> Boolean

    isKreaTV :: String -> Boolean

    isLinux :: String -> Boolean

    isMSIE :: String -> Boolean

    isMacOSX :: String -> Boolean

    isMobileSafari :: String -> Boolean

    isNetBSD :: String -> Boolean

    isOpenBSD :: String -> Boolean

    isOpera :: String -> Boolean

    isOperaMini :: String -> Boolean

    isSafari :: String -> Boolean

    isSeaMonkey :: String -> Boolean

    isWindows :: String -> Boolean

    majorVersion :: String -> Maybe Number

    name :: String -> Maybe String

    parse :: String -> Maybe UserAgent

    platform :: String -> Maybe String

    vendor :: String -> Maybe String

    version :: String -> Maybe Number