-- Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
--
-- Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
-- provided that the above copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
-- NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module Web.UAParser (
  UserAgent(..),
  parse,
  name,
  majorVersion,
  version,
  platform,
  vendor,
  isAndroidBrowser,
  isChrome,
  isEkioh,
  isFirefox,
  isMobileSafari,
  isMSIE,
  isOpera,
  isOperaMini,
  isSafari,
  isAndroid,
  isIOS,
  isKreaTV,
  isLinux,
  isMacOSX,
  isWindows
  ) where

import Data.Maybe
import Data.Array
import qualified Data.String.Regex as RE

foreign import floor "var floor = Math.floor;" :: Number -> Number
foreign import readFloat "var readFloat = parseFloat;" :: String -> Number

data UserAgent = UserAgent { name :: String
                           , majorVersion :: Number
                           , version :: Number
                           , platform :: String
                           , vendor :: String
                           }

instance showUserAgent :: Show UserAgent where
  show (UserAgent u) = u.name ++ " " ++ show u.majorVersion ++ " (" ++ u.vendor ++ ")"

instance eqUserAgent :: Eq UserAgent where
  (==) (UserAgent a) (UserAgent b) = a.name == b.name && a.version == b.version &&
                                     a.platform == b.platform && a.vendor == b.vendor
  (/=) a b = not (a == b)

mkUserAgent name version platform vendor = UserAgent { name: name
                                                     , majorVersion: floor version
                                                     , version: version
                                                     , platform: platform
                                                     , vendor: vendor
                                                     }

{- Detectors -}

-- Browsers
isAndroidBrowser :: String -> Boolean
isAndroidBrowser = regexTest "\\sAndroid.+\\sVersion/[\\d|\\.]+\\sMobile\\sSafari"

isChrome :: String -> Boolean
isChrome ua = regexTest "\\sChrome/\\d+" ua && not (isOpera ua)

isEkioh :: String -> Boolean
isEkioh = regexTest "\\sEkioh/\\d+"

isFirefox :: String -> Boolean
isFirefox = regexTest "\\sGecko/\\d+\\s?.+\\sFirefox/\\d+"

isMobileSafari :: String -> Boolean
isMobileSafari = regexTest "AppleWebKit/[\\d|\\.]+\\+?\\s.+Version/[\\d|\\.]+\\sMobile/\\w+\\sSafari/[\\d|\\.]+"

isMSIE :: String -> Boolean
isMSIE ua = regexTest ";\\sMSIE\\s[\\d|\\.]+;" ua && not (isOpera ua)

isOpera :: String -> Boolean
isOpera ua = regexTest' "Ope?ra?[\\s|/]\\d+" "i" ua && not (isOperaMini ua)

isOperaMini :: String -> Boolean
isOperaMini = regexTest "Opera\\sMini/\\d+"

isSafari :: String -> Boolean
isSafari ua = regexTest "AppleWebKit/[\\d|\\.]+\\+?\\s.+\\sSafari/[\\d|\\.]+" ua &&
              not (isChrome ua) &&
              not (isMobileSafari ua) &&
              not (isOpera ua)

-- Platforms
isAndroid :: String -> Boolean
isAndroid = regexTest "Android[\\s|;]"

isIOS :: String -> Boolean
isIOS ua = regexTest "\\(iPhone" ua || regexTest "\\(iPad" ua || regexTest "\\(iPod" ua

isKreaTV :: String -> Boolean
isKreaTV = regexTest "\\sKreaTV\\s"

isLinux :: String -> Boolean
isLinux ua = regexTest "Linux" ua &&
             not (isAndroid ua) &&
             not (isKreaTV ua)

isMacOSX :: String -> Boolean
isMacOSX = regexTest "Mac\\sOS\\sX"

isWindows :: String -> Boolean
isWindows = regexTest "Windows"

{- API -}

parse :: String -> Maybe UserAgent
parse ua | isAndroidBrowser ua = let version = extractVersionNumber <<< regexMatch "Version/([\\d|\\.]+)" $ ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Android Browser" version platform "Google"
parse ua | isChrome ua         = let version = extractVersionNumber <<< regexMatch "Chrome/([\\d|\\.]+)" $ ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Chrome" version platform "Google"
parse ua | isEkioh ua          = let version = extractVersionNumber <<< regexMatch "Ekioh/([\\d|\\.]+)" $ ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Ekioh" version platform "Ekioh"
parse ua | isFirefox ua        = let version = extractVersionNumber <<< regexMatch "Firefox/([\\d|\\.]+)" $ ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Firefox" version platform "Mozilla"
parse ua | isMobileSafari ua   = let version = extractVersionNumber <<< regexMatch "Version/([\\d|\\.]+)" $ ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Mobile Safari" version platform "Apple"
parse ua | isMSIE ua           = let version = extractVersionNumber <<< regexMatch "MSIE ([\\d|\\.]+);" $ ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Internet Explorer" version platform "Microsoft"
parse ua | isOpera ua          = let version = if regexTest "Version/\\d+" ua
                                               then extractVersionNumber <<< regexMatch "Version/([\\d|\\.]+)" $ ua
                                               else extractVersionNumber $ regexMatch' "Ope?ra?[\\s|/]([\\d|\\.]+)" "i" ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Opera" version platform "Opera"
parse ua | isOperaMini ua      = let version = extractVersionNumber <<< regexMatch "Opera\\sMini/([\\d|\\.]+)" $ ua
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Opera Mini" version platform "Opera"
parse ua | isSafari ua         = let version = if regexTest "Version/\\d+" ua
                                               then extractVersionNumber <<< regexMatch "Version/([\\d|\\.]+)" $ ua
                                               else webkitToSafariVersion <<< regexMatch "Safari/(\\d+)" $ ua
                                     webkitToSafariVersion matches = case readFloat <<< fromMaybe "0" $ matches !! 1 of
                                                                          419 -> 2
                                                                          418 -> 2
                                                                          417 -> 2
                                                                          416 -> 2
                                                                          412 -> 2
                                                                          312 -> 1.3
                                                                          125 -> 1.2
                                                                          124 -> 1.2
                                                                          85  -> 1
                                                                          _   -> 0
                                     platform = extractPlatform ua
                                 in  Just $ mkUserAgent "Safari" version platform "Apple"
parse _                        = Nothing

name :: String -> Maybe String
name ua = do (UserAgent agent) <- parse ua
             return agent.name

majorVersion :: String -> Maybe Number
majorVersion ua = do (UserAgent agent) <- parse ua
                     return agent.majorVersion

version :: String -> Maybe Number
version ua = do (UserAgent agent) <- parse ua
                return agent.version

platform :: String -> Maybe String
platform ua = do (UserAgent agent) <- parse ua
                 return agent.platform

vendor :: String -> Maybe String
vendor ua = do (UserAgent agent) <- parse ua
               return agent.vendor

{- Helper -}

regexTest expr = regexTest' expr ""

regexTest' expr flags = RE.test $ RE.regex expr (RE.parseFlags flags)

regexMatch expr = regexMatch' expr ""

regexMatch' expr flags = RE.match $ RE.regex expr (RE.parseFlags flags)

extractVersionNumber matches = readFloat <<< fromMaybe "0" $ matches !! 1

extractPlatform ua | isAndroid ua = "Android"
extractPlatform ua | isIOS ua     = "iOS"
extractPlatform ua | isKreaTV ua  = "KreaTV"
extractPlatform ua | isLinux ua   = "Linux"
extractPlatform ua | isMacOSX ua  = "Mac OS X"
extractPlatform ua | isWindows ua = "Windows"
extractPlatform _                 = "Unknown"
