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
  isChrome,
  isFirefox,
  isMobileSafari,
  isMSIE,
  isOpera,
  isOperaMini,
  isSafari
  ) where

import Data.Maybe
import Data.Array
import qualified Data.String.Regex as RE

foreign import floor "var floor = Math.floor;" :: Number -> Number
foreign import readFloat "var readFloat = parseFloat;" :: String -> Number

data UserAgent = UserAgent { name :: String
                           , majorVersion :: Number
                           , version :: Number
                           , vendor :: String
                           }

instance showUserAgent :: Show UserAgent where
  show (UserAgent u) = u.name ++ " " ++ show u.majorVersion ++ " (" ++ u.vendor ++ ")"

instance eqUserAgent :: Eq UserAgent where
  (==) (UserAgent a) (UserAgent b) = a.name == b.name && a.version == b.version && a.vendor == b.vendor
  (/=) a b = not (a == b)

mkUserAgent name version vendor = UserAgent { name: name
                                            , majorVersion: floor version
                                            , version: version
                                            , vendor: vendor
                                            }

{- Detectors -}

isChrome :: String -> Boolean
isChrome ua = regexTest "\\sChrome/\\d+" ua && not (isOpera ua)

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

{- API -}

parse :: String -> Maybe UserAgent
parse ua | isChrome ua       = let version = extractVersionNumber <<< regexMatch "Chrome/([\\d|\\.]+)" $ ua
                               in  Just $ mkUserAgent "Chrome" version "Google"
parse ua | isFirefox ua      = let version = extractVersionNumber <<< regexMatch "Firefox/([\\d|\\.]+)" $ ua
                               in  Just $ mkUserAgent "Firefox" version "Mozilla"
parse ua | isMobileSafari ua = let version = extractVersionNumber <<< regexMatch "Version/([\\d|\\.]+)" $ ua
                               in  Just $ mkUserAgent "Mobile Safari" version "Apple"
parse ua | isMSIE ua         = let version = extractVersionNumber <<< regexMatch "MSIE ([\\d|\\.]+);" $ ua
                               in  Just $ mkUserAgent "Internet Explorer" version "Microsoft"
parse ua | isOpera ua        = let version = if regexTest "Version/\\d+" ua
                                             then extractVersionNumber <<< regexMatch "Version/([\\d|\\.]+)" $ ua
                                             else extractVersionNumber $ regexMatch' "Ope?ra?[\\s|/]([\\d|\\.]+)" "i" ua
                               in  Just $ mkUserAgent "Opera" version "Opera"
parse ua | isOperaMini ua    = let version = extractVersionNumber <<< regexMatch "Opera\\sMini/([\\d|\\.]+)" $ ua
                               in  Just $ mkUserAgent "Opera Mini" version "Opera"
parse ua | isSafari ua       = let version = if regexTest "Version/\\d+" ua
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
                               in  Just $ mkUserAgent "Safari" version "Apple"
parse _                      = Nothing

{- Helper -}

regexTest expr = regexTest' expr ""

regexTest' expr flags = RE.test $ RE.regex expr (RE.parseFlags flags)

regexMatch expr = regexMatch' expr ""

regexMatch' expr flags = RE.match $ RE.regex expr (RE.parseFlags flags)

extractVersionNumber matches = readFloat <<< fromMaybe "0" $ matches !! 1
