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
  isMSIE,
  isSafari
  ) where

import Data.Maybe
import Data.Array
import Data.String.Regex (regex, test, match)

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

{- Detectors -}

isChrome ua = regexTest "\\sChrome/\\d+" ua

isFirefox ua = regexTest "\\sGecko/\\d+\\s?.+\\sFirefox/\\d+" ua

isMSIE ua = regexTest ";\\sMSIE\\s[\\d|\\.]+;" ua

isSafari ua = isNew || isOld
  where
  isNew = regexTest "AppleWebKit/[\\d|\\.]+\\+?\\s.+\\sVersion/[\\d|\\.]+\\sSafari/\\d+" ua
  isOld = regexTest "AppleWebKit/[\\d|\\.]+\\+?\\s\\(KHTML,\\slike Gecko\\)\\sSafari/[\\d|\\.]+" ua

{- API -}

parse :: String -> Maybe UserAgent
parse ua | isChrome ua  = let version = extractVersionNumber <<< regexMatch "Chrome/([\\d|\\.]+)" $ ua
                          in  Just $ UserAgent { name: "Chrome"
                                               , majorVersion: floor version
                                               , version: version
                                               , vendor: "Google"
                                               }
parse ua | isFirefox ua = let version = extractVersionNumber <<< regexMatch "Firefox/([\\d|\\.]+)" $ ua
                          in  Just $ UserAgent { name: "Firefox"
                                               , majorVersion: floor version
                                               , version: version
                                               , vendor: "Mozilla"
                                               }
parse ua | isMSIE ua    = let version = extractVersionNumber <<< regexMatch "MSIE ([\\d|\\.]+);" $ ua
                          in  Just $ UserAgent { name: "Internet Explorer"
                                               , majorVersion: floor version
                                               , version: version
                                               , vendor: "Microsoft"
                                               }
parse ua | isSafari ua  = let version = if regexTest "Version/\\d+" ua
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
                          in  Just $ UserAgent { name: "Safari"
                                               , majorVersion: floor version
                                               , version: version
                                               , vendor: "Apple"
                                               }
parse _                 = Nothing

{- Helper -}

buildRegex expr = regex expr { global: false
                             , ignoreCase: false
                             , multiline: false
                             , sticky: false
                             , unicode: false
                             }

regexTest expr target = test (buildRegex expr) target

regexMatch expr target = match (buildRegex expr) target

extractVersionNumber matches = readFloat <<< fromMaybe "0" $ matches !! 1
