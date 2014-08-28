-- Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
--
-- Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
-- provided that the above copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
-- NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module Main where

import Data.Array
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Debug.Trace
import Web.UAParser

data TestCase = TestCase { userAgent :: String
                         , expectedResult :: UserAgent
                         }

{- User Agents -}

chrome = [ TestCase { userAgent: "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 37
                                                , version: 37
                                                , vendor: "Google"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.21 (KHTML, like Gecko) Chrome/19.0.1041.0 Safari/535.21"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 19
                                                , version: 19
                                                , vendor: "Google"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Macintosh; AMD Mac OS X 10_8_2) AppleWebKit/535.22 (KHTML, like Gecko) Chrome/18.6.872"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 18
                                                , version: 18.6
                                                , vendor: "Google"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US) AppleWebKit/525.19 (KHTML, like Gecko) Chrome/1.0.154.42 Safari/525.19"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 1
                                                , version: 1
                                                , vendor: "Google"
                                                }
                    }
         ]

firefox = [ TestCase { userAgent: "Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 31
                                                 , version: 31
                                                 , vendor: "Mozilla"
                                                 }
                     }
          , TestCase { userAgent: "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.1.16) Gecko/20120421 Gecko Firefox/11.0"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 11
                                                 , version: 11
                                                 , vendor: "Mozilla"
                                                 }
                     }
          , TestCase { userAgent: "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.9) Gecko/20100915 Gentoo Firefox/3.6.9"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 3
                                                 , version: 3.6
                                                 , vendor: "Mozilla"
                                                 }
                     }
          , TestCase { userAgent: "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8) Gecko/20060130 Ubuntu/1.5.dfsg-4ubuntu6 Firefox/1.5"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 1
                                                 , version: 1.5
                                                 , vendor: "Mozilla"
                                                 }
                     }
          ]

safari = [ TestCase { userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 5
                                                , version: 5.1
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Windows; U; Windows NT 6.1; sv-SE) AppleWebKit/533.19.4 (KHTML, like Gecko) Version/5.0.3 Safari/533.19.4"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 5
                                                , version: 5
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Windows; U; Windows NT 6.0; fr-ch) AppleWebKit/531.9 (KHTML, like Gecko) Version/4.0.3 Safari/531.9"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 4
                                                , version: 4
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Macintosh; U; PPC Mac OS X; sv-se) AppleWebKit/419 (KHTML, like Gecko) Safari/419.3"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 2
                                                , version: 2
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Macintosh; U; PPC Mac OS X; it-it) AppleWebKit/124 (KHTML, like Gecko) Safari/125.1"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 1
                                                , version: 1.2
                                                , vendor: "Apple"
                                                }
                    }
          ]

none = [ TestCase { userAgent: "NotABrowser/11.11"
                  , expectedResult: failingUA
                  }
       ]

{- Helper -}

failingUA = UserAgent { name: "None"
                      , majorVersion: 0
                      , version: 0
                      , vendor: "None"
                      }

eq :: forall a. (Eq a, Show a) => a -> a -> String
eq a b = if a == b then "OK" else "ERROR " ++ show a ++ " does not equal " ++ show b

runTestCases :: [TestCase] -> [String]
runTestCases tc = map (\(TestCase t) -> eq t.expectedResult <<< fromMaybe failingUA <<< parse $ t.userAgent) tc

{- Runner -}

main = do
  trace "Detects Chrome"
  print $ runTestCases chrome

  trace "Detects Firefox"
  print $ runTestCases firefox

  trace "Detects Safari"
  print $ runTestCases safari

  trace "Ignores unknown browsers"
  print $ runTestCases none
