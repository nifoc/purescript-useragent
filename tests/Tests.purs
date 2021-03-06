-- Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
--
-- Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
-- provided that the above copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
-- DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
-- NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module Tests where

import Data.Maybe
import Data.Array
import Control.Monad.Eff
import Debug.Trace

import Web.UAParser

data TestCase = TestCase { userAgent :: String
                         , expectedResult :: UserAgent
                         }

{- User Agents -}

androidBrowser = [ TestCase { userAgent: "Mozilla/5.0 (Linux; U; Android 4.0.3; ko-kr; LG-L160L Build/IML74K) AppleWebkit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Safari/534.30"
                            , expectedResult: UserAgent { name: "Android Browser"
                                                        , majorVersion: 4
                                                        , version: 4
                                                        , platform: "Android"
                                                        , vendor: "Google"
                                                        }
                            }
                 , TestCase { userAgent: "Mozilla/5.0 (Linux; U; Android 2.1-update1; es-mx; SonyEricssonE10a Build/2.0.A.0.504) AppleWebKit/530.17 (KHTML, like Gecko) Version/4.0 Mobile Safari/530.17"
                            , expectedResult: UserAgent { name: "Android Browser"
                                                        , majorVersion: 4
                                                        , version: 4
                                                        , platform: "Android"
                                                        , vendor: "Google"
                                                        }
                            }
                 , TestCase { userAgent: "Mozilla/5.0 (Linux; U; Android 1.6; ar-us; SonyEricssonX10i Build/R2BA026) AppleWebKit/528.5+ (KHTML, like Gecko) Version/3.1.2 Mobile Safari/525.20.1"
                            , expectedResult: UserAgent { name: "Android Browser"
                                                        , majorVersion: 3
                                                        , version: 3.1
                                                        , platform: "Android"
                                                        , vendor: "Google"
                                                        }
                            }
                 ]

chrome = [ TestCase { userAgent: "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 37
                                                , version: 37
                                                , platform: "Windows"
                                                , vendor: "Google"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.21 (KHTML, like Gecko) Chrome/19.0.1041.0 Safari/535.21"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 19
                                                , version: 19
                                                , platform: "Linux"
                                                , vendor: "Google"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Macintosh; AMD Mac OS X 10_8_2) AppleWebKit/535.22 (KHTML, like Gecko) Chrome/18.6.872"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 18
                                                , version: 18.6
                                                , platform: "Mac OS X"
                                                , vendor: "Google"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US) AppleWebKit/525.19 (KHTML, like Gecko) Chrome/1.0.154.42 Safari/525.19"
                    , expectedResult: UserAgent { name: "Chrome"
                                                , majorVersion: 1
                                                , version: 1
                                                , platform: "Windows"
                                                , vendor: "Google"
                                                }
                    }
         ]

ekioh = [ TestCase { userAgent: "Mozilla/5.0 (Linux) AppleWebKit/534.51 (KHTML, like Gecko) Ekioh/2.2.4.7-moto-mob Safari/534.51:534 Motorola KreaTV STB VIP1003"
                   , expectedResult: UserAgent { name: "Ekioh"
                                               , majorVersion: 2
                                               , version: 2.2
                                               , platform: "KreaTV"
                                               , vendor: "Ekioh"
                                               }
                   }
        ]

firefox = [ TestCase { userAgent: "Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 31
                                                 , version: 31
                                                 , platform: "Windows"
                                                 , vendor: "Mozilla"
                                                 }
                     }
          , TestCase { userAgent: "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.1.16) Gecko/20120421 Gecko Firefox/11.0"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 11
                                                 , version: 11
                                                 , platform: "Linux"
                                                 , vendor: "Mozilla"
                                                 }
                     }
          , TestCase { userAgent: "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.9) Gecko/20100915 Gentoo Firefox/3.6.9"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 3
                                                 , version: 3.6
                                                 , platform: "Linux"
                                                 , vendor: "Mozilla"
                                                 }
                     }
          , TestCase { userAgent: "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8) Gecko/20060130 Ubuntu/1.5.dfsg-4ubuntu6 Firefox/1.5"
                     , expectedResult: UserAgent { name: "Firefox"
                                                 , majorVersion: 1
                                                 , version: 1.5
                                                 , platform: "Linux"
                                                 , vendor: "Mozilla"
                                                 }
                     }
          ]

mobileSafari = [ TestCase { userAgent: "Mozilla/5.0 (iPad; CPU OS 6_0 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/6.0 Mobile/10A5376e Safari/8536.25"
                          , expectedResult: UserAgent { name: "Mobile Safari"
                                                      , majorVersion: 6
                                                      , version: 6
                                                      , platform: "iOS"
                                                      , vendor: "Apple"
                                                      }
                          }
               , TestCase { userAgent: "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_3_3 like Mac OS X; en-us) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8J2 Safari/6533.18.5"
                          , expectedResult: UserAgent { name: "Mobile Safari"
                                                      , majorVersion: 5
                                                      , version: 5
                                                      , platform: "iOS"
                                                      , vendor: "Apple"
                                                      }
                          }
               , TestCase { userAgent: "Mozilla/5.0 (iPod; U; CPU like Mac OS X; en) AppleWebKit/420.1 (KHTML, like Gecko) Version/3.0 Mobile/4A93 Safari/419.3"
                          , expectedResult: UserAgent { name: "Mobile Safari"
                                                      , majorVersion: 3
                                                      , version: 3
                                                      , platform: "iOS"
                                                      , vendor: "Apple"
                                                      }
                          }
               ]

msie = [ TestCase { userAgent: "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)"
                  , expectedResult: UserAgent { name: "Internet Explorer"
                                              , majorVersion: 10
                                              , version: 10
                                              , platform: "Windows"
                                              , vendor: "Microsoft"
                                              }
                  }
       , TestCase { userAgent: "Mozilla/4.0 (Windows; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 2.0.50727)"
                  , expectedResult: UserAgent { name: "Internet Explorer"
                                              , majorVersion: 6
                                              , version: 6
                                              , platform: "Windows"
                                              , vendor: "Microsoft"
                                              }
                  }
       ]

opera = [ TestCase { userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 1094) AppleWebKit/537.36 (KHTML like Gecko) Chrome/35.0.1916.153 Safari/537.36 OPR/22.0.1471.70"
                   , expectedResult: UserAgent { name: "Opera"
                                               , majorVersion: 22
                                               , version: 22
                                               , platform: "Mac OS X"
                                               , vendor: "Opera"
                                               }
                   }
        , TestCase { userAgent: "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.52 Safari/537.36 OPR/15.0.1147.100"
                   , expectedResult: UserAgent { name: "Opera"
                                               , majorVersion: 15
                                               , version: 15
                                               , platform: "Windows"
                                               , vendor: "Opera"
                                               }
                   }
        , TestCase { userAgent: "Opera/9.80 (Windows NT 6.0) Presto/2.12.388 Version/12.14"
                   , expectedResult: UserAgent { name: "Opera"
                                               , majorVersion: 12
                                               , version: 12.14
                                               , platform: "Windows"
                                               , vendor: "Opera"
                                               }
                   }
        , TestCase { userAgent: "Opera/9.62 (X11; Linux x86_64; U; ru) Presto/2.1.1"
                   , expectedResult: UserAgent { name: "Opera"
                                               , majorVersion: 9
                                               , version: 9.62
                                               , platform: "Linux"
                                               , vendor: "Opera"
                                               }
                   }
        , TestCase { userAgent: "Mozilla/4.0 (compatible; MSIE 6.0; Windows 98; en) Opera 8.52"
                   , expectedResult: UserAgent { name: "Opera"
                                               , majorVersion: 8
                                               , version: 8.52
                                               , platform: "Windows"
                                               , vendor: "Opera"
                                               }
                   }
        ]

operaMini = [ TestCase { userAgent: "Opera/9.80 (iPhone; Opera Mini/8.0.0/34.2336; U; en) Presto/2.8.119 Version/11.104"
                       , expectedResult: UserAgent { name: "Opera Mini"
                                                   , majorVersion: 8
                                                   , version: 8
                                                   , platform: "iOS"
                                                   , vendor: "Opera"
                                                   }
                       }
            , TestCase { userAgent: "Opera/9.80 (Android; Opera Mini/7.5.33361/31.1350; U; en) Presto/2.8.119 Version/11.10"
                       , expectedResult: UserAgent { name: "Opera Mini"
                                                   , majorVersion: 7
                                                   , version: 7.5
                                                   , platform: "Android"
                                                   , vendor: "Opera"
                                                   }
                       }
            ]

safari = [ TestCase { userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 5
                                                , version: 5.1
                                                , platform: "Mac OS X"
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Windows; U; Windows NT 6.1; sv-SE) AppleWebKit/533.19.4 (KHTML, like Gecko) Version/5.0.3 Safari/533.19.4"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 5
                                                , version: 5
                                                , platform: "Windows"
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Windows; U; Windows NT 6.0; fr-ch) AppleWebKit/531.9 (KHTML, like Gecko) Version/4.0.3 Safari/531.9"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 4
                                                , version: 4
                                                , platform: "Windows"
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Macintosh; U; PPC Mac OS X; sv-se) AppleWebKit/419 (KHTML, like Gecko) Safari/419.3"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 2
                                                , version: 2
                                                , platform: "Mac OS X"
                                                , vendor: "Apple"
                                                }
                    }
         , TestCase { userAgent: "Mozilla/5.0 (Macintosh; U; PPC Mac OS X; it-it) AppleWebKit/124 (KHTML, like Gecko) Safari/125.1"
                    , expectedResult: UserAgent { name: "Safari"
                                                , majorVersion: 1
                                                , version: 1.2
                                                , platform: "Mac OS X"
                                                , vendor: "Apple"
                                                }
                    }
          ]

seaMonkey = [ TestCase { userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:19.0) Gecko/20100101 Firefox/19.0 SeaMonkey/2.16.2"
                       , expectedResult: UserAgent { name: "SeaMonkey"
                                                   , majorVersion: 2
                                                   , version: 2.16
                                                   , platform: "Mac OS X"
                                                   , vendor: "SeaMonkey Council"
                                                   }
                       }
            , TestCase { userAgent: "Mozilla/5.0 (X11; Linux i686; rv:12.0) Gecko/20120502 SeaMonkey/2.9.1"
                       , expectedResult: UserAgent { name: "SeaMonkey"
                                                   , majorVersion: 2
                                                   , version: 2.9
                                                   , platform: "Linux"
                                                   , vendor: "SeaMonkey Council"
                                                   }
                       }
            , TestCase { userAgent: "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.14) Gecko/20100930 SeaMonkey/2.0.9"
                       , expectedResult: UserAgent { name: "SeaMonkey"
                                                   , majorVersion: 2
                                                   , version: 2
                                                   , platform: "Windows"
                                                   , vendor: "SeaMonkey Council"
                                                   }
                       }
            , TestCase { userAgent: "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv:1.8.1.13) Gecko/20080313 SeaMonkey/1.1.9"
                       , expectedResult: UserAgent { name: "SeaMonkey"
                                                   , majorVersion: 1
                                                   , version: 1.1
                                                   , platform: "Mac OS X"
                                                   , vendor: "Mozilla"
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
                      , platform: "Unknown"
                      , vendor: "None"
                      }

eq :: forall a. (Eq a, Show a) => a -> a -> String
eq a b = if a == b then "OK" else "ERROR " ++ show a ++ " does not equal " ++ show b

runTestCases :: [TestCase] -> [String]
runTestCases = map (\(TestCase t) -> eq t.expectedResult <<< fromMaybe failingUA <<< parse $ t.userAgent)

{- Runner -}

main = do
  trace "Detects Android Browser"
  print $ runTestCases androidBrowser

  trace "Detects Chrome"
  print $ runTestCases chrome

  trace "Detects Ekioh"
  print $ runTestCases ekioh

  trace "Detects Firefox"
  print $ runTestCases firefox

  trace "Detects Mobile Safari"
  print $ runTestCases mobileSafari

  trace "Detects Internet Explorer"
  print $ runTestCases msie

  trace "Detects Opera"
  print $ runTestCases opera

  trace "Detects Opera Mini"
  print $ runTestCases operaMini

  trace "Detects Safari"
  print $ runTestCases safari

  trace "Detects SeaMonkey"
  print $ runTestCases seaMonkey

  trace "Ignores unknown browsers"
  print $ runTestCases none
