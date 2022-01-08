import qualified Control.Monad as Monad
import qualified Derulo
import qualified System.Exit as Exit
import qualified Test.HUnit as Test

main :: IO ()
main = do
  counts <- Test.runTestTT $ Test.TestList
    [ Derulo.readJSON " null " Test.~?= Just Derulo.Null
    , Derulo.showJSON Derulo.Null Test.~?= "null"
    , Derulo.readJSON "null" Test.~?= Just Derulo.Null
    , Derulo.readJSON "true" Test.~?= Just (Derulo.Boolean True)
    , Derulo.readJSON "false" Test.~?= Just (Derulo.Boolean False)
    , Derulo.readJSON "0e0" Test.~?= Just (Derulo.Number 0 0)
    , Derulo.readJSON "12e34" Test.~?= Just (Derulo.Number 12 34)
    , Derulo.readJSON "-12e-34" Test.~?= Just (Derulo.Number (-12) (-34))
    , Derulo.readJSON "\"\"" Test.~?= Just (Derulo.String "")
    , Derulo.readJSON "\"js\"" Test.~?= Just (Derulo.String "js")
    , Derulo.readJSON "\"\\\"\\\\\\b\\f\\n\\r\\t\""
      Test.~?= Just (Derulo.String "\"\\\b\f\n\r\t")
    , Derulo.readJSON "\"\\u001f\"" Test.~?= Just (Derulo.String "\x1f")
    , Derulo.readJSON "[]" Test.~?= Just (Derulo.Array [])
    , Derulo.readJSON "[null]" Test.~?= Just (Derulo.Array [Derulo.Null])
    , Derulo.readJSON "[true,false]"
      Test.~?= Just (Derulo.Array [Derulo.Boolean True, Derulo.Boolean False])
    , Derulo.readJSON "{}" Test.~?= Just (Derulo.Object [])
    , Derulo.readJSON "{\"\":null}"
      Test.~?= Just (Derulo.Object [("", Derulo.Null)])
    , Derulo.readJSON "{\"t\":true,\"f\":false}" Test.~?= Just
      (Derulo.Object [("t", Derulo.Boolean True), ("f", Derulo.Boolean False)])
    , Derulo.showJSON Derulo.Null Test.~?= "null"
    , Derulo.showJSON (Derulo.Boolean True) Test.~?= "true"
    , Derulo.showJSON (Derulo.Boolean False) Test.~?= "false"
    , Derulo.showJSON (Derulo.Number 0 0) Test.~?= "0e0"
    , Derulo.showJSON (Derulo.Number 12 34) Test.~?= "12e34"
    , Derulo.showJSON (Derulo.Number (-12) (-34)) Test.~?= "-12e-34"
    , Derulo.showJSON (Derulo.String "") Test.~?= "\"\""
    , Derulo.showJSON (Derulo.String "js") Test.~?= "\"js\""
    , Derulo.showJSON (Derulo.String "\"\\\b\f\n\r\t")
      Test.~?= "\"\\\"\\\\\\b\\f\\n\\r\\t\""
    , Derulo.showJSON (Derulo.String "\x1f") Test.~?= "\"\\u001f\""
    , Derulo.showJSON (Derulo.Array []) Test.~?= "[]"
    , Derulo.showJSON (Derulo.Array [Derulo.Null]) Test.~?= "[null]"
    , Derulo.showJSON
        (Derulo.Array [Derulo.Boolean True, Derulo.Boolean False])
      Test.~?= "[true,false]"
    , Derulo.showJSON (Derulo.Object []) Test.~?= "{}"
    , Derulo.showJSON (Derulo.Object [("", Derulo.Null)])
      Test.~?= "{\"\":null}"
    , Derulo.showJSON
        (Derulo.Object
          [("t", Derulo.Boolean True), ("f", Derulo.Boolean False)]
        )
      Test.~?= "{\"t\":true,\"f\":false}"
    , Derulo.readJSON "[123.456e-789]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123456 (-792)])
    , Derulo.readJSON
        "[0.4e00669999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999969999999006]"
      Test.~?= Just
                 (Derulo.Array
                   [ Derulo.Number
                       (-4)
                       669999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999969999999005
                   ]
                 )
    , Derulo.readJSON "[-1e+9999]"
      Test.~?= Just (Derulo.Array [Derulo.Number (-1) 9999])
    , Derulo.readJSON "[1.5e+9999]"
      Test.~?= Just (Derulo.Array [Derulo.Number 15 9998])
    , Derulo.readJSON "[-123123e100000]"
      Test.~?= Just (Derulo.Array [Derulo.Number (-123123) 100000])
    , Derulo.readJSON "[123123e100000]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123123 100000])
    , Derulo.readJSON "[123e-10000000]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123 (-10000000)])
    , Derulo.readJSON "[-123123123123123123123123123123]" Test.~?= Just
      (Derulo.Array [Derulo.Number (-123123123123123123123123123123) 0])
    , Derulo.readJSON "[100000000000000000000]"
      Test.~?= Just (Derulo.Array [Derulo.Number 100000000000000000000 0])
    , Derulo.readJSON "[-237462374673276894279832749832423479823246327846]"
      Test.~?= Just
                 (Derulo.Array
                   [ Derulo.Number
                       (-237462374673276894279832749832423479823246327846)
                       0
                   ]
                 )
    , Derulo.readJSON "{\"\\uDFAA\":0}"
      Test.~?= Just (Derulo.Object [("\57258", Derulo.Number 0 0)])
    , Derulo.readJSON "[\"\\uDADA\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\56026"])
    , Derulo.readJSON "[\"\\uD888\\u1234\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55432\4660"])
    , Derulo.readJSON "[\"\\uD800\\n\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55296\n"])
    , Derulo.readJSON "[\"\\uDd1ea\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\56606a"])
    , Derulo.readJSON "[\"\\uD800\\uD800\\n\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55296\55296\n"])
    , Derulo.readJSON "[\"\\ud800\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55296"])
    , Derulo.readJSON "[\"\\ud800abc\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55296abc"])
    , Derulo.readJSON "[\"\\uDd1e\\uD834\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\56606\55348"])
    , Derulo.readJSON "[\"\\uDFAA\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\57258"])
    , Derulo.readJSON "\65279{}" Test.~?= Nothing
    , Derulo.readJSON "[1 true]" Test.~?= Nothing
    , Derulo.readJSON "[\"\": 1]" Test.~?= Nothing
    , Derulo.readJSON "[\"\"]," Test.~?= Nothing
    , Derulo.readJSON "[,1]" Test.~?= Nothing
    , Derulo.readJSON "[1,,2]" Test.~?= Nothing
    , Derulo.readJSON "[\"x\",,]" Test.~?= Nothing
    , Derulo.readJSON "[\"x\"]]" Test.~?= Nothing
    , Derulo.readJSON "[\"\",]" Test.~?= Nothing
    , Derulo.readJSON "[\"x\"" Test.~?= Nothing
    , Derulo.readJSON "[x" Test.~?= Nothing
    , Derulo.readJSON "[3[4]]" Test.~?= Nothing
    , Derulo.readJSON "[1:2]" Test.~?= Nothing
    , Derulo.readJSON "[,]" Test.~?= Nothing
    , Derulo.readJSON "[-]" Test.~?= Nothing
    , Derulo.readJSON "[   , \"\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"a\",\n4\n,1," Test.~?= Nothing
    , Derulo.readJSON "[1,]" Test.~?= Nothing
    , Derulo.readJSON "[1,,]" Test.~?= Nothing
    , Derulo.readJSON "[\"\va\"\\f]" Test.~?= Nothing
    , Derulo.readJSON "[*]" Test.~?= Nothing
    , Derulo.readJSON "[\"\"" Test.~?= Nothing
    , Derulo.readJSON "[1," Test.~?= Nothing
    , Derulo.readJSON "[1,\n1\n,1" Test.~?= Nothing
    , Derulo.readJSON "[{}" Test.~?= Nothing
    , Derulo.readJSON "[fals]" Test.~?= Nothing
    , Derulo.readJSON "[nul]" Test.~?= Nothing
    , Derulo.readJSON "[tru]" Test.~?= Nothing
    , Derulo.readJSON "123\NUL" Test.~?= Nothing
    , Derulo.readJSON "[++1234]" Test.~?= Nothing
    , Derulo.readJSON "[+1]" Test.~?= Nothing
    , Derulo.readJSON "[+Inf]" Test.~?= Nothing
    , Derulo.readJSON "[-01]" Test.~?= Nothing
    , Derulo.readJSON "[-1.0.]" Test.~?= Nothing
    , Derulo.readJSON "[-2.]" Test.~?= Nothing
    , Derulo.readJSON "[-NaN]" Test.~?= Nothing
    , Derulo.readJSON "[.-1]" Test.~?= Nothing
    , Derulo.readJSON "[.2e-3]" Test.~?= Nothing
    , Derulo.readJSON "[0.1.2]" Test.~?= Nothing
    , Derulo.readJSON "[0.3e+]" Test.~?= Nothing
    , Derulo.readJSON "[0.3e]" Test.~?= Nothing
    , Derulo.readJSON "[0.e1]" Test.~?= Nothing
    , Derulo.readJSON "[0E+]" Test.~?= Nothing
    , Derulo.readJSON "[0E]" Test.~?= Nothing
    , Derulo.readJSON "[0e+]" Test.~?= Nothing
    , Derulo.readJSON "[0e]" Test.~?= Nothing
    , Derulo.readJSON "[1.0e+]" Test.~?= Nothing
    , Derulo.readJSON "[1.0e-]" Test.~?= Nothing
    , Derulo.readJSON "[1.0e]" Test.~?= Nothing
    , Derulo.readJSON "[1 000.0]" Test.~?= Nothing
    , Derulo.readJSON "[1eE2]" Test.~?= Nothing
    , Derulo.readJSON "[2.e+3]" Test.~?= Nothing
    , Derulo.readJSON "[2.e-3]" Test.~?= Nothing
    , Derulo.readJSON "[2.e3]" Test.~?= Nothing
    , Derulo.readJSON "[9.e+]" Test.~?= Nothing
    , Derulo.readJSON "[Inf]" Test.~?= Nothing
    , Derulo.readJSON "[NaN]" Test.~?= Nothing
    , Derulo.readJSON "[\65297]" Test.~?= Nothing
    , Derulo.readJSON "[1+2]" Test.~?= Nothing
    , Derulo.readJSON "[0x1]" Test.~?= Nothing
    , Derulo.readJSON "[0x42]" Test.~?= Nothing
    , Derulo.readJSON "[Infinity]" Test.~?= Nothing
    , Derulo.readJSON "[0e+-1]" Test.~?= Nothing
    , Derulo.readJSON "[-123.123foo]" Test.~?= Nothing
    , Derulo.readJSON "[-Infinity]" Test.~?= Nothing
    , Derulo.readJSON "[-foo]" Test.~?= Nothing
    , Derulo.readJSON "[- 1]" Test.~?= Nothing
    , Derulo.readJSON "[-012]" Test.~?= Nothing
    , Derulo.readJSON "[-.123]" Test.~?= Nothing
    , Derulo.readJSON "[-1x]" Test.~?= Nothing
    , Derulo.readJSON "[1ea]" Test.~?= Nothing
    , Derulo.readJSON "[1.]" Test.~?= Nothing
    , Derulo.readJSON "[.123]" Test.~?= Nothing
    , Derulo.readJSON "[1.2a-3]" Test.~?= Nothing
    , Derulo.readJSON "[1.8011670033376514H-308]" Test.~?= Nothing
    , Derulo.readJSON "[012]" Test.~?= Nothing
    , Derulo.readJSON "[\"x\", truth]" Test.~?= Nothing
    , Derulo.readJSON "{[: \"x\"}\n" Test.~?= Nothing
    , Derulo.readJSON "{\"x\", null}" Test.~?= Nothing
    , Derulo.readJSON "{\"x\"::\"b\"}" Test.~?= Nothing
    , Derulo.readJSON "{\127464\127469}" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"a\" 123}" Test.~?= Nothing
    , Derulo.readJSON "{key: 'value'}" Test.~?= Nothing
    , Derulo.readJSON "{\"a\" b}" Test.~?= Nothing
    , Derulo.readJSON "{:\"b\"}" Test.~?= Nothing
    , Derulo.readJSON "{\"a\" \"b\"}" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":" Test.~?= Nothing
    , Derulo.readJSON "{\"a\"" Test.~?= Nothing
    , Derulo.readJSON "{1:1}" Test.~?= Nothing
    , Derulo.readJSON "{9999E9999:1}" Test.~?= Nothing
    , Derulo.readJSON "{null:null,null:null}" Test.~?= Nothing
    , Derulo.readJSON "{\"id\":0,,,,,}" Test.~?= Nothing
    , Derulo.readJSON "{'a':0}" Test.~?= Nothing
    , Derulo.readJSON "{\"id\":0,}" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"b\"}/**/" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"b\"}/**//" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"b\"}//" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"b\"}/" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"b\",,\"c\":\"d\"}" Test.~?= Nothing
    , Derulo.readJSON "{a: \"b\"}" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"a" Test.~?= Nothing
    , Derulo.readJSON "{ \"foo\" : \"bar\", \"a\" }" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"b\"}#" Test.~?= Nothing
    , Derulo.readJSON " " Test.~?= Nothing
    , Derulo.readJSON "[\"\\uD800\\\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\uD800\\u\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\uD800\\u1\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\uD800\\u1x\"]" Test.~?= Nothing
    , Derulo.readJSON "[\233]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\\NUL\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\x00\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\\\\\\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\\t\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\\127744\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\u00A\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\uD834\\uDd\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\uD800\\uD800\\x\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\a\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\\uqqqq\"]" Test.~?= Nothing
    , Derulo.readJSON "[\\u0020\"asd\"]" Test.~?= Nothing
    , Derulo.readJSON "[\\n]" Test.~?= Nothing
    , Derulo.readJSON "\"" Test.~?= Nothing
    , Derulo.readJSON "['single quote']" Test.~?= Nothing
    , Derulo.readJSON "abc" Test.~?= Nothing
    , Derulo.readJSON "[\"\\" Test.~?= Nothing
    , Derulo.readJSON "[\"a\NULa\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"new\nline\"]" Test.~?= Nothing
    , Derulo.readJSON "[\"\t\"]" Test.~?= Nothing
    , Derulo.readJSON "\"\\UA66D\"" Test.~?= Nothing
    , Derulo.readJSON "\"\"x" Test.~?= Nothing
    , Derulo.readJSON "[\8288]" Test.~?= Nothing
    , Derulo.readJSON "\65279" Test.~?= Nothing
    , Derulo.readJSON "<.>" Test.~?= Nothing
    , Derulo.readJSON "[<null>]" Test.~?= Nothing
    , Derulo.readJSON "[1]x" Test.~?= Nothing
    , Derulo.readJSON "[1]]" Test.~?= Nothing
    , Derulo.readJSON "[\"asd]" Test.~?= Nothing
    , Derulo.readJSON "a\229" Test.~?= Nothing
    , Derulo.readJSON "[True]" Test.~?= Nothing
    , Derulo.readJSON "1]" Test.~?= Nothing
    , Derulo.readJSON "{\"x\": true," Test.~?= Nothing
    , Derulo.readJSON "[][]" Test.~?= Nothing
    , Derulo.readJSON "]" Test.~?= Nothing
    , Derulo.readJSON "[" Test.~?= Nothing
    , Derulo.readJSON "" Test.~?= Nothing
    , Derulo.readJSON "[\NUL]" Test.~?= Nothing
    , Derulo.readJSON "2@" Test.~?= Nothing
    , Derulo.readJSON "{}}" Test.~?= Nothing
    , Derulo.readJSON "{\"\":" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":/*comment*/\"b\"}" Test.~?= Nothing
    , Derulo.readJSON "{\"a\": true} \"x\"" Test.~?= Nothing
    , Derulo.readJSON "['" Test.~?= Nothing
    , Derulo.readJSON "[," Test.~?= Nothing
    , Derulo.readJSON "[{" Test.~?= Nothing
    , Derulo.readJSON "[\"a" Test.~?= Nothing
    , Derulo.readJSON "[\"a\"" Test.~?= Nothing
    , Derulo.readJSON "{" Test.~?= Nothing
    , Derulo.readJSON "{]" Test.~?= Nothing
    , Derulo.readJSON "{," Test.~?= Nothing
    , Derulo.readJSON "{[" Test.~?= Nothing
    , Derulo.readJSON "{\"a" Test.~?= Nothing
    , Derulo.readJSON "{'a'" Test.~?= Nothing
    , Derulo.readJSON "[\"\\{[\"\\{[\"\\{[\"\\{" Test.~?= Nothing
    , Derulo.readJSON "*" Test.~?= Nothing
    , Derulo.readJSON "{\"a\":\"b\"}#{}" Test.~?= Nothing
    , Derulo.readJSON "[\\u000A\"\"]" Test.~?= Nothing
    , Derulo.readJSON "[1" Test.~?= Nothing
    , Derulo.readJSON "[ false, nul" Test.~?= Nothing
    , Derulo.readJSON "[ true, fals" Test.~?= Nothing
    , Derulo.readJSON "[ false, tru" Test.~?= Nothing
    , Derulo.readJSON "{\"asd\":\"asd\"" Test.~?= Nothing
    , Derulo.readJSON "\229" Test.~?= Nothing
    , Derulo.readJSON "[\8288]" Test.~?= Nothing
    , Derulo.readJSON "[\f]" Test.~?= Nothing
    , Derulo.readJSON "[[]   ]" Test.~?= Just (Derulo.Array [Derulo.Array []])
    , Derulo.readJSON "[\"\"]" Test.~?= Just (Derulo.Array [Derulo.String ""])
    , Derulo.readJSON "[]" Test.~?= Just (Derulo.Array [])
    , Derulo.readJSON "[\"a\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "a"])
    , Derulo.readJSON "[false]"
      Test.~?= Just (Derulo.Array [Derulo.Boolean False])
    , Derulo.readJSON "[null, 1, \"1\", {}]" Test.~?= Just
      (Derulo.Array
        [Derulo.Null, Derulo.Number 1 0, Derulo.String "1", Derulo.Object []]
      )
    , Derulo.readJSON "[null]" Test.~?= Just (Derulo.Array [Derulo.Null])
    , Derulo.readJSON "[1\n]" Test.~?= Just (Derulo.Array [Derulo.Number 1 0])
    , Derulo.readJSON " [1]" Test.~?= Just (Derulo.Array [Derulo.Number 1 0])
    , Derulo.readJSON "[1,null,null,null,2]" Test.~?= Just
      (Derulo.Array
        [ Derulo.Number 1 0
        , Derulo.Null
        , Derulo.Null
        , Derulo.Null
        , Derulo.Number 2 0
        ]
      )
    , Derulo.readJSON "[2] " Test.~?= Just (Derulo.Array [Derulo.Number 2 0])
    , Derulo.readJSON "[123e65]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123 65])
    , Derulo.readJSON "[0e+1]" Test.~?= Just (Derulo.Array [Derulo.Number 0 1])
    , Derulo.readJSON "[0e1]" Test.~?= Just (Derulo.Array [Derulo.Number 0 1])
    , Derulo.readJSON "[ 4]" Test.~?= Just (Derulo.Array [Derulo.Number 4 0])
    , Derulo.readJSON
        "[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]\n"
      Test.~?= Just (Derulo.Array [Derulo.Number (-1) (-78)])
    , Derulo.readJSON "[20e1]"
      Test.~?= Just (Derulo.Array [Derulo.Number 20 1])
    , Derulo.readJSON "[-0]" Test.~?= Just (Derulo.Array [Derulo.Number 0 0])
    , Derulo.readJSON "[-123]"
      Test.~?= Just (Derulo.Array [Derulo.Number (-123) 0])
    , Derulo.readJSON "[-1]"
      Test.~?= Just (Derulo.Array [Derulo.Number (-1) 0])
    , Derulo.readJSON "[-0]" Test.~?= Just (Derulo.Array [Derulo.Number 0 0])
    , Derulo.readJSON "[1E22]"
      Test.~?= Just (Derulo.Array [Derulo.Number 1 22])
    , Derulo.readJSON "[1E-2]"
      Test.~?= Just (Derulo.Array [Derulo.Number 1 (-2)])
    , Derulo.readJSON "[1E+2]" Test.~?= Just (Derulo.Array [Derulo.Number 1 2])
    , Derulo.readJSON "[123e45]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123 45])
    , Derulo.readJSON "[123.456e78]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123456 75])
    , Derulo.readJSON "[1e-2]"
      Test.~?= Just (Derulo.Array [Derulo.Number 1 (-2)])
    , Derulo.readJSON "[1e+2]" Test.~?= Just (Derulo.Array [Derulo.Number 1 2])
    , Derulo.readJSON "[123]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123 0])
    , Derulo.readJSON "[123.456789]"
      Test.~?= Just (Derulo.Array [Derulo.Number 123456789 (-6)])
    , Derulo.readJSON "{\"asd\":\"sdf\", \"dfg\":\"fgh\"}" Test.~?= Just
      (Derulo.Object
        [("asd", Derulo.String "sdf"), ("dfg", Derulo.String "fgh")]
      )
    , Derulo.readJSON "{\"asd\":\"sdf\"}"
      Test.~?= Just (Derulo.Object [("asd", Derulo.String "sdf")])
    , Derulo.readJSON "{\"a\":\"b\",\"a\":\"c\"}" Test.~?= Just
      (Derulo.Object [("a", Derulo.String "b"), ("a", Derulo.String "c")])
    , Derulo.readJSON "{\"a\":\"b\",\"a\":\"b\"}" Test.~?= Just
      (Derulo.Object [("a", Derulo.String "b"), ("a", Derulo.String "b")])
    , Derulo.readJSON "{}" Test.~?= Just (Derulo.Object [])
    , Derulo.readJSON "{\"\":0}"
      Test.~?= Just (Derulo.Object [("", Derulo.Number 0 0)])
    , Derulo.readJSON "{\"foo\\u0000bar\": 42}"
      Test.~?= Just (Derulo.Object [("foo\NULbar", Derulo.Number 42 0)])
    , Derulo.readJSON "{ \"min\": -1.0e+28, \"max\": 1.0e+28 }" Test.~?= Just
      (Derulo.Object
        [("min", Derulo.Number (-10) 27), ("max", Derulo.Number 10 27)]
      )
    , Derulo.readJSON
        "{\"x\":[{\"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], \"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}"
      Test.~?= Just
                 (Derulo.Object
                   [ ( "x"
                     , Derulo.Array
                       [ Derulo.Object
                           [ ( "id"
                             , Derulo.String
                               "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                             )
                           ]
                       ]
                     )
                   , ( "id"
                     , Derulo.String "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                     )
                   ]
                 )
    , Derulo.readJSON "{\"a\":[]}"
      Test.~?= Just (Derulo.Object [("a", Derulo.Array [])])
    , Derulo.readJSON
        "{\"title\":\"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430\" }"
      Test.~?= Just
                 (Derulo.Object
                   [ ( "title"
                     , Derulo.String
                       "\1055\1086\1083\1090\1086\1088\1072 \1047\1077\1084\1083\1077\1082\1086\1087\1072"
                     )
                   ]
                 )
    , Derulo.readJSON "{\n\"a\": \"b\"\n}"
      Test.~?= Just (Derulo.Object [("a", Derulo.String "b")])
    , Derulo.readJSON "[\"\\u0060\\u012a\\u12AB\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "`\298\4779"])
    , Derulo.readJSON "[\"\\uD801\\udc37\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55297\56375"])
    , Derulo.readJSON "[\"\\ud83d\\ude39\\ud83d\\udc8d\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55357\56889\55357\56461"])
    , Derulo.readJSON "[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\"\\/\b\f\n\r\t"])
    , Derulo.readJSON "[\"\\\\u0000\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\\u0000"])
    , Derulo.readJSON "[\"\\\"\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\""])
    , Derulo.readJSON "[\"a/*b*/c/*d//e\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "a/*b*/c/*d//e"])
    , Derulo.readJSON "[\"\\\\a\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\\a"])
    , Derulo.readJSON "[\"\\\\n\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\\n"])
    , Derulo.readJSON "[\"\\u0012\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\DC2"])
    , Derulo.readJSON "[\"\\uFFFF\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\65535"])
    , Derulo.readJSON "[\"asd\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "asd"])
    , Derulo.readJSON "[ \"asd\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "asd"])
    , Derulo.readJSON "[\"\\uDBFF\\uDFFF\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\56319\57343"])
    , Derulo.readJSON "[\"new\\u00A0line\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "new\160line"])
    , Derulo.readJSON "[\"\1114111\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\1114111"])
    , Derulo.readJSON "[\"\114687\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\114687"])
    , Derulo.readJSON "[\"\65535\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\65535"])
    , Derulo.readJSON "[\"\\u0000\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\NUL"])
    , Derulo.readJSON "[\"\\u002c\"]"
      Test.~?= Just (Derulo.Array [Derulo.String ","])
    , Derulo.readJSON "[\"\960\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\960"])
    , Derulo.readJSON "[\"asd \"]"
      Test.~?= Just (Derulo.Array [Derulo.String "asd "])
    , Derulo.readJSON "\" \"" Test.~?= Just (Derulo.String " ")
    , Derulo.readJSON "[\"\\uD834\\uDd1e\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55348\56606"])
    , Derulo.readJSON "[\"\\u0821\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\2081"])
    , Derulo.readJSON "[\"\\u0123\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\291"])
    , Derulo.readJSON "[\"\8232\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\8232"])
    , Derulo.readJSON "[\"\8233\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\8233"])
    , Derulo.readJSON "[\"\\u0061\\u30af\\u30EA\\u30b9\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "a\12463\12522\12473"])
    , Derulo.readJSON "[\"new\\u000Aline\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "new\nline"])
    , Derulo.readJSON "[\"\DEL\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\DEL"])
    , Derulo.readJSON "[\"\\uA66D\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\42605"])
    , Derulo.readJSON "[\"\\u005C\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\\"])
    , Derulo.readJSON "[\"\9026\12852\9026\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\9026\12852\9026"])
    , Derulo.readJSON "[\"\\uDBFF\\uDFFE\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\56319\57342"])
    , Derulo.readJSON "[\"\\uD83F\\uDFFE\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55359\57342"])
    , Derulo.readJSON "[\"\\u200B\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\8203"])
    , Derulo.readJSON "[\"\\u2064\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\8292"])
    , Derulo.readJSON "[\"\\uFDD0\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\64976"])
    , Derulo.readJSON "[\"\\uFFFE\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\65534"])
    , Derulo.readJSON "[\"\\u0022\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\""])
    , Derulo.readJSON "[\"\8364\119070\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\8364\119070"])
    , Derulo.readJSON "[\"a\DELa\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "a\DELa"])
    , Derulo.readJSON "false" Test.~?= Just (Derulo.Boolean False)
    , Derulo.readJSON "42" Test.~?= Just (Derulo.Number 42 0)
    , Derulo.readJSON "-0.1" Test.~?= Just (Derulo.Number (-1) (-1))
    , Derulo.readJSON "null" Test.~?= Just Derulo.Null
    , Derulo.readJSON "\"asd\"" Test.~?= Just (Derulo.String "asd")
    , Derulo.readJSON "true" Test.~?= Just (Derulo.Boolean True)
    , Derulo.readJSON "\"\"" Test.~?= Just (Derulo.String "")
    , Derulo.readJSON "[\"a\"]\n"
      Test.~?= Just (Derulo.Array [Derulo.String "a"])
    , Derulo.readJSON "[true]"
      Test.~?= Just (Derulo.Array [Derulo.Boolean True])
    , Derulo.readJSON " [] " Test.~?= Just (Derulo.Array [])
    , Derulo.readJSON "[1.0]"
      Test.~?= Just (Derulo.Array [Derulo.Number 10 (-1)])
    , Derulo.readJSON "[1.000000000000000005]"
      Test.~?= Just (Derulo.Array [Derulo.Number 1000000000000000005 (-18)])
    , Derulo.readJSON "[1000000000000000]\n"
      Test.~?= Just (Derulo.Array [Derulo.Number 1000000000000000 0])
    , Derulo.readJSON "[10000000000000000999]"
      Test.~?= Just (Derulo.Array [Derulo.Number 10000000000000000999 0])
    , Derulo.readJSON "[1E-999]"
      Test.~?= Just (Derulo.Array [Derulo.Number 1 (-999)])
    , Derulo.readJSON "[1E6]" Test.~?= Just (Derulo.Array [Derulo.Number 1 6])
    , Derulo.readJSON "{\"\233\":\"NFC\",\"e\769\":\"NFD\"}" Test.~?= Just
      (Derulo.Object
        [("\233", Derulo.String "NFC"), ("e\769", Derulo.String "NFD")]
      )
    , Derulo.readJSON "{\"e\769\":\"NFD\",\"\233\":\"NFC\"}" Test.~?= Just
      (Derulo.Object
        [("e\769", Derulo.String "NFD"), ("\233", Derulo.String "NFC")]
      )
    , Derulo.readJSON "{\"a\":1,\"a\":2}" Test.~?= Just
      (Derulo.Object [("a", Derulo.Number 1 0), ("a", Derulo.Number 2 0)])
    , Derulo.readJSON "{\"a\":1,\"a\":1}" Test.~?= Just
      (Derulo.Object [("a", Derulo.Number 1 0), ("a", Derulo.Number 1 0)])
    , Derulo.readJSON "{\"a\":0, \"a\":-0}\n" Test.~?= Just
      (Derulo.Object [("a", Derulo.Number 0 0), ("a", Derulo.Number 0 0)])
    , Derulo.readJSON "[\"\\uD800\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55296"])
    , Derulo.readJSON "[\"\\uD800\\uD800\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55296\55296"])
    , Derulo.readJSON "[\"\\uD800\\uD800\\uD800\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "\55296\55296\55296"])
    , Derulo.readJSON "[\"A\\u0000B\"]"
      Test.~?= Just (Derulo.Array [Derulo.String "A\NULB"])
    ]

  let
    hasErrors = Test.errors counts /= 0
    hasFailures = Test.failures counts /= 0
  Monad.when (hasErrors || hasFailures) Exit.exitFailure
