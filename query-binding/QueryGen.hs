import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface


cabal = Cabal { cabal_pkgname = "query-binding" 
              , cabal_cheaderprefix = "QB"
              , cabal_moduleprefix = "Query.Binding" }

extraDep = []
-- [ ("Engine", ["Query.Binding.Vector.Template"]) ]


cabalattr = 
    CabalAttr 
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = [ ]
    , cabalattr_extralibdirs = []
    }


vectorint_ = TemplateApp t_vector "CInt" "std::vector<int>"


engineWrapper :: Class
engineWrapper =
  Class cabal "EngineWrapper" [] mempty Nothing
  [ Constructor [ cstring "configfile" ] Nothing
  , Virtual (cppclass_ jsonWrapper) "query" [ cppclass jsonWrapper "input" ] Nothing
  , Destructor Nothing
  ]

jsonWrapper :: Class
jsonWrapper =
  Class cabal "JsonWrapper" []  mempty Nothing
  [ Constructor [ cstring "str" ] Nothing
  ]


classes = [ engineWrapper, jsonWrapper ]

toplevelfunctions = [] 
  -- [ TopLevelFunction (cppclassref_ json) "json::parse" [cstring "txt"] (Just "parse") ]  

t_vector = TmplCls cabal "Vector" "std::vector" "t"
             [ TFunNew [ ]
             , TFun void_ "push_back" "push_back" [(TemplateParam "t","x")] Nothing
             , TFun void_ "pop_back"  "pop_back"  []                        Nothing
             , TFun (TemplateParam "t") "at" "at" [int "n"]                 Nothing
             , TFun int_  "size"      "size"      []                        Nothing
             , TFunDelete
             ]


templates = [ ( t_vector, HdrName "Vector.h" ) 
            ] 


headerMap = [ ( "EngineWrapper", ([], [HdrName "/home/wavewave/repo/srcp/nlp-prototype/rnn++/src/similarity/similarity.h"]))
            , ( "JsonWrapper"  , ([], [HdrName "/home/wavewave/repo/srcp/nlp-prototype/rnn++/src/similarity/similarity.h"]))
            -- , ( "json"         , ([NS "nlohmann"], [HdrName "utils/json.h"]))
            ]

main :: IO ()
main = do 
  simpleBuilder "Query.Binding" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates)
    [ ] extraDep


