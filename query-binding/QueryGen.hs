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


-- vectorint_ = TemplateApp t_vector "CInt" "std::vector<int>"


engineWrapper :: Class
engineWrapper =
  Class cabal "EngineWrapper" [] mempty Nothing
  [ Constructor [ cstring "configfile" ] Nothing
  , Virtual (cppclass_ json) "register_documents" [ cstring "str", cppclass json "input" ] Nothing    
  , Virtual (cppclass_ json) "query" [ cppclass json "input" ] Nothing
  , Destructor (Just "deleteEngineWrapper")
  ]

{- 
jsonWrapper :: Class
jsonWrapper =
  Class cabal "JsonWrapper" []  mempty Nothing
  [ Constructor [ cstring "str" ] Nothing
  , Virtual cstring_ "serialize" [ ] Nothing
  , Destructor (Just "deleteJsonWrapper")
  ]
-}

json :: Class
json =
  Class cabal "json" []  mempty (Just "Json")
  [ Constructor [ ] Nothing
  , Static (cppclasscopy_ json) "parse" [cstring "txt"] Nothing 
    
  ]



classes = [ engineWrapper, json ] -- jsonWrapper

toplevelfunctions =
  [ TopLevelFunction cstring_ "serialize" [cppclass json "j"] Nothing ]
--    [ TopLevelFunction (cppclasscopy_ json) "json::parse" [cstring "txt"] (Just "parse") ]  

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


headerMap = [ ( "EngineWrapper", ([], [HdrName "similarity/similarity.h"]))
            , ( "JsonWrapper"  , ([], [HdrName "similarity/similarity.h"]))
            , ( "json"         , ([NS "nlohmann"], [HdrName "utils/json.h", HdrName "similarity/similarity.h"]))
            ]

main :: IO ()
main = do 
  simpleBuilder "Query.Binding" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates)
    [ ] extraDep


