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

cabalattr = 
    CabalAttr 
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = [ ]
    , cabalattr_extralibdirs = []
    }

engineWrapper :: Class
engineWrapper =
  Class cabal "EngineWrapper" [] mempty Nothing
  [ Constructor [ int "typ", cstring "configfile" ] Nothing
  , Virtual (cppclass_ json_t) "register_documents" [ cstring "str", cppclass json_t "input" ] Nothing    
  , Virtual (cppclass_ json_t) "query" [ cppclass json_t "input" ] Nothing
  -- , Virtual (cppclass_ json_t) "suggest" [ cppclass json_t "input" ] Nothing    
  , Destructor (Just "deleteEngineWrapper")
  ]

json_t :: Class
json_t =
  Class cabal "json_t" []  mempty (Just "Json_t")
  [ Constructor [ ] Nothing
  , Static (cppclasscopy_ json_t) "parse" [cstring "txt"] Nothing 
    
  ]

classes = [ engineWrapper, json_t ] 

toplevelfunctions = [ TopLevelFunction cstring_ "serialize" [cppclass json_t "j"] Nothing ]

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


headerMap = [ ( "EngineWrapper", ([NS "util"], [HdrName "similarity/similarity.h"]))
            , ( "json_t"         , ([NS "util"], [HdrName "utils/json.h", HdrName "similarity/similarity.h" ]))
            ]

main :: IO ()
main = do 
  simpleBuilder "Query.Binding" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates)
    [ ] extraDep
