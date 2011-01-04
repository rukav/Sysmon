-- |
-- Module      :  Derive
-- Copyright   :  (c) Vitaliy Rukavishnikov 2011
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  virukav@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generate average functions for Sysmon data type

module Derive where
import Average
import Language.Haskell.TH
import Control.Monad

genpe :: String -> Int -> Q ([PatQ],[ExpQ]) 
genpe s n = do 
    ns <- replicateM n (newName s)
    return (map varP ns, map varE ns) 
 
deriveAverage t = do
  TyConI (DataD _ _ _ constructors _) <- reify t
  let avgClause (RecC name fields) = do
        ([xsp], [xsv]) <- genpe "xs" 1
        (pats, vars) <- genpe "x" (length fields)

        let mkApp [x,y] = appE x y
            mkApp (x:ys) = appE (mkApp (init (x:ys))) (last ys)
        
        let vare s = varE (mkName s)   
        let dec (vp, fld) = valD 
             vp 
             (normalB 
                (appE
                    (vare "avg") 
                    (appE 
                       (appE (vare "map") (varE fld)) 
                        xsv)
                )
             ) []

        let declst (vp, fld) = valD
             vp
             (normalB
               (appE
                  (appE (vare "map") (vare "avg"))
                  (appE (vare "transpose") 
                     (appE 
                       (appE (vare "map") (varE fld))
                       xsv)))) []

        let decl (vp, (fld, _, typ)) = case typ of
                 AppT _ _ -> declst (vp, fld)
                 _ -> dec (vp, fld)

        let decls = map decl $ zip pats fields
        clause [xsp] (normalB $ mkApp ((conE name) : vars)) decls

  body <- mapM avgClause constructors
  return [InstanceD [] (AppT (ConT $ mkName "Averageable") (ConT t)) 
           [FunD (mkName "avg") body]
         ]
