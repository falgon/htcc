{-|
Module      : Htcc.Visualizer.Core
Description : Build AST from C source code
Copyright   : (c) roki, 2019
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX

Build AST from C source code
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Htcc.Visualizer.Core (
    visualize
) where

import qualified Data.Text                 as T
import           Data.Tree                 (Tree (..))
import           Diagrams.Backend.SVG      (SVG, renderPretty)
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree (renderTree, slHSep, slVSep,
                                            symmLayout')

import           Htcc.CRules.Types         as CT
import           Htcc.Parser               (ASTs)
import           Htcc.Parser.AST.Core      (ATKind (..), ATree (..),
                                            fromATKindFor)
import           Htcc.Utils                (putStrLnErr)

-- | the function to convert `ATree` to `Data.Tree`
encodeTree :: Show i => ATree i -> Tree String
encodeTree ATEmpty = Node "Null" []
encodeTree (ATNode ATAdd _ l r) = Node "+" [encodeTree l, encodeTree r]
encodeTree (ATNode ATAddPtr _ l r) = Node "+" [encodeTree l, encodeTree r]
encodeTree (ATNode ATSub _ l r) = Node "-" [encodeTree l, encodeTree r]
encodeTree (ATNode ATSubPtr _ l r) = Node "-" [encodeTree l, encodeTree r]
encodeTree (ATNode ATPtrDis _ l r) = Node "-" [encodeTree l, encodeTree r]
encodeTree (ATNode ATMul _ l r) = Node "*" [encodeTree l, encodeTree r]
encodeTree (ATNode ATDiv _ l r) = Node "/" [encodeTree l, encodeTree r]
encodeTree (ATNode ATMod _ l r) = Node "%" [encodeTree l, encodeTree r]
encodeTree (ATNode ATAddAssign _ l r) = Node "+=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATSubAssign _ l r) = Node "-=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATMulAssign _ l r) = Node "*=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATDivAssign _ l r) = Node "/=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATAddPtrAssign _ l r) = Node "+=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATSubPtrAssign _ l r) = Node "-=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATLAnd _ l r) = Node "&&" [encodeTree l, encodeTree r]
encodeTree (ATNode ATLOr _ l r) = Node "||" [encodeTree l, encodeTree r]
encodeTree (ATNode ATAnd _ l r) = Node "&" [encodeTree l, encodeTree r]
encodeTree (ATNode ATAndAssign _ l r) = Node "&=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATOr _ l r) = Node "|" [encodeTree l, encodeTree r]
encodeTree (ATNode ATOrAssign _ l r) = Node "|=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATXor _ l r) = Node "^" [encodeTree l, encodeTree r]
encodeTree (ATNode ATXorAssign _ l r) = Node "^=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATBitNot _ l r) = Node "~" [encodeTree l, encodeTree r]
encodeTree (ATNode ATShl _ l r) = Node "<<" [encodeTree l, encodeTree r]
encodeTree (ATNode ATShlAssign _ l r) = Node "<<=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATShr _ l r) = Node ">>" [encodeTree l, encodeTree r]
encodeTree (ATNode ATShrAssign _ l r) = Node ">>=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATLT _ l r) = Node "<" [encodeTree l, encodeTree r]
encodeTree (ATNode ATLEQ _ l r) = Node "<=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATGT _ l r) = Node ">" [encodeTree l, encodeTree r]
encodeTree (ATNode ATGEQ _ l r) = Node ">=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATEQ _ l r) = Node "==" [encodeTree l, encodeTree r]
encodeTree (ATNode ATNEQ _ l r) = Node "!=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATNot _ l _) = Node "!" [encodeTree l]
encodeTree (ATNode ATAddr _ l _) = Node "&" [encodeTree l]
encodeTree (ATNode ATDeref _ l _) = Node "*" [encodeTree l]
encodeTree (ATNode ATAssign _ l r) = Node "=" [encodeTree l, encodeTree r]
encodeTree (ATNode ATPreInc _ l r) = Node "++ (pre)" [encodeTree l, encodeTree r]
encodeTree (ATNode ATPreDec _ l r) = Node "-- (pre)" [encodeTree l, encodeTree r]
encodeTree (ATNode ATPostInc _ l r) = Node "++ (post)" [encodeTree l, encodeTree r]
encodeTree (ATNode ATPostDec _ l r) = Node "-- (post)" [encodeTree l, encodeTree r]
encodeTree (ATNode (ATNum n) t l r) = Node (show n ++ " (" ++ show (CT.toTypeKind t) ++ ")") [encodeTree l, encodeTree r]
encodeTree (ATNode (ATConditional a b c) _ _ _) = Node "?:" [encodeTree a, encodeTree b, encodeTree c]
encodeTree (ATNode ATComma _ l r) = Node "," [encodeTree l, encodeTree r]
encodeTree (ATNode ATCast t l _) = Node ("(" ++ show (CT.toTypeKind t) ++ ")\n(type cast)") [encodeTree l]
encodeTree (ATNode (ATMemberAcc _) _ l r) = Node "." [encodeTree l, encodeTree r]
encodeTree (ATNode ATReturn _ l r) = Node "return" [encodeTree l, encodeTree r]
encodeTree (ATNode ATIf _ l r) = Node "if" [encodeTree l, encodeTree r]
encodeTree (ATNode ATElse _ l r) = Node "else" [encodeTree l, encodeTree r]
encodeTree (ATNode (ATSwitch th xs) _ l r) = Node "switch" $ encodeTree th : map encodeTree xs ++ [encodeTree l, encodeTree r]
encodeTree (ATNode (ATCase _ v) _ l r) = Node ("case " ++ show v) [encodeTree l, encodeTree r]
encodeTree (ATNode (ATDefault _) _ l r) = Node "default" [encodeTree l, encodeTree r]
encodeTree (ATNode ATWhile _ l r) = Node "while" [encodeTree l, encodeTree r]
encodeTree (ATNode (ATFor atf) _ l r) = Node "for" $ map (encodeTree . fromATKindFor) atf ++ [encodeTree l, encodeTree r]
encodeTree (ATNode ATBreak _ l r) = Node "break" [encodeTree l, encodeTree r]
encodeTree (ATNode ATContinue _ l r) = Node "continue" [encodeTree l, encodeTree r]
encodeTree (ATNode (ATGoto lbl) _ l r) = Node ("goto " ++ T.unpack lbl) [encodeTree l, encodeTree r]
encodeTree (ATNode (ATLabel lbl) _ l r) = Node (":" ++ T.unpack lbl) [encodeTree l, encodeTree r]
encodeTree (ATNode (ATBlock xs) _ _ _) = Node "{}" $ map encodeTree xs
encodeTree (ATNode (ATLVar t o) _ l r) = Node (show t ++ " lvar" ++ show o) [encodeTree l, encodeTree r]
encodeTree (ATNode (ATGVar t n) _ l r) = Node (show t ++ " " ++ T.unpack n) [encodeTree l, encodeTree r]
encodeTree (ATNode (ATDefFunc fname Nothing) t lhs _) = Node (show (CT.toTypeKind t) ++ " " ++ T.unpack fname ++ "()") [encodeTree lhs]
encodeTree (ATNode (ATDefFunc fname (Just args)) t lhs _) = Node (show (CT.toTypeKind t) ++ " " ++ T.unpack fname ++ "(some arguments)") $ map encodeTree args ++ [encodeTree lhs]
encodeTree (ATNode (ATCallFunc fname Nothing) _ lhs rhs) = Node (T.unpack fname ++ "()") [encodeTree lhs, encodeTree rhs]
encodeTree (ATNode (ATCallFunc fname (Just args)) _ lhs rhs) = Node (T.unpack fname ++ "(some arguments)") $ map encodeTree args ++ [encodeTree lhs, encodeTree rhs]
encodeTree (ATNode ATExprStmt _ lhs _) = encodeTree lhs
encodeTree (ATNode (ATStmtExpr exps) _ lhs rhs) = Node "({})" $ map encodeTree exps ++ [encodeTree lhs, encodeTree rhs]
encodeTree (ATNode (ATNull _) _ _ _) = Node "" []

renderNTree :: Tree String -> QDiagram SVG V2 Double Any
renderNTree nt = renderTree
    (\a -> letter a `atop` circle 2.5 # fc white)
    (~~)
    (symmLayout' (with & slHSep .~ 6 & slVSep .~ 6) nt)
    where
        letter a = text a # font "monospace" # fontSize (local 0.7)

-- | Build AST from C source code
visualize :: Show i => ASTs i -> SizeSpec V2 Double -> FilePath -> IO ()
visualize ast ss fpath = let et = map encodeTree ast in if not (null et) then
    renderPretty fpath ss (foldr ((|||) . renderNTree) (renderNTree $ head et) $ tail et) else
        putStrLnErr "There is nothing to describe"
