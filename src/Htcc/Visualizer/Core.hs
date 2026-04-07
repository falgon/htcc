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
    hasRenderableTree,
    mkWidth,
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

hasRenderableTree :: Show i => ATree i -> Bool
hasRenderableTree = not . null . encodeForest

normalizeRenderableTree :: ATree i -> ATree i
normalizeRenderableTree ATEmpty                       = ATEmpty
normalizeRenderableTree (ATNode (ATNull inner) _ _ _) = normalizeRenderableTree inner
normalizeRenderableTree (ATNode ATExprStmt _ lhs _)   = normalizeRenderableTree lhs
normalizeRenderableTree tree                          = tree

encodeForest :: Show i => ATree i -> [Tree String]
encodeForest tree = case normalizeRenderableTree tree of
    ATEmpty ->
        []
    normalizedTree ->
        [encodeTree normalizedTree]

encodeChildren :: Show i => [ATree i] -> [Tree String]
encodeChildren = concatMap encodeForest

encodeChildrenPreservingEmpty :: Show i => [ATree i] -> [Tree String]
encodeChildrenPreservingEmpty =
    map (encodeTree . normalizeRenderableTree)

-- | the function to convert `ATree` to `Data.Tree`
encodeTree :: Show i => ATree i -> Tree String
encodeTree ATEmpty = Node "Null" []
encodeTree (ATNode ATAdd _ l r) = Node "+" $ encodeChildren [l, r]
encodeTree (ATNode ATAddPtr _ l r) = Node "+" $ encodeChildren [l, r]
encodeTree (ATNode ATSub _ l r) = Node "-" $ encodeChildren [l, r]
encodeTree (ATNode ATSubPtr _ l r) = Node "-" $ encodeChildren [l, r]
encodeTree (ATNode ATPtrDis _ l r) = Node "-" $ encodeChildren [l, r]
encodeTree (ATNode ATMul _ l r) = Node "*" $ encodeChildren [l, r]
encodeTree (ATNode ATDiv _ l r) = Node "/" $ encodeChildren [l, r]
encodeTree (ATNode ATMod _ l r) = Node "%" $ encodeChildren [l, r]
encodeTree (ATNode ATAddAssign _ l r) = Node "+=" $ encodeChildren [l, r]
encodeTree (ATNode ATSubAssign _ l r) = Node "-=" $ encodeChildren [l, r]
encodeTree (ATNode ATMulAssign _ l r) = Node "*=" $ encodeChildren [l, r]
encodeTree (ATNode ATDivAssign _ l r) = Node "/=" $ encodeChildren [l, r]
encodeTree (ATNode ATAddPtrAssign _ l r) = Node "+=" $ encodeChildren [l, r]
encodeTree (ATNode ATSubPtrAssign _ l r) = Node "-=" $ encodeChildren [l, r]
encodeTree (ATNode ATLAnd _ l r) = Node "&&" $ encodeChildren [l, r]
encodeTree (ATNode ATLOr _ l r) = Node "||" $ encodeChildren [l, r]
encodeTree (ATNode ATAnd _ l r) = Node "&" $ encodeChildren [l, r]
encodeTree (ATNode ATAndAssign _ l r) = Node "&=" $ encodeChildren [l, r]
encodeTree (ATNode ATOr _ l r) = Node "|" $ encodeChildren [l, r]
encodeTree (ATNode ATOrAssign _ l r) = Node "|=" $ encodeChildren [l, r]
encodeTree (ATNode ATXor _ l r) = Node "^" $ encodeChildren [l, r]
encodeTree (ATNode ATXorAssign _ l r) = Node "^=" $ encodeChildren [l, r]
encodeTree (ATNode ATBitNot _ l r) = Node "~" $ encodeChildren [l, r]
encodeTree (ATNode ATShl _ l r) = Node "<<" $ encodeChildren [l, r]
encodeTree (ATNode ATShlAssign _ l r) = Node "<<=" $ encodeChildren [l, r]
encodeTree (ATNode ATShr _ l r) = Node ">>" $ encodeChildren [l, r]
encodeTree (ATNode ATShrAssign _ l r) = Node ">>=" $ encodeChildren [l, r]
encodeTree (ATNode ATLT _ l r) = Node "<" $ encodeChildren [l, r]
encodeTree (ATNode ATLEQ _ l r) = Node "<=" $ encodeChildren [l, r]
encodeTree (ATNode ATGT _ l r) = Node ">" $ encodeChildren [l, r]
encodeTree (ATNode ATGEQ _ l r) = Node ">=" $ encodeChildren [l, r]
encodeTree (ATNode ATEQ _ l r) = Node "==" $ encodeChildren [l, r]
encodeTree (ATNode ATNEQ _ l r) = Node "!=" $ encodeChildren [l, r]
encodeTree (ATNode ATNot _ l _) = Node "!" $ encodeChildren [l]
encodeTree (ATNode ATSizeof _ l _) = Node "sizeof" $ encodeChildren [l]
encodeTree (ATNode ATAlignof _ l _) = Node "_Alignof" $ encodeChildren [l]
encodeTree (ATNode ATAddr _ l _) = Node "&" $ encodeChildren [l]
encodeTree (ATNode ATDeref _ l _) = Node "*" $ encodeChildren [l]
encodeTree (ATNode ATAssign _ l r) = Node "=" $ encodeChildren [l, r]
encodeTree (ATNode ATPreInc _ l r) = Node "++ (pre)" $ encodeChildren [l, r]
encodeTree (ATNode ATPreDec _ l r) = Node "-- (pre)" $ encodeChildren [l, r]
encodeTree (ATNode ATPostInc _ l r) = Node "++ (post)" $ encodeChildren [l, r]
encodeTree (ATNode ATPostDec _ l r) = Node "-- (post)" $ encodeChildren [l, r]
encodeTree (ATNode (ATNum n) t l r) = Node (show n ++ " (" ++ show (CT.toTypeKind t) ++ ")") $ encodeChildren [l, r]
encodeTree (ATNode (ATConditional a b c) _ _ _) = Node "?:" $ encodeChildrenPreservingEmpty [a, b, c]
encodeTree (ATNode ATComma _ l r) = Node "," $ encodeChildren [l, r]
encodeTree (ATNode ATCast t l _) = Node ("(" ++ show (CT.toTypeKind t) ++ ")\n(type cast)") $ encodeChildren [l]
encodeTree (ATNode (ATMemberAcc _) _ l r) = Node "." $ encodeChildren [l, r]
encodeTree (ATNode ATReturn _ l r) = Node "return" $ encodeChildren [l, r]
encodeTree (ATNode ATIf _ l r) = Node "if" $ encodeChildrenPreservingEmpty [l, r]
encodeTree (ATNode ATElse _ l r) = Node "else" $ encodeChildrenPreservingEmpty [l, r]
encodeTree (ATNode (ATSwitch th xs) _ l r) =
    Node "switch" $
        encodeChildren [th]
            <> encodeChildrenPreservingEmpty xs
            <> if null xs
                then encodeChildrenPreservingEmpty [l, r]
                else encodeChildren [l, r]
encodeTree (ATNode (ATCase _ v) _ l r) =
    Node ("case " ++ show v) $ encodeChildrenPreservingEmpty [l] <> encodeChildren [r]
encodeTree (ATNode (ATDefault _) _ l r) =
    Node "default" $ encodeChildrenPreservingEmpty [l] <> encodeChildren [r]
encodeTree (ATNode ATWhile _ l r) = Node "while" $ encodeChildrenPreservingEmpty [l, r]
encodeTree (ATNode (ATFor atf) _ l r) =
    Node "for" $
        encodeChildrenPreservingEmpty (map fromATKindFor atf)
            <> encodeChildren [l, r]
encodeTree (ATNode ATBreak _ l r) = Node "break" $ encodeChildren [l, r]
encodeTree (ATNode ATContinue _ l r) = Node "continue" $ encodeChildren [l, r]
encodeTree (ATNode (ATGoto lbl) _ l r) = Node ("goto " ++ T.unpack lbl) $ encodeChildren [l, r]
encodeTree (ATNode (ATLabel lbl) _ l r) = Node (":" ++ T.unpack lbl) $ encodeChildren [l, r]
encodeTree (ATNode (ATBlock xs) _ _ _) = Node "{}" $ encodeChildrenPreservingEmpty xs
encodeTree (ATNode (ATLVar t o) _ l r) = Node (show t ++ " lvar" ++ show o) $ encodeChildren [l, r]
encodeTree (ATNode (ATGVar t n) _ l r) = Node (show t ++ " " ++ T.unpack n) $ encodeChildren [l, r]
encodeTree (ATNode (ATFuncPtr name) _ _ _) = Node ("funcptr " ++ T.unpack name) []
encodeTree (ATNode (ATDefFunc fname Nothing) t lhs _) = Node (show (CT.toTypeKind t) ++ " " ++ T.unpack fname ++ "()") $ encodeChildren [lhs]
encodeTree (ATNode (ATDefFunc fname (Just args)) t lhs _) = Node (show (CT.toTypeKind t) ++ " " ++ T.unpack fname ++ "(some arguments)") $ encodeChildren (args <> [lhs])
encodeTree (ATNode (ATCallFunc fname Nothing) _ lhs rhs) = Node (T.unpack fname ++ "()") $ encodeChildren [lhs, rhs]
encodeTree (ATNode (ATCallFunc fname (Just args)) _ lhs rhs) = Node (T.unpack fname ++ "(some arguments)") $ encodeChildren (args <> [lhs, rhs])
encodeTree (ATNode (ATCallPtr Nothing) _ lhs rhs) = Node "(*)(...)" $ encodeChildren [lhs, rhs]
encodeTree (ATNode (ATCallPtr (Just args)) _ lhs rhs) = Node "(*)(some arguments)" $ encodeChildren (lhs : args <> [rhs])
encodeTree (ATNode ATExprStmt _ lhs _) = encodeTree lhs
encodeTree (ATNode (ATStmtExpr exps) _ lhs rhs) =
    Node "({})" $ encodeChildrenPreservingEmpty exps <> encodeChildren [lhs, rhs]
encodeTree (ATNode (ATNull _) _ _ _) = Node "" []
-- TODO: handle ATFunc

renderNTree :: Tree String -> QDiagram SVG V2 Double Any
renderNTree nt = renderTree
    (\a -> letter a `atop` circle 2.5 # fc white)
    (~~)
    (symmLayout' (with & slHSep .~ 6 & slVSep .~ 6) nt)
    where
        letter a = text a # font "monospace" # fontSize (local 0.7)

-- | Build AST from C source code
visualize :: Show i => ASTs i -> SizeSpec V2 Double -> FilePath -> IO ()
visualize ast ss fpath = case concatMap encodeForest ast of
    [] ->
        putStrLnErr "There is nothing to describe"
    firstTree:otherTrees ->
        renderPretty fpath ss $
            foldr ((|||) . renderNTree) (renderNTree firstTree) otherTrees
