{-# LANGUAGE RecordWildCards #-}

module Slim.Sim where

import Slim
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Text.PrettyPrint
import Data.IORef
import Data.Foldable

type Document = (Map ElementId Node, ElementId)
data Node = Node
  { n_elementId :: ElementId
  , n_namespace :: Namespace
  , n_tagName :: TagName
  , n_attributes :: Map AttributeName AttributeValue
  , n_text :: Maybe String
  , n_eventSources :: Set EventName
  , n_children :: [ElementId]
  } deriving Show

newNode :: ElementId -> Namespace -> TagName -> Node
newNode n_elementId n_namespace n_tagName = Node
  { n_attributes = Map.empty
  , n_text = Nothing
  , n_eventSources = Set.empty
  , n_children = []
  , ..
  }

applyAction :: Document -> ElementAction -> Document
applyAction (nodes, rootId) action =
  case action of
    Create ei ns tn ->
      (Map.insert ei (newNode ei ns tn) nodes, rootId)

    Replace ei1 ei2 ->
      let f n = n { n_children = [if ei1 == ei then ei2 else ei | ei <- n_children n] }
      in (Map.map f nodes, if ei1 == rootId then ei2 else rootId)

    Destroy ei ->
      let f n = n { n_children = filter (ei /=) (n_children n) }
      in (Map.map f (Map.delete ei nodes), rootId)

    SetAttribute ei an av ->
      let f n = n { n_attributes = Map.insert an av (n_attributes n) }
      in (Map.adjust f ei nodes, rootId)

    UnsetAttribute ei an ->
      let f n = n { n_attributes = Map.delete an (n_attributes n) }
      in (Map.adjust f ei nodes, rootId)

    SetText ei t ->
      let f n = n { n_text = t }
      in (Map.adjust f ei nodes, rootId)

    AddChildren ei eis ->
      let f n = n { n_children = (n_children n) ++ eis }
      in (Map.adjust f ei nodes, rootId)

    Subscribe ei en ->
      let f n = n { n_eventSources = Set.insert en (n_eventSources n) }
      in (Map.adjust f ei nodes, rootId)

    Unsubscribe ei en ->
      let f n = n { n_eventSources = Set.delete en (n_eventSources n) }
      in (Map.adjust f ei nodes, rootId)

ppDocument :: Document -> String
ppDocument (nodes, rootId) = renderStyle style (ppNode $ nodes ! rootId)
  where
    ppAttribute (k, v) = text k <> text "=" <> text v
    ppEventSource k = text ("on" ++ k)
    ppChildren childIds = vcat [ppNode n | Just n <- map (`Map.lookup` nodes) childIds]
    ppNode Node { .. } =
      text "<" <> ppElementName <+> ppAttributes <+> ppEventSources <> text ">" $$
        nest 4 (maybe empty text n_text $$ ppChildren n_children) $$
        text "</" <> text n_tagName <> text "#" <> int n_elementId <> text ">"
      where
        ppElementName = text n_tagName <> text "#" <> int n_elementId
        ppAttributes = hsep (ppAttribute <$> Map.toList n_attributes)
        ppEventSources = hsep (ppEventSource <$> Set.toList n_eventSources)

findNode :: Document -> (Node -> Bool) -> Node
findNode = findNodeN 0

findNodeN :: Int -> Document -> (Node -> Bool) -> Node
findNodeN x doc f =
  case drop x (findNodes doc f) of
    (n:_) -> n
    [] -> newNode (-1) Nothing "not found"

findNodes :: Document -> (Node -> Bool) -> [Node]
findNodes (nodes, rootId) f = filter f (bfs [nodes ! rootId])
  where
    bfs [] = []
    bfs ns = ns ++ bfs [nodes ! i | n <- ns, i <- n_children n]

startSim :: StartComponent void -> IO (Document, Node -> EventName -> EventData -> IO Document)
startSim s = do
  (as, rootId, fire) <- runStartRoot s
  let doc = foldl' applyAction (Map.empty, rootId) as
  ref <- newIORef doc
  let
    fire' n en ed = do
      doc <- readIORef ref
      as <- fire (n_elementId n, en, ed)
      let doc' = foldl' applyAction doc as
      writeIORef ref doc'
      return doc'
  return (doc, fire')
