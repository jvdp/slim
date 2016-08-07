{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Slim
  ( Local
  , Shared
  , Event
  , Behavior
  , stepper
  , accumB
  , track
  , never
  , merge
  , mergeAll
  , useB
  , useE
  , whenJust
  , Component
  , Static
  , Dynamic
  , MasterDomEvent
  , DomEventInfo
  , ElementId
  , Namespace
  , TagName
  , AttributeName
  , AttributeValue
  , EventName
  , EventData
  , ElementAction(..)
  , Start
  , StartComponent(..)
  , Void
  , runStartRoot
  , startC
  , startB
  , silence
  , getEvent
  , addEvent
  , replaceEvent
  , textComponent
  , containerComponent
  , emptyComponent
  , mount
  ) where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Void
import Debug.Trace

import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Trans.Class

data Local t
data Shared

data Event t a where
  LocalE ::
    { e_run :: Execution (Event Shared a)
    } -> Event (Local t) a
  SharedE ::
    { e_subscribe :: (a -> Execution ()) -> Execution (Execution ())
    } -> Event Shared a

instance Functor (Event (Local t)) where
  fmap f (LocalE me) = LocalE (fmap f <$> me)

instance Functor (Event Shared) where
  fmap f (SharedE g) = SharedE $ \h -> g (h . f)

instance Monoid (Event (Local t) a) where
  mempty = LocalE (return mempty)
  mappend (LocalE me1) (LocalE me2) = LocalE (mappend <$> me1 <*> me2)

instance Monoid (Event Shared a) where
  mempty = SharedE (const (return (return ())))
  mappend (SharedE f) (SharedE g) = SharedE (\h -> (>>) <$> f h <*> g h)

data Behavior t a where
  LocalB ::
    { b_run :: Execution (Behavior Shared a)
    } -> Behavior (Local t) a
  SharedB ::
    { b_sample :: Execution a
    , b_pulses :: Event Shared ()
    } -> Behavior Shared a

instance Functor (Behavior (Local t)) where
  fmap f (LocalB mb) = LocalB (fmap f <$> mb)

instance Functor (Behavior Shared) where
  fmap f (SharedB mx e) = SharedB (f <$> mx) e

instance Applicative (Behavior (Local t)) where
  pure x = LocalB (return (pure x))
  LocalB mbf <*> LocalB mbx = LocalB ((<*>) <$> mbf <*> mbx)

instance Applicative (Behavior Shared) where
  pure x = SharedB (return x) mempty
  SharedB mf e1 <*> SharedB mx e2 = SharedB (mf <*> mx) (e1 <> e2)

stepper :: a -> Event (Local t) a -> Behavior (Local t) a
stepper x e = accumB x (const <$> e)

accumB :: a -> Event (Local t) (a -> a) -> Behavior (Local t) a
accumB x e = LocalB $ do
  ref <- liftIO $ newIORef x
  e' <- e_run e
  e_subscribe e' $ \f -> do
    x <- liftIO $ readIORef ref
    let x' = f x
    liftIO $ writeIORef ref x'
  return SharedB
    { b_sample = liftIO $ readIORef ref
    , b_pulses = () <$ e'
    }

trackM :: Eq k => Behavior (Local t) [k] -> (k -> Execution a) -> Behavior (Local t) [a]
trackM lbks f = LocalB $ do
  let
    update xs k = do
      case lookup k xs of
        Just x -> return (k, x)
        Nothing -> do
          x <- f k
          return (k, x)
  bks <- b_run lbks
  ks <- b_sample bks
  xs <- mapM (update []) ks
  ref <- liftIO $ newIORef xs
  e_subscribe (changes bks) $ \ks' -> do
    xs <- liftIO $ readIORef ref
    xs' <- mapM (update xs) ks'
    liftIO $ writeIORef ref xs'
  return SharedB
    { b_sample = map snd <$> liftIO (readIORef ref)
    , b_pulses = b_pulses bks
    }

useB :: Behavior Shared a -> Behavior (Local t) a
useB b = LocalB (return b)

useE :: Event Shared a -> Event (Local t) a
useE e = LocalE (return e)

never :: Event (Local t) a
never = mempty

merge :: Event (Local t) a -> Event (Local t) a -> Event (Local t) a
merge = mappend

mergeAll :: [Event (Local t) a] -> Event (Local t) a
mergeAll = mconcat

changes :: Behavior Shared a -> Event Shared a
changes (SharedB mx (SharedE f)) = SharedE $ \h -> f (\() -> mx >>= h)

whenJust :: Event t (Maybe a) -> Event t a
whenJust e = case e of
  LocalE me -> LocalE (whenJust <$> me)
  SharedE f -> SharedE $ \g -> f (maybe (return ()) g)

newEvent :: IO (Event Shared a, a -> Execution ())
newEvent = do
  counter <- newIORef 0
  registry <- newIORef IntMap.empty
  value <- newIORef Nothing

  let
    subscribe handler = liftIO $ do
      i <- readIORef counter
      writeIORef counter (i + 1)
      modifyIORef registry (IntMap.insert i handler)
      return . liftIO $ do
        modifyIORef registry (IntMap.delete i)

    fire x = do
      liftIO $ writeIORef value (Just x)
      handlers <- IntMap.elems <$> liftIO (readIORef registry)
      mapM_ ($ x) handlers

  return (SharedE subscribe, fire)

type ComponentRegistry = Map ComponentId ElementId
type Execution = RWST MasterDomEvent [ElementAction] (ComponentRegistry, ComponentId, ElementId) IO
type MasterDomEvent = Event Shared DomEventInfo
type DomEventInfo = (ElementId, EventName, EventData)

type EventData = String

data EventRouter a b = EventRouter
  { er_dom :: ElementId -> Event Shared b
  , er_sub :: [Event Shared a] -> Event Shared b
  }

instance Functor (EventRouter a) where
  fmap f EventRouter { .. } = EventRouter
    { er_dom = fmap f . er_dom
    , er_sub = fmap f . er_sub
    }

mkEvent :: EventRouter a b -> ElementId -> [Event Shared a] -> Event Shared b
mkEvent EventRouter { .. } ei subEvents = er_dom ei <> er_sub subEvents

nullRouter :: EventRouter a b
nullRouter = EventRouter
  { er_dom = const mempty
  , er_sub = const mempty
  }

childRouter :: EventRouter a a
childRouter = EventRouter
  { er_dom = const mempty
  , er_sub = mconcat
  }

type ComponentId = Int

data Component t a where
  StaticComponent ::
    { c_elementDefinition :: ElementDefinition
    , c_eventRouter :: EventRouter b a
    , c_children :: [Component Static b]
    } -> Component Static a
  DynamicComponent ::
    { c_id :: ComponentId
    , c_event :: Event Shared a
    } -> Component (Dynamic t) a
  MountedComponent ::
    { c_component :: Component (Dynamic t) a
    } -> Component Static a



data Dynamic t
data Static

instance Functor (Component t) where
  fmap f component = case component of
    StaticComponent { .. } -> StaticComponent { c_eventRouter = fmap f c_eventRouter, .. }
    DynamicComponent { .. } -> DynamicComponent { c_event = fmap f c_event, .. }
    MountedComponent { .. } -> MountedComponent { c_component = fmap f c_component }

mount :: Component (Dynamic t) a -> Component Static a
mount = MountedComponent

textComponent :: Maybe String -> String -> [(String, String)] -> String -> Component Static Void
textComponent ed_namespace ed_tagName ed_attributes text = StaticComponent { .. }
  where
    c_eventRouter = nullRouter
    c_children = []
    c_elementDefinition = ElementDefinition { .. }
    ed_text = Just text
    ed_eventSources = []

containerComponent :: Maybe String -> String -> [(String, String)] -> [Component Static a] -> Component Static a
containerComponent ed_namespace ed_tagName ed_attributes c_children = StaticComponent { .. } 
  where
    c_eventRouter = childRouter
    c_elementDefinition = ElementDefinition { .. }
    ed_text = Nothing
    ed_eventSources = []

emptyComponent :: Maybe String -> String -> [(String, String)] -> Component Static Void
emptyComponent ed_namespace ed_tagName ed_attributes = StaticComponent { .. }
  where
    c_eventRouter = nullRouter
    c_children = []
    c_elementDefinition = ElementDefinition { .. }
    ed_text = Nothing
    ed_eventSources = []

silence :: Component Static void -> Component Static a
silence c =
  case c of
    StaticComponent { .. } -> StaticComponent { c_eventRouter = nullRouter, c_elementDefinition = c_elementDefinition { ed_eventSources = [] }, .. }
    MountedComponent { .. } -> MountedComponent { c_component = c_component { c_event = mempty }, .. }


addEvent :: EventName -> (EventData -> a) -> Component Static a -> Component Static a
addEvent en f c@StaticComponent { .. } =
  StaticComponent
    { c_eventRouter = c_eventRouter { er_dom = \ei -> fmap f (getDomEvent en ei) <> er_dom ei }
    , c_elementDefinition = c_elementDefinition { ed_eventSources = en : ed_eventSources }
    , ..
    }
  where
    ElementDefinition { .. } = c_elementDefinition
    EventRouter { .. } = c_eventRouter

replaceEvent :: EventName -> Component Static void -> Component Static EventData
replaceEvent en = addEvent en id . silence

getDomEvent :: EventName -> ElementId -> Event Shared EventData
getDomEvent en ei = SharedE $ \f -> do
  mde <- ask
  e_subscribe mde $ \(ei', en', ed) ->
    if ei == ei' && en == en'
      then f ed
      else return ()


getEvent :: Component (Dynamic t) a -> Event (Local t) a
getEvent DynamicComponent { .. } = useE c_event


data ElementDefinition = ElementDefinition
  { ed_namespace :: Namespace
  , ed_tagName :: TagName
  , ed_attributes :: [(AttributeName, AttributeValue)]
  , ed_text :: Maybe String
  , ed_eventSources :: [EventName]
  }


type Namespace = Maybe String
type TagName = String
type AttributeName = String
type AttributeValue = String
type EventName = String
type ElementId = Int

data RenderedComponent where
  RenderedComponent ::
    { rc_component :: Component t a
    , rc_id :: ElementId
    , rc_children :: [RenderedComponent]
    } -> RenderedComponent

type Reconciliation a = RWS (ComponentId -> ElementId) [ElementAction] ElementId a

freshElementId :: Reconciliation ElementId
freshElementId = do
  i <- get
  put (i + 1)
  return i

data ElementAction
  = Create ElementId Namespace TagName
  | Replace ElementId ElementId
  | Destroy ElementId
  | SetAttribute ElementId AttributeName AttributeValue
  | UnsetAttribute ElementId AttributeName
  | SetText ElementId (Maybe String)
  | AddChildren ElementId [ElementId]
  | Subscribe ElementId EventName
  | Unsubscribe ElementId EventName

firstRender :: Component t a -> Reconciliation (RenderedComponent, Event Shared a)
firstRender rc_component = case rc_component of
  StaticComponent { c_elementDefinition = ElementDefinition { .. }, .. } ->
    do
      rc_id <- freshElementId
      tell $
        [ Create rc_id ed_namespace ed_tagName
        , SetText rc_id ed_text
        ] ++
        [ Subscribe rc_id en | en <- ed_eventSources
        ] ++
        [ SetAttribute rc_id an v | (an,v) <- ed_attributes
        ]
      (rc_children, subEvents) <- unzip <$> mapM firstRender c_children
      tell [AddChildren rc_id [childId | RenderedComponent { rc_id = childId } <- rc_children]]
      return (RenderedComponent { .. }, mkEvent c_eventRouter rc_id subEvents)

  DynamicComponent { .. } ->
    do
      rc_id <- ($ c_id) <$> ask
      return (RenderedComponent { rc_children = [], .. }, c_event)

  MountedComponent { .. } ->
    firstRender c_component

reconcile :: RenderedComponent -> Component t a -> Reconciliation (RenderedComponent, Event Shared a)
reconcile
  rc@RenderedComponent { rc_component = StaticComponent { c_elementDefinition = prevElementDefinition }, .. }
  rc_component@StaticComponent { .. }
  | isCompatible prevElementDefinition c_elementDefinition =
    do
      updateElement rc c_elementDefinition
      (rc_children, subEvents) <- unzip <$> reconcileChildren rc_id rc_children c_children
      return (RenderedComponent { .. }, mkEvent c_eventRouter rc_id subEvents)

reconcile
  renderedComponent@RenderedComponent { rc_component = DynamicComponent { c_id = prevId } }
  DynamicComponent { .. }
  | prevId == c_id =
    return (renderedComponent, c_event)

reconcile renderedComponent MountedComponent { .. } =
  reconcile renderedComponent c_component

reconcile renderedComponent component =
  do
    terminate renderedComponent
    (renderedComponent', event) <- firstRender component
    tell [Replace (rc_id renderedComponent) (rc_id renderedComponent')]
    return (renderedComponent', event)

reconcileChildren :: ElementId -> [RenderedComponent] -> [Component t a] -> Reconciliation [(RenderedComponent, Event Shared a)]
reconcileChildren parentId renderedComponents components = catMaybes <$> mapM reconcileChild (zipMaybe renderedComponents components)
  where
    reconcileChild m =
      case m of
        (Just renderedComponent, Just component) -> do
          (renderedComponent', event) <- reconcile renderedComponent component
          return (Just (renderedComponent', event))
        (Just renderedComponent, Nothing) -> do
          terminate renderedComponent
          return Nothing
        (Nothing, Just component) -> do
          (renderedComponent, event) <- firstRender component
          tell [AddChildren parentId [rc_id renderedComponent]]
          return (Just (renderedComponent, event))

zipMaybe :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipMaybe (x:xs) (y:ys) = (Just x, Just y) : zipMaybe xs ys
zipMaybe [] ys = [(Nothing, Just y) | y <- ys]
zipMaybe xs [] = [(Just x, Nothing) | x <- xs]

isCompatible :: ElementDefinition -> ElementDefinition -> Bool
isCompatible node node' =
  ed_namespace node == ed_namespace node' && ed_tagName node == ed_tagName node'

updateElement :: RenderedComponent -> ElementDefinition -> Reconciliation ()
updateElement RenderedComponent { rc_component = StaticComponent { c_elementDefinition = old }, .. } new = do
  tell $ updateText ++ updateAttributes ++ updateEventSources
  where
    updateText
      | ed_text old /= ed_text new = [SetText rc_id (ed_text new)]
      | otherwise = []

    updateAttributes =
      [ UnsetAttribute rc_id an | an <- Map.keys $ Map.difference oldAttributes newAttributes
      ] ++
      [ SetAttribute rc_id an v | (an,v) <- Map.toList $ Map.differenceWith updateAttr newAttributes oldAttributes
      ]
      where
        oldAttributes = Map.fromList (ed_attributes old)
        newAttributes = Map.fromList (ed_attributes new)
        updateAttr new old = if new == old then Nothing else Just new

    updateEventSources =
      [ Unsubscribe rc_id en | en <- Set.toList $ Set.difference oldEventSources newEventSources
      ] ++
      [ Subscribe rc_id en | en <- Set.toList $ Set.difference newEventSources oldEventSources
      ]
      where
        oldEventSources = Set.fromList (ed_eventSources old)
        newEventSources = Set.fromList (ed_eventSources new)


terminate :: RenderedComponent -> Reconciliation ()
terminate RenderedComponent { .. } = do
  tell [Destroy rc_id]
  mapM_ terminate rc_children




newtype Deferred m a = Deferred { unDeferred :: WriterT [Deferred m ()] m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadTrans Deferred where
  lift x = Deferred (lift x)

defer :: Monad m => Deferred m () -> Deferred m ()
defer m = Deferred (tell [m])

runDeferred :: Monad m => Deferred m a -> m a
runDeferred m = do
  (x, ms) <- runWriterT (unDeferred m)
  unless (null ms) (runDeferred (sequence_ ms))
  return x

executeB :: Behavior (Local t) a -> Deferred Execution (Behavior Shared a)
executeB lb = do
  ref <- liftIO (newIORef (error "behavior not yet initialized"))
  defer . lift $ do
    sb <- b_run lb
    liftIO (writeIORef ref sb)

  return SharedB
    { b_sample = b_sample =<< liftIO (readIORef ref)
    , b_pulses = SharedE $ \h -> do
        SharedB { .. } <- liftIO (readIORef ref)
        e_subscribe b_pulses h
    }

executeE :: Event (Local t) a -> Deferred Execution (Event Shared a)
executeE le = lift (e_run le)

data StartComponent a = StartComponent (forall s. Start s (Component (Dynamic s) a))

newtype Start t a = Start { unStart :: Deferred Execution a } deriving (Functor, Applicative, Monad, MonadFix)

executeStart :: Start t a -> Execution a
executeStart = runDeferred . unStart

runStartRoot :: StartComponent a -> IO ([ElementAction], ElementId, DomEventInfo -> IO [ElementAction])
runStartRoot (StartComponent sc) = do
  (mde, fire) <- newEvent
  (comp, s@(reg, _, _), as) <- runRWST (executeStart sc) mde (Map.empty, 0, 0)
  ref <- newIORef s
  let
    update dei = do
      s <- readIORef ref
      ((), s', as) <- runRWST (fire dei) mde s
      writeIORef ref s'
      return as
  return (as, reg ! c_id comp, update)

executeReconciliation :: Reconciliation a -> Execution a
executeReconciliation r = do
  (reg, cid, eid) <- get
  let (x, eid', as) = runRWS r (reg !) eid
  tell as
  put (reg, cid, eid')
  return x

freshComponentId :: Execution ComponentId
freshComponentId = do
  (reg, cid, eid) <- get
  put (reg, cid + 1, eid)
  return cid

setComponentElementId :: ComponentId -> ElementId -> Execution ()
setComponentElementId cid eid = do
  (reg, cid', eid') <- get
  put (Map.insert cid eid reg, cid', eid')

startC :: Behavior (Local t) (Component Static a) -> Start t (Component (Dynamic t) a)
startC b = Start $ do
  c_id <- lift freshComponentId
  (c_event, redirect) <- liftIO proxyEvent
  b' <- executeB b
  defer . defer . lift $ do
    comp <- b_sample b'
    (rc, ev) <- executeReconciliation (firstRender comp)
    setComponentElementId c_id (rc_id rc)
    redirect ev
    ref <- liftIO $ newIORef rc
    e_subscribe (changes b') $ \comp' -> do
      rc <- liftIO $ readIORef ref
      (rc', ev) <- executeReconciliation (reconcile rc comp')
      setComponentElementId c_id (rc_id rc')
      liftIO $ writeIORef ref rc'
      redirect ev
    return ()
  return DynamicComponent { .. }

proxyEvent :: IO (Event Shared a, Event Shared a -> Execution ())
proxyEvent = do
  (ev, fire) <- newEvent
  ref <- newIORef (return ())
  let
    redirect ev' = do
      join (liftIO $ readIORef ref)
      unsubscribe <- e_subscribe ev' fire
      liftIO $ writeIORef ref unsubscribe
  return (ev, redirect)


startB :: Behavior (Local t) a -> Start t (Behavior Shared a)
startB b = Start (executeB b)

startE :: Event (Local t) a -> Start t (Event Shared a)
startE e = Start (executeE e)

track :: Eq k => Behavior (Local t) [k] -> (k -> StartComponent a) -> Behavior (Local t) [Component (Dynamic t) a]
track b f = trackM b $ \k -> case f k of StartComponent c -> executeStart c


instance Show (EventRouter a b) where
  show _ = "<< EventRouter >>"

instance Show (Event t a) where
  show _ = "<< Event >>"

deriving instance Show ElementDefinition
deriving instance Show ElementAction
deriving instance Show (Component t a)
deriving instance Show RenderedComponent

