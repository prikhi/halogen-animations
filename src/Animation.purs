module Animation
    ( component
    , Query
    )
    where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)


{-

TODO: Animation Component:

    * Model
    * Style Queue - new styles, wait times
    * Update Queue on timing diffs
    * Spring Equations?
    * Rendering a Child Component
-}

data Query a
    = Initialize a


type State
    = {}


data Slot
    = ChildSlot

derive instance eqAnimationChildSlot :: Eq Slot
derive instance ordAnimationChildSlot :: Ord Slot


-- | A wrapper component around some component you want to animate.
-- |
-- | The component tracks the animation state and generates a style property
-- | that your component should utilize in it's `render` function.
-- |
-- | You can interrupt an animation or queue more style changes from both the
-- | child component and the parent of the animation component.
component :: forall m g i o prop r
    .  (H.IProp ( style :: String | r) (prop Unit) -> H.Component HH.HTML g i o m)
   -- ^ A function building the child component from a style property.
   -> i
   -- ^ The input value for the child component.
   -> (o -> Maybe (Query Unit))
   -- ^ The child component's output message handler.
   -> H.Component HH.HTML Query Unit Void m
component child i o =
    H.lifecycleParentComponent
        { initialState: const initialState
        , render: render child i o
        , eval
        , receiver: const Nothing
        , initializer: Nothing
        , finalizer: Nothing
        }


-- | The animation state.
initialState :: State
initialState = {}


-- | Render the child component with the current animation style.
-- |
-- | Your child component should use the style property passed to it when it
-- | renders it's element.
render :: forall f r prop g o i m
    . (H.IProp ( style :: String | r) prop -> H.Component HH.HTML g i o m)
   -- ^ A function building the child component from a style property.
   -> i
   -- ^ The input value for the child component.
   -> (o -> Maybe (f Unit))
   -- ^ The child component's output message handler.
   -> State
   -- ^ The animation state
   -> HH.HTML (H.ComponentSlot HH.HTML g m Slot (f Unit)) (f Unit)
render child i o state =
    HH.slot ChildSlot (child $ style (pure unit)) i o


-- | Advance the animation state.
eval :: forall f. Applicative f => Query ~> f
eval = case _ of
    Initialize a -> pure a
