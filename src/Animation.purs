module Animation
    ( component
    , Query
    )
    where
{-

    TODO: Animation Component:

        * Model
        * Style Queue - new styles, wait times
        * Update Queue on timing diffs
        * Spring Equations
        * Rendering a Child Component

-}

import Prelude

import Data.Maybe (Maybe(Nothing))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)



data Query a
    = Initialize a


type State
    = {}


data Slot
    = ChildSlot

derive instance eqAnimationChildSlot :: Eq Slot
derive instance ordAnimationChildSlot :: Ord Slot


component :: forall m g i o prop r
    .  (H.IProp ( style :: String | r) (prop Unit) -> H.Component HH.HTML g i o m)
   -> i
   -> (o -> Maybe (Query Unit))
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


initialState :: State
initialState = {}


-- | Render the child component with the current animation style.
render :: forall f r prop g o i m
    . (H.IProp ( style :: String | r) prop -> H.Component HH.HTML g i o m)
   -> i
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
