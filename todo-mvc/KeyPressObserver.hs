{-# LANGUAGE TypeFamilies #-}

module KeyPressObserver where

import Francium
import Francium.Component
import Francium.HTML

data KeyPressObserver a =
  KeyPressObserver a

instance Component a => Component (KeyPressObserver a) where
  data Output b e
       (KeyPressObserver a) = KeyPressObserverOut{keyPressed :: e Int,
                                                  passThrough :: Output b e a}
  construct (KeyPressObserver a) =
    do inner <- construct a
       keyPressEv <- newDOMEvent
       return Instantiation {render =
                               fmap (\html ->
                                       with html (onKeyPress keyPressEv) [])
                                    (render inner)
                            ,outputs =
                               KeyPressObserverOut (domEvent keyPressEv)
                                                   (outputs inner)}
