module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Graphics.Canvas as C
import Control.Monad.Eff.Ref (modifyRef, writeRef, readRef, newRef)
import Data.Maybe (Maybe(Just))
import Partial.Unsafe (unsafePartial)

foreign import requestAnimationFrame :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit -> Eff ( canvas :: C.CANVAS | e ) Unit
foreign import onKeyChange :: forall e. Int -> (Boolean -> Eff ( canvas :: C.CANVAS | e ) Unit) -> Eff ( canvas :: C.CANVAS | e ) Unit

screenSize :: C.Dimensions
screenSize = { width : 800.0,
               height : 600.0 }

type Player = {
    x :: Number,
    y :: Number
}

type InputState = {
    up :: Boolean,
    down :: Boolean,
    left :: Boolean,
    right :: Boolean
}
initialInputState :: InputState
initialInputState = { up : false,
                      down : false,
                      left : false,
                      right : false }

type GameState = {
    player :: Player
}
initialGameState :: GameState
initialGameState = { player : { x : 0.0, y : 0.0 }}

update :: InputState -> GameState -> GameState
update inputs gs@{ player } =
    let moveIf :: Boolean -> Number -> Number -> Player -> Player
        moveIf keyDown dx dy p = if keyDown
                                    then p { x = p.x + dx, y = p.y + dy }
                                    else p

        speed = 2.0
        player' =     (moveIf inputs.up 0.0 (-speed)
                   >>> moveIf inputs.down 0.0 speed
                   >>> moveIf inputs.left (-speed) 0.0
                   >>> moveIf inputs.right speed 0.0) player
    in gs { player = player' }

clearCanvas :: forall e. C.Context2D -> Eff ( canvas :: C.CANVAS | e ) Unit
clearCanvas ctx = void do
    C.setFillStyle "rgba(50, 50, 50, 0.3)" ctx
    C.fillRect ctx { x : 0.0, y : 0.0, w : screenSize.width, h : screenSize.height }

drawPlayer :: forall e. C.Context2D -> Player -> Eff ( canvas :: C.CANVAS | e ) Unit
drawPlayer ctx { x, y } = void do
    C.setFillStyle "#F00" ctx
    C.fillRect ctx { x, y, w : 32.0, h : 32.0 }

main :: Eff _ Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "gamecanvas"
    C.setCanvasDimensions screenSize canvas

    ctx <- C.getContext2D canvas

    stateRef <- newRef initialGameState
    inputRef <- newRef initialInputState

    onKeyChange 87 (\pressed -> void do
        modifyRef inputRef (_ { up = pressed })
    )
    onKeyChange 83 (\pressed -> void do
        modifyRef inputRef (_ { down = pressed })
    )
    onKeyChange 65 (\pressed -> void do
        modifyRef inputRef (_ { left = pressed })
    )
    onKeyChange 68 (\pressed -> void do
        modifyRef inputRef (_ { right = pressed })
    )

    let loop = void do
            currState <- readRef stateRef
            inputs    <- readRef inputRef
            let nextState = update inputs currState

            clearCanvas ctx
            drawPlayer ctx nextState.player

            writeRef stateRef nextState
            requestAnimationFrame loop

    requestAnimationFrame loop
