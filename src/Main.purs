module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Graphics.Canvas as C
import Control.Monad.Eff.Random (randomRange, RANDOM)
import Control.Monad.Eff.Ref (modifyRef, writeRef, readRef, newRef)
import Data.HeytingAlgebra ((||))
import Data.Maybe (Maybe(Just))
import Math (pi, cos, sin)
import Partial.Unsafe (unsafePartial)

foreign import requestAnimationFrame :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit -> Eff ( canvas :: C.CANVAS | e ) Unit
foreign import onKeyChange :: forall e. Int -> (Boolean -> Eff ( canvas :: C.CANVAS | e ) Unit) -> Eff ( canvas :: C.CANVAS | e ) Unit

screenSize :: C.Dimensions
screenSize = { width : 800.0,
               height : 600.0 }

type Point = {
    x :: Number,
    y :: Number
}
point :: Number -> Number -> Point
point x y = { x, y }

type Circle = {
    x :: Number,
    y :: Number,
    r :: Number
}
circle :: Number -> Number -> Number -> Circle
circle x y r = { x, y, r }

circleContains :: Circle -> Point -> Boolean
circleContains { x:cx, y:cy, r } { x:px, y:py } = ((px-cx) * (px-cx) + (py-cy) * (py-cy)) < r * r

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

type Player = {
    x :: Number,
    y :: Number,
    vx :: Number,
    vy :: Number,
    angle :: Number,
    rad :: Number
}
initialPlayer :: Player
initialPlayer = { x : 100.0, y : 100.0, vx : 0.0, vy : 0.0, angle : 0.0, rad : 16.0 }

getPlayerPoints :: Player -> Array Point
getPlayerPoints player =
    let x = player.x
        y = player.y
        r = player.rad
        a1 = player.angle
        a2 = player.angle + (3.0 * pi / 4.0)
        a3 = player.angle + (5.0 * pi / 4.0)
        p1 = point ((cos a1) * r + x) ((sin a1) * r + y)
        p2 = point ((cos a2) * r + x) ((sin a2) * r + y)
        p3 = point ((cos a3) * r + x) ((sin a3) * r + y)
    in [p1, p2, p3]

type Coin = {
    x :: Number,
    y :: Number
}
newCoin :: forall e. C.Dimensions -> Eff ( random :: RANDOM | e ) Coin
newCoin { width: w, height : h } = do
    x <- randomRange 0.0 w
    y <- randomRange 0.0 h
    pure { x, y }

type GameState = {
    player :: Player,
    coin :: Coin
}
initialGameState :: forall e. Eff ( random :: RANDOM | e ) GameState
initialGameState = do
    coin <- newCoin screenSize
    pure { player : initialPlayer, coin }

update :: InputState -> GameState -> Eff _ GameState
update inputs gs@{ player, coin } = do
    let player' = updatePlayer inputs player
        
    coin' <- ifM (pure (pickupCoin player' coin)) (newCoin screenSize) (pure coin)
                
    pure (gs { player = player', coin = coin' })
    
pickupCoin :: Player -> Coin -> Boolean
pickupCoin p c = onCoin pts c
    where
        pts = getPlayerPoints p
        
        onCoin :: Array Point -> Coin -> Boolean
        onCoin [p1, p2, p3] c =
            let cc = circleContains (circle c.x c.y 16.0)
            in cc p1 || cc p2 || cc p3
        onCoin _ _ = false

    
updatePlayer :: InputState -> Player -> Player
updatePlayer inputs player =
    let
        rotateIf :: Boolean -> Number -> Player -> Player
        rotateIf keyDown d p = if keyDown then p { angle = p.angle + d } else p
        
        forwardIf :: Boolean -> Number -> Player -> Player
        forwardIf keyDown d p =
            let dx = (cos p.angle) * d
                dy = (sin p.angle) * d
            in if keyDown then p { vx = dx, vy = dy } else p { vx = 0.0, vy = 0.0 }
            
        move :: Player -> Player
        move p = p { x = p.x + p.vx, y = p.y + p.vy }
        
        wrapEdges :: Player -> Player
        wrapEdges p = p { x = wrap 0.0 screenSize.width p.x,
                          y = wrap 0.0 screenSize.height p.y }
            where
                wrap :: Number -> Number -> Number -> Number
                wrap l h v
                    | v < l     = wrap l h (v + h - l)
                    | v > h     = wrap l h (v - h + l)
                    | otherwise = v
        
        rotSpeed = 0.1
        moveSpeed = 10.0
        
    in  (   rotateIf inputs.left (-rotSpeed)
        >>> rotateIf inputs.right (rotSpeed)
        >>> forwardIf inputs.up moveSpeed
        >>> move
        >>> wrapEdges ) player

clearCanvas :: forall e. C.Context2D -> Eff ( canvas :: C.CANVAS | e ) Unit
clearCanvas ctx = void do
    C.setFillStyle "rgba(50, 50, 50, 0.3)" ctx
    C.fillRect ctx { x : 0.0, y : 0.0, w : screenSize.width, h : screenSize.height }

drawPlayer :: forall e. C.Context2D -> Player -> Eff ( canvas :: C.CANVAS | e ) Unit
drawPlayer ctx player = void do
    let pts = getPlayerPoints player
    drawPlayer' pts
    
    where
        drawPlayer' :: Array Point -> Eff ( canvas :: C.CANVAS | e ) Unit
        drawPlayer' [p1, p2, p3] = void do
            C.setFillStyle "#F00" ctx
            C.fillPath ctx $ do
                C.moveTo ctx p1.x p1.y
                C.lineTo ctx p2.x p2.y
                C.lineTo ctx p3.x p3.y
                C.closePath ctx
        drawPlayer' _ = pure unit
    

drawCoin :: forall e. C.Context2D -> Coin -> Eff ( canvas :: C.CANVAS | e ) Unit
drawCoin ctx { x, y } = void do
    C.setFillStyle "#BB2" ctx
    C.beginPath ctx
    C.arc ctx { x, y, r : 16.0, start : 0.0, end : 2.0 * pi }
    C.closePath ctx
    C.fill ctx

main :: Eff _ Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "gamecanvas"
    C.setCanvasDimensions screenSize canvas

    ctx <- C.getContext2D canvas

    iState <- initialGameState
    stateRef <- newRef iState
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
            nextState <- update inputs currState

            clearCanvas ctx
            drawPlayer ctx nextState.player
            drawCoin ctx nextState.coin

            writeRef stateRef nextState
            requestAnimationFrame loop

    requestAnimationFrame loop
