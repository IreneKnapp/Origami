module Main (main) where

import Data.Array
import Data.StateVar
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit


data Point = Point Double Double Double


data PolygonModel =
  PolygonModel {
      polygonModelPoints :: Array Int Point,
      polygonModelFaces :: Array Int [Int]
    }


flatSquarePolygonModel :: PolygonModel
flatSquarePolygonModel =
  PolygonModel {
      polygonModelPoints =
        array (0, 3)
              [(0, Point (-0.5) ( 0.5) ( 0.0)),
               (1, Point ( 0.5) ( 0.5) ( 0.0)),
               (2, Point ( 0.5) (-0.5) ( 0.0)),
               (3, Point (-0.5) (-0.5) ( 0.0))],
      polygonModelFaces =
        array (0, 0)
              [(0, [0, 1, 2, 3])]
    }


data ProgramState
  = ProgramStateRunning { }
  | ProgramStateDone


main :: IO ()
main = do
  GLFW.initialize
  GLFW.openWindow (GL.Size 480 480)
                  [GLFW.DisplayRGBBits 8 8 8,
                   GLFW.DisplayAlphaBits 8,
                   GLFW.DisplayDepthBits 8,
                   GLFW.DisplayStencilBits 0]
                  GLFW.Window
  GLFW.windowTitle $= "Origami"
  GLFW.keyCallback $= \key buttonState -> do
    GLFW.terminate
    exitSuccess
  let loop = do
        GLFW.waitEvents
        loop
  loop
