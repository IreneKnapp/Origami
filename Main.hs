module Main (main) where

import Control.Concurrent
import qualified Data.Array as A
import Data.StateVar
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import System.Exit


data ProgramState =
  ProgramState {
      programStatePolygonModel :: MVar PolygonModel
    }


data Point = Point GL.GLdouble GL.GLdouble GL.GLdouble


data PolygonModel =
  PolygonModel {
      polygonModelPoints :: A.Array Int Point,
      polygonModelFaces :: A.Array Int [Int]
    }


flatSquarePolygonModel :: PolygonModel
flatSquarePolygonModel =
  PolygonModel {
      polygonModelPoints =
        A.array (0, 7)
                [(0, Point ( 0.00) ( 0.50) ( 0.00)),
                 (1, Point ( 0.01) ( 0.50) ( 0.01)),
                 (2, Point (-0.50) (-0.01) ( 0.01)),
                 (3, Point (-0.50) ( 0.00) ( 0.00)),
                 (4, Point ( 0.50) ( 0.50) ( 0.01)),
                 (5, Point ( 0.50) (-0.50) ( 0.01)),
                 (6, Point (-0.50) (-0.50) ( 0.01)),
                 (7, Point (-0.50) ( 0.00) ( 0.01))],
      polygonModelFaces =
        A.array (0, 1)
                [(0, [0, 1, 2, 3]),
                 (1, [1, 4, 5, 6, 7, 2])]
    }


main :: IO ()
main = do
  polygonModelMVar <- newMVar flatSquarePolygonModel
  let state = ProgramState {
                  programStatePolygonModel = polygonModelMVar
                }
  GLFW.initialize
  GLFW.openWindow (GL.Size 480 480)
                  [GLFW.DisplayRGBBits 8 8 8,
                   GLFW.DisplayAlphaBits 8,
                   GLFW.DisplayDepthBits 8,
                   GLFW.DisplayStencilBits 0]
                  GLFW.Window
  GLFW.windowTitle $= "Origami"
  setupGraphics
  GLFW.disableSpecial GLFW.AutoPollEvent
  GLFW.keyCallback $= \key buttonState -> do
    GLFW.terminate
    exitSuccess
  GLFW.windowRefreshCallback $= redraw state
  redraw state
  let loop = do
        GLFW.waitEvents
        loop
  loop


setupGraphics :: IO ()
setupGraphics = do
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 175.0 1.0 (-1.0) 1.0
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  GL.frontFace $= GL.CW
  projectionMatrix <- get $ GL.matrix (Just GL.Projection)
    :: IO (GL.GLmatrix GL.GLdouble)
  modelMatrix <- get $ GL.matrix (Just $ GL.Modelview 0)
    :: IO (GL.GLmatrix GL.GLdouble)
  viewport <- get GL.viewport
  putStrLn $ "Viewport " ++ (show viewport)
  center <- GL.project (GL.Vertex3 0.0 0.0 0.0)
                       modelMatrix projectionMatrix viewport
  corner <- GL.project (GL.Vertex3 (-0.5) (-0.5) 0.0)
                       modelMatrix projectionMatrix viewport
  putStrLn $ (show center) ++ " " ++ (show corner)
  GL.depthFunc $= Just GL.Less
  GL.clearDepth $= 1.0
  GL.clearColor $= GL.Color4 1.0 1.0 1.0 0.0
  GL.shadeModel $= GL.Smooth


redraw :: ProgramState -> IO ()
redraw state = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  model <- readMVar $ programStatePolygonModel state
  drawPolygonModel model


drawPolygonModel :: PolygonModel -> IO ()
drawPolygonModel model = do
  GL.polygonMode $= (GL.Fill, GL.Fill)
  let loop [] = return ()
      loop (face : rest) = do
        let points = map (\index -> polygonModelPoints model A.! index) face
        GL.cullFace $= Just GL.Back
        GL.color $ (GL.Color3 1.0 0.9 0.9 :: GL.Color3 GL.GLdouble)
        GL.renderPrimitive GL.Polygon $ do
          mapM (\(Point x y z) -> GL.vertex $ GL.Vertex3 x y z) points
        GL.cullFace $= Just GL.Front
        GL.color $ (GL.Color3 1.0 0.95 0.95 :: GL.Color3 GL.GLdouble)
        GL.renderPrimitive GL.Polygon $ do
          mapM (\(Point x y z) -> GL.vertex $ GL.Vertex3 x y z) points
        loop rest
  loop $ A.elems $ polygonModelFaces model
  GL.flush
  GLFW.swapBuffers
