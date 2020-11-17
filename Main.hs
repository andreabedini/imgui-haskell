module Main where

import Control.Monad (when, unless)
import Data.IORef
import qualified ImGui.Internal.Bindings as ImGui
import qualified ImGui.Internal.GLFW as ImGui
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLEW as GLEW


simpleErrorCallback e s =
  putStrLn $ unwords [show e, show s]


withWindow f = do
  Just window <- GLFW.createWindow 1200 800 "CIMGUI" Nothing Nothing
  GLFW.makeContextCurrent (Just window)

  f window

  GLFW.setErrorCallback $ Just simpleErrorCallback
  GLFW.destroyWindow window

  GLFW.terminate


loop window do_stuff = do
  r <- GLFW.windowShouldClose window
  unless r $ do
    do_stuff
    loop window do_stuff


main :: IO ()
main = do
  True <- GLFW.init

  GLFW.setErrorCallback $ Just simpleErrorCallback
  let glsl_version = "#version 150"
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)

  withWindow $ \window -> do
    -- init
    GLEW.c_glewInit
    ctx <- ImGui.createContext Nothing
    io <- ImGui.getIO
    ImGui.glfwInitForOpenGL window True
    ImGui.openGL3Init glsl_version

    --
    (width, height) <- GLFW.getWindowSize window
    let pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
    GL.viewport GL.$= (pos, size)

    showWindow <- newIORef True

    loop window $ do
      GLFW.pollEvents

      ImGui.openGL3NewFrame
      ImGui.glfwNewFrame
      ImGui.newFrame

      sw <- readIORef showWindow
      when sw $ do
        (isVisible, isOpen) <- ImGui.begin "Test" ImGui.ImGuiWindowFlags_None
        when isVisible $ do
          ImGui.text "Test"
          b <- ImGui.button "Test" (ImGui.makeImVec2 0 0)
          when b $ print "click!"

          fr <- ImGui.framerate io
          ImGui.text $ "Framerate " ++ show fr

        unless isVisible $ print "holding off"
        writeIORef showWindow isOpen
        ImGui.end

      -- Rendering
      ImGui.render

      (width, height) <- GLFW.getFramebufferSize window
      let pos   = GL.Position 0 0
          size  = GL.Size (fromIntegral width) (fromIntegral height)
      GL.viewport GL.$= (pos, size)
      GL.clearColor GL.$= GL.Color4 0.45 0.55 0.60 1
      GL.clear [GL.ColorBuffer]

      ImGui.getDrawData >>= ImGui.openGL3RenderDrawData

      GLFW.swapBuffers window

    ImGui.openGL3Shutdown
    ImGui.glfwShutdown
    ImGui.destroyContext ctx

