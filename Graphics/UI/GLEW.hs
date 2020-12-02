{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.UI.GLEW where

-- ImGui impl functions
foreign import ccall unsafe "glew.h glewInit"
     c_glewInit :: IO Bool
