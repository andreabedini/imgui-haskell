{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.UI.GLEW where

import Foreign.C

-- ImGui impl functions
foreign import ccall unsafe "glew.h glewInit"
     c_glewInit :: IO Bool
