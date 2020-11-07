{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include <cimgui.h>
#include <cimgui_impl.h>

{# context lib="cimgui" #}

module ImGui.Internal.Bindings where

import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

-- Enums
{# enum ImGuiCol_ as ImGuiCol {} deriving (Show, Eq) #}
{# enum ImGuiColorEditFlags_ as ImGuiColorEditFlags {} deriving (Show, Eq) #}
{# enum ImGuiInputTextFlags_ as ImGuiInputTextFlags {} deriving (Show, Eq) #}
{# enum ImGuiSliderFlags_ as ImGuiSliderFlags {} deriving (Show, Eq) #}
{# enum ImGuiWindowFlags_ as ImGuiWindowFlags {} deriving (Show, Eq) #}

{# pointer *ImVec2 as ImVec2Ptr foreign newtype #}
{# pointer *ImVec4 as ImVec4Ptr foreign newtype #}
{# pointer *ImFontAtlas as ImFontAtlas  #}
{# pointer *ImDrawData as ImDrawData #}
{# pointer *ImGuiContext as ImGuiContext #}
{# pointer *ImGuiIO as ImGuiIO #}
{# pointer *ImGuiStyle as ImGuiStyle #}
{# pointer *ImGuiID as ImGuiID #}
{# pointer *ImGuiInputTextCallback as ImGuiInputTextCallback #}

framerate :: ImGuiIO -> IO Double
framerate = fmap realToFrac . {# get ImGuiIO.Framerate #}

-- ImGui impl functions
{# fun ImGui_ImplOpenGL3_Init as openGL3Init { `String' } -> `Bool' #}
{# fun ImGui_ImplOpenGL3_NewFrame as openGL3NewFrame { } -> `()' #}
{# fun ImGui_ImplOpenGL3_RenderDrawData as openGL3RenderDrawData { `ImDrawData' } -> `()' #}
{# fun ImGui_ImplOpenGL3_Shutdown as openGL3Shutdown { } -> `()' #}

-- Imgui functions
{# fun igCreateContext as createContext { maybeFontAtlas `Maybe ImFontAtlas' } -> `ImGuiContext' #}
{# fun igDestroyContext as destroyContext { `ImGuiContext' } -> `()' #}
{# fun igGetCurrentContext as getCurrentContext { } -> `ImGuiContext' #}
{# fun igSetCurrentContext as setCurrentContext { `ImGuiContext' } -> `()' #}
{# fun igGetIO as getIO { } -> `ImGuiIO' #}
{# fun igGetStyle as getStyle { } -> `ImGuiStyle' #}
{# fun igNewFrame as newFrame { } -> `()' #}
{# fun igEndFrame as endFrame { } -> `()' #}
{# fun igRender as render { } -> `()' #}
{# fun igGetDrawData as getDrawData { } -> `ImDrawData' #}
{# fun igShowDemoWindow as showDemoWindow { `Bool' } -> `()' #}
{# fun igShowAboutWindow as showAboutWindow { `Bool' } -> `()' #}
{# fun igShowMetricsWindow as showMetricsWindow { `Bool' } -> `()' #}
{# fun igShowStyleEditor as showStyleEditor { `ImGuiStyle' } -> `()' #}
{# fun igShowStyleSelector as showStyleSelector { `String' } -> `Bool' #}
{# fun igShowFontSelector as showFontSelector { `String' } -> `()' #}
{# fun igShowUserGuide as showUserGuide { } -> `()' #}
{# fun igGetVersion as getVersion { } -> `String' #}
{# fun igStyleColorsDark as styleColorsDark { `ImGuiStyle' } -> `()' #}
{# fun igStyleColorsClassic as styleColorsClassic { `ImGuiStyle' } -> `()' #}
{# fun igStyleColorsLight as styleColorsLight { `ImGuiStyle' } -> `()' #}

{# fun igBegin as begin { `String', withOne- `Bool' peekBool*, `ImGuiWindowFlags' } -> `Bool' #}
{# fun igEnd as end { } -> `()' #}

{# fun igBeginChildID as beginChildID { %`ImGuiID', %`ImVec2Ptr', `Bool', `ImGuiWindowFlags' } -> `()' #}
{# fun igEndChild as endChild { } -> `()' #}
{# fun igButton as button {`String', %`ImVec2Ptr'} -> `Bool'#}
{# fun igText as text { `String' } -> `()' #}
{# fun igPopStyleColor as popStyleColor { `Int' } -> `()' #}
{# fun igInputText as inputText  { `String', `String' peekCString*, `Int', cFromEnum `ImGuiInputTextFlags', id `FunPtr (Ptr () -> IO CInt)' , `Ptr ()'  } -> `Bool' #}

{# fun igSliderAngle as sliderAngle { `String', alloca- `Float' peekReal*, `Float', `Float', `String', `ImGuiSliderFlags' } -> `Bool' #}

{# fun igSliderInt  as sliderInt  { `String', alloca-  `Int'   peekIntegral*,       `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}
{# fun igSliderInt2 as sliderInt2 { `String', alloca2- `[Int]' peekIntegralArray2*, `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}
{# fun igSliderInt3 as sliderInt3 { `String', alloca3- `[Int]' peekIntegralArray3*, `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}
{# fun igSliderInt4 as sliderInt4 { `String', alloca4- `[Int]' peekIntegralArray4*, `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}

-- Creating structs
{# fun pure ImVec2_ImVec2Float as makeImVec2 {`Float', `Float'} -> `ImVec2Ptr'#}
{# fun pure ImVec4_ImVec4Float as makeImVec4 {`Float', `Float', `Float', `Float'} -> `ImVec4Ptr'#}

-- Utility functions

withOne f = with 1 f

maybeFontAtlas :: Maybe ImFontAtlas -> ImFontAtlas
maybeFontAtlas = maybe nullPtr id

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

peekIntegral :: (Integral a, Storable a, Integral b) => Ptr a -> IO b
peekIntegral = fmap fromIntegral . peek

peekReal :: (Real a, Storable a, Fractional b) => Ptr a -> IO b
peekReal = fmap realToFrac . peek

peekBool :: Ptr CInt -> IO Bool
peekBool = fmap toBool . peek

alloca2 :: Storable a => (Ptr a -> IO b) -> IO b
alloca2 = allocaArray 2

alloca3 :: Storable a => (Ptr a -> IO b) -> IO b
alloca3 = allocaArray 3

alloca4 :: Storable a => (Ptr a -> IO b) -> IO b
alloca4 = allocaArray 4

peekIntegralArray :: (Integral a, Storable a, Num b) => Int -> Ptr a -> IO [b]
peekIntegralArray n = fmap (map fromIntegral) . peekArray n

peekIntegralArray2 :: (Integral a, Storable a, Num b) => Ptr a -> IO [b]
peekIntegralArray2 = peekIntegralArray 2

peekIntegralArray3 :: (Integral a, Storable a, Num b) => Ptr a -> IO [b]
peekIntegralArray3 = peekIntegralArray 3

peekIntegralArray4 :: (Integral a, Storable a, Num b) => Ptr a -> IO [b]
peekIntegralArray4 = peekIntegralArray 4

peekRealArray :: (Real a, Storable a, Fractional b) => Int -> Ptr a -> IO [b]
peekRealArray n = fmap (map realToFrac) . peekArray n

peekRealArray2 :: (Real a, Storable a, Fractional b) => Ptr a -> IO [b]
peekRealArray2 = peekRealArray 2

peekRealArray3 :: (Real a, Storable a, Fractional b) => Ptr a -> IO [b]
peekRealArray3 = peekRealArray 3

peekRealArray4 :: (Real a, Storable a, Fractional b) => Ptr a -> IO [b]
peekRealArray4 = peekRealArray 4
