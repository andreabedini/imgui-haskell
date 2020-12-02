{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include <cimgui.h>
#include <cimgui_impl.h>

{# context lib="cimgui" #}

module ImGui.Internal.Bindings where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils


-------------------------------------------------------------------------------
-- Forward declarations and basic types
-------------------------------------------------------------------------------

{# pointer *ImDrawChannel #}
{# pointer *ImDrawCmd #}
{# pointer *ImDrawData #}
{# pointer *ImDrawList #}
{# pointer *ImDrawListSharedData #}
{# pointer *ImDrawListSplitter #}
{# pointer *ImDrawVert #}
{# pointer *ImFont #}
{# pointer *ImFontAtlas #}
{# pointer *ImFontConfig #}
{# pointer *ImFontGlyph #}
{# pointer *ImFontGlyphRangesBuilder #}
{# pointer *ImColor #}
{# pointer *ImGuiContext #}
{# pointer *ImGuiIO #}
{# pointer *ImGuiInputTextCallbackData #}
{# pointer *ImGuiListClipper #}
{# pointer *ImGuiOnceUponAFrame #}
{# pointer *ImGuiPayload #}
{# pointer *ImGuiSizeCallbackData #}
{# pointer *ImGuiStorage #}
{# pointer *ImGuiStyle #}
{# pointer *ImGuiTextBuffer #}
{# pointer *ImGuiTextFilter #}


{# enum ImGuiCol_ as ImGuiCol {} deriving (Show, Eq) #}
{# enum ImGuiCond_ as ImGuiCond {} deriving (Show, Eq) #}
{# enum ImGuiDataType_ as ImGuiDataType {} deriving (Show, Eq) #}
{# enum ImGuiDir_ as ImGuiDir {} deriving (Show, Eq) #}
{# enum ImGuiKey_ as ImGuiKey {} deriving (Show, Eq) #}
{# enum ImGuiNavInput_ as ImGuiNavInput {} deriving (Show, Eq) #}
{# enum ImGuiMouseButton_ as ImGuiMouseButton {} deriving (Show, Eq) #}
{# enum ImGuiMouseCursor_ as ImGuiMouseCursor {} deriving (Show, Eq) #}
{# enum ImGuiStyleVar_ as ImGuiStyleVar {} deriving (Show, Eq) #}
{# enum ImDrawCornerFlags_ as ImDrawCornerFlags {} deriving (Show, Eq) #}
{# enum ImDrawListFlags_ as ImDrawListFlags {} deriving (Show, Eq) #}
{# enum ImFontAtlasFlags_ as ImFontAtlasFlags {} deriving (Show, Eq) #}
{# enum ImGuiBackendFlags_ as ImGuiBackendFlags {} deriving (Show, Eq) #}
{# enum ImGuiButtonFlags_ as ImGuiButtonFlags {} deriving (Show, Eq) #}
{# enum ImGuiColorEditFlags_ as ImGuiColorEditFlags {} deriving (Show, Eq) #}
{# enum ImGuiConfigFlags_ as ImGuiConfigFlags {} deriving (Show, Eq) #}
{# enum ImGuiComboFlags_ as ImGuiComboFlags {} deriving (Show, Eq) #}
{# enum ImGuiDragDropFlags_ as ImGuiDragDropFlags {} deriving (Show, Eq) #}
{# enum ImGuiFocusedFlags_ as ImGuiFocusedFlags {} deriving (Show, Eq) #}
{# enum ImGuiHoveredFlags_ as ImGuiHoveredFlags {} deriving (Show, Eq) #}
{# enum ImGuiInputTextFlags_ as ImGuiInputTextFlags {} deriving (Show, Eq) #}
{# enum ImGuiKeyModFlags_ as ImGuiKeyModFlags {} deriving (Show, Eq) #}
{# enum ImGuiPopupFlags_ as ImGuiPopupFlags {} deriving (Show, Eq) #}
{# enum ImGuiSelectableFlags_ as ImGuiSelectableFlags {} deriving (Show, Eq) #}
{# enum ImGuiSliderFlags_ as ImGuiSliderFlags {} deriving (Show, Eq) #}
{# enum ImGuiTabBarFlags_ as ImGuiTabBarFlags {} deriving (Show, Eq) #}
{# enum ImGuiTabItemFlags_ as ImGuiTabItemFlags {} deriving (Show, Eq) #}
{# enum ImGuiTreeNodeFlags_ as ImGuiTreeNodeFlags {} deriving (Show, Eq) #}
{# enum ImGuiWindowFlags_ as ImGuiWindowFlags {} deriving (Show, Eq) #}

{# pointer *ImGuiID #}
{# pointer *ImGuiInputTextCallback #}
{# pointer *ImGuiSizeCallback #}

{# pointer *ImVec2 #}

imVec2_x :: ImVec2 -> IO Float
imVec2_x = fmap realToFrac . {# get ImVec2.x #}

imVec2_y :: ImVec2 -> IO Float
imVec2_y = fmap realToFrac . {# get ImVec2.y #}

{# pointer *ImVec4 #}

-------------------------------------------------------------------------------
-- ImGui: Dear ImGui end-user API
-------------------------------------------------------------------------------

-- Context creation and access
-- Each context create its own ImFontAtlas by default. You may instance one
-- yourself and pass it to CreateContext() to share a font atlas between imgui
-- contexts.
-- None of those functions is reliant on the current context.

{# fun igCreateContext as createContext { maybeFontAtlas `Maybe ImFontAtlas' } -> `ImGuiContext' #}
{# fun igDestroyContext as destroyContext { `ImGuiContext' } -> `()' #}
{# fun igGetCurrentContext as getCurrentContext { } -> `ImGuiContext' #}
{# fun igSetCurrentContext as setCurrentContext { `ImGuiContext' } -> `()' #}

-- Main

{-|
  access the IO structure (mouse/keyboard/gamepad inputs, time, various
  configuration options/flags)
-}
{# fun igGetIO as getIO { } -> `ImGuiIO' #}

{-|
  access the Style structure (colors, sizes). Always use PushStyleCol(),
  PushStyleVar() to modify style mid-frame!
-}
{# fun igGetStyle as getStyle { } -> `ImGuiStyle' #}

{-|
  start a new Dear ImGui frame, you can submit any command from this point
  until Render()/EndFrame().
-}
{# fun igNewFrame as newFrame { } -> `()' #}

{-|
  ends the Dear ImGui frame. automatically called by Render(). If you don't
  need to render data (skipping rendering) you may call EndFrame() without
  Render()... but you'll have wasted CPU already! If you don't need to
  render, better to not create any windows and not call NewFrame() at all!
-}
{# fun igEndFrame as endFrame { } -> `()' #}

{-|
  ends the Dear ImGui frame, finalize the draw data. You can get call
  GetDrawData() to obtain it and run your rendering function (up to v1.60,
  this used to call io.RenderDrawListsFn(). Nowadays, we allow and prefer
  calling your render function yourself.)
-}
{# fun igRender as render { } -> `()' #}

{-|
  valid after Render() and until the next call to NewFrame(). this is what
  you have to render.
-}
{# fun igGetDrawData as getDrawData { } -> `ImDrawData' #}

-- Demo, Debug, Information

{-|
  create Demo window (previously called ShowTestWindow). demonstrate most
  ImGui features. call this to learn about the library! try to make it
  always available in your application!
-}
{# fun igShowDemoWindow as showDemoWindow { withOne- `Bool' peekBool* } -> `()' #}

{-|
  create About window. display Dear ImGui version, credits and build/system
  information.
-}
{# fun igShowAboutWindow as showAboutWindow { withOne- `Bool' peekBool* } -> `()' #}

{-|
  create Debug/Metrics window. display Dear ImGui internals: draw commands
  (with individual draw calls and vertices), window list, basic internal
  state, etc.
-}
{# fun igShowMetricsWindow as showMetricsWindow { withOne- `Bool' peekBool* } -> `()' #}

{-|
  add style editor block (not a window). you can pass in a reference
  ImGuiStyle structure to com pare to, revert to and save to (else it uses
  the default style)
-}
{# fun igShowStyleEditor as showStyleEditor { `ImGuiStyle' } -> `()' #}

{-|
   add style selector block (not a window), essentially a combo listing the
   default styles.
-}
{# fun igShowStyleSelector as showStyleSelector { `String' } -> `Bool' #}

{-|
  add font selector block (not a window), essentially a combo listing the
  loaded fonts.
-}
{# fun igShowFontSelector as showFontSelector { `String' } -> `()' #}

{-|
  add basic help/info block (not a window): how to manipulate ImGui as a
  end-user (mouse/keyboard controls).
-}
{# fun igShowUserGuide as showUserGuide { } -> `()' #}

{-|
  get the compiled version string e.g. "1.23" (essentially the compiled
  value for IMGUI_VERSION)
-}
{# fun igGetVersion as getVersion { } -> `String' #}

-- Styles

{-| new, recommended style (default) -}
{# fun igStyleColorsDark as styleColorsDark { `ImGuiStyle' } -> `()' #}

{-| classic imgui style -}
{# fun igStyleColorsClassic as styleColorsClassic { `ImGuiStyle' } -> `()' #}

{-| best used with borders and a custom, thicker font -}
{# fun igStyleColorsLight as styleColorsLight { `ImGuiStyle' } -> `()' #}

-- Windows

{# fun igBegin as begin { `String', withOne- `Bool' peekBool*, `ImGuiWindowFlags' } -> `Bool' #}
{# fun igEnd as end { } -> `()' #}

-- Child Windows

{# fun igBeginChildStr as beginChildStr { `String', %`ImVec2', `Bool', `ImGuiWindowFlags' } -> `()' #}
{# fun igBeginChildID as beginChildID { %`ImGuiID', %`ImVec2', `Bool', `ImGuiWindowFlags' } -> `()' #}
{# fun igEndChild as endChild { } -> `()' #}

-- Windows Utilities

{# fun igIsWindowAppearing as isWindowAppearing { } -> `Bool' #}
{# fun igIsWindowCollapsed as isWindowCollapsed { } -> `Bool' #}
{# fun igIsWindowFocused as isWindowFocused { `ImGuiFocusedFlags' } -> `Bool' #}
{# fun igIsWindowHovered as isWindowHovered { `ImGuiHoveredFlags' } -> `Bool' #}

{# fun igGetWindowDrawList as getWindowDrawList { } -> `ImDrawList' #}
{# fun igGetWindowPos as getWindowPos { kazoom- `ImVec2' peek* } -> `()' #}

framerate :: ImGuiIO -> IO Double
framerate = fmap realToFrac . {# get ImGuiIO.Framerate #}

-- ImGui impl functions
{# fun ImGui_ImplOpenGL3_Init as openGL3Init { `String' } -> `Bool' #}
{# fun ImGui_ImplOpenGL3_NewFrame as openGL3NewFrame { } -> `()' #}
{# fun ImGui_ImplOpenGL3_RenderDrawData as openGL3RenderDrawData { `ImDrawData' } -> `()' #}
{# fun ImGui_ImplOpenGL3_Shutdown as openGL3Shutdown { } -> `()' #}


{# fun igButton as button {`String', %`ImVec2' } -> `Bool'#}
{# fun igText as text { `String' } -> `()' #}
{# fun igPopStyleColor as popStyleColor { `Int' } -> `()' #}
{# fun igInputText as inputText  { `String', `String' peekCString*, `Int', cFromEnum `ImGuiInputTextFlags', id `FunPtr (Ptr () -> IO CInt)' , `Ptr ()'  } -> `Bool' #}

{# fun igSliderAngle as sliderAngle { `String', alloca- `Float' peekReal*, `Float', `Float', `String', `ImGuiSliderFlags' } -> `Bool' #}

{# fun igSliderInt  as sliderInt  { `String', alloca-  `Int'   peekIntegral*,       `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}
{# fun igSliderInt2 as sliderInt2 { `String', alloca2- `[Int]' peekIntegralArray2*, `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}
{# fun igSliderInt3 as sliderInt3 { `String', alloca3- `[Int]' peekIntegralArray3*, `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}
{# fun igSliderInt4 as sliderInt4 { `String', alloca4- `[Int]' peekIntegralArray4*, `Int', `Int', `String', `ImGuiSliderFlags' } -> `Bool' #}

-- Creating structsA
{# fun ImVec2_ImVec2Nil as createImVec2 {} -> `ImVec2' #}
{# fun pure ImVec2_ImVec2Float as makeImVec2 {`Float', `Float'} -> `ImVec2'#}
--{# fun pure ImVec4_ImVec4Float as makeImVec4 {`Float', `Float', `Float', `Float'} -> `ImVec4'#}

-- Utility functions

kazoom :: (ImVec2 -> IO ()) -> IO ImVec2
kazoom f = do
  v <- createImVec2
  f v
  pure v

withOne :: (Storable a, Num a) => (Ptr a -> IO b) -> IO b
withOne = with 1

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
