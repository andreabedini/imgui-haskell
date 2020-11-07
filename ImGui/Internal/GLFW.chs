{-# LANGUAGE ForeignFunctionInterface #-}

#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS
#include <cimgui.h>
#include <cimgui_impl.h>

module ImGui.Internal.GLFW where

import GHC.Generics
import Foreign.Ptr

import Graphics.UI.GLFW (Window)
import Bindings.GLFW (C'GLFWwindow)

{# pointer *GLFWwindow as GLFWwindowPtr -> C'GLFWwindow #}

{# fun ImGui_ImplGlfw_InitForOpenGL as glfwInitForOpenGL { unWindow `Window', `Bool' } -> `()' #}

{# fun ImGui_ImplGlfw_Shutdown as glfwShutdown { } -> `()' #}

{# fun ImGui_ImplGlfw_NewFrame as glfwNewFrame { } -> `()' #}

{# fun ImGui_ImplGlfw_MouseButtonCallback as glfwMouseButtonCallback { unWindow `Window', `Int', `Int', `Int' } -> `()' #}

{# fun ImGui_ImplGlfw_ScrollCallback as glfwScrollCallback { unWindow `Window', `Double', `Double' } -> `()' #}

{# fun ImGui_ImplGlfw_KeyCallback as glfwKeyCallback { unWindow `Window', `Int', `Int', `Int', `Int' } -> `()' #}

{# fun ImGui_ImplGlfw_CharCallback as glfwCharCallback { unWindow `Window', `Int' } -> `()' #}


unWindow :: Window -> Ptr C'GLFWwindow
unWindow = unK1 . unM1 . unM1 . unM1 . GHC.Generics.from
