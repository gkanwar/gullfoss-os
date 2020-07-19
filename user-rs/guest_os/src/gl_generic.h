#ifndef GL_GENERIC_H
#define GL_GENERIC_H

/**
 * OpenGL is super irritating, especially when we're dealing with OSX vs Linux.
 * Include the appropriate set of things depending on arch here, including GLFW.
 */
#ifdef __APPLE__
#define GL_SILENCE_DEPRECATION
#else
#include <glad/glad.h>
#endif
#define GLFW_INCLUDE_GLCOREARB
#include <GLFW/glfw3.h>

#endif
