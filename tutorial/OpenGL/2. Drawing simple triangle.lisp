#!/usr/bin/ol
(import (lib opengl))
(gl:run

   "2. Drawing simple triangle"

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1))

; draw
(lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_TRIANGLES)
      (glVertex2f -0.2 -0.2)
      (glVertex2f +0.2 -0.2)
      (glVertex2f -0.0 +0.3)
   (glEnd)))
