# **Pong**: yet another Haskell retrogame

This version of Pong is based, in large part, on Sven Panitz's tutorial on HOpenGL. Essentially,
I'm tearing it apart and putting it back together to see how it works.

That said, I've made a few improvements:

* Code is broken out into modules, making everything easier to improve
* All functions are explicitly typed, reducing mystery-meat type inference errors.
* Keyboard bindings are cleaned up. Keys move when pressed and stop when lifted. Different keys
are used for up and down directions.

And a few improvements have yet to be made:

* Need to draw a dashed line down the middle
* Scores should be displayed.
* Ball should speed up after a few hits
* Maybe a little color, some sounds...
* The speed of the game needs to become independent of the speed of the computer. Ideally,
we should cap the frame rate using GLUT's timer callback and elapsedTime


##To play:

The keys 'Q' and 'A' move the left paddle. <br>
The keys 'O' and 'L' move the right paddle. <br>
Space key serves the ball. <br>
Escape key exits the game. <br>

##Some ideas for the future:

* Create a fully 3D ball and paddles, give light, and maybe a little depth on the Z axis.
* Make the *game itself* 3D. Perhaps able to rotate? (Although that might render it a bit evil...)
