# Sorcery SDL2 - A Common Lisp 2D Game

![Screenshot](assets/sorcery-screenshot.png)

This is a simple Common Lisp 2D Game.

Release the 8 apprentices before the end ot time.

Use items to open doors or kill monsters.

Keyboard controls :
- 'q' : quit game
- key up : up
- key left : left
- key right : right
- space : fire

At the end, go to the sanctuary to join the apprentices.


## Dependencies

* [SDL2](https://www.libsdl.org/) and [SDL2_image](https://www.libsdl.org/projects/SDL_image/)
```sh
sudo apt install libsdl2-dev libsdl2-image-dev
```

### Common Lisp

* [SBCL](https://www.sbcl.org/)
* [Quicklisp](https://www.quicklisp.org/beta/)

* [sdl2-engine](https://github.com/fferrere/sdl2-engine)
```sh
git clone https://github.com/fferrere/sdl2-engine
```
* [quadtree](https://github.com/fferrere/quadtree)
```sh
git clone https://github.com/fferrere/quadtree
```
* [cl-entity-system](https://github.com/fferrere/cl-entity-system)
```sh
git clone https://github.com/fferrere/cl-entity-system
```

## Start

```lisp
(ql:quickload "sorcery-sdl2")
(sorcery-sdl2:start)
```

## Author & licence
* Author : Frédéric Ferrère
* Licence : MIT
