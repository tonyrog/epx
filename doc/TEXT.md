# Text, glyphs and transforms

epx has two ways to draw text:

* **efnt fonts** (`epx:draw_string/4`, `epx_gc:set_font/1`): pre-rendered
  glyph bitmaps. They are fast and drawn untransformed. Every size of a
  TrueType font is available through the font cache (see README,
  *Fonts*).
* **TrueType outlines** (`epx:sft_*`, `epx_text`): the glyphs are
  rendered from the outline with libschrift, so they can be rotated,
  scaled, sheared, put along a path, and drawn with sub pixel
  positions.

This document is about the second way.

## Glyph metrics

A font is opened at a size in **pixels per em**:
`Sft = epx_text:open("Arial", 40)` (or `epx:sft_fileopen(File, 40)`).
Positions are in pixels with **y downwards**.

```
              advance_width
          |<------------------->|
          .                     .
          .   +---------+  <- box top  (By, negative)
          .   |  ####   |       .
          .   | #    #  |       .
          .   | #    #  |       .
          .   |  #####  |       .
  pen ->  o---|------#--|-------o----  baseline (y = 0)
  (0,0)   .   |  ####   |       .      next pen point
          .   +---------+  <- box bottom (By + H)
          .   ^
          .   Bx
```

* **Pen point:** the origin on the baseline where the glyph starts. The
  next glyph starts `advance_width` further along (plus kerning).
* **Glyph box:** `epx:sft_glyph_box(Sft, Glyph) -> {Bx, By, W, H}` is the
  rectangle the rendered glyph image covers, relative to the pen point.
  `By` is negative when the glyph rises above the baseline. Use `Bx`,
  not `left_side_bearing`, to place glyph images; the two can differ by
  a pixel.
* `epx:sft_glyph_info/2` gives `#gmetrics{advance_width, ...}`, and
  `epx:sft_glyph_kerning(Sft, Left, Right)` gives the kerning adjustment.
* `epx:sft_info(Sft, ascent | descent | line_gap)`: the descent is
  negative.

## Three coordinate spaces

| space | origin, axes | used for |
|---|---|---|
| **pen space** | the pen point of one glyph, x right, y down | the glyph outline |
| **user space** | what you draw with on the pixmap | positions, paths |
| **device space** | the pixmap pixels | the result |

The glyph transform `T` maps pen space to user space. The pixmap's own
transform (its ltm, set with `epx:pixmap_ltm_translate/rotate/scale`)
maps user space to device space. That gives the global/local split:

```
device = Pixmap.ltm  *  T  *  pen
         (global)      (local, per glyph)
```

`epx:sft_glyph_draw(Sft, Gc, Glyph, T, Pixmap)` draws one glyph that
way, blended with the gc foreground color and fader. Remember that the
alpha of the text color is added to the glyph coverage, so use alpha 0:
`{0,R,G,B}`. Sub pixel positions are kept, so text can move smoothly.
`T = undefined` is the identity, which puts the pen point at (0,0).

`epx:sft_glyph_render_to/7` is the older call. It places the top left
corner of the transformed glyph box at an integer (X,Y), so the pen
point is lost when the glyph is rotated. It is kept for compatibility,
and it ignores the pixmap ltm.

## Building the transform

`epx_t2d` composes like a canvas: each call is done *before* the
transforms already in the matrix (`T * Op`). Read a chain from the last
operation to the first, as the steps a pen space point goes through:

```erlang
T0 = epx_t2d:identity(),
T1 = epx_t2d:translate(T0, X, Y),       %% 3. move to (X,Y)
T2 = epx_t2d:rotate_rad(T1, A),         %% 2. rotate around the origin
T  = epx_t2d:translate(T2, -Ax, -Ay).   %% 1. move the anchor to the origin
%% T = Translate(X,Y) * Rotate(A) * Translate(-Ax,-Ay)
```

`epx_t2d:compose(A, B)` is `B * A`: first A, then B.

A positive angle turns from +x towards +y. Since y points down, that is
clockwise on the screen.

### The anchor

A rotation or scale happens around the origin of pen space, which is the
pen point. To turn a glyph around another point, translate that point
to the origin first. That point is the **anchor**:

| anchor | pen space point | natural for |
|---|---|---|
| `pen` | (0, 0) | strings: glyphs follow the baseline |
| `baseline_center` | (advance/2, 0) | text on a path: the glyph sits on the curve |
| `center` | center of the glyph box | spinning a single glyph in place |
| `{X,Y}` | any point | everything else |

```erlang
%% T = Translate(X,Y) * Rotate * Scale * Translate(-Anchor)
T = epx_text:glyph_transform(Sft, Glyph,
        [{x,100},{y,80},{anchor,center},{rotate,math:pi()/4},{scale,2}]),
epx:sft_glyph_draw(Sft, Gc, Glyph, T, Pixmap).
%% or in one call
epx_text:draw_glyph(Pixmap, Gc, Sft, $g, [{x,100},{y,80},{anchor,center},...]).
```

## Strings

```erlang
epx_text:draw_string(Pixmap, Gc, Sft, X, Y, "Hello")          %% -> pen end {X1,Y1}
epx_text:draw_string(Pixmap, Gc, Sft, X, Y, "Hello",
                     [{rotate, A}, {align, center}, {kerning, true}, {spacing, 1}])
epx_text:advance(Sft, "Hello")                                %% width with kerning
epx_text:layout(Sft, "Hello", Opts)                           %% one map per glyph
```

The string has its own transform around its baseline origin (X,Y). Each
glyph gets `StringT * Translate(Pen, 0)`, where `Pen` is its position
along the string.

## Glyphs placed by a function

`layout/3` puts the glyphs on a straight baseline: the pen of glyph i is
at x = `pen`. `draw_glyphs/6` lets a function map that baseline position
to a transform, which covers any placement:

```erlang
epx_text:draw_glyphs(Pixmap, Gc, Sft, "wave",
    fun(#{ glyph := G, pen := Pen, advance := Adv, width := Width,
           index := I, char := C, box := Box }) ->
            X = 50 + Pen + Adv/2,
            Y = 100 + 10*math:sin(Pen/20),
            epx_text:glyph_transform(Sft, G, [{x,X},{y,Y},
                                              {anchor,baseline_center},
                                              {rotate,0.3*math:cos(Pen/20)}])
            %% or return skip to leave the glyph out
    end, []).
```

## Text along a path

A path is a function from T in [0,1] to a point: `fun(T) -> {X,Y}`.

```erlang
Circle = fun(T) -> A = 2*math:pi()*T,
                   {200 + 100*math:cos(A), 200 + 100*math:sin(A)} end,
epx_text:draw_path(Pixmap, Gc, Sft, "Around and around", Circle,
                   [{align, center}, {baseline, 4}])
```

How it works:

1. The path is sampled (`{samples, N}`, 256) into a table of
   `{S, X, Y}`, where S is the distance along the path
   (`epx_text:path_table/2`). The glyphs are placed by **distance**, not
   by T, so the spacing is even even when the function is not.
2. The string is laid out on a straight baseline. Glyph i has its middle
   at the distance `S = Start + Pen + Advance/2`.
3. The glyph's `baseline_center` anchor is put at the path point for S
   (`path_point/2`). The glyph is turned along the chord from S - Adv/2
   to S + Adv/2, which follows the curve over the whole glyph and gives
   a calmer result than the tangent at a single point:

   ```
   T = Translate(path(S)) * Rotate(angle) * Translate(-Adv/2, -Dy)
   ```

Options:
* `{align, left|center|right}`: where on the path the text goes.
* `{offset, S}`: extra distance along the path. Animate this to make the
  text run.
* `{baseline, Dy | center}`: the baseline Dy pixels above the path.
  `center` runs the path through the middle of the text.
* `{kerning, B}`, `{spacing, Px}`.

Glyphs whose middle falls outside the path are not drawn. The return
value is the number of glyphs drawn.

Text is put on the left side of the direction of travel, which is
"above" for a path from left to right. Reverse the path (`fun(T) ->
F(1-T) end`) to put the text on the other side.

## Global transform

The pixmap ltm applies to everything drawn with `sft_glyph_draw`, so a
whole page of text can be scaled, rotated or scrolled without changing
the text code:

```erlang
epx:pixmap_ltm_translate(Pixmap, 60, 400),
epx:pixmap_ltm_rotate(Pixmap, -0.2),
epx_text:draw_string(Pixmap, Gc, Sft, 0, 0, "rotated page"),
epx:pixmap_ltm_reset(Pixmap).
```

epxw sets the ltm to the view transform while it calls `draw/3`, so
TrueType text drawn there follows scrolling and zoom.

## Example

`epx_demo/src/epx_glyph_path.erl` runs text around an ellipse and puts a
second line of glyphs on a wave with `draw_glyphs/6`.
