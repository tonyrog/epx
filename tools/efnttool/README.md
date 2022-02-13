EFNTTOOL
========

Create anti-aliased or bitmap font to be used with epx.
EPX prefer font files that can be mapped into memory and rendered
easily.

Example:

    efnttool -latin1 -o foo.efnt /usr/X11/share/fonts/TTF/Vera.ttf 12

This command will translate the latin1 character glyphs from Vera.ttf in
pixel size 12, into a gray scale pixmap format (A8) named foo.efnt.
If the foo.efnt is put in epx/priv/font directory it may be found
by the epx_font manager and easily be used by the Erlang programs.
Use epx\_font:refresh() to reload fonts from file system.

# Syntax

    efnttool [options] <fontfile> <fontsize>
    
The <fontfile> is norammly a True type font file but may be anything
that is accepted by freetype.

# Options

    -v

Turn on debug output more -v for extra debug

    -start <charcode>

The first char code to emit a glyph for.

    -stop

The last char code to emit a glyph for.

    -latin1

Stands for -start 0 -end 255

    -ascii

Stands for -start 0 -end 127

    -efnt2

Emit a more compact anti-aliased format than A8 (default), this format can
however not be memory mapped, if the output file is named with a .c
extension then "magically" a C source file is generated that can be
compiled into small embedded platforms.

    -xres <dpi>
	
Set the resolution of the width of the generated output, in dots per inch.

    -yres <dpi>

Set the resolution of the height of the generated output, in dots per inch.

    -scale <magnifier>

Set the scale magnification factor, integer >= 1. This can for example
be used to generated big blocky look on fixed size type faces.

    -bitmap

Load bitmap font if available. Use A8 format but   

    -o <file>

Set the name of the output font. If -o is given then the name is
derived from the input file, <file>.efnt or <file>.efnt2 when -efnt2
is in effect. If the output file is set to <file>.c when -efnt2 is
given then a C source file is generated.
