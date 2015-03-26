Erlang PiXel library
====================

epx is a library for manipulating pixels. Various operation exist
to operate on the pixels.

To create a small pixmap and set some pixels you do not even have to
start the epx application, just do:

    Pixmap = epx:pixmap_create(10, 10, rgb),
    epx:pixmap_fill(Pixmap, red),
    epx_pixmap_put_pixel(Pixmap, 4, 4, white).

Now you can read the pixels:
 
    {A,R,G,B} = epx:pixmap_get_pixel(Pixmap, 3, 3).
    <<R,G,B>> = epx:pixmap_get_pixels(Pixmap, 3, 3, 1, 1).



# epx on frame buffer device (linux)

    epx:debug(debug).
    epx:start().
    {ok, BID } = epx_backend:create("fb", [{backend,"fb"}, {width, 1280}, {height, 1024}, {input_mouse_device, "/dev/input/event7"}, {input_keyboard_device, "/dev/input/event5"}]).
    B = epx_backend:backend(BID).
    W=epx:window_create(0,0,1280,1024, [button_press, key_press, key_release]).
    epx:window_attach(W, B).


## howto run framebuffer on vmware (ubuntu)

Append "vmwgfx.enable_fbdev=no" to GRUB_CMDLINE_LINUX_DEFAULT in file /etc/default/grub. "no" is not a correct value, driver will complain. But this is a bug workaround :-)

May look something like this:

    GRUB_CMDLINE_LINUX_DEFAULT="splash quiet vmwgfx.enable_fbdev=no"

Then the fb driver may be probed:

    sudo modprobe uvesafb

## Packages needed to build epx using top-level Makefile on Raspberry pi and Ubuntu

	sudo apt-get install libx11-dev autoconf libgif-dev


