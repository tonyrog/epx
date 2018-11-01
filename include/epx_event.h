/***************************************************************************
 *
 * Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
 *
 * This software is licensed as described in the file COPYRIGHT, which
 * you should have received as part of this distribution. The terms
 * are also available at http://www.rogvall.se/docs/copyright.txt.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYRIGHT file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 ***************************************************************************/
#ifndef __EPX_EVENT_H__
#define __EPX_EVENT_H__

#include <stdint.h>

/* keyboard modifiers */
#define EPX_KBD_MOD_NONE  		0x0000
#define EPX_KBD_MOD_LSHIFT		0x0001
#define EPX_KBD_MOD_RSHIFT		0x0002
#define EPX_KBD_MOD_LCTRL 		0x0040
#define EPX_KBD_MOD_RCTRL 		0x0080
#define EPX_KBD_MOD_LALT  		0x0100
#define EPX_KBD_MOD_RALT  		0x0200
#define EPX_KBD_MOD_LMETA 		0x0400
#define EPX_KBD_MOD_RMETA 		0x0800
#define EPX_KBD_MOD_NUM   		0x1000
#define EPX_KBD_MOD_CAPS  		0x2000
#define EPX_KBD_MOD_ALTGR 		0x4000
#define EPX_KBD_MOD_SCR		0x8000

#define EPX_KBD_MOD_CTRL	(EPX_KBD_MOD_LCTRL|EPX_KBD_MOD_RCTRL)
#define EPX_KBD_MOD_SHIFT	(EPX_KBD_MOD_LSHIFT|EPX_KBD_MOD_RSHIFT)
#define EPX_KBD_MOD_ALT	(EPX_KBD_MOD_LALT|EPX_KBD_MOD_RALT)
#define EPX_KBD_MOD_META	(EPX_KBD_MOD_LMETA|EPX_KBD_MOD_RMETA)

#define EPX_KBD_KEY_UNKNOWN	0
/* Following special control keysyms are mapped to ASCII*/
#define EPX_KBD_KEY_BACKSPACE	8
#define EPX_KBD_KEY_TAB		9
#define EPX_KBD_KEY_ENTER		13
#define EPX_KBD_KEY_ESCAPE		27
/* Keysyms from 32-126 are mapped to ASCII*/

#define EPX_KBD_KEY_NONASCII_MASK	0xFF00
/* Following keysyms are mapped to private use portion of Unicode-16*/
/* arrows + home/end pad*/
#define EPX_KBD_KEY_FIRST		0xF800

#define EPX_KBD_KEY_LEFT		0xF800
#define EPX_KBD_KEY_RIGHT		0xF801
#define EPX_KBD_KEY_UP		0xF802
#define EPX_KBD_KEY_DOWN		0xF803
#define EPX_KBD_KEY_INSERT		0xF804
#define EPX_KBD_KEY_DELETE		0xF805
#define EPX_KBD_KEY_HOME		0xF806
#define EPX_KBD_KEY_END		0xF807
#define EPX_KBD_KEY_PAGEUP		0xF808
#define EPX_KBD_KEY_PAGEDOWN	0xF809

/* Numeric keypad*/
#define EPX_KBD_KEY_KP0		0xF80A
#define EPX_KBD_KEY_KP1		0xF80B
#define EPX_KBD_KEY_KP2		0xF80C
#define EPX_KBD_KEY_KP3		0xF80D
#define EPX_KBD_KEY_KP4		0xF80E
#define EPX_KBD_KEY_KP5		0xF80F
#define EPX_KBD_KEY_KP6		0xF810
#define EPX_KBD_KEY_KP7		0xF811
#define EPX_KBD_KEY_KP8		0xF812
#define EPX_KBD_KEY_KP9		0xF813
#define EPX_KBD_KEY_KP_PERIOD	0xF814
#define EPX_KBD_KEY_KP_DIVIDE	0xF815
#define EPX_KBD_KEY_KP_MULTIPLY	0xF816
#define EPX_KBD_KEY_KP_MINUS	0xF817
#define EPX_KBD_KEY_KP_PLUS	0xF818
#define EPX_KBD_KEY_KP_ENTER	0xF819
#define EPX_KBD_KEY_KP_EQUALS	0xF81A

/* Function keys */
#define EPX_KBD_KEY_F1		0xF81B
#define EPX_KBD_KEY_F2		0xF81C
#define EPX_KBD_KEY_F3		0xF81D
#define EPX_KBD_KEY_F4		0xF81E
#define EPX_KBD_KEY_F5		0xF81F
#define EPX_KBD_KEY_F6		0xF820
#define EPX_KBD_KEY_F7		0xF821
#define EPX_KBD_KEY_F8		0xF822
#define EPX_KBD_KEY_F9		0xF823
#define EPX_KBD_KEY_F10		0xF824
#define EPX_KBD_KEY_F11		0xF825
#define EPX_KBD_KEY_F12		0xF827

/* Key state modifier keys*/
#define EPX_KBD_KEY_NUMLOCK	0xF828
#define EPX_KBD_KEY_CAPSLOCK	0xF829
#define EPX_KBD_KEY_SCROLLOCK	0xF82A
#define EPX_KBD_KEY_LSHIFT		0xF82B
#define EPX_KBD_KEY_RSHIFT		0xF82C
#define EPX_KBD_KEY_LCTRL		0xF82D
#define EPX_KBD_KEY_RCTRL		0xF82E
#define EPX_KBD_KEY_LALT		0xF82F
#define EPX_KBD_KEY_RALT		0xF830
#define EPX_KBD_KEY_LMETA		0xF831
#define EPX_KBD_KEY_RMETA		0xF832
#define EPX_KBD_KEY_ALTGR		0xF833

/* Misc function keys*/
#define EPX_KBD_KEY_PRINT		0xF834
#define EPX_KBD_KEY_SYSREQ		0xF835
#define EPX_KBD_KEY_PAUSE		0xF836
#define EPX_KBD_KEY_BREAK		0xF837
#define EPX_KBD_KEY_QUIT		0xF838	/* virtual key*/
#define EPX_KBD_KEY_MENU		0xF839	/* virtual key*/
#define EPX_KBD_KEY_REDRAW		0xF83A	/* virtual key*/

/* Mouse button & Wheel */
#define EPX_BUTTON_LEFT            0x0001
#define EPX_BUTTON_MIDDLE          0x0002
#define EPX_BUTTON_RIGHT           0x0004
#define EPX_WHEEL_UP               0x0008
#define EPX_WHEEL_DOWN             0x0010
#define EPX_WHEEL_LEFT             0x0020
#define EPX_WHEEL_RIGHT            0x0040

/* Handheld function keys*/
/* #define EPX_KBD_KEY_RECORD		0xF840 -- Replaced by HAVi code */
/* #define EPX_KBD_KEY_PLAY		0xF841 -- Replaced by HAVi code */
#define EPX_KBD_KEY_CONTRAST	0xF842
#define EPX_KBD_KEY_BRIGHTNESS	0xF843
#define EPX_KBD_KEY_SELECTUP	0xF844
#define EPX_KBD_KEY_SELECTDOWN	0xF845
#define EPX_KBD_KEY_ACCEPT	0xF846
#define EPX_KBD_KEY_CANCEL	0xF847
#define EPX_KBD_KEY_APP1	0xF848
#define EPX_KBD_KEY_APP2	0xF849
#define EPX_KBD_KEY_APP3        0xF84A
#define EPX_KBD_KEY_APP4        0xF84B
#define EPX_KBD_KEY_SUSPEND     0xF84C
#define EPX_KBD_KEY_END_NORMAL	0xF84D	/* insert additional keys before this*/

/* used both as event type and event mask */
#define EPX_EVENT_KEY_PRESS        0x00000001
#define EPX_EVENT_KEY_RELEASE      0x00000002
#define EPX_EVENT_POINTER_MOTION   0x00000004
#define EPX_EVENT_BUTTON_PRESS     0x00000008
#define EPX_EVENT_BUTTON_RELEASE   0x00000010
#define EPX_EVENT_FOCUS_IN         0x00000020
#define EPX_EVENT_FOCUS_OUT        0x00000040
#define EPX_EVENT_ENTER            0x00000080
#define EPX_EVENT_LEAVE            0x00000100
#define EPX_EVENT_CONFIGURE        0x00000200
#define EPX_EVENT_RESIZE           0x00000400
#define EPX_EVENT_NO_AUTO_REPEAT   0x00000800  // only in mask
#define EPX_EVENT_EXPOSE           0x00001000
// Button mask is used to filter events, only containing the buttons
#define EPX_EVENT_BUTTON_MASK      0x007F0000
#define EPX_EVENT_BUTTON_ANY       0x00000000
#define EPX_EVENT_BUTTON_LEFT      0x00010000
#define EPX_EVENT_BUTTON_MIDDLE    0x00020000
#define EPX_EVENT_BUTTON_RIGHT     0x00040000
#define EPX_EVENT_WHEEL_MASK       0x00780000
#define EPX_EVENT_WHEEL_UP         0x00080000
#define EPX_EVENT_WHEEL_DOWN       0x00100000 
#define EPX_EVENT_WHEEL_LEFT       0x00200000
#define EPX_EVENT_WHEEL_RIGHT      0x00400000

#define EPX_EVENT_CLOSE            0x40000000  /* window manager close event */
#define EPX_EVENT_DESTROYED        0x80000000  /* object destroyed notification */
#define EPX_EVENT_ALL              0xFFFFFFFF

struct _epx_window_t;

typedef struct _epx_event_t {
    uint32_t type;                 // event type
    struct _epx_window_t* window;  // Event window
    union {
	struct {
	    uint32_t button;
	    int32_t x;
	    int32_t y;
	    int32_t z;
	} pointer;

	struct {
	    int32_t w;
	    int32_t h;
	    int32_t d;
	} dimension;

	struct {
	    int32_t x;
	    int32_t y;
	    int32_t w;
	    int32_t h;
	} area;
	
	struct {
	    uint16_t mod;   // modifier keys 
	    uint16_t code;  // internal code 
	    uint16_t sym;   // key code 
	} key;
    };
} epx_event_t;

#endif

