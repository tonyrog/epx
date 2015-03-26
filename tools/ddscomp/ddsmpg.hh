#ifndef __DDSMPG_HH__
#define __DDSMPG_HH__

extern "C" {
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
}

#include "ddsimg.hh"

class DDSMpg : public DDSImage
{
public:
    DDSMpg(void);
    ~DDSMpg(void);

    int width(void)  { return mPixmaps[0]->width; };
    int height(void) { return mPixmaps[0]->height; };
    int frames(void) { return mFrames; };

    epx_pixel_t pixel(int i, int x, int y) {
	return epx_pixmap_get_pixel(mPixmaps[i], x, y);
    };

    epx_pixmap_t* getPixmap(int i) {
	if ((i >= 0) && (i < mFrames))
	    return mPixmaps[i];
	return NULL;
    }

    void setPixmap(int i, epx_pixmap_t* aPixmap) {
	if ((i >= 0) && (i < mFrames)) {
	    epx_pixmap_t* old = mPixmaps[i];
	    if (old)
		epx_pixmap_destroy(old);
	    mPixmaps[i] = aPixmap;
	}
    }

    int  useAlpha(int i) {  return 0; }

    u_int8_t red(int i, int x, int y)   { return pixel(i,x,y).r; };
    u_int8_t green(int i, int x, int y) { return pixel(i,x,y).g; };
    u_int8_t blue(int i, int x, int y)  { return pixel(i,x,y).b; };
    u_int8_t alpha(int i, int x, int y) { return pixel(i,x,y).a; };

    int  load(char* file_name, int start, int stop);
    void  unload(void);

private:
    int mFrames;        // number of frames
    int mFrameStart;    // first frame number  (offset)
    int mFrameStop;     // last frame number   (offset)
    epx_pixmap_t** mPixmaps;
};

#endif

