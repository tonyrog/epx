#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "ddsmpg.hh"

static void error(const char * s, ...)
{
    va_list args;
    va_start(args, s);
    vfprintf(stderr, s, args);
    fprintf(stderr, "\n");
    va_end(args);
}

DDSMpg::DDSMpg(void)
{
    mFrames = 0;
    mFrameStart = 0;
    mFrameStop  = 0;
    mPixmaps = NULL;
}

DDSMpg::~DDSMpg(void)
{
    if (mPixmaps) {
	for (int i = 0; i < mFrames; i++)
	    epx_pixmap_destroy(mPixmaps[i]);
	free(mPixmaps);
    }
}

void DDSMpg::unload()
{
    if (mPixmaps) {
	for (int i = 0; i < mFrames; i++)
	    epx_pixmap_destroy(mPixmaps[i]);
	free(mPixmaps);
    }
    mFrames = 0;
    mPixmaps = NULL;
    mFrameStart = 0;
    mFrameStop = 0;
}

int DDSMpg::load(char* file_name, int start, int stop)
{
    SwsContext*     sws = NULL;
    AVFormatContext *pFormatCtx;
    int             i, f, videoStream;
    AVCodecContext  *pCodecCtx;
    AVCodec         *pCodec;
    AVFrame         *pFrame;
    AVFrame         *pFrameRGB;
    int             numBytes;
    uint8_t         *buffer;
    AVFormatParameters params;
    AVPacket        packet;
    int             frameFinished;
    int             height;
    int             width;

    // Register all formats and codecs
    av_register_all();

    // Open video file
    if(av_open_input_file(&pFormatCtx, file_name, NULL, 0, &params)!=0) {
	error("%s: could not be opened for reading", file_name);
        return -1;
    }

    // Retrieve stream information
    if(av_find_stream_info(pFormatCtx)<0) {
	error("%s: can not find stream info", file_name);
	av_close_input_file(pFormatCtx);
	return -1;
    }

    // Dump information about file onto standard error
    dump_format(pFormatCtx, 0, file_name, false);

    // Find the first video stream
    videoStream=-1;
    for(i=0; i< (int) pFormatCtx->nb_streams; i++) {
        if(pFormatCtx->streams[i]->codec->codec_type==CODEC_TYPE_VIDEO) {
            videoStream=i;
            break;
        }
    }
    if(videoStream==-1) {
	error("%s: no video found", file_name);
	av_close_input_file(pFormatCtx);
	return -1;
    }

    // Get a pointer to the codec context for the video stream
    pCodecCtx=pFormatCtx->streams[videoStream]->codec;

    // Find the decoder for the video stream
    pCodec=avcodec_find_decoder(pCodecCtx->codec_id);
    if(pCodec==NULL) {
	error("%s: no codec found", file_name);
	av_close_input_file(pFormatCtx);
	return -1;
    }

    // Inform the codec that we can handle truncated bitstreams -- i.e.,
    // bitstreams where frame boundaries can fall in the middle of packets
    if(pCodec->capabilities & CODEC_CAP_TRUNCATED)
        pCodecCtx->flags|=CODEC_FLAG_TRUNCATED;

    // Open codec
    if(avcodec_open(pCodecCtx, pCodec)<0) {
	error("%s: could not open codec", file_name);
	av_close_input_file(pFormatCtx);
	return -1;
    }

    // Allocate video frame
    if ((pFrame=avcodec_alloc_frame()) == NULL) {
	error("%s: out of memory", file_name);
	av_close_input_file(pFormatCtx);
	return -1;
    }

    // Allocate an AVFrame structure
    if ((pFrameRGB=avcodec_alloc_frame()) == NULL) {
	error("%s: out of memory", file_name);
	av_close_input_file(pFormatCtx);
	return -1;
    }

    height = pCodecCtx->height;
    width  = pCodecCtx->width;

    // Determine required buffer size and allocate buffer
    numBytes=avpicture_get_size(PIX_FMT_RGB24, width, height);
    buffer=new uint8_t[numBytes];

    // Assign appropriate parts of buffer to image planes in pFrameRGB
    avpicture_fill((AVPicture *)pFrameRGB, buffer, PIX_FMT_RGB24,
		   width, height);

    // Read frames and save first five frames to disk
    i=0;
    f=0;
    mFrameStart = start;
    while(av_read_frame(pFormatCtx, &packet)>=0) {
	// Is this a packet from the video stream?
	if(packet.stream_index==videoStream) {
	    // Decode video frame
	    avcodec_decode_video(pCodecCtx, pFrame, &frameFinished,
				 packet.data, packet.size);

	    // Did we get a video frame?
	    if ((i>=start) && ((stop==0)||(i<=stop)) && frameFinished) {
		u_int8_t* sp;
		u_int8_t* pixels24;
		size_t    imageDim = width*height;
		u_int8_t* dst_data[4];
		int       dst_linesize[4];
		EPixmap* pixmap;
		int x, y;

		sws = sws_getCachedContext(sws,
					   width, height, pCodecCtx->pix_fmt,
					   width, height, PIX_FMT_RGB24,
					   SWS_BICUBIC, NULL, NULL, NULL);
		pixels24 = (u_int8_t*) malloc(imageDim*3);
		pixmap = EPixmapCreate(width, height, EPIXEL_TYPE_RGBA);
		mPixmaps = (EPixmap**) realloc(mPixmaps,(f+1)*sizeof(EPixmap*));
		mPixmaps[f++] = pixmap;

		dst_data[0] = pixels24;
		dst_data[1] = dst_data[2] = dst_data[3 ] = NULL;

		dst_linesize[0] = 3*width;
		dst_linesize[1] = dst_linesize[2] = dst_linesize[3] = 0;

		sws_scale(sws, pFrame->data, pFrame->linesize, 0,
			  height, dst_data, dst_linesize);

		/* convert rgb24 data to DDSPixel format */
		sp = pixels24;
		for (y = 0; y < height; y++) {
		    for (x = 0; x < width; x++) {
			EPixel_t p;

			p.r = sp[0];
			p.g = sp[1];
			p.b = sp[2];
			p.a = 255;
			EPixmapPutPixel(pixmap, x, y, 0, p);
			sp += 3;
		    }
		}
		free(pixels24);
	    }
	    // Free the packet that was allocated by av_read_frame
	    av_free_packet(&packet);
	}
    }
    mFrameStop = mFrameStart + f;
    mFrames    = f;

    // Free the RGB image
    delete [] buffer;
    av_free(pFrameRGB);

    // Free the YUV frame
    av_free(pFrame);

    // Close the codec
    avcodec_close(pCodecCtx);

    // Close the video file
    av_close_input_file(pFormatCtx);

    return 0;
}
