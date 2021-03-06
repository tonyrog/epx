%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
-ifndef(__JPG_HRL__).
-define(__JPG_HRL__, true).

-define(M_MARK, 16#FF).        %% JPEG marker byte
-define(M_SOF0, 16#C0).      %% Start Of Frame N
-define(M_SOF1, 16#C1).      %% N indicates which compression process
-define(M_SOF2, 16#C2).      %% Only SOF0-SOF2 are now in common use
-define(M_SOF3, 16#C3).
-define(M_DHT,  16#C4).      %% Define Huffman Table
-define(M_SOF5, 16#C5).      %% NB: codes C4 and CC are NOT SOF markers
-define(M_SOF6, 16#C6).
-define(M_SOF7, 16#C7).
-define(M_SOF9, 16#C9).
-define(M_SOF10,16#CA).
-define(M_SOF11,16#CB).
-define(M_SOF13,16#CD).
-define(M_SOF14,16#CE).
-define(M_SOF15,16#CF).
-define(M_RST0, 16#D0).
-define(M_RST1, 16#D1).
-define(M_RST2, 16#D2).
-define(M_RST3, 16#D3).
-define(M_RST4, 16#D4).
-define(M_RST5, 16#D5).
-define(M_RST6, 16#D6).
-define(M_RST7, 16#D7).
-define(M_SOI,  16#D8).       %% Start Of Image (beginning of datastream)
-define(M_EOI,  16#D9).       %% End Of Image (end of datastream)
-define(M_SOS,  16#DA).       %% Start Of Scan (begins compressed data)
-define(M_DQT,  16#DB).       %% Define Quantization Table
-define(M_DNL,  16#DC).       %% Define Number of Lines
-define(M_DRI,  16#DD).       %% Define Restart Interval
-define(M_DHP,  16#DE).       %% Define Hierarchical Progression
-define(M_EXP,  16#DF).       %% Expand Reference Component
-define(M_APP0, 16#E0).       %% Jfif marker
-define(M_APP1, 16#E1).       %% Exif marker
-define(M_APP2, 16#E2).       %% ICC color profile, FlashPix
-define(M_APP3, 16#E3).       %% HPS Tag for Steroscopic JPEG images
-define(M_APP4, 16#E4).
-define(M_APP5, 16#E5).
-define(M_APP6, 16#E6).
-define(M_APP7, 16#E7).
-define(M_APP8, 16#E8).
-define(M_APP9, 16#E9).
-define(M_APP10, 16#EA).   %% ActiveObject (multimedia messages/captions)
-define(M_APP11, 16#EB).   %% HELIOS JPEG Resources (OPI Postscript)
-define(M_APP12, 16#EC).   %% Picture Info, Photoshop Save for Web
-define(M_APP13, 16#ED).
-define(M_APP14, 16#EE).
-define(M_APP15, 16#EF).

-define(M_COM,  16#FE).       %% COMment 

-endif.


