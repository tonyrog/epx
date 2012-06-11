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
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%   Utils for decoding Exif tags
%%% @end
%%% Created :  6 Mar 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(epx_exif).

-export([decode_tag/1]).

-include("epx_exif.hrl").

decode_tag(Tag) when is_integer(Tag) ->
    case Tag of
	?ExposureTime -> 'ExposureTime';
	?FNumber -> 'FNumber';
	?ExposureProgram -> 'ExposureProgram';
	?ISOSpeedRatings -> 'ISOSpeedRatings';
	?ExifVersion -> 'ExifVersion';
	?DateTimeOriginal -> 'DateTimeOriginal';
	?DateTimeDigitized -> 'DateTimeDigitized';
	?ComponentsConfiguration -> 'ComponentsConfiguration';
	?CompressedBitsPerPixel -> 'CompressedBitsPerPixel';
	?ShutterSpeedValue -> 'ShutterSpeedValue';
	?ApertureValue -> 'ApertureValue';
	?BrightnessValue -> 'BrightnessValue';
	?ExposureBiasValue -> 'ExposureBiasValue';
	?MaxApertureValue -> 'MaxApertureValue';
	?SubjectDistance -> 'SubjectDistance';
	?MeteringMode -> 'MeteringMode';
	?LightSource -> 'LightSource';
	?Flash -> 'Flash';
	?FocalLength -> 'FocalLength';
	?MakerNote -> 'MakerNote';
	?UserComment -> 'UserComment';
	?SubsecTime -> 'SubsecTime';
	?SubsecTimeOriginal -> 'SubsecTimeOriginal';
	?SubsecTimeDigitized -> 'SubsecTimeDigitized';
	?FlashPixVersion -> 'FlashPixVersion';
	?ColorSpace -> 'ColorSpace';
	?ExifImageWidth -> 'ExifImageWidth';
	?ExifImageHeight -> 'ExifImageHeight';
	?RelatedSoundFile -> 'RelatedSoundFile';
	?ExifInteroperabilityOffset -> 'ExifInteroperabilityOffset';
	?FocalPlaneXResolution -> 'FocalPlaneXResolution';
	?FocalPlaneYResolution -> 'FocalPlaneYResolution';
	?FocalPlaneResolutionUnit -> 'FocalPlaneResolutionUnit';
	?ExposureIndex -> 'ExposureIndex';
	?SensingMethod -> 'SensingMethod';
	?FileSource -> 'FileSource';
	?SceneType -> 'SceneType';
	?CFAPattern -> 'CFAPattern';
	?InteroperabilityIndex -> 'InteroperabilityIndex';
	?InteroperabilityVersion -> 'InteroperabilityVersion';
	?RelatedImageFileFormat -> 'RelatedImageFileFormat';
	?RelatedImageWidth -> 'RelatedImageWidth';
	?RelatedImageLength -> 'RelatedImageLength';
	Tag -> Tag
    end;
decode_tag(Tag) ->
    Tag.

    
	    


