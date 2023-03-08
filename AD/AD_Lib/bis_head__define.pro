Pro BIS_head__define

;+
; NAME:
;		BIS_HEAD__DEFINE
; VERSION:
;		7.1
; PURPOSE:
;		Defines the {BIS_HEAD} substructure used with APEX CCD data processing
;		routines.
; CATEGORY:
;		APEX data processing.
; CALLING SEQUENCE:
;		None
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard.  Defines the structure BIS_HEAD, containing selected data
;		from a BIS type CCD image header, as follows:
;
;			FORMAT	:	Integer, data format.
;			VERSION	:	Integer, header version #.
;			HDRBLKS	:	Integer, header size, in 512 byte blocks.
;			TYPE	:	String indicating type of data in the frame.
;			TITLE	:	8-element string array, may contain user comments.
;			NCOUNTS	:	2-element long integer vector.  Contains total number of
;						counts in frame, followed by reference counts.
;			NOVERFL	:	2-element long integer vector.  Contains number of 
;						underflows, number of 1 byte overflows, number of 2 byte
;						overflows.
;			MINIMUM	:	Long integer, minimum counts in a pixel.
;			MAXIMUM	:	Long integer, maximum of counts in a pixel.
;			FILENAM	:	String, original frame filename.
;			CREATED	:	String, date and time of creation.
;			ANGLES	:	4-element float vector, contains diffractometer angles
;						(2T, OM, PH, CH)
;			NPIXELB	:	2-element long integer vector.  Number of bytes per 
;						pixel, in data and in underflow table.
;			NROWS	:	Long integer, number of data rows.
;			NCOLS	:	Long integer, number of data colums.
;			CENTER	:	4-element float, raw and unwarped detector centers.
;			DISTANC	:	2-element float, sample-detector and sample-grid 
;						distances, in cm.
;			TRAILER	:	Byte pointer to trailer info, if present.
;			CORRECT	:	String, flood table correction filename.
;			CCDPARM	:	5-element float, various CCD parameters.
;			DARK	:	String, filename for dark current correction.
; MODIFICATION HISTORY:
;		Created 25-JUL-2009 by Mati Meron.
;-

	on_error, 1

	field_list = [ 'format', 'version', 'hdrblks', 'type', 'title', $
				'ncounts', 'noverfl', 'minimum', 'maximum', $
				'filenam', 'created', $
				'angles', 'npixelb', 'nrows', 'ncols', 'center', 'distanc', $
				'trailer', 'correct', 'ccdparm', 'dark' ]

	dum =	create_struct(name = 'BIS_head' , field_list, $
			0, 0, 0,'',strarr(8), lonarr(2), lonarr(3), 0l, 0l, '', '', $
			fltarr(4),lonarr(2),0l,0l,fltarr(4),fltarr(2), 0l,'', fltarr(5),'')

	return
end