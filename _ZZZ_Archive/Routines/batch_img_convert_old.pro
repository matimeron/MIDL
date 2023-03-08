Pro Batch_img_convert_old, file, inp_dir = idr, out_dir = odr, last = lst, $
	image_type = imt, verbose = vrb, _extra =_e

;+
; NAME:
;		BATCH_IMG_CONVERT
; VERSION:
;		8.42
; PURPOSE:
;		Converts multiple image files (TIF or JPG) to BRUKER SFRM files.
; CATEGORY:
;		Area detector data processing.
; CALLING SEQUENCE:
;		BATCH_IMG_CONVERT, FILE [,keywords]
; INPUTS:
;	FILE
;		Full or partial file name (file only, no path info) may include wild
;		cards.  All files in the input directory (see below) fitting FILE will
;		be processed.  If not given, FILE defaults to '*'.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	INP_DIR
; 		Complete file name for the input folder.  If not given, is querried
; 		about interactively.
; 	OUT_DIR
; 		Same as INP_DIR for the output folder.  The folder may be created
; 		during the call.
; 	/LAST
; 		Switch.  If set, the INP_DIR and OUT_DIR values from the previous call
; 		are recycled (if there was no previous call, an error will result).
;
; 		Note:	Any of INP_DIR, OUT_DIR and CUT may still be provided when
; 				LAST is set, and the provided value will override the previous
; 				one.
; 		Note2:	The keyword /INTERACTIVE (see function FILE_GET) may be used,
; 				with LAST, to modify the folder selections.
; 	IMAGE_TYPE
; 		Character scalar, an image type.  Currently acceptable types are:
; 		TIF, TIFF, JPG, JPEG.
; 
; 		Note:	If FILE (see above) includes an extension, IMAGE_TYPE (if given)
; 				is ignored.  If no extension and no IMAGE_TYPE are present, 
; 				last used type is assumed and if there was no previously used
; 				type then TIF will be used as default.
;	/VERBOSE
;		Switch.  If set, the names of the converted files are displayed to the
;		screen.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Optional screen output and saved files.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		BIS_CINFO.  Contains:
;			LEXS	:	Status variable, set to 1 when previous data is defined.
;			LIDR	:	Full name of last input folder.
;			LODR	:	Full name of last output folder.
;			LIND	:	The index (in the image type list) of last image type .
;
;		Note:	If an new image type (different from the one last used) is 
;				provided, either through FILE or IMAGE_TYPE, but no files of 
;				this type are found in INP_DIR, the value of LIND won't change. 
; SIDE EFFECTS:
;		If the output folder already contains files with same names as the new
;		ones being written, the older files will be overwritten with no warning.
; RESTRICTIONS:
;		The input folder must exist and contain at least one file of the type
;		specified by the FILE input.
; PROCEDURE:
;		Straightforward, finds all the eligible files and converts them calling
;		IMG_TO_SFRM.  Also calls DEFAULT, FILE_GET, FNAMPARSE, STREQ, 
;		STRMATCH_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-MAY-2015 by Mati Meron.
;-

	common BIS_cinfo, lexs, lidr, lodr, lind
	on_error, 1

	iext = ['.tif','.tiff','.jpg','.jpeg']
	bext = '.sfrm'
	rtn = string(13b)

	lexs = Default(lexs,0,/dtyp)
	if keyword_set(lst) then begin
		if lexs then begin
			idir = File_get(Default(idr,lidr),/dir,$
				title='Select input folder',stat=istat,_extra=_e)
			odir = File_get(Default(odr,lodr),/dir,$
				title='Select output folder',stat=ostat,_extra=_e)
		endif else message, '"LAST" not defined yet!
	endif else begin
		idir = File_get(idr,/dir,title='Select input folder',stat=istat)
		odir = File_get(odr,/dir,title='Select output folder',stat=ostat)
	endelse
	if istat and ostat then begin
		if not lexs then lind = 0
		lidr = idir
		lodr = odir
		lexs = 1
	endif

	wfil = Fnamparse(Default(file,'*',/dtyp),ext=ext)
	if Streq(ext,'') then begin
		if Type(imt) eq 0 then ind = lind else ind = Strmatch_mm('.'+imt,iext)
	endif else ind = Strmatch_mm(ext,iext)
	if ind ge 0 then ext = iext[ind] else message, 'Unacceptable image type!'
	flist = file_search(idir+wfil+ext,count=nfil)

	if nfil gt 0 then begin
		lind = ind
		vrbfl = keyword_set(vrb)
		for i = 0, nfil-1 do begin
			fnam = Fnamparse(flist[i])
			out = odir + fnam + bext
			Img_to_sfrm, in = flist[i], out = out, _extra = _e
			if vrbfl then print, '	Converting ' + fnam, i+1, nfil, rtn, $
			form = '(a,"	",i0," of ",i0,a)'
		endfor
		if vrbfl then print, '				DONE!'
	endif else message, 'No data files found!', /con

	return
end