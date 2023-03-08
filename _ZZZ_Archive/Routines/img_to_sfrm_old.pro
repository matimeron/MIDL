Pro Img_to_sfrm_old, infile= inam, outfile= onam, get_header= get, rotate= rot,$
	_extra = _e

;+
; NAME:
;		IMG_TO_SFRM
; VERSION:
;		8.42
; PURPOSE:
;		Converts an image file into a Bruker BIS file.
; CATEGORY:
;		Data Input/Output.
; CALLING SEQUENCE:
;		IMG_TO_SFRM, [keywords]
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
; 	INFILE
; 		Name of the input (image) file.  If not given, will be queried for 
; 		interactively.
; 	OUTFILE
; 		Same as above, for the output file.
; 	/GET_HEADER
; 		Switch.  If given as character string, it'll be used as the name of the
; 		BIS header file.  If simply set, the routine will query for a header 
; 		file interactively.  The default is to use the last header read.
; 	/ROTATE
; 		Given as integer scalar (see IDL ROTATE function for details) specifies
; 		rotation of the image prior to the conversion.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to imbedded
;		routines.  Not to be used directly.
; OUTPUTS:
; 		None, other than a file being saved.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
; 		BIS_HEADER.  Contains:
; 			BEXS	:	Status variable, set to 1 when previous data is defined.
; 			SHEAD	:	Previously used SFRM header, character array.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the input file must be in one of the standard image
;		formats (JPEG or TIFF, more can be added if needed).
; PROCEDURE:
;		Straightforward, Reads a BIS header and the image file.  Modifies the
;		BIS header with the image data, combines header and image and saves the
;		combination as a standard BIS file.  Calls READ_BIS_HEADER and 
;		STRARR_PUT.  Calls SDEP from IMPORTS.  Calls SCAN_PD_READ from SPEC.  
;		Also calls ARRLOC, DEFAULT and FILE_GET, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-APR-2015 by Mati Meron.
;-

	common bis_header, bexs, shead

	on_error, 1
	ds = sdep(/ds)

	if keyword_set(get) or not Default(bexs,0,/dtyp) then begin
		if keyword_set(get) then hnam = string(get) else $ 
		hnam= getenv('root_mm')+strjoin(['AD','Data','bis_file_header.sfrm'],ds) 
		shead = (head = $
		Read_BIS_header(file=hnam,title='Please Select a Header Source'))
		bexs = 1
	endif else head = shead
	hlen = n_elements(head)

	rnam = $
	File_get(inam,stat=stat,title='Please Select an Image Data File',_extra=_e)
	if stat then begin
		dat = ulong(Scan_pd_read(filename=rnam,rotate=rot,/jimg,_extra=_e) > 0)
		dim = size(dat,/dim)
		ncounts = total(dat,/pres)
		maximum = max(dat,maxloc,min=minimum)
		maxxy = Arrloc(maxloc,size(dat))
		finfo = file_info(rnam)
		sectime = finfo.atime < finfo.ctime < finfo.mtime
		datime = systime(0,sectime)
		sdatime = string(bin_date(datime),form='(i0)')
		date = string(sdatime[[1,2,0]],form='(i02,"/",i02,"/",i04)')
		time = string(sdatime[[3,4,5]],form='(i02,":",i02,":",i02)')

		putloc = 8l
		Strarr_put, head, string(ncounts,form='(i0)'), putloc, phra='ncounts'
		Strarr_put, head, '0', putloc+36, phra='ncounts'
		Strarr_put, head, string(minimum,form='(i0)'), putloc, phra='minimum'
		Strarr_put, head, string(maximum,form='(i0)'), putloc, phra='maximum'
		Strarr_put, head, string(maxxy[0],form='(f0)'), putloc, phra='maxxy'
		Strarr_put, head, string(maxxy[1],form='(f0)'), putloc+36, phra='maxxy'
		Strarr_put, head, date, putloc, phra='created'
		Strarr_put, head, time, putloc + 36, phra='created'
		Strarr_put, head, rnam, putloc, phra = 'filenam'
		Strarr_put, head, string(dim[1],form='(i0)'),8,phra='nrows'
		Strarr_put, head, string(dim[0],form='(i0)'),8,phra='ncols'
	endif else message, 'Missing input data!'

	wnam= File_get(onam,default='sfrm',/write,/over,/auto,stat=stat,_extra=_e)
	if stat then begin
		linlen = 80l
		rform = strcompress('('+string(hlen)+'a'+string(linlen)+')',/rem)
		openw, wrun, wnam, /get_lun
		printf, wrun, head, format = rform
		point_lun, wrun, hlen*linlen
		writeu, wrun, dat
		free_lun, wrun
	endif else message, 'File not written!', /cont

	return
end