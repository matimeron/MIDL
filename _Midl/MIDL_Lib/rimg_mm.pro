Function Rimg_mm, fname, weights = wei, rot = rot, show = sho, topsize = top, $
	dimensions = dim, _extra = _e

;+
; NAME:
;		RIMG_MM
; VERSION:
;		8.12
; PURPOSE:
;		Interface to the graphics READ routines.
; CATEGORY:
;		I/O
; CALLING SEQUENCE:
;		Result = RIMG_MM( [FNAME] [, keywords])
; INPUTS:
;	FNAME
;		Character variable or constant, translating to a file name.  In standard
;		operation the user will be selecting or modifying FNAME through
;		DIALOG_PICKFILE.  This can be overriden by using the keyword /AUTO
;		which is passed by _EXTRA to FILE_GET.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	WEIGHTS
;		If the image file contains 3 color channels (R,G,B), RIMG_MM combines
;		these channels to generate a grayscale image (unless grayscale option is
;		already available in the generic read routine).  WEIGHTS, when given as 
;		a 3-element vector, is used to provide relative weights for the 
;		averaging.  If not given, WEIGHTS defaults to [1/3,1/3,1/3].
;	ROT
;		Integer scalar, specifies rotation as in the IDL ROTATE function.
;	/SHOW
;		Switch.  If set, the image being read is also displayed to the screen.
;	TOPSIZE
;		Specifies the maximal dimension (in any direction, in pixels) of the 
;		display, when /SHOW is set, else has no effect.  Default value is 512.
;	DIMENSIONS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Returns the image as a 2D array.
; OPTIONAL OUTPUT PARAMETERS:
;	DIMENSIONS
;		Returns the dimensions of the image, as a 2-element long vector, in a
;		[h,v] format.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, utilizing built in IDL image reading routines.  Calls
;		DISPLAY_MM, FILE_GET, FNAMPARSE and STRMATCH_MM, from MIDL. 
; MODIFICATION HISTORY:
;		Created 5-OCT-2011 by Mati Meron.
;-

	on_error, 1

	posib = '.' + ['tif','tiff','jpg','jpeg']
	poscod = [0,0,1,1]
	wfname = File_get(fname,stat=stat,_extra=_e)
	dim = [0l,0l]

	if stat then begin
		dum = Fnamparse(wfname,ext=ext)
		wtyp = Strmatch_mm(ext,posib)
		if wtyp ge 0 then wcod = poscod[wtyp] $
		else message, 'File type not on list!'

		case wcod of
			0	:	dat = read_tiff(wfname,_extra=_e)
			1	:	read_jpeg, wfname, dat, /grayscale, _extra=_e
			else:	message, 'Undefined code!'
		endcase

		res = float(dat)
		siz = size(res)
		dim = siz[1:2]

		case siz[0] of
			2	:
			3	:	begin
						if keyword_set(wei) then begin
							if n_elements(wei) eq 3 then begin
								wwei = wei/total(wei)
								for i = 0, 2 do res[i,*,*] = wwei[i]*res[i,*,*]
							endif else message, 'WEIGHTS needs 3 elements', /con
						endif else res = res/3
						res = total(res,1)
					end
			else:	message, 'Unrecognizable data format!'
		endcase

		if keyword_set(rot) then res = rotate(res,rot)
		if keyword_set(sho) then $
		Display_mm, res, top = Default(top,512l,/dtyp), /auz, /ave, _extra = _e
	endif else message, 'Bad or missing file!'

	return, res
end