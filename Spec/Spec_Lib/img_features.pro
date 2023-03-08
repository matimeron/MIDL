Function Img_features, dat, xonly = xon, yonly = yon, min = min, max = max, $
	smooth = smo, threshold = tre, clean = cle, extyp = ext, _extra = _e

;+
; NAME:
;		IMG_FEATURES
; VERSION:
;		8.33
; PURPOSE:
;		Identifies "features", i.e. locii of 1D maxima and/or minima, in an 
;		image.
; CATEGORY:
;		Array Function.
; CALLING SEQUENCE:
;		Result = IMG_FEATURES ( DAT,[,keywords])
; INPUTS:
;	DAT
;		Either a 2D [M,N] array, or the standard data representation 3D array
;		with dimensions [4,M,N] ([3,M,N] is also acceptable).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/XONLY												| Only one of these
;		Switch.  Specifies feature search in the X		| keywords may be used.
;		 direction only.								| If none is, full
;	/YONLY												| enhancement is
;		Switch.  Specifies feature search in the Y		| performed.
;		direction only.
;	SMOOTH
;		Scalar or vector of length 2, if given serves as smoothing width for 
;		BSMOOTH_MM.  If given as scalar, same width is applied in both 
;		dimensions.  The smoothing (when specified) is performed prior to 
;		extrema search.
;		
;		Note:	If XONLY or YONLY is set, smoothing is performed only in the
;				specified direction.
;	/MIN												|	One and only one of
;		Switch.  If specified the search is for minima.	|	these two may be
;	/MAX												|	specified.  The
;		Switch.  If specified the search is for maxima.	|	default is MAX.
;	THRESHOLD
;		Scalar value, used as a threshold for EXTREMA (see there).  Default is 0
;	/CLEAN
;		Switch.  If set, the result, after extrema search, is cleaned up with
;		IMG_EXORCISE (see there) in order to eliminate isolated extrema.
;	EXTYP
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns an array, of the same format as the input DAT, with values of 
;		+-1 at the location of the extrema of DAT (maxima or minima, according 
;		to the keywords) and 0 everywhere else.
;		If the input is data representation, then the above applies to 
;		DAT[2,*,*] only.  If DAT[3,*,*] (error page) is present, it is zeroed
;		on output.
; OPTIONAL OUTPUT PARAMETERS:
;	EXTYP
;		Returns the type of extremum used, -1 for minimum, 1 for maximum, 
;		0 for both.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		DAT must be a numeric 2D array or a data representation (3D) array.
; PROCEDURE:
;		Straightforward.  Calls IMG_EXORCISE.  Also calls EXTREMA, ISNUM, 
;		ONE_OFF, SMOOTH_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAY-2009 by Mati Meron, as a modification of the core of the
;		routine HASH_FIND, written by Brian Leahy on 31-MAR-2009.
;		Modified 20-FEB-2014 by Mati Meron.  Replaced SMOOTH_MM with BSMOOTH_MM.
;		Added keywords XONLY and YONLY.  Enabled handling 3D data 
;		representation arrays.
;		Modified 5-OCT-2014 by Mati Meron.  Internal change, replaced BSMOOTH_MM
;		with SMOOTH_MM.
;-

	on_error, 1

	siz = size(dat)
	case siz[0] of
		2	:	ffl = 0
		3	:	ffl = 1
		else:	message, 'Not image data!'
	endcase
	res = dat

	if ffl then begin
		arr = reform(dat[2,*,*])
		siz = size(arr)
	endif else arr = dat
	pres = make_array(size=siz,typ=Type(arr)>2)

	wha = [1,1]
	whi = One_of(yon,xon)
	if whi ge 0 then wha[whi] = 0
	if Isnum(smo) then begin
		if n_elements(smo) eq 1 then wsmo = [smo,smo] else wsmo = smo
		wsmo = (wsmo*wha) > 1 			
		arr = Smooth_mm(arr,wsmo,/edge,_extra=_e)
	endif
	typs = [0,-1,1]
	ext = typs[1 + One_of(min,max,/nozero)]

	if wha[0] then begin
		for i = 0, siz[2]-1 do begin
			loc= Extrema(arr[*,i],min=min,max=max,thre=tre,sig=sig,num=num)
			if num gt 0 then pres[loc,i] = sig
		endfor
	endif
	if wha[1] then begin
		for i = 0, siz[1]-1 do begin
			loc = Extrema(arr[i,*],min=min,max=max,thre=tre,sig=sig,num=num)
			if num gt 0 then pres[i,loc] = sig
		endfor
	endif

	if keyword_set(cle) then begin
		if ext ge 0 then pres=pres < Img_exorcise(pres>0,4,sub=0,_extra=_e)
		if ext le 0 then pres=pres>(-Img_exorcise(-(pres<0),4,sub=0,_extra=_e))
	endif

	if ffl then begin
		res[2,*,*] = pres
		res[3,*,*] = 0
	endif else res = pres

	return, res
end