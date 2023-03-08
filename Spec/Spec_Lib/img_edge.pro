Function Img_edge, dat, wid, xonly= xon, yonly= yon, high=hi, low=lo, _extra= _e

;+
; NAME:
;		IMG_EDGE
; VERSION:
;		8.216
; PURPOSE:
;		Performes edge enhancement on an image.
; CATEGORY:
;		Array Function.
; CALLING SEQUENCE:
;		Result = IMG_EDGE( DAT, WID [, keywords])
; INPUTS:
;	DAT
;		Either a 2D [M,N] array, or the standard data representation 3D array
;		with dimensions [4,M,N] ([3,M,N] is also acceptable).
;	WID
;		The width of the edge enhancement window.  Can be given either as a
;		scalar (in which case it is applied to all the dimensions of the array,
;		or as a vector (in which case each entry applies to one dimension).  The
;		WIDTH entry(s) should be an odd number(s), if it is even the next higher
;		odd number(s) will be used.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/XONLY												| Only one of these
;		Switch.  Specifies edge enhancement in the X	| keywords may be used.
;		 direction only.								| If none is, full
;	/YONLY												| enhancement is
;		Switch.  Specifies edge enhancement in the Y	| performed.
;		direction only.
;	/HI													| Only one of these
;		Switch.  If set only positive result values		| keywords may be used.
;		(corresponding to high data areas) are kept.	| If none is, the full
;	/LO													| (pos. and neg.) value
;		Switch.  If set only negative result values		| set is returned.
;		(corresponding to low data areas) are kept.
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		Returns the edge enhancement result, in the same format as DAT.  In the
;		case of a full [4,M,N] input, both the data (DAT[2,*,*]) and errors 
;		(DAT[3,*,*]) are processed (with squared kernel applied to errors).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Edge enhancement using Savitzky-Golay second derivative approximation.
;		See EDGE_MM for details.  Calls EDGE_MM, FPU_FIX, ISNUM and ONE_OF, 
;		from MIDL. 
; MODIFICATION HISTORY:
;		Created 10-JAN-2014 by Mati Meron.
;-

	on_error, 1

	if Isnum(wid) then begin
		siz = size(dat)
		case siz[0] of
			2	:	ffl = 0
			3	:	ffl = 1
			else:	message, 'Not image data!'
		endcase

		go = [1,1,1]
		whi = One_of(yon,xon)
		go[whi] = 0
		if n_elements(wid) eq 1 then wwid = [wid,wid] else wwid = wid
		wwid = wwid*go[0:1] > 1

		res = dat
		hol = One_of(hi,lo)
		if ffl then begin
			res[2,*,*] = Edge_mm(res[2,*,*],wwid,/edge,_extra=_e)
			if siz[1] gt 3 then $
			res[3,*,*] = Edge_mm(res[3,*,*],wwid,/edge,/err,_extra=_e)
			if hol ge 0 then res[2,*,*] = (-1)^hol*res[2,*,*] > 0
		endif else begin
			res = Edge_mm(res,wwid,/edge,_extra=_e)
			if hol ge 0 then res = (-1)^hol*res > 0
		endelse
	endif else message, 'Missing width, cannot calculate!

	return, FPU_fix(res)
end