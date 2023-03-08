Function Img_smooth, dat, wid, xonly = xon, yonly = yon, kernel= ker, _extra= _e

;+
; NAME:
;		IMG_SMOOTH
; VERSION:
;		8.33
; PURPOSE:
;		Performes smoothing (binomial by default) of an image.
; CATEGORY:
;		Array Function.
; CALLING SEQUENCE:
;		Result = IMG_SMOOTH( DAT, WID [, keywords])
; INPUTS:
;	DAT
;		Either a 2D [M,N] array, or the standard data representation 3D array 
;		with dimensions [4,M,N] ([3,M,N] is also acceptable).
;	WID
;		The width of the smoothing window.  Can be given either as a scalar (in
;		which case it is applied to all the dimensions of the array, or as a
;		vector (in which case each entry applies to one dimension).  The WIDTH
;		entry(s) should be an odd number(s), if it is even the next higher odd
;		number(s) will be used.
;		
;		Note:	The binomial distribution is the discrete equivalent of a 
;				Gaussian with sigma = sqrt(wid-1)/2.  Thus, the effective
;				smoothing width is proportional to sqrt(wid), not wid.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/XONLY
;		Switch.  Specifies smoothing in the X direction only.
;	/YONLY
;		Switch.  Specifies smoothing in the Y direction only.
;
;		Note:	Only one of these keywords may be used.  If none is, full 
;				smoothing is performed.
;	KERNEL
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to embedded routines.  Not to be
;		used directly.
;
;		Note:	Of special interest are the three keywords BINCOEF, SAVGOL and
;				BOX of SMOOTH_MM, specifying the type of smoothing kernel to be
;				used.  At most one of the three may be specified, if non is, the
;				default is BINCOEF (invoking binomial coefficients).
; OUTPUTS:
;		Returns the smoothed result, in the same format as DAT.  In the case of
;		a full [4,M,N] input, both the data (DAT[2,*,*]) and errors (DAT[3,*,*])
;		are smoothed (with squared smoothing applied to errors).
;		
;		Note:	If the data is not equispaced, it is first reformated into 
;				equalized coordinates and then, following the smoothing, 
;				reformatted back into original coordinates. 
; OPTIONAL OUTPUT PARAMETERS:
;	KERNEL
;		Returns the kernel used for smoothing.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward binomial smoothing, see BSMOOTH_MM for details.  Calls
;		FPU_FIX, IMG_EQUALIZE, IMG_REFORM, ISNUM, ONE_OF, SMOOTH_MM and TOLER,
;		from MIDL. 
; MODIFICATION HISTORY:
;		Created 10-JAN-2014 by Mati Meron.
;		Modified 5-OCT-2014 by Mati Meron.  Internal changes.  Added keyword 
;		KERNEL.
;-

	on_error, 1

	if Isnum(wid) then begin
		siz = size(dat)
		case siz[0] of
			2	:	ffl = 0
			3	:	ffl = 1
			else:	message, 'Not an image data!'
		endcase

		go = [1,1,1]
		whi = One_of(yon,xon)
		go[whi] = 0
		if n_elements(wid) eq 1 then wwid = [wid,wid] else wwid = wid
		wwid = wwid*go[0:1] > 1

		if ffl then begin
			oxv = reform(dat[0,*,0])
			oyv = reform(dat[1,0,*])
			res = Img_equalize(dat,_extra=_e)
			res[2,*,*] = Smooth_mm(res[2,*,*],wwid,/edge,ker=ker,_extra=_e)
			if siz[1] gt 3 then $
			res[3,*,*] = Smooth_mm(res[3,*,*],wwid,/edge,/err,_extra=_e)
			res = Img_reform(res,8*Toler(dat),xval=oxv,yval=oyv)
		endif else res = Smooth_mm(dat,wwid,/edge,ker=ker,_extra=_e)
	endif else res = dat

	typ = Calctype(res) > 4
	ker = Cast(ker,typ,typ)

	return, FPU_fix(res)
end