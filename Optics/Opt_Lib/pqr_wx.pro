Function PQR_wx, pqr, x, lam = lam, scale = sca, type = typ, tsq = tsq

;+
; NAME:
;		PQR_WX
; VERSION:
;		8.61
; PURPOSE:
;		Evaluates spatial aperture transformation of PQR format optical data.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = PQR_WX( PQR, X [, keywords])
; INPUTS:
;	PQR
;		1D PQR data, i.e. a 3 element vector or a [3,N] array.
;	X
;		Aperture size, may be a scalar or a vector of length N, where N is the
;		second dimension of the PQR data (when present).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	LAM
; 		Wavelength.  May be a scalar or a vector of length N, where N is the
;		second dimension of the PQR data (when present).  If given, diffraction
;		effects are included in the evalution.
;
;		Note:	The wavalength needs to be in the same units as the aperture
;		size, multiplied be the relative scaling factor from longitudinal to
;		transverse coordinates.
;	SCALE
;		Character string specifying whether the "tranverse parameters" are given
;		using the SIGMA (rms size) HALF WIDTH or FULL WIDTH (rms*sqrt(4*pi))
;		standard.  Input, if given, must be one of "sigma", "half", "full" (only
;		first 2 characters are needed).  Default is SIGMA.
;		If LAM is not given, SCALE has no effect.
;	TYPE
;		Integer scalar representing data type.  If given, the result is cast to
;		this type. Default output type is DOUBLE.
;	TSQ
;		Optional output, see below.
; OUTPUTS:
;		Returns the input PQR data transformed by a spatial aperture of size X,
;		optionally with diffraction effects included.
; OPTIONAL OUTPUT PARAMETERS:
;	TSQ
;		Returns the squared transmission factor of the aperture, as a vector of
;		length N.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The restrictions on the input data, as listed above.
; PROCEDURE:
;		Follows the procedure listed in the "PQR Optical Parameters" write-up,
;		in my archive.  Calls CAST, ISNUM and STRMATCH_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2018 by Mati Meron.
;-

	on_error, 1

	dim = size(pqr,/dim)
	if dim[0] eq 3 and n_elements(dim) le 2 then begin
		if n_elements(dim) eq 2 then dlen = dim[1] else dlen = 1
		res = (wpqr = reform(Cast(pqr,5)))
		wx = reform(Cast(x,5))
		if n_elements(wx) eq 1 then wx = wx[0] $
		else if n_elements(wx) ne dlen then message,'Dimensional inconsistency!'
		if Isnum(lam) then begin
			wlam = lam
			if n_elements(wlam) eq 1 then wlam = wlam[0] $
			else if n_elements(wlam) ne dlen then message, $
			'Lambda dimensional inconsistency!'
			posib = ['sigma','half','full']
			fac = [4*!dpi, 2, 1]
			whi = Strmatch_mm(sca,posib,2) > 0
			wlam = wlam/fac[whi]
		endif else wlam = 0
		tsq = wx^2/(wx^2 + wpqr[1,*])
		res[0,*] = tsq*wpqr[0,*] + (1-tsq)*wlam^2
		res[1,*] = tsq*wpqr[1,*]
		res[2,*] = tsq*wpqr[2,*]
		if Isnum(typ) then res = Cast(res,typ,typ)
	endif else message, 'Bad PQR input!'

	return, res
end