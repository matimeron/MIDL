Function PQR_df, pqr, d, type = typ

;+
; NAME:
;		PQR_DF
; VERSION:
;		8.61
; PURPOSE:
;		Evaluates diffuser transformation of PQR format optical data.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = PQR_DF( PQR, D [, TYPE = TYP])
; INPUTS:
;	PQR
;		1D PQR data, i.e. a 3 element vector or a [3,N] array.
;	D
;		Focal parameter, may be a scalar or a vector of length N, where N is the
;		second dimension of the PQR data (when present).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	TYPE
;		Integer scalar representing data type.  If given, the result is cast to
;		this type. Default output type is DOUBLE.
; OUTPUTS:
;		Returns the input PQR data transformed by a diffuser with parameter D.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The restrictions on the input data, as listed above.
; PROCEDURE:
;		Follows the procedure listed in the "PQR Optical Parameters" write-up,
;		in my archive (note, the diffuser parameter is denoted by "tau" over
;		there.  Calls CAST and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2018 by Mati Meron.
;-

	on_error, 1

	dim = size(pqr,/dim)
	if dim[0] eq 3 and n_elements(dim) le 2 then begin
		if n_elements(dim) eq 2 then dlen = dim[1] else dlen = 1
		res = (wpqr = reform(Cast(pqr,5)))
		wd = reform(Cast(d,5))
		if n_elements(wd) eq 1 then wd = wd[0] else $
		if n_elements(wd) ne dlen then message, 'Dimensional inconsistency!'
		res[0,*] = wpqr[0,*] + wd^2*wpqr[1,*]
		if Isnum(typ) then res = Cast(res,typ,typ)
	endif else message, 'Bad PQR input!'

	return, res
end