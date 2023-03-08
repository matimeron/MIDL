Function Mat_ident, n

;+
; NAME:
;		MTR_IDENT
; VERSION:
;		8.72
; PURPOSE:
;		Generates an identity matrix.
; CATEGORY:
;		Math Utility.
; CALLING SEQUENCE:
;		Result = MAT_IDENT ( N)
; INPUTS:
;	N
;		Positive scalar, mandatory.  Rounded internally to nearest integer.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns an NxN identity matrix. Output type will follow input, but no
;		lower than FLOAT.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the restrictions on N (see above).
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, DIAGOARR and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2021 by Mati Meron.
;-

	on_error, 1

	if Isnum(n) and n_elements(n) eq 1 then begin
		nn = round(n)
		if nn ge 1 then begin
			typ = Calctype(n,0.)
			one = Cast(1,typ)
			res = Diagoarr(replicate(one,nn))
			if nn eq 1 then res = res[0]
		endif else message, 'Dimension must be >= 1!'
	endif else message, 'Bad or missing input!'

	return, res
end