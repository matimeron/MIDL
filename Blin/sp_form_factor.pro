Function Sp_form_factor, q, r, qz = qz

;+
; NAME:
;		SP_FORM_FACTOR
; VERSION:
;		5.7
; PURPOSE:
;		Calculates the spherical scattering form factor.
; CATEGORY:
;		Mathematical, x-ray.
; CALLING SEQUENCE:
;		Result = SP_FORM_FACTOR( Q [, R] [,QZ = QZ])
; INPUTS:
;	Q
;		Numeric, arbitrary, represents the scattering Q-vector or (if QZ is
;		given), the in-plane part of the Q-vector, i.e. Q_XY.
; OPTIONAL INPUT PARAMETERS:
;	R
;		Numeric, scalar, the radius of the scattering sphere.  Defaults to 1.
; KEYWORD PARAMETERS:
;	QZ
;		Optional numeric scalar, the value of QZ.  If given, the scattering Q
;		is given by Q_scat = sqrt(Q^2 + QZ^2).  Defaults to 0.
; OUTPUTS:
;		Returns the value(s) of the form factor, in the same format as the
;		input Q.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, from definition.  Calls DEFAULT, FPU_FIX, ISNUM, and
;		SP_BESELJ from MIDL.
; MODIFICATION HISTORY:
;		Created 25-SEP-2005 by Mati Meron.
;		Modified 15-NOV-2006 by Mati Meron.  Added keyword QZ.
;-

	r = Default(r,1.)
	if Isnum(qz) then begin
		if n_elements(qz) eq 1 then wq = sqrt(q^2 + qz[0]^2) $
		else message, 'Q_z must be a scalar!'
	endif else wq = q
	qr = wq*r
	res = 0.*qr + 1.
	dum = where(qr ne 0, ndum)
	if ndum gt 0 then res[dum] = (3*Sp_beselj(qr[dum],1)/qr[dum])^2

	return, FPU_fix(res)
end