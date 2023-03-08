Function Det_fun, p, q, grad = grd, hess = hes

;+
; NAME:
;		DET_FUN
; VERSION:
;		4.8
; PURPOSE:
;		Evaluates the logarithm of a detector count function.  Primary purpose
;		is to serve as evaluation function for fitting, in DETFIT.
; CATEGORY:
;		Mathematical, x-ray specific.
; CALLING SEQUENCE:
;		Result = DET_FUN(P, Q [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the parameters of the function.  The full
;		length of the vector is 3, though a partial vector can be given.  The
;		parameters, in order, are:
;
;		P[0] :	Natural log of the full (absent absorbers) intensity.
;		P[1] :	Natural log of the decrease per absorber.  Note, this value is
;				always negative.
;		P[2] :	The time width of the detector pulse.
;
;		If some of the parameters are missing, they're replaced by 0.
;	Q
;		Numeric vector.  each entry represents number of absorbers.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		Returns the evaluated function.
; OPTIONAL OUTPUT PARAMETERS:
;	GRAD
;		Returns the gradient of the result as a 2D matrix of dimension [Nq,Np]
;	HESS
;		Returns the Hessian (second derivative) of the result as a 3D array
;		of dimension [Nq,Np,Np]
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Evaluates the log of the detector count function, given as:
;		res = P[0] + P[1]*Q - P[2]*exp(P[0] + P[1]*Q)
;		Calls CALCTYPE and CAST from MIDL.
; MODIFICATION HISTORY:
;		Created 20-FEB-2004 by Mati Meron.
;-

	on_error, 1

	if n_params() ne 2 then message, 'Wrong number of inputs!'
	typ = Calctype(0.,p,q)
	defp = fltarr(3)
	if n_elements(p) lt 3 then wp = [p,defp[n_elements(p):*]] else wp = p

	lr = wp[0] + wp[1]*q
	elr = -2*exp(lr)
	res = lr + wp[2]*elr

	if arg_present(grd) then begin
		grd = make_array(n_elements(q),3,typ=typ)
		grd[*,0] = 1 + wp[2]*elr
		grd[*,1] = q*grd[*,0]
		grd[*,2] = elr
	endif

	if arg_present(hes) then begin
		hes = make_array(n_elements(q),3,3,typ=typ)
		hes[*,0,0] = wp[2]*elr
		hes[*,0,1] = (hes[*,1,0] = wp[2]*q*elr)
		hes[*,1,1] = wp[2]*q^2*elr
		hes[*,0,2] = (hes[*,2,0] = elr)
		hes[*,1,2] = (hes[*,2,1] = q*elr)
	endif

	return, Cast(res,typ,typ,/fix)
end