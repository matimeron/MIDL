Function X2_fun, p, t, grad = grd, hess = hes

;+
; NAME:
;		X2_fun
; VERSION:
;		4.9
; PURPOSE:
;		Evaluates a functional approximation to the values of <x^2> in single
;		file diffusion.  Primary purpose is to serve as evaluation function for
;		fitting, in X2_FIT.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = X2_FUN(P, T [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the 3 parameters of the function.
;	T
;		Numeric vector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		Returns the evaluated function  The dimension of the result is the
;		dimension of T.
; OPTIONAL OUTPUT PARAMETERS:
;	GRAD
;		Returns the gradient of the result as a 2D matrix of dimension [Nt,Np]
;	HESS
;		Returns the Hessian (second derivative) of the result as a 3D array
;		of dimension [Nt,Nt,Np]
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Evaluates the function as
;
;		<X^2> = 1/(P[0]*T^(-1/2) + P[1]*T^(-1)) + P[2]
;
;		Calls CALCTYPE, CAST, and FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 10-MAY-04 by Mati Meron.
;-

	on_error, 1

	if n_params() ne 2 then message, 'Wrong number of inputs!'
	typ = Calctype(p,t,0.)
	np = n_elements(p)
	if np ne 3 then message, '3 parameters required!
	gfl = arg_present(grd)
	hfl = arg_present(hes)

	nt = n_elements(t)
	wt = Cast(reform([t],nt),typ)
	res = 0.*wt + p[2]
	if gfl then begin
		grd = make_array(nt,np,typ=typ)
		grd[*,2] = 1
	endif
	if hfl then hes = make_array(nt,nt,np,typ=typ)

	pos = where(wt gt 0, npos)
	if npos gt 0 then begin
		ith = 1/sqrt(wt[pos])
		pres = 1/(ith*(p[0] + p[1]*ith))
		res[pos] = res[pos] + pres
		if gfl then begin
			fac = -pres^2
			grd[pos,0] = (grd[pos,1] = -pres^2*ith)
			grd[pos,1] = grd[pos,1]*ith
			grd = Cast(grd,typ,typ,/fix)
		endif
		if hfl then begin
			hes[pos,0,0] = (hes[pos,0,1] = (hes[pos,1,0] = (hes[pos,1,1] = $
			2*(pres*ith)^3)))
			hes[pos,0,0] = hes[pos,0,0]/ith
			hes[pos,1,1] = hes[pos,1,1]*ith
			hes = Cast(hes,typ,typ,/fix)
		endif
	endif

	return, FPU_fix(res)
end