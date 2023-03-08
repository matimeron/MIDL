Function Mgauss_fun, p, q, grad = grd, hess = hes

;+
; NAME:
;		MGAUSS_FUN
; VERSION:
;		7.09
; PURPOSE:
;		Evaluates a sum of Gaussians with polynomial background.  Primary
;		purpose is to serve as evaluation function for fitting, in GAUSSFIT_MM.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = MGAUSS_FUN(P, Q [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the parameters of the function.  The length
;		of the vector must be divisible by 3.  The first 3 parameters are
;		evaluated (with Q)  quadratic background of the form of
;		P[0] + P[1]*Q + P[2]*Q^2.  Each additional triple is taken as parameters
;		of a Gaussian, in an [amplitude, center, sigma] order, i.e. it generates
;		a Gaussian of the form of P[3*N]*exp(-1/2*((Q - P[3*N+1])/P[3*N+2])^2).
;	Q
;		Numeric vector.  Serves as the X values for the generated function.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		Returns the evaluated function as a sum of quadratic background and one
;		or more gaussians.  The dimension of the result is the dimension of Q.
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
;		Straightforward, evaluation from definitions.  Calls ARREQ, CALCTYPE,
;		CAST, FPU_FIX and POLEVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2004 by Mati Meron.
;		Modified 10-SEP-2008 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if n_params() ne 2 then message, 'Wrong number of inputs!'
	typ = Calctype(p,q,0.)
	np = n_elements(p)
	if (np mod 3) ne 0 then message, 'Wrong number of parameters!'
	ngau = np/3 - 1
	nq = n_elements(q)
	wq = Cast(reform([q],nq),typ)
	gfl = arg_present(grd)
	hfl = arg_present(hes)

	res = Poleval(wq,p[0:2])
	if gfl then begin
		grd = make_array(nq,np,typ=typ)
		grd[*,0:2] = [[0*wq+1],[wq],[wq^2]]
	endif
	if hfl then hes = make_array(nq,np,np,typ=typ)

	for i = 0l, ngau-1 do begin
		j = 3*(i+1)
		pp = p[j:j+2]
		if not Arreq(pp,[0,0,0]) then begin
			z = (wq - pp[1])/pp[2]
			f = FPU_fix(exp(-z^2/2))
			res = res + pp[0]*f
			if gfl then grd[*,j:j+2]=[[f],[pp[0]/pp[2]*z*f],[pp[0]/pp[2]*z^2*f]]
			if hfl then begin
				hes[*,j,j+1] = (hes[*,j+1,j] = z*f/pp[2])
				hes[*,j,j+2] = (hes[*,j+2,j] = z^2*f/pp[2])
				hes[*,j+1,j+1] = pp[0]/pp[2]^2*(z^2-1)*f
				hes[*,j+1,j+2] = (hes[*,j+2,j+1] = pp[0]/pp[2]^2*z*(z^2-2)*f)
				hes[*,j+2,j+2] = pp[0]/pp[2]^2*z^2*(z^2-3)*f
			endif
		endif
	endfor

	if gfl then grd = Cast(grd,typ,typ,/fix)
	if hfl then hes = Cast(hes,typ,typ,/fix)

	return, Cast(res,typ,typ,/fix)
end