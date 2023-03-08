Function CAB1_fun, p, q, grad = grd, hess = hes

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
	if np eq 2 then begin
		x = p[0]
		y = p[1]
	endif else message, 'Wrong P input!'

	qsiz = size(q)
	if Arreq(qsiz[0:1],[2,3]) then begin
		nq = qsiz[2]
		a = Cast(reform(q[1,*]),typ)
		b = Cast(reform(q[2,*]),typ)
		c = Cast(reform(q[0,*]),typ)
	endif else message, 'Wrong Q input!'

	gfl = arg_present(grd)
	hfl = arg_present(hes)

	f = x^2 + x*y*a + y^2*b - x*c
	if gfl then begin
		dfx = 2*x + y*a -c
		dfy = x*a + 2*y*b
	endif
	if hfl then begin
		dfxx = 2
		dfyy = 2*b
		dfxy = a
	endif

	res = total(f^2)/2
	if gfl then grd = [total(f*dfx),total(f*dfy)]
	if hfl then begin
		dresxx = total(f*dfxx + dfx^2)
		dresyy = total(f*dfyy + dfy^2)
		dresxy = total(f*dfxy + dfx*dfy)
		hes = reform([[[dresxx,dresxy]],[[dresxy,dresyy]]])
	endif

	return, Cast(res,typ,typ,/fix)
end