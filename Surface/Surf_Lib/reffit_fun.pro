Function Reffit_fun, p, q, grad = grd, hess = hes

;+
; NAME:
;		REFFIT_FUN
; VERSION:
;		7.0
; PURPOSE:
;		Evaluates the logarithm of a reflectivity function.  Primary purpose
;		is to serve as evaluation function for fitting, in REFFIT.
; CATEGORY:
;		Mathematical, x-ray specific.
; CALLING SEQUENCE:
;		Result = REFFIT_FUN(P, Q [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the parameters of the function.  The full
;		length of the vector is 5, though a partial vector can be given.  The
;		parameters, in order, are:
;
;		P[0] :	q_c^2, i.e. square of the critical q-value.
;		P[1] :	4*k*mu, where k is the k-value and mu the absorption
;				coefficient (in same units as q and k).
;		P[2] :	The offset of the provided Q values from the "true" Q.  The
;				true value is obtained by *adding* the offset to the provided
;				values.
;		P[3] :	The square of the surface roughness (in units inverse to those
;				of q and k.
;		P[4] :	Logarithm of the normalization coefficient.
;
;		If some of the parameters are missing, they're replaced by 0 with the
;		exception of P[1] which is replaced by TOLER, i.e. the machine
;		smallness parameter.
;	Q
;		Numeric vector.  The Q values for which the reflectivity is to be
;		calculated.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		Returns the evaluated function, see PROCEDURE below.
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
;		Evaluates Fresnel reflectivity, including possible q-offset, roughness
;		and normalization coefficient.  Calls CALCTYPE, CAST, IMAGINARY_MM,
;		REAL_MM and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JAN-2004 by Mati Meron.
;		Modified 10-MAR-2008 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if n_params() ne 2 then message, 'Wrong number of inputs!'
	typ = Calctype(0.,p,q)
	defp = [0.,Toler(p),0,0,0]
	if n_elements(p) lt 5 then wp = [p,defp[n_elements(p):*]] else wp = p
	ic = dcomplex(0,1)

	z = wp[0] - ic*wp[1]
	wq = q + wp[2]
	izq = (zq = sqrt(wq^2 - z))
	zer = where(zq eq 0, nzer,comp = nozer,ncomp = nnozer)
	if nzer gt 0 then izq[zer] = 0
	if nnozer gt 0 then izq[nozer] = 1/zq[nozer]
	res = 2*Real_mm(alog((wq-zq)/(wq+zq))) - wp[3]*wq^2 + wp[4]

	if arg_present(grd) then begin
		dum = 2*wq/z*izq
		grd = make_array(n_elements(q),5,typ=typ)
		grd[*,0] = Real_mm(dum)
		grd[*,1] = Imaginary_mm(dum)
		grd[*,2] = -2*(2*Real_mm(izq) + wp[3]*wq)
		grd[*,3] = -wq^2
		grd[*,4] = 1
	endif

	if arg_present(hes) then begin
		dumf = wq/z*izq*(izq^2 - 2/z)
		dums = 2*izq^3
		hes = make_array(n_elements(q),5,5,typ=typ)
		hes[*,0,0] = Real_mm(dumf)
		hes[*,0,1] = (hes[*,1,0] = Imaginary_mm(dumf))
		hes[*,1,1] = -Real_mm(dumf)
		hes[*,0,2] = (hes[*,2,0] = -Real_mm(dums))
		hes[*,1,2] = (hes[*,2,1] = -Imaginary_mm(dums))
		hes[*,2,2] = 2*(Real_mm(wq*dums) - wp[3])
		hes[*,2,3] = (hes[*,3,2] = -2*wq)
	endif

	return, Cast(res,typ,typ,/fix)
end