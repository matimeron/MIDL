Function Qchain_fun, p, q, gval = gvl, der = der, grad = grd, hess = hes

;+
; NAME:
;		QCHAIN_FUN
; VERSION:
;		8.15
; PURPOSE:
;		Generates a function consisting of a set of quadratic arcs.
; CATEGORY:
;		Mathematical Function (specialized).
; CALLING SEQUENCE:
;		Result = QCHAIN_FUN (P, Q, GVAL = GVL [, /DER, keywords])
; INPUTS:
;	P
;		Numerical vector, mandatory.  Corresponds to the CURV values in QCHAIN
;		(see there).
;	Q
;		Numerical vector, mandatory.  Corresponds to the X input of QCHAIN.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GVAL
;		Numeric vector (at least 2 elements), containing the "grid" values.
;		Mandatory.
;	/DER
;		Switch.  If set explicitly to zero, a QCHAIN function is returned, else 
;		the first derivative of a QCHAIN function is returned.
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		If DER is set to 0, returns a QCHAIN function of Q with curvatures P,
;		else returns the derivative of such function with respect to Q.
; OPTIONAL OUTPUT PARAMETERS:
;	GRAD
;		Returns the gradient of the result with respect to P, as a 2D matrix of 
;		dimension [Nq,Np]
;	HESS
;		Returns the Hessian (second derivative) of the result with respect to P,
;		as a 3D array of dimension [Nq,Np,Np]
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		QCHAIN_FUN is a front end to the function QCHAIN in MIDL, adapted for
;		use with the optimization function FGH_EXT.  
;		Calls CALCTYPE, DEFAULT and QCHAIN, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(p,q,0.)
	np = n_elements(p)
	nq = n_elements(q)
	der = Default(der,1,/dtyp)

	res = Qchain(q,curv=p,gval=gvl,der=der)
	if arg_present(grd) then begin
		grd = make_array(nq,np,typ=typ)
		pp = make_array(np,typ=typ)
		pp[np-1] = 1
		for i = 0l, np-1 do begin
			pp = shift(pp,1)
			grd[*,i] = Qchain(q,curv=pp,gval=gvl,der=der)
		endfor
	endif
	if arg_present(hes) then hes = make_array(nq,np,np,typ=typ)

	return, res
end