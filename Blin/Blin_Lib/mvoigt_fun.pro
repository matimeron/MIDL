Function Mvoigt_fun, p, q, grad = grd, hess = hes

;+
; NAME:
;		MVOIGT_FUN
; VERSION:
;		8.0
; PURPOSE:
;		Evaluates a sum of Voigt functions with polynomial background.  Primary
;		purpose is to serve as evaluation function for fitting, in PEAK_FIT.
; CATEGORY:
;		Mathematical, general.
; CALLING SEQUENCE:
;		Result = MVOIGT_FUN(P, Q [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the parameters of the function.  The length
;		of the vector - 3 must be divisible by 4.  The first 3 parameters are
;		evaluated (with Q)  quadratic background of the form of
;		P[0] + P[1]*Q + P[2]*Q^2.  Each additional 4-tuple is taken as 
;		parameters of a Voigtian, in an [amplitude, center, sigma, gamma] order,
;		i.e. it generates a Voigtian of the form of 
;		P[M]*VOIGT(Q - P[M+1],P[M+2],P[M+3])/VOIGT(0,P[M+2],P[M+3]) where
;		M = 3 + 4*N (N = 0, 1, ...)
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
;		The elements of P corresponding to "sigma" and "gamma" of successive
;		Voigtians are supposed to be positive and if they're not such, they'll
;		be changed to positive.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, evaluation from definitions.  Calls ARREQ, CALCTYPE,
;		CAST, NERFC_MM and POLEVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-2010 by Mati Meron.
;		Modified 5-JAN-2011 by Mati Meron.  Internal changes.
;-

	on_error, 1

	if n_params() ne 2 then message, 'Wrong number of inputs!'
	ctyp = Calctype(p,q,complex(0))
	otyp = Calctype(p,q,0.)
	np = n_elements(p)
	if ((np-3) mod 4) ne 0 then message, 'Wrong number of parameters!'
	ngau = (np-3)/4
	nq = n_elements(q)
	wq = Cast(reform([q],nq),ctyp)
	gfl = arg_present(grd)
	hfl = arg_present(hes)

	res = Poleval(wq,p[0:2])
	if gfl then begin
		grd = make_array(nq,np,typ=ctyp)
		grd[*,0:2] = [[0*wq+1],[wq],[wq^2]]
	endif
	if hfl then hes = make_array(nq,np,np,typ=ctyp)

	ic = Cast(dcomplex(0,1),ctyp,ctyp)
	st = Cast(sqrt(2d),ctyp,ctyp)
	ec = Cast(1/sqrt(!dpi),ctyp,ctyp)
	for i = 0l, ngau-1 do begin
		j = 3 + 4*i
		p[j+2:j+3] = abs(p[j+2:j+3])
		pp = p[j:j+3]
		if not Arreq(pp,[0,0,0,0]) then begin
			z = (pp[3] - ic*(wq - pp[1]))/(st*pp[2])
			w = pp[3]/(st*pp[2])
			fz = Nerfc_mm(z)
			fw = Nerfc_mm(w)
			a = fz/fw
			res = res + pp[0]*a
			if gfl then begin
				dfz = 2*(z*fz - ec)
				dfw = 2*(w*fw - ec)
				bz = dfz/fw
				bw = dfw/fw
				prat = pp[0]/pp[2]
				grd[*,j] = a
				grd[*,j+1] = prat*ic/st*bz
				grd[*,j+2] = prat*(w*bw*a - z*bz)
				grd[*,j+3] = prat/st*(bz - bw*a)
			endif
			if hfl then begin
				ddfz = (2 + 4*z^2)*fz - 4*ec*z
				ddfw = (2 + 4*w^2)*fw - 4*ec*w
				cz = ddfz/fw
				cw = ddfw/fw
				pprat = prat/pp[2]
				hes[*,j,j+1] = (hes[*,j+1,j] = 1/pp[2]*ic/st*bz)
				hes[*,j,j+2] = (hes[*,j+2,j] = 1/pp[2]*(w*bw*a - z*bz))
				hes[*,j,j+3] = (hes[*,j+3,j] = 1/pp[2]/st*(bz - bw*a))
				hes[*,j+1,j+1] = -pprat/2*cz
				hes[*,j+1,j+2] = (hes[*,j+2,j+1] = $
				pprat*ic/st*(w*bw*bz - bz - z*cz))
				hes[*,j+1,j+3] = (hes[*,j+3,j+1] = pprat*ic/2*(cz - bw*bz))
				hes[*,j+2,j+2] = $
				pprat*(2*(1 - w*bw)*(z*bz - w*bw*a) + z^2*cz - w^2*cw*a)
				hes[*,j+2,j+3] = (hes[*,j+3,j+2] = $
				pprat/st*((w+z)*bw*bz - 2*w*bw^2*a + bw*a - bz + w*cw*a - z*cz))
				hes[*,j+3,j+3] = pprat/2*(2*bw*(bw*a - bz) + cz - cw*a)
			endif
		endif
	endfor

	if gfl then grd = Cast(grd,otyp,otyp,/fix)
	if hfl then hes = Cast(hes,otyp,otyp,/fix)

	return, Cast(res,otyp,otyp,/fix)
end