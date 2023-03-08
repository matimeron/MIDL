Function Ref_fun, p, z, q, grad = grd, hess = hes

;+
; NAME:
;		REF_FUN
; VERSION:
;		4.5
; PURPOSE:
;		Evaluates the relative (to Fresnell) reflectivity of a graded slab.
;		Primary purpose is to serve as evaluation function for fitting, in
;		REF_SOLVE.
; CATEGORY:
;		Mathematical, x-ray specific.
; CALLING SEQUENCE:
;		Result = REF_FUN(P [, Z, Q] [, keywords])
; INPUTS:
;	P
;		Numeric vector, containing the forward differences of the relative (to
;		bulk) density function.  The relative density is assumed to be changing
;		stepwise at the locations Z (see below), remaining constant between
;		these locations.  Thus, P is the vector:
;
;			[Dens[Z[1]] - Dens[Z[0]], Dens[Z[2]] - Dens[Z[1]], ...]
;
;	Z
;		Numeric vector, the set of depths where jumps in density occur.
;	Q
;		Numeric vector.  The Q values for which the reflectivity is to be
;		calculated.
;
;	Note 1 :	The P input is mandatory.  Z and Q are mandatory if the common
;				block M_REF_KEEP (see below) has not been initialized, optional
;				otherwise.  If the common block has been initialized and Z, Q
;				are not provided, their relevant combination is supplied from
;				the common block.  If Z, Q are provided, the common block is
;				reinitialized.
;	Note 2 :	If and when Z is provided, it must be of same length as P.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GRAD
;		Optional output, see below.
;	HESS
;		Optional output, see below.
; OUTPUTS:
;		Returns the evaluated function as an approximated square of the
;		Fourier transform of the density gradient.  The dimension of the result
;		is the dimension of Q.
; OPTIONAL OUTPUT PARAMETERS:
;	GRAD
;		Returns the gradient of the result as a 2D matrix of dimension [Nq,Np]
;	HESS
;		Returns the Hessian (second derivative) of the result as a 3D array
;		of dimension [Nq,Np,Np]
; COMMON BLOCKS:
;		M_REF_KEEP.  Contains the following:
;
;		TYP	:	Data type code, the highest of the types of P, Z, Q and no
;				lower than 4.
;		NZ :	The length of Z (and P).
;		NQ :	The length of Q.
;		MARR :	An [NQ,NP,NP] complex array.  The value of the [r,s,t]
;				component is exp(i*Q[r]*(Z[s] - Z[t]))
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		As mentioned in Notes 1-2, above.
; PROCEDURE:
;		Evaluates the relative reflectivity as an absolute value squared of the
;		Fourier transform of the density gradient (see Nielsen, p. 83).
;		Calls CALCTYPE, CAST, ISNUM, REAL_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2003 by Mati Meron.
;-

	common m_ref_keep, typ, nz, nq, marr
;	on_error, 1

	case (n_params() < 3) of
		1	:	if Isnum(marr) then rfl = nz eq 1 $
				else message, 'Not initialized!'
		3	:	begin
					typ = Calctype(0.,z,p,q)
					nz = n_elements(z)
					rfl = nz eq 1
					if n_elements(p) ne nz then $
					message, 'P-Z dimensional mismatch!'
					nq = n_elements(q)
					eiqz = exp(dcomplex(0,1)*[q]#[z])
					marr = make_array(nq,nz,nz,typ=Type(eiqz),val=1)
					if rfl then marr = reform(marr,nq,nz,nz)
					for i = 0l, nz - 1 do begin
						marr[*,*,i] = marr[*,*,i]*eiqz
						marr[*,i,*] = marr[*,i,*]*conj(eiqz)
					endfor
				end
		 else:	message, 'Wrong number of inputs!'
	endcase

	if arg_present(grd) then begin
		if rfl then tarr = reform(marr,nq,nz,nz) else tarr = marr
		for i = 0l, nz - 1 do tarr[*,i,*] = p[i]*tarr[*,i,*]
		grd = 2*Cast(Real_mm(total(tarr,2)),typ,typ,/fix)
		if rfl then grd = reform(grd,nq,nz)
	endif
	if arg_present(hes) then hes = 2*Cast(Real_mm(marr),typ,typ,/fix)
	if rfl then hes = reform(hes,nq,nz,nz)

	parr = p#p
	if rfl then tarr = reform(marr,nq,nz,nz) else tarr = marr
	for i = 0l, nq - 1 do tarr[i,*,*] = parr*tarr[i,*,*]

	if rfl then prt = reform(total(tarr,3),nq,nz) else prt = total(tarr,3)
	return, Cast(Real_mm(total(prt,2)),typ,typ,/fix)
end