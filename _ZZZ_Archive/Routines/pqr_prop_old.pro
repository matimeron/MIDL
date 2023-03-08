Function PQR_prop_old, psq, qsq, r, f = f, w = w, l = l, sler = s, $
	rpsq = rpsq, rqsq = rqsq, rr = rr, tsq = tsq, u = u, v = v

;+
; NAME:
;		PQR_PROP
; VERSION:
;		8.6
; PURPOSE:
;		Evaluates beam propagation in PQR representation.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		result = PQR_PROP, PSQ [, QSQ, R] [, F = F, W = W, L = L], [keywords].
; INPUTS:
;	PSQ
;		Value (or values) of P^2 (or of all the parameters).
;	QSQ
;		Value (or values) of Q^2.
;	R
;		Value (or values) of R.
;
;		Note 1:	If a single input is provided, it is taken to be a "packed"
;				representation of all three parameters.  In this case the input
;				must be one of
;					a)	A 3-element vector or a [3,N] array.
;					b)	A 6-element vector or a [6,N] array.  In this case the
;						input is assumed "doubled", with the first 2-columns
;						being PSQ values, the second two columns QSQ values and
;						the last 2 columns R values.  This option exists for
;						dealing with both transverse beam directions at once.
;		Note2:	If all three inputs are provided they must all scalars, all
;				vectors of same lenght, or a mix of scalars and vectors of same
;				length.  Any other combination is not acceptable.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;
;	Important note:
;		The formalism used here decouples "longitudinal parameters", such as F
;		and L, and transverse parameters, such as W.  Thus they need not be
;		given in same units, but they need to be consistent with the parameters
;		used in the evaluation of PSQ, QSQ and R.  Usually the longitudinal
;		parameters are given in meters and the transverse ones in mm and mr (for
;		angles).  This means that the slope error SLER, which is a transverse
;		parameter, needs to be given in mr (not microradians) as well.
;	F
;		Focal Length.  Default value is infinite (i.e. no focusing)
;	W
;		Aperture size.  Must be in same format (sigma or full width) as the
;		input data.  Default value is infinite, (i.e. no focusing).
;	L
;		Propagation distance.  Default value is zero (more accurately, a very
;		small value).
;
;		Note:	When the input is a [3,N] array, F, W and L (if defined) must
;				be scalars or vectors of length N.
;				When the input is a [6,N] array, F, W and L must be scalars,
;				vectors of length 2, or [2,N] arrays.
;	SLER
;		Scattering slope error.  Must be in same format (sigma or full width)
;		as the input data.  Default value is 0.
;
;		Note:	If the input is a [3,N] array, SLER (if defined) must be a
;				scalar.
;				If the input is a [6,N] array, SLER (if defined) may be a
;				scalar, in which case it is applied only to the second
;				dimension, or a 2-element vector, in which case the first value
;				is applied to the first dimension and the second to the second.
;	RPSQ
;		Optional output, see below.
;	RQSQ
;		Optional output, see below.
;	RR
;		Optional output, see below.
;	TSQ
;		Optional output, see below.
;	U
;		Optional output, see below.
;	V
;		Optional output, see below.
; OUTPUTS:
;		Returns the values of the P^2, Q^2 and R parameters, following focusing,
;		slitting and propagation, in a "packed" form, i.e. either a 3-element
;		vector or a [3,N] array.
; OPTIONAL OUTPUT PARAMETERS:
;	RPSQ
;		Returns the value(s) of the propagated P^2 parameter.
;	RQSQ
;		Returns the value(s) of the propagated Q^2 parameter.
;	RR
;		Returns the value(s) of the propagated R parameter.
;	TSQ
;		Returns the value(s) of the T^2 parameter (T is the transmission factor
;		of the aperture).
;	U
;		Returns the value(s) of the U parameter (see writeup for explanation).
;	V
;		Returns the value(s) of the V parameter (see writeup for explanation).
;		Note that V = TSQ*U
;
;		Note:	If the "doubled input" option is used, output parameters U and V
;		are set to !NULL.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		No restriction, but the accuracy may be reduced for L = 0 (or L not
;		provided).
; PROCEDURE:
;		Follows the derivations in the PQR writeup.  Details elsewhere.  Calls
;		CALCTYPE, CAST, CODIMS and SIGN, from MIDL.  May call itself recursively
; MODIFICATION HISTORY:
;		Created 15-JUN-2016 by Mati Meron.
;		Modified 5-AUG-2016 by Mati Meron.  Optional output option changes.
;		Modified 20-DEC-2017 by Mati Meron.  Optional output option changes.
;		Modified 25-DEC-2017 by Mati Meron.  Added SLER for slope error
;		effects.  Previously these effects were evaluated by a separate routine.
;-

	on_error, 1

	typ = Calctype(0.,psq,qsq,r,def=4)
	eps = (machar()).epsneg

	skipfl = 0
	case n_params() of
		1	:	begin
					siz = size(psq,/dim)
					if siz[0] eq 3 then begin
						tem = Cast(psq,5)
						case n_elements(siz) of
							1	:	begin
										wpsq = tem[0]
										wqsq = tem[1]
										wr = tem[2]
										dim = 1
									end
							2	:	begin
										wpsq = reform(tem[0,*])
										wqsq = reform(tem[1,*])
										wr = reform(tem[2,*])
										dim = siz[1]
									end
							else:	message, 'Bad input!'
						endcase
					endif else begin
						if siz[0] eq 6 then begin
							if n_elements(f) ne 0 then begin
								case (size(f,/dim))[0] of
									0	:	ff = (sf = f)
									2	:	begin
												ff = f[0,*]
												sf = f[1,*]
											end
									else:	message, 'Bad F dimensions!
								endcase
							endif
							if n_elements(w) ne 0 then begin
								case (size(w,/dim))[0] of
									0	:	fw = (sw = w)
									2	:	begin
										fw = w[0,*]
										sw = w[1,*]
									end
									else:	message, 'Bad W dimensions!
								endcase
							endif
							if n_elements(l) ne 0 then begin
								case (size(l,/dim))[0] of
									0	:	fl = (sl = l)
									2	:	begin
										fl = l[0,*]
										sl = l[1,*]
									end
									else:	message, 'Bad L dimensions!
								endcase
							endif
							case n_elements(s) of
								0	:
								1	:	begin
											fs = 0
											ss = s[0]
										end
								2	:	begin
											fs = s[0]
											ss = s[1]
										end
								else:	message, 'Bad slope error input!'
							endcase
							fres = PQR_prop_old(psq[[0,2,4],*], $
								f=ff,w=fw,l=fl,sler=fs,tsq=ftsq)
							sres = PQR_prop_old(psq[[1,3,5],*], $
								f=sf,w=sw,l=sl,sler=ss,tsq=stsq)
							rsiz = size(fres)
							rsiz[1] = 6
							res = make_array(size=rsiz)
							res[[0,2,4],*] = fres
							res[[1,3,5],*] = sres
							if arg_present(rpsq) then rpsq = res[0:1,*] $
							else rpsq = !null
							if arg_present(rqsq) then rqsq = res[2:3,*] $
							else rqsq = !null
							if arg_present(rr) then rr = res[4:5,*] $
							else rr = !null
							if arg_present(tsq) then $
							tsq = transpose([[ftsq],[stsq]]) else tsq= !null
							u = (v = !null)
							skipfl = 1
						endif else message, 'Bad input!'
					endelse
				end
		3	:	begin
					if Codims(psq,qsq,r,dim=dim) then begin
						wpsq = Cast(psq,5)
						wqsq = Cast(qsq,5)
						wr = Cast(r,5)
					endif else message, 'Inconsistent input dimensions!'
				end
		else:	message, 'Bad input!'
	endcase
	if skipfl then return, res

	if dim gt 1 then begin
		if n_elements(wpsq) eq 1 then wpsq = replicate(wpsq,dim)
		if n_elements(wqsq) eq 1 then wqsq = replicate(wqsq,dim)
		if n_elements(wr) eq 1 then wr = replicate(wr,dim)
	endif

	nfwl = [n_elements(f),n_elements(w),n_elements(l)]
	if dim gt 0 then begin
		dum = where(nfwl gt 1 and nfwl ne dim, ndum)
		if ndum eq 0 then rdim = dim else message, 'Dimensional inconsistency!'
	endif else begin
		dum = where(nfwl gt 1, ndum)
		if ndum gt 0 then begin
			rdim = min(nfwl[dum], max=hi)
			if rdim lt hi then message, 'Dimensional inconsistency!'
		endif else rdim = 1
	endelse
	rfl = rdim gt 1

	case nfwl[0] of
		0	:	inf = 0d
		1	:	inf = 1d/(abs(f[0]) > eps)*Sign(f[0],/noz)
		else:	inf = 1d/(abs(f) > eps)*Sign(f,/noz)
	endcase
	if rfl and nfwl[0] le 1 then inf = replicate(inf,rdim)

	case nfwl[1] of
		0	:	inw = 0d
		1	:	inw = 1d/(abs(w[0]) > eps)*Sign(w[0],/noz)
		else:	inw = 1d/(abs(w) > eps)*Sign(w,/noz)
	endcase
	if rfl and nfwl[1] le 1 then inw = replicate(inw,rdim)

	lfl = 1
	case nfwl[2] of
		0	:	usl = eps
		1	:	usl = (abs(Cast(l[0],5)) > eps)*Sign(l[0],/noz)
		else:	usl = (abs(Cast(l,5)) > eps)*Sign(l,/noz)
	endcase
	if rfl and nfwl[2] le 1 then usl = replicate(usl,rdim)
	inl = 1d/usl

	case n_elements(s) of
		0	:
		1	:	wpsq = wpsq + (2*s[0])^2*wqsq
		else:	message, 'Slope error must be a scalar!'
	endcase

	tsq = 1/(1 + wqsq*inw^2)
	u = wr + wqsq*(inl - inf)
	v = tsq*u
	rpsq = tsq*wpsq
	rqsq = usl^2*(v^2 + rpsq)/(tsq*wqsq)
	rr = rqsq/usl - v

	tsq = Cast(tsq,typ,typ)
	u = Cast(u,typ,typ)
	v = Cast(v,typ,typ)
	rpsq = Cast(rpsq,typ,typ)
	rqsq = Cast(rqsq,typ,typ)
	rr = Cast(rr,typ,typ)

	res = transpose([[rpsq],[rqsq],[rr]])

	return, res
end