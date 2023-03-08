Function PQR_prop, psq, qsq, r, s = s, w = w, f = f, l = l, $
	ene= ene, blam= lam, tscal= sca, rpsq = rpsq, rqsq = rqsq, rr = rr, tsq= tsq

;+
; NAME:
;		PQR_PROP
; VERSION:
;		8.61
; PURPOSE:
;		Evaluates beam propagation in PQR representation.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = PQR_PROP, PSQ [, QSQ, R] [, F = F, W = W, L = L], [keywords].
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
;	S
;		Slope error.
;	W
;		Aperture size.
;	F
;		Focal Length.
;	L
;		Propagation distance.
;
;		Note 1:	For all 4 parameters above, the default is "doesn't apply".
;
;		Note 2:	The formalism used here decouples "transverse parameters",
;				such as S and W, and "longitudinal parameters", such as F and L.
;				Thus, they need not be given in same units, but they need to be
;				consistent with the parameters used in the evaluation of PSQ,
;				QSQ and R.  Usually the longitudinal parameters are given in
;				meters and the transverse ones in mm and mr (for angles).  This
;				means that the slope error S, which is a transverse parameter,
;				needs to be given in mr (not microradians).
;
;		Note 3:	When the input is a [3,N] array, S, W, F and L, if defined, must
;				be scalars or vectors of length N.
;				When the input is a [6,N] array, S, W, F and L must be scalars,
;				vectors of length 2, or [2,N] arrays.
;	ENE
;		The photon energy values, in keV corresponding to the P,Q,R	|
;		inputs.	Given either as scalar, or a vector of length N, 	| At most
;		where N is the length of the inputs.						| of these
;	BLAM															| two may
;		The beam wavelength(s) in Angstrom, corresponding to the	| be given.
;		P,Q,R inputs.  Same  input format as ENE (above).			|
;		
;		Note:	The ENE or PWL inputs are only used for evaluating diffraction
;				broadening in apertures.  If W (aperture size(s)) is not used
;				in the call, ENE / PWL have no effect.  If W is used but ENE or
;				PWL are not provided, there is no diffraction broadening.
;	TSCAL
;		Character string specifying whether the "tranverse parameters" are given
;		using the SIGMA (rms size) HALF WIDTH or FULL WIDTH (rms*sqrt(4*pi))
;		standard.  Input, if given, must be one of "sigma", "half", "full" (only
;		first 2 characters are needed).  Default is SIGMA.
;		SCALE is only relevant to diffraction broadening, has no effect
;		otherwise.
;	RPSQ
;		Optional output, see below.
;	RQSQ
;		Optional output, see below.
;	RR
;		Optional output, see below.
;	TSQ
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
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Follows the formalism in the "PQR optical parameters" writeup.  Details
;		elsewhere.  Calls PQR_PRE, PQR_DF, PQR_WX, PQR_FC, PQR_ZP and PQR_POST.
;		Calls ARREQ, CAST, ISNUM, FPU_FIX and ONE_OF, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUN-2016 by Mati Meron.
;		Modified 5-AUG-2016 by Mati Meron.  Optional output option changes.
;		Modified 20-DEC-2017 by Mati Meron.  Optional output option changes.
;		Modified 25-DEC-2017 by Mati Meron.  Added SLER for slope error
;		effects.  Previously these effects were evaluated by a separate routine.
;		Rewritten 5-MAR-2018 by Mati Meron.  Added diffraction broadening option
;		through the keywords ENE, BLAM, TSCAL.
;-

	on_error, 1

	mult = 1e-4
	wha = PQR_pre(psq,qsq,r,fir=fir,sec=sec,length=len,type=typ)
	case One_of(ene,lam) of
		-1	:	wlam = 0
		0	:	wlam = mult*Cast(!srcon.conv/ene,typ,typ)
		1	:	wlam = mult*lam
	endcase

	case wha of
		0	:	message, 'Missing input(s)!'
		1	:	begin
					if Isnum(s) then begin
						ws = reform(s)
						siz = size(ws)
						if siz[0] le 1 then fir = PQR_df(fir,2*ws) $
						else message, 'Bad slope error dimensions!
					endif
					if Isnum(w) then begin
						ww = reform(w)
						siz = size(ww)
						if siz[0] le 1 then $
						fir = PQR_wx(fir,ww,lam=wlam,sca=sca,tsq=tsq) $
						else message, 'Bad slit dimensions!
					endif else tsq = replicate(1.,len)
					if Isnum(f) then begin
						wf = reform(f)
						siz = size(wf)
						if siz[0] le 1 then fir = PQR_fc(fir,wf) $
						else message, 'Bad focal length dimensions!
					endif
					if Isnum(l) then begin
						wl = reform(l)
						siz = size(wl)
						if siz[0] le 1 then fir = PQR_zp(fir,wl) $
						else message, 'Bad propagation length dimensions!
					endif
					res = PQR_post(fir,typ=typ,rpsq=rpsq,rqsq=rqsq,rr=rr)
					tsq = Cast(tsq,typ,typ)
				end
		2	:	begin
					if Isnum(s) then begin
						ws = reform(s)
						siz = size(ws)
						if Arreq(siz[0:1],[1,2]) then siz[0] = 2
						if siz[0] le 2 then begin
							if siz[0] le 1 then begin
								fir = PQR_df(fir,2*ws)
								sec = PQR_df(sec,2*ws)
							endif else begin
								fir = PQR_df(fir,2*ws[0,*])
								sec = PQR_df(sec,2*ws[1,*])
							endelse
						endif else message, 'Bad slope error dimensions!
					endif
					if Isnum(w) then begin
						ww = reform(w)
						siz = size(ww)
						if Arreq(siz[0:1],[1,2]) then siz[0] = 2
						if siz[0] le 2 then begin
							if siz[0] le 1 then begin
								fir = PQR_wx(fir,ww,lam=wlam,scale=sca,tsq=ftsq)
								sec = PQR_wx(sec,ww,lam=wlam,scale=sca,tsq=stsq)
							endif else begin
								fir = PQR_wx(fir,ww[0,*],lam=wlam,scale=sca,$
									tsq=ftsq)
								sec = PQR_wx(sec,ww[1,*],lam=wlam,scale=sca,$
									tsq=stsq)
							endelse
						endif else message, 'Bad slit dimensions!
					endif else ftsq = (stsq = replicate(1.,len))
					if Isnum(f) then begin
						wf = reform(f)
						siz = size(wf)
						if Arreq(siz[0:1],[1,2]) then siz[0] = 2
						if siz[0] le 2 then begin
							if siz[0] le 1 then begin
								fir = PQR_fc(fir,wf)
								sec = PQR_fc(sec,wf)
							endif else begin
								fir = PQR_fc(fir,wf[0,*])
								sec = PQR_fc(sec,wf[1,*])
							endelse
						endif else message, 'Bad focal length dimensions!
					endif
					if Isnum(l) then begin
						wl = reform(l)
						siz = size(wl)
						if Arreq(siz[0:1],[1,2]) then siz[0] = 2
						if siz[0] le 2 then begin
							if siz[0] le 1 then begin
								fir = PQR_zp(fir,wl)
								sec = PQR_zp(sec,wl)
							endif else begin
								fir = PQR_zp(fir,wl[0,*])
								sec = PQR_zp(sec,wl[1,*])
							endelse
						endif else message, 'Bad propagation length dimensions!
					endif
					res = PQR_post(fir,sec,typ=typ,rpsq=rpsq,rqsq=rqsq,rr=rr)
					tsq = Cast([ftsq,stsq],typ,typ)
				end
	endcase

	return, FPU_fix(res)
end