Function Gauss_moment, n, sigma = sig, width = wid

;+
; NAME:
;		GAUSS_MOMENT
; VERSION:
;		8.63
; PURPOSE:
;		Calculates the moments of a gaussian distribution (possibly truncated).
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = GAUSS_MOMENT (N [, SIGMA = SIG] [, WIDTH = WID])
; INPUTS:
;	N
;		Non-negative integer scalar, the order of the required moment.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SIGMA
;		Numeric, the sigma value(s) of the Gaussian distibution(s). Defaults
;		to 1.
;	WIDTH
;		Numeric, truncation width(s).  If not given, the distribution is not
;		truncated.
;
;		Note:	If both SIGMA and WIDTH are given, they must be both scalars,
;				one scalar and one vector, or both vectors of same length.
; OUTPUTS:
;		Returns the value(s) of the required moment.  The result is of the same
;		form and type as the higher of SIGMA and WIDTH (but no lower than
;		FLOAT).  However, even if the inputs are of type FLOAT or lower, if the
;		result is too large to be accomodated as FLOAT it'll be returned as
;		DOUBLE.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None, other then the joint restriction on SIGMA and WIDTH formats (see
;		above).
; PROCEDURE:
; 		Straightforward.  Calls CALCTYPE, CAST, CODIMS, DEFAULT, IGAMMA_MM and
; 		LNGAMMA_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 15-MAY-2018 by Mati Meron.
;-

	on_error, 1

	if n_elements(n) eq 1 then begin
		wn = round(n[0])
		hn = wn/2
		typ = Calctype(0.,sig,wid,def=4)
		wsig = Default(sig,1d,/dtyp)
		cod = Codims(wsig,wid,ninp=nin,dims=dim)
		if cod then begin
			if wn eq 2*hn then begin
				res = $
				exp(lngamma_mm(n+1d)-lngamma_mm(hn+1d)-hn*alog(2d)+n*alog(wsig))
				if nin eq 2 then begin
					arg = (wid/wsig)^2/8
					res = res*Igamma_mm(arg,hn+0.5d)/Igamma_mm(arg,0.5d)
				endif
			endif else res = replicate(0d,dim > 1)
			if max(res) gt (machar()).xmax then typ = typ > 5
			res = Cast(res,typ,typ,/fix)
		endif else message, 'Dimensional inconsistency!'
	endif else message, 'N input must be a scalar!'

	return, res
end