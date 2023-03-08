Function Randcorr, seed, len, clen, exp_av = eav, bin_av = bav

;+
; NAME:
;		RANDCORR
; VERSION:
;		8.33
; PURPOSE:
;		Generates a set of correlated random variables.
; CATEGORY:
;		Mathematical function.
; CALLING SEQUENCE:
;		Result = RANDCORR(SEED, LEN [, CLEN] [, keywords])
; INPUTS:
;	SEED
;		A named variable containing the seed value for random number generation.
;		Does not need to be initialized prior to call.  For details see IDL
;		routine RANDOMN.
;	LEN
;		Length of the random vector to be produced.
; OPTIONAL INPUT PARAMETERS:
;	CLEN
;		Correlation length parameter.  Defaults to 1 (i.e. no correlation).  
;		The meaning of this parameter depends on the type of averaging (see 
;		below).  For exponential averaging CLEN is equivalent to exponential 
;		correlation length.  For binomial coefficient averaging CLEN is half
;		the width of the binomial distribution used.
; KEYWORD PARAMETERS:
;	/EXP_AV													|	at most one of
;		Switch.  If set, exponential averaging is used.		|	these two 
;		This is also the default.							|	keywords can be
;	/BIN_AV													|	set.  If none is
;		Switch.  If set, binomial coefficient averaging		|	then EXP_AV is
;		(a.k.a. "quasi gaussian") is used					|	used.
; OUTPUTS:
;		Returns a vector of normally distributed random numbers with nonzero
;		correlation (for CLEN > 1).
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Performs Q_averaging or binomial averaging of a standard normal 
;		sequence, using the routine QAVER (or SMOOTH_MM) from MIDL.  Also calls
;		BINCOEFF, CALCTYPE, CAST, DEFAULT, ONE_OF and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUN-2000 by Mati Meron.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 20-OCT-2011 by Mati Meron.  Added binomial averaging option.
;		Modified 5-OCT-2014 by Mati Meron.  Internal change.
;-

	typ = Calctype(0.,len,clen,def=4)
	eps = Toler(typ=typ)
	wclen = Default(clen,1.,lo=4) > 1.
	case (One_of(eav,bav) > 0) of
		0	:	begin
					plen = floor(alog(eps)/alog((1-1/wclen) > eps))
					tem = randomn(seed,len+plen)
					res = sqrt(2*wclen-1)*(Qaver(tem,wclen))[plen:*]
				end
		1	:	begin
					tem = randomn(seed,len+2*wclen)
					res = 2d^(2*wclen)/sqrt(Bincoef(4d*wclen,2*round(wclen)))* $
					(Smooth_mm(tem,2*wclen+1))[wclen:len+wclen-1]
				end
		else:	message, 'Unrecognizable input!'
	endcase

	return, Cast(res,typ,typ,/fix)
end