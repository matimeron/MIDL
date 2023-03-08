Function R_corr, dat, i, j, lag_range = lag, $
	lvalues = lval, lcount = lcon, variance = var, _extra = _e

;+
; NAME:
;		R_CORR
; VERSION:
;		4.2
; PURPOSE:
;		Calculates position correlations for time-series data.
; CATEGORY:
;		Data Analysis.
; CALLING SEQUENCE:
;		Result = R_CORR( DAT [I, J [ keywords]])
; INPUTS:
;	DAT
;		2D numeric array of position values.  The first dimension represents
;		particle number, the second dimension represents time.  Optionally a 1D
;		array can be used, in this case only self correlation can be calculated.
; OPTIONAL INPUT PARAMETERS:
;	I
;		First particle index, defaults to 0.
;	J
;		Second particle index, defaults to I.
; KEYWORD PARAMETERS:
;	LAG_RANGE
;		Range of time-lag values, see CM_CORR for details.
;	LVALUES
;		Optional output, see below.
;	LCOUNT
;		Optional output, see below.
;	VARIANCE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass extra keywords to CM_CORR through
;		DAT_CORR.
; OUTPUTS:
;		Returns the cross correlation (or auto-correlation for I = J) of the
;		positions of particles # I and J for the time lag values provided in
;		LAG_RANGE.
; OPTIONAL OUTPUT PARAMETERS:
;	LVALUES
;		Returns the set of lag values used in the calculation.  See CM_CORR.
;	LCOUNT
;		Returns the number of lag values used.  See CM_CORR.
;	VARIANCE
;		Returns the variance of the result.  See CM_CORR.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The values of I, J must be >= 0 and <= first_dimension_of_DAT.
; PROCEDURE:
;	 	R_CORR is simply a front-end for DAT_CORR.  See details there.
; MODIFICATION HISTORY:
;		Created 15-SEP-2001 by Mati Meron.
;-

	on_error, 1

	return, Dat_corr(dat,i,j,lag=lag,lval=lval,lcount=lcon,var=var,_extra=_e)
end