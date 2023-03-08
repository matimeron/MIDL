Function Dat_corr, dat, i, j, lag_range = lag, velocity = vel, $
	lvalues = lval, lcount = lcon, variance = var, _extra = _e

;+
; NAME:
;		DAT_CORR
; VERSION:
;		4.2
; PURPOSE:
;		Calculates position and/or velocity correlations for tie-series data.
; CATEGORY:
;		Data Analysis.
; CALLING SEQUENCE:
;		Result = DAT_CORR( DAT [I, J [ keywords]])
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
;	/VELOCITY
;		Switch.  If set, the correlation of velocities (i.e. differences of
;		positions) is calculated.
;	LVALUES
;		Optional output, see below.
;	LCOUNT
;		Optional output, see below.
;	VARIANCE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass extra keywords to CM_CORR.
; OUTPUTS:
;		Returns the cross correlation (or auto-correlation for I = J) of the
;		positions (or velocities if /VELOCITY is set) of particles # I and J
;		for the time lag values provided in LAG_RANGE.
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
;		Uses CLEAN_DATA to prepare the data and generate the appropriate masks,
;		then calls on CM_CORR for the actual calculation.  Also call DEFAULT
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2001 by Mati Meron.
;-

	on_error, 1
	i = Default(i,0l,/dtyp) > 0
	j = Default(j, i,/dtyp) > 0
	siz = size(dat)

	case siz[0] of
		0	:	message, 'Bad or missing data!'
		1	:	if i ne 0 or j ne 0 then message, 'Bad i/j values!'
		2	:	if max([i,j]) ge siz[1] then message, 'Excessive i/j values!'
		else:	message, 'Number of dimensions cannot exceed 2!'
	endcase

	tem = Clean_data(dat,mask=msk,vel=vel)
	res = Cm_corr(tem[i,*],tem[j,*],xmask=msk[i,*],ymask=msk[j,*],lag=lag,$
		lval=lval, lcount = lcon, variance = var, _extra = _e)

	return, res
end