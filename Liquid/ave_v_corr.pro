Function Ave_v_corr, dat, nei, full = ful, lag = lag, $
	variance = var, error = err, val_nei = wnei, val_lag = wlag

;+
; NAME:
;		AVE_V_CORR
; VERSION:
;		4.2
; PURPOSE:
;		Calculates velocity correlations, averaged over multiple particles.
; CATEGORY:
;		Data Analysis
; CALLING SEQUENCE:
;		Result = AVE_V_CORR( DAT [, NEI] [,Keywords])
; INPUTS:
;	DAT
;		Data array.  A 2D numeric array with first index representing particle
;		number and second index representing time slice.
;	NEI
;		The neighbor nubers(s) for which the averaging is to be performed.
; OPTIONAL INPUT PARAMETERS:
;	NEI
;		The neighbor nubers(s) for which the averaging is to be performed.
;		The meaning of the averaging is this of Binhua's Eq. 3.  Thus, if
;		Nei = n then the averaged correlation is given by
;
;			Sum(Corr(V_i,V_(i+n)))/P(n)
;
;		where P(n) is the number of velocity pairs appearing in the summation.
;
;		NEI can be given in a variety of forms, as follows
;
;			1)	Scalar value (example: 3) : In this case average correlation is
;				evaluated *only* for the value given.  For the NEI in the
;				example this means only for N = 3.
;			2)  One element vector (example : [3]) : In this case average
;				correlations are evaluated for all values of N between 0 and
;				the value provided, inclusive.  Thus, if NEI is given as [3],
;				average correlations for N = 0, 1, 2, 3 are calculated.
;			3)	Two element vector (example : [2,5]) : In this case the
;				evaluation is done for all values of N starting with the first
;				element of NEI and ending with the second.  In the example
;				given this means N = 2, 3, 4, 5.
;			4)	Multi-element vector (example: [1,4,5,9]) : In this case the
;				evaluation is done for the values provided, i.e. for
;				N = 1, 4, 5, 9 in the example
;
;		If not provided, NEI defaults to zero.  The correlation is trivially 1
;		in this case.
; KEYWORD PARAMETERS:
;	/FULL
;		Switch.  If set, NEI is set to (NUM - 1) where NUM is the number of
;		particles present.  In this case the evaluation is performed for
;		the full set of N = 0, 1, ... (NUM-1).
;	LAG
;		An optional time lag to be used in the correlation calculation.  LAG
;		Can be given as integer scalar, 1, 2, or multielement vector, following
;		same rules as NEI.  The default value is 0.
;	VARIANCE
;		Optional output, see below.
;	ERROR
;		Optional output, see below.
;	VAL_NEI
;		Optional output, see below.
;	VAL_LAG
;		Optional output, see below.
; OUTPUTS:
;		Returns the values of velocity correlations for all "neighbor numbers"
;		(and, optionally, time lags) specified, averaged over all the particles
;		in the data set.  The general output is a 2D array, RES[M,N], where
;		M is the number of "neighbor values" specified and N the number of time
;		lags.  Thus, RES[*,i] is the set of correlations for all neighbor
;		numbers at time i, while res[j,*] is the set for neighbor number j and
;		all times specified.  If only a single value of time lag (usually 0) is
;		used, the trailing dimension of 1 is supressed.
; OPTIONAL OUTPUT PARAMETERS:
;	VARIANCE
;		Returns an array of variances for all the results in the output.  An
;		Array of same size as the output.
;	ERROR
;		Returns an array of statistical errors for all the results in the
;		output.  This is simply sqrt(VARIANCE).
;	VAL_NEI
;		Returns a vector of all the values of NEI used in the calculation.
;	VAL_LAG
;		Returns a vector of all the values of LAG used in the calculation.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The values of NEI must be in the range [0:first_dimension_of_DAT -1]
;		The values of NEI must be in the range [0:second_dimension_of_DAT -1]
; PROCEDURE:
;		Calls V_CORR to perform the correlations. Also calls DEFAULT and
;		MAKE_RANGE from MIDL.
; MODIFICATION HISTORY:
;		Created 10-FEB-2002 by Mati Meron.
;-

	on_error, 1

	siz = size(dat)
	if siz[0] ne 2 then message, 'Improper data!'

	if keyword_set(ful) then wnei = [siz[1] - 1] $
	else wnei = Default(nei,0l,/dtyp) > 0
	if max(wnei) ge siz[1] then message, 'Neighbor number exceeds maximum!'
	wnei = Make_range(wnei)

	wlag = Make_range(lag,def=0)

	ncor = siz[1] - wnei
	nne = n_elements(wnei)
	nla = n_elements(wlag)
	res = (var = fltarr(nne,nla))

	for i = 0, nne-1 do begin
		corr = (cvar = fltarr(ncor[i],nla))
		for j = 0l, ncor[i]-1 do begin
			corr[j,*] = V_corr(dat,j,j+wnei[i],lag=wlag,var=pvar)
			cvar[j,*] = pvar
		endfor
		res[i,*] = total(corr,1)/ncor[i]
		var[i,*] = total(cvar,1)/ncor[i]^2
	endfor
	err = sqrt(var)

	return, res
end