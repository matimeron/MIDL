Function Peak_scamom, x, y, ser, order = ord, error = err

;+
; NAME:
;		PEAK_SCAMOM
; VERSION:
;		4.9
; PURPOSE:
;       Calculating data moments.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_SCAMOM( [X,] Y [, SER] [, keywords])
; INPUTS:
;	X
;		Numeric, A vector (scalar is considered to be a vector of length 1), an
;		[2,*] array or a [3,*] array.
; OPTIONAL INPUT PARAMETERS:
;	Y
;		Numeric, acceptable only when X is a vector, in which case Y has to be
;		a vector of same length.
;	SER
;		Numeric, same restrictions as for Y.  Taken as the values of the
;		statistical y-errors.
;
;		Note:	If only one input is provided then:
;				1)	If it is a vector, it is taken to be Y and X is generated
;					internally with a spacing of 1.  SER is taken as 0.
;				2)	If it is a [2,*] array, it is split into X and Y vectors.
;					SER is taken as 0.
;				3)	If it is a [3,*] array, it is split into X, Y and SER.
; KEYWORD PARAMETERS:
;	ORDER
;		Numeric scalar, the order of the moment to be calculated.
;	ERROR
;		Optional output, see below.
; OUTPUTS:
;		Returns the calculated moment value, *scaled* by the appropriate power
;		of the second moment.  Thus, the result for a given order p is
;
;			M_p/(M_2)^(p/2)
;
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		Returns the statistical error of the calculated moment.  The calculation
;		requires the values of SER (see above) being provided, directly or
;		indirectly.  If these values are missing, the returned error is zero.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls PEAK_MOMENT and PEAK_STSUMS.
; MODIFICATION HISTORY:
;		Created 10-SEP-2003 by Mati Meron.
;		Modified 15-APR-2004 by Mati Meron.  Internal processing changes.
;-

	on_error, 1

	nor = round(ord)
	nxy = Peak_stsums(x,y,ser,x=wx,es=ses,erfl=erfl)
	if nxy gt 0 then begin
		if nor gt 2 then begin
			nh = nor/2.
			wx = wx - Peak_moment(x,y,ser,ord=1,/ori,/int)
			mn = Peak_moment(x,y,ser,ord=nor,/int)
			mt = Peak_moment(x,y,ser,ord=2,/int)
			res = mn/mt^nh
			if erfl then begin
				tem = ((nh-1)*mn + wx^nor -nh*mn/mt*wx^2 - $
					nor*wx*Peak_moment(x,y,ser,ord=nor-1,/int))/mt^nh
				err= sqrt(total((tem*ses)^2))
			endif else err = 0.
		endif else begin
			if nor eq 1 then res = 0. else res = 1.
			err = 0.
		endelse
	endif else message, 'Bad or missing input!'

	return, res
end