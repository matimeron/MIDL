Function Ref_eval, zval = zvl, dval = dvl, qval = qvl

;+
; NAME:
;		REF_FUN
; VERSION:
;		4.5
; PURPOSE:
;		Evaluates the relative (to Fresnell) reflectivity of a graded slab.
; CATEGORY:
;		Mathematical, x-ray specific.
; CALLING SEQUENCE:
;		Result = REF_EVAL( ZVAL = ZVL, DVAL = DVL, QVAL = QVL)
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ZVAL
;		Numeric vector, a set of the Z (depth) values where the density changes.
;	DVAL
;		Numeric vector, a set of density values D corresponding to ZVAL.  The
;		value DVAL[i] is the density in the interval (ZVAL[i-1] - ZVAL[i])
;
;		Note 1:	ZVAL can also be given as a [2,*] array.  In this case the
;				first column is taken as Z and the second as D.  D should not
;				be provided separately through DVAL in such case.
;		Note 2:	Z and D, when provided separately, must have same length.
;		Note 3:	Both Z[0] and D[0] are expected to be 0.  If Z[0] > 0, zeroes
;				are tagged on to the beginning of Z and D.  If D[0] > 0 (with
;				Z[0] = 0), an error results.
;		Note 4:	If neither Z nor D are given, zero values are substituted
;				for both.  The relative reflectivity in this case is 1 for all Q
;	QVAL
;		Numeric vector.  The Q values for which the reflectivity is to be
;		calculated.  Mandatory.
; OUTPUTS:
;		Returns the reflectivity function corresponding to the provided density
;		profile, for all input values of Q.  The dimension of the result is the
;		dimension of Q.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		None
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		As mentioned in Notes 1-4, above.
; PROCEDURE:
;		Serves as a front end to REF_FUN which performs the actual calculation
;		after the data has been prepared by REF_EVAL.  See REF_FUN for details.
;		Calls CALCTYPE, DIF and SPLIT_XY, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2003 by Mati Meron.
;-

	on_error, 1

	if n_elements(zvl) eq 0 and n_elements(dvl) eq 0 then begin
		z = (d = 0.)
		nz = (ins = 1)
	endif else nz = Split_xy(zvl, dvl, x = z, y = d, inpx = ins, /keep)

	if nz gt 0 and ins then begin
		typ = Calctype(0.,z,d)
		s = sort(z)
		z = z[s]
		d = d[s]
		if z[0] ge 0 then begin
			if z[0] gt 0 then begin
				z = [0,z]
				d = [0,d]
				nz = nz + 1
			endif
		endif else message, 'Z must be nonnegative!'
		if d[0] ne 0 then message, 'First density value must be 0!'
	endif else message, 'Inconsistent Z and/or density values!'

	nq = n_elements(qvl)
	if nq gt 0 then begin
		s = sort(qvl)
		q = qvl[s]
		if q[0] lt 0 then message, 'Q must be nonnegative!'
	endif else message, 'Missing or inconsistent Q values!'

	return, Ref_fun((Dif([d,1.]))[1:*],z,q)
end