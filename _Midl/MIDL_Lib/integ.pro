Function Integ, x, y, ser, delta = dex, value_only = val, error = err

;+
; NAME:
;		INTEG
; VERSION:
;		8.35
; PURPOSE:
;		Integrates a function provided as an array of points.
; CATEGORY:
;		Mathematical Function (array).
; CALLING SEQUENCE:
;		Result = INTEG([X,] Y [, keywords])
; INPUTS:
;	Y
;		A vector containing the Y coordinates of the data.
;
;		Alternatively Y may be an array of the "standard 1D data type", i.e.
;		an [2,*] or [3,*] array, in which case the first column will be taken
;		as X values and the second as Y values.  If ERROR (see below) is 
;		provided with a variable name, a third column (if present) will be used
;		as Y-errors values.
; OPTIONAL INPUT PARAMETERS:
;	X
;		A vector containing the X coordinates of the data.  If not provided,
;		it is assumed that the X coordinates are equally spaced, with a default
;		default spacing of 1. (unless changed by the DELTA keyword).
;	SER
;		A vector containing the statistical errors of the data.  If provided, 
;		must be of same length as the X and Y vectors.
; KEYWORD PARAMETERS:
;	DELTA
;		Sets the spacing of the X coordinates of the data.  If the X coord.
;		are explicitly provided (see X input above) DELTA is ignored.
;	/VALUE_ONLY
;		Switch.  Normally INTEG returns the integral function of Y, as a vector
;		(see OUTPUTS below).  If VALUE_ONLY is set, only the full value of the
;		integral is returned as scalar.  This makes the execution faster.
;	ERROR
;		Optional output, see below.
; OUTPUTS:
;		Normally returns the integral function of Y, i.e. a vector whose i-th
;		entry is the integral of Y from X(0) to X(i) (and therefore the last
;		entry is the full integral of Y.  If the optional keyword VALUE_ONLY is
;		set, only the full integral is returned, as a scalar.
; OPTIONAL OUTPUT PARAMETERS:
;	ERROR
;		If Y-error data is present, either in the form of the SER parameter or
;		as a 3-rd column in a [3,N] input, returns the statistical error of the
;		full integral.  The result is a scalar.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The Y vector (and the X vector, if provided) must be of length >= 3.
; PROCEDURE:
;		Simpson integration, where the mid-interval points are obtained from
;		cubic interpolation using Neville's algorithm.
;		Uses CALCTYPE, DEFAULT, FPU_FIX and SPLIT_XY and from MIDL.  When 
;		evaluating errors, calls itself recussively.
; MODIFICATION HISTORY:
;		Created 20-FEB-1992 by Mati Meron.
;		Modified 15-SEP-1998 by Mati Meron.  Underflow filtering added.
;		Checked for operation under Windows, 25-JAN-2001, by Mati Meron.
;		Modified 20-SEP-2008 by Mati Meron.  Added standard data type (i.e.
;		[2-3,*] array processing capability.
;		Modified 15-DEC-2014 by Mati Meron.  Added the input SER and the 
;		keyword ERROR, to facilitate statistical error evaluation.
;-

	on_error, 1

	nv = Split_xy(x,y,ser,x_ret=wx,y_ret=wy,z_ret=wser,inpx=inx,inpz=erfl)
	if nv lt 3 then message, 'Insufficient data!'
	if not inx then wx = wx*Default(dex,1.)
	if erfl then begin
		ex = wx
		ey = wy
	endif
	wx = [2*wx[0] - wx[1], wx, 2*wx[nv-1] - wx[nv-2]]
	wy = [0., wy, 0.]
	dtyp = Calctype(wx,wy,1.,/strict)

	p = make_array(4, type = dtyp)
	q = p
	nw = nv + 1
	for i = 1, 3 do begin
		k = nw - i
		p[i] = wy[i]
		q[i] = wy[k]
		for j = i - 1, 1, - 1 do begin
			l = nw - j
			p[j] = ((wx[0]-wx[i])*p[j] + (wx[j]-wx[0])*p(j+1))/(wx[j]-wx[i])
			q[j] = ((wx[nw] - wx[k])*q[j] + (wx[l]-wx[nw])*q[j+1])/(wx[l]-wx[k])
		endfor
	endfor
	wy[0] = p[1]
	wy[nw] = q[1]

	xc = .5*(wx[2:nv] + wx[1:nv-1])
	q = make_array(nv - 1, 4, type = dtyp)
	for i = 0, 3 do begin
		k = nv - 2 + i
		q[*,i] = wy[i:k]
		for j = i - 1, 0, -1 do begin
			l = nv - 2 + j
			q[*,j] = ((xc - wx[i:k])*q[*,j] + (wx[j:l] - xc)*q[*,j+1]) $
				/(wx[j:l] - wx[i:k])
		endfor
	endfor

	res = [0,(wy[1:nv-1] + wy[2:nv] + 4*q[*,0])*(wx[2:nv] - wx[1:nv-1])/6.]
	if keyword_set(val) then res = total(res) else res = total(res,/cum)

	if erfl and arg_present(err) then begin
		ival = res[-1]
		dy = total(abs(ey))/nv
		if dy gt 0 then begin
			coe = make_array(nv,typ=dtyp)
			for i = 0, nv-1 do begin
				ty = ey
				ty[i] = ty[i] + dy
				coe[i] = Integ(ex,ty,/val)
			endfor
			coe = (coe - ival)/dy
			err = sqrt(total((coe*wser)^2))
		endif else err = 0
	endif else err = 0

	return, FPU_fix(res)
end