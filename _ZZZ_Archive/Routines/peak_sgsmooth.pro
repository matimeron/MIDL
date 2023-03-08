Function Peak_sgsmooth, x, y, ser, width = wid, deriv = nder, ret_range = ret, $
	_extra = _e

;+
; NAME:
;		PEAK_SGSMOOTH
; VERSION:
;		7.04
; PURPOSE:
;       Smoothing a spectrum or (optionally) its derivative.
; CATEGORY:
;       Data analysis.
; CALLING SEQUENCE:
;       Result = PEAK_SGSMOOTH ( X [, Y] [, SER] [, keywords])
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
;	WIDTH
;		Smoothing width.  Should be an odd integer, any other input is rounded
;		upwards to the nearest odd integer.
;	DERIV
;		Derivative order.  Default is zero, highest acceptable is 3.
;	RET_RANGE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to PEAK_STRIP, as well as
;		plotting keywords.  Not to be used directly.
;
;		The following keywords are accepted by PEAK_STRIP:
;
;		/SORT	:	Forces sorting of data.
;		/ROI	:	Allows interactive region of interest selection.	| only
;		RANGE	:	Indices range for region of interest.				| 1 out
;		XRANGE	:	x-values range for region of interest.				| of
;		SIGFACTOR:	Range definition in terms of standard deviations.	| these
;		AMPFACTOR:	Range definition in terms of fraction of amplitude.	|
;		BACKGROUND:	Background value(s).							| only 1 out
;		BRANGE	:	Range of channels for background estimation.	| of these
;		/SHOW	:	Forces display of spectrum and selected part.
; OUTPUTS:
;		Returns a smoothed version of the original spectrum, in the standard
;		format of a [2,*] or [3,*] (depending on input) array.  If the input
;		includes statistical errors, statistical errors for the smoothed
;		spectrum are included in the output.
; OPTIONAL OUTPUT PARAMETERS:
;	RET_RANGE
;		Returns the boundaries of the range used by PEAK_SGSMOOTH (in XRANGE
;		units) as a 2-element vector, [low_limit, high_limit].
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The number of data points cannot be smaller than the width used.
; PROCEDURE:
;		Uses standard (for equal x-spacing) or modified (for variable x spacing)
;		Savitzky-Golay procedure.  End points are approximated by propagating
;		the end values to the required distance.  Calls PEAK_STRIP.
;		Calls DEFAULT, DIF, JOIN_XY, SMOOTH_MM, SOLVE_LINSYS, SORPURGE,
;		SPLIT_XY, TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2003 by Mati Meron.
;		Modified 15-APR-2004 by Mati Meron.  Internal processing changes.
;		Modified 10-JUN-2004 by Mati Meron.  Added keyword RET_RANGE.
;		Renamed from PEAK_SMOOTH to PEAK_SGSMOOTH 5-MAY-2008 by Mati Meron.  
;		The name PEAK_SMOOTH refers now to a different routine.
;-

	on_error, 1

	ndmx = 3
	nder = Default(nder,0,/dtype) > 0
	if nder gt ndmx then message, 'Derivatives of order higher than ' + $
	string(ndmx,form='(i2)') + ' are currently not supported!'
	nord = 2 + nder/2

	sxy = Peak_strip(x,y,ser,ret=ret,_extra=_e)
	nxy = Split_xy(sxy,x=wx,y=wy,z=wser,inpz=erfl,/keep)
	if nxy gt 0 then begin
		dwid = ((nder + 1)/2*2 + 1l) > 3
		wwid = Default(wid,dwid,/dtype) > dwid
		m = wwid/2

		s = Sorpurge(wx,net=nl)
		if nl ge 2*m+1 then begin
			wx = wx[s]
			wy = wy[s]
			if erfl then wser = wser[s]
		endif else message, 'Data array too short for intended operation!'

		typ = Type(wx)
		eps = 4*Toler(wx)
		check = Dif(wx,2,/cen,/lin)

		if max(abs(check)) gt eps then begin
			lm = lindgen(m) + 1
			ewx = [(wx[0]-wx[1])*reverse(lm) + wx[0],wx,$
					(wx[nl-1]-wx[nl-2])*lm + wx[nl-1]]
			ewy = [replicate(wy[0],m),wy, replicate(wy[nl-1],m)]
			smy = wy
			if erfl then begin
				ewser = [replicate(wser[0],m),wser, replicate(wser[nl-1],m)]
				smser = wser
			endif

			alp = (pdx = Make_array(2*m+1,type=typ))
			war = Make_array(nord+1,nord+1,type=typ)
			kap = Make_array(nord+1,type=typ)
			kap[nder] = factorial(nder)

			for i = 0l, nl-1 do begin
				dx = ewx[i:i+2*m] - ewx[i+m]
				if max(abs(dx-pdx)) gt eps then begin
					for k = 0, nord do begin
						for l = 0, k do begin
							war[k,l] = (war[l,k] = total(dx^(k+l)))
						endfor
					endfor
					gam = Solve_linsys(war,kap,/svd)
					alp = 0*alp
					for j = 0, nord do alp = alp + gam[j]*dx^j
				endif
				smy[i] = total(ewy[i:i+2*m]*alp)
				if erfl then smser[i] = sqrt(total((ewser[i:i+2*m]*alp)^2))
				pdx = dx
			endfor
		endif else begin
			ddx = total(Dif(wx,/lin))/nl
			smy = Smooth_mm(wy,2*m+1,deriv=nder,/edg)/ddx^nder
			if erfl then begin
				dx = ddx*(lindgen(2*m+1) - m)
				war = Make_array(nord+1,nord+1,type=typ)
				kap = Make_array(nord+1,type=typ)
				kap[nder] = factorial(nder)
				for k = 0, nord do begin
					for l = 0, k do begin
						war[k,l] = (war[l,k] = total(dx^(k+l)))
					endfor
				endfor
				gam = Solve_linsys(war,kap,/svd)
				smser = sqrt(total(gam*kap)*wser^2)
			endif
		endelse
		res = Join_xy(wx,smy,smser)
	endif else message, 'Bad or missing input!'

	return, res
end