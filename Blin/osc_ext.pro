Pro Osc_ext, dat, width= wid, minind= mnd, maxind= mxd, number= nos, quiet= qui

;+
; NAME:
;		OSC_EXT
; VERSION:
;		8.42
; PURPOSE:
;		Locates minima and maxima in oscillatory data.
; CATEGORY:
;		Mathematical, Liquid compression specific.
; CALLING SEQUENCE:
;		OSC_EXT, DAT [, keywords]
; INPUTS:
;	DAT
;		Data vector.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	WIDTH
; 		Scalar, the width to be used with data smoothing.  Default is 29.
; 	MININD
; 		Optional output, see below.
; 	MAXIND
; 		Optional output, see below.
; 	NUMBER
; 		Optional output, see below.
;	/QUIET
;		Switch.  If set, warning messages are supressed.
; OUTPUTS:
; 		Returned through the optional output parameters.
; OPTIONAL OUTPUT PARAMETERS:
; 	MININD
; 		Returns a vector of the DAT minima values.
; 	MAXIND
; 		Returns a vector of the DAT maxima values.
; 	NOS
; 		Returns the smaller of the lengths of MININD and MAXIND.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward, smoothes the data and locates maxima and minima.  
; 		Repeats the process with growing threshold till no further changes 
; 		occur.  Calls DEFAULT, DIF, EXTREMA and SMOOTH_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2014 by Mati Meron.
;-

	on_error, 1

	mnd = (mxd = [])
	nos = 0l
	quifl = keyword_set(qui)
	wwid = Default(wid,29,/dtyp)
	wdat = Smooth_mm(dat,wwid,/edg)	
	difs = abs(Dif(wdat,/lin))
	dum = where(difs gt 0, ndif)
	if ndif gt 0 then begin
		lstep = min(difs[dum],max=hstep)
		thre = (step = (2*lstep > 1e-2*hstep))
		ext = Extrema(wdat,sig=sig,thre=thre,num=num)
		done = 0
		repeat begin
			thre = thre + step
			if thre lt hstep then begin
				eext = Extrema(wdat,sig=ssig,thre=thre,num=nnum)
				if nnum ne num then begin
					ext = eext
					sig = ssig
					num = nnum
				endif else done = 1
			endif else break
		endrep until done
		if done then begin
			mnd = ext[where(sig eq -1,nmnd)]
			mxd = ext[where(sig eq 1,nmxd)]
			nos = nmnd < nmxd
		endif else if not quifl then message, "Can't uniquely locate!", /con
	endif else if not quifl then message, 'Constant input, no extrema!',/con

	return
end