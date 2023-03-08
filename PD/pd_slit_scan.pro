Pro PD_slit_scan, snum, fnum, slit =sli, xy_pix= xpx, z_pix= zpx, result= res, $
	_extra= _e

;+
; NAME:
;		PD_SLIT_SCAN
; VERSION:
;		8.02
; PURPOSE:
;		Performs the equivalent of a slit scan across a PD image.
; CATEGORY:
;		SPEC PD data processing.
; CALLING SEQUENCE:
;		Result = PD_SLIT_SCAN( SNUM, FNUM [, keywords])
; INPUTS:
;	SNUM
;		Single Scan number.
;	FNUM
;		Frame number within the scan.
;
;		Note:	Slit scan should be performed within a single frame.  If
;				multiple frames are given or FNUM is not provided (translating
;				to "all the frames") a warning will be printed to the screen.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SLIT
;		A 2 element vector specifying electronic slit dimensions, in pixels, in
;		[horizontal,vertical] order.
;	XY_PIX													|
;		The horizontal location (in pixels) of the "slit"	|	One and only
;		for vertical scans.									|	one of these
;	Z_PIX													|	two parameters
;		The vertical location (in pixels) of the "slit"		|	must be given.
;		for horizontal scans.								|
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Standard output is graphics only, a plot of the slit scan values.
;		Additional output is provided through RESULT.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the scan result in the standard [3,*] form:
;
;		Column 0	:	Pixel numbers (vertical or horizontal)
;		Column 1	:	Sums of counts within slit.
;		Column 2	:	Data errors.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward summation.  Calls SCAN_PD_FRAMES, SCAN_PD_READ, 
;		SCAN_SHOW and SPEC_FILE_CHECK.  Also calls ONE_OF and WHERINSTRUCT, 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 30-MAR-2009 by Mati Meron.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;-

	on_error, 1

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	wfnum = Scan_PD_frames(wsnum,fnum,nframes=nfnum,_extra=_e)
	if nfnum gt 1 then message, 'Multiple frames specified', /con

	if n_elements(sli) eq 2 then hs = round(sli)/2 $
	else message, 'Slit needs two elements!'
	dat = Scan_PD_read(wsnum,wfnum,/raw,title=tit,_extra=_e)
	lims = transpose([[0,0],[size(dat,/dim)-1]])

	hcheck = (Wherinstruct('hro',_e))[0]
	if hcheck ge 0 then lims[*,0] = (_e.(hcheck))
	vcheck = (Wherinstruct('vro',_e))[0]
	if vcheck ge 0 then lims[*,1] = (_e.(vcheck))

	hv = One_of(zpx,xpx,val=val)
	if hv ge 0 then begin
		nind = lims[1,hv] - lims[0,hv] - 2*hs[hv] + 1
		vh = 1 - hv
		if (val ge (lims[0,vh] + hs[vh])) and (val le (lims[1,vh] - hs[vh])) $
		and (nind gt 0) then begin
			ind = hs[hv] + lindgen(nind)
			lo = ind - hs[hv]
			hi = ind + hs[hv]
			res = fltarr(3,nind)
			res[0,*] = ind + lims[0,hv]
			aind = replicate(val - lims[0,vh],nind)
			alo = aind - hs[vh]
			ahi = aind + hs[vh]

			if hv then begin
				hlo = alo
				hhi = ahi
				vlo = lo
				vhi = hi
			endif else begin
				hlo = lo
				hhi = hi
				vlo = alo
				vhi = ahi
			endelse

			napfl = not ((Scan_field_read(wsnum,'pdstat'))[0]- 3)/2
			for i = 0, nind-1 do begin
				res[1,i] = total(dat[2,hlo[i]:hhi[i],vlo[i]:vhi[i]])
				if napfl then res[2,*] = sqrt(res[1,i]) $
				else res[2,i]= sqrt(total(dat[3,hlo[i]:hhi[i],vlo[i]:vhi[i]]^2))
			endfor
		endif else message, 'Slit or location out of range!'
	endif else message, 'Either XY or Z must be given!'

	tloc = ' at ' + ['vertical','horizontal'] + ' location of '
	stit = 'Slit size: [ ' + strjoin(string(2*hs+1,form='(i0)'),', ') + $
	'] pix.' + tloc[hv] + string(val,form='(i0)') + ' pix.' 
	Scan_show, res, xtit = '(pix.)', title = tit, subtit = stit, ymarg = [6,2],$
	_extra = _e, /count

	return
end