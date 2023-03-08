Function Scan_PD_lcoo_old, snum, fnum, direction = dir, center = cnt, angle = ang, $
	pinhole = pnh, _extra = _e

;+
; NAME:
;		SCAN_PD_LCOO
; VERSION:
;		8.32
; PURPOSE:
;		Calculates 1D coordinates for a PD frame.
; CATEGORY:
;		SPEC/PD data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_LCOO( SNUM, DIRECTION = DIR, [,/ANGLE] [,_EXTRA = _E)
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Single frame number.  If given, the coordinates for this specific frame
;		are calculated.
; KEYWORD PARAMETERS:
;	DIRECTION
;		Specifies direction of coordinate axis.  Can be given either as string
;		input, with possible values of "hor" and "ver" (only first letter
;		matters) or as numerical input, 0 for horizontal, 1 for vertical.
;	CENTER
;		Location of the nominal center of the reflectivity peak, in	pixels.
;		Provided as a 2-element vector ([xy] order).  If not given defaults to
;		the value from the SPEC file (see SCAN_PD_CENTER).
;	/ANGLE
;		Switch.  Specifies that the output is in units of angle (Dth for
;		horizontal, Beta for vertical) instead of Q_xy or Q_z (the default).
;	PINHOLE
;		Specifies that horizontal pinhole angle transformations are to be used.
;		If set or provided with any positive value, the distance PDSDIST from
;		the structure FILDAT (see common block) is used in the transformation.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to LIMG_COO_CONV.
;		Of special interest are the keywords QTOT and SIGNED (see there).
; OUTPUTS:
;		Returns a vector of 1D-coordinates pertaining to the PD detector in scan
;		SNUM.  For vertical orientation the result in units of Q_z (inverse
;		angstrem) or the angle Beta (in degrees).  For horizontal orientation it
;		is Q_xy or the angle Dth.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Non, but if Dth (Beta)is not constant within the scan for horizontal
;		(vertical) direction, errors may occur. A warning is issued in this case
; PROCEDURE:
;		Straightforward, uses LIMG_COO_CONV, SCAN_GET_ANGS, SCAN_PD_FAROFF, 
;		SCAN_PD_FRAMES, SCAN_PD_CENTER and SCAN_Q_ANGS.  Calls DEFAULT, FPU_FIX,
;		ISNUM, STRMATCH_MM and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2008 by Mati Meron as a generalization of PD_ZCOO.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 30-MAR-2009 by Mati Meron.  Internal changes.
;		Modified 10-AUG-2009 by Mati Meron.  Added single frame capability.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 15-APR-2010 by Mati Meron.  Internal changes.
;		Modified 20-FEB-2011 by Mati Meron.  Added far detector support.
;		Modified 20-JUL-2011 by Mati meron.  Internal changes.
;		Modified 10-JAN-2012 by Mati Meron.  Added keyword CENTER.
;		Modified 25-OCT-2013 by Mati Meron.  Internal changes.
;		Modiifed 20-JUL-2014 by Mati Meron.  Added keyword PINHOLE.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if Isnum(dir) then dir = 0 > fix(dir) < 1 $
	else dir = Strmatch_mm(Default(dir,'',/dtyp),['hor','ver'],1,/nosub)
	if dir lt 0 then message, 'Direction must be given!'
	cent = Default(cnt,Scan_PD_center(snum,ori=ori,/sing,list=wsnum,_extra=_e))
	cur = fildat.scan[wsnum]
	warn = intarr(3)
	warn[2-dir] = 1
	if n_elements(fnum) gt 0 then begin
		wfnum = Scan_PD_frames(snum,fnum,nframes=nfnum,_extra=_e)
		if nfnum eq 1 then angs = Scan_q_angs(snum,wfnum,/xrev) $
		else message, 'Only single frame or no frame allowed!'
	endif else begin
		wfnum = 0
		angs = Scan_get_angs(snum,warn=warn)
	endelse
	dist = cur.pdist + (cur.g_l)[3]*(1/cos(!dtor*angs[1]) - 1)
	dim = cur.pddim[(ori+dir) mod 2]
	xyz = lindgen(dim)
	check = (Wherinstruct((['hro','vro'])[dir],_e))[0]
	if check ge 0 then xyz = xyz[(_e.(check))[0] > 0:(_e.(check))[1] < (dim-1)]
	if cur.pdfar then cent = cent - Scan_PD_faroff(snum,wfnum,alp=angs[0]) $
	else if keyword_set(pnh) then pdist = cur.pdsdist
	res = Limg_coo_conv(xyz,dist=dist,pdist=pdist,ipix=cur.pdpix,cen=cent[dir],$
	alp= angs[0], bet= angs[1], dth= angs[2], lam= cur.lambda, dir= dir, /xrev,$
	qval= 1-keyword_set(ang), _extra= _e)

	return, FPU_fix(res)
end